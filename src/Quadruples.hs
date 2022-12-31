module Quadruples where

import Control.Monad.RWS
import Data.Data
import Data.Foldable
import Data.Map
import Data.Maybe
import Data.Set
import Latte.Abs qualified as Latte
import Latte.ErrM
import Text.Printf (printf)

-- module which translates code to internal representation

type Loc = Int

data Arg = Var Loc | Const Integer | None | Target LabelName

instance Show Quadruples.Arg where
  show :: Quadruples.Arg -> String
  show (Var loc) = "%" ++ show loc
  show (Const i) = show i
  show None = "_"
  show (Target label) = label

data Op
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  | And
  | Or
  deriving (Show, Data, Typeable)

data CmpOp = Eq | Neq | Lt | Gt | Le | Ge deriving (Show, Data, Typeable)

data SingOp = Neg | Not deriving (Show, Data, Typeable)

data Quadruple
  = BinOp Op Arg Arg Loc
  | SingleArgOp SingOp Arg Loc
  | CmpBinOp CmpOp Arg Arg Loc
  | Assign Arg Loc
  | Get Int Loc
  | Put Int Loc
  | Call String [Arg]
  | Label LabelName
  | Jump LabelName
  | JumpIf Arg LabelName LabelName
  | ReturnVoid
  | Return Arg
  | Nop

instance Show Op where
  show (Label label) = label ++ ":"
  show op = show $ toConstr op

type LabelName = String

instance Show Quadruple where
  show :: Quadruple -> String
  show (BinOp op arg1 arg2 loc) = printf "%s = %s %s %s" (show $ Var loc) (show arg1) (show op) (show arg2)
  show (SingleArgOp op arg loc) = printf "%s = %s %s" (show $ Var loc) (show op) (show arg)
  show (CmpBinOp op arg1 arg2 loc) = printf "%s = %s %s %s" (show $ Var loc) (show arg1) (show op) (show arg2)
  show (Assign arg loc) = printf "%s = %s" (show $ Var loc) (show arg)
  show (Get i loc) = printf "%s = GetArg i32* %%0, i32 %d" (show $ Var loc) i
  show (Put i loc) = printf "PutArg i32* %%0, i32 %d, i32 %s" i (show $ Var loc)
  show (Call name args) = printf "Call %s %s" name (show args)
  show (Label label) = printf "%s:" label
  show (Jump label) = printf "Jump %s" label
  show (JumpIf arg label1 label2) = printf "JumpIf %s %s %s" (show arg) label1 label2

data Env = Env {nextLoc :: Loc, varMap :: Data.Map.Map String Loc}

data QFuncData = QFuncData
  { fnNames :: Data.Set.Set String,
    -- where on the stack the argument of current function is
    funcArgs :: Map String Integer
  }

type Context = RWST QFuncData [Quadruple] Env Err

translate :: Latte.Program -> Err [Quadruple]
translate p = do
  (_, _, quadruples) <-
    runRWST
      (transProgram p)
      (QFuncData Data.Set.empty Data.Map.empty)
      (Env 1 Data.Map.empty)
  return quadruples

transProgram :: Latte.Program -> Context ()
transProgram (Latte.Program loc topDefs) =
  mapM_ transTopDef topDefs

failure :: Show a => a -> Context ()
failure x = fail $ "Undefined case: " ++ show x

transTopDef :: Latte.TopDef -> Context ()
transTopDef x = case x of
  Latte.FnDef _ type_ (Latte.Ident ident) args block -> do
    let argsMap = transArgs args
    tellLabel ident
    res <-
      local (\env -> env {funcArgs = argsMap}) $
        transBlock block
    -- if the function does not return, return void
    unless res $ tell [Quadruple ReturnVoid None None None]

-- | Returns the map of argument names and their location on stack
transArgs :: [Latte.Arg] -> Map String Integer
transArgs args = Data.Map.fromList $ zip (Prelude.map (\(Latte.Arg _ _ (Latte.Ident ident)) -> ident) args) [1 ..]

transBlock :: Latte.Block -> Context Bool
transBlock (Latte.Block _ stmts) = do
  env <- get
  isRet <-
    foldM
      ( \ret stmt ->
          if ret
            then return True
            else transStmt stmt
      )
      False
      stmts
  -- return to the previous environment
  put env
  return isRet

transBlockLabels :: Latte.Block -> LabelName -> LabelName -> Context Bool
transBlockLabels block inLabel outLabel = do
  tellLabel inLabel
  isRet <- transBlock block
  unless isRet $ tell [Quadruple Jump (Target outLabel) None None]
  return isRet

-- | Translates a statement to a list of quadruples
-- returns true, if the statement is a return statement
transStmt :: Latte.Stmt -> Context Bool
transStmt x = case x of
  Latte.Empty _ -> return False
  Latte.BStmt _ block -> transBlock block
  Latte.Decl _ type_ items -> do
    mapM_ transItem items
    return False
  Latte.Ass _ (Latte.Ident ident) expr -> do
    res <- transExpr expr
    var <- getVarLoc ident
    tell [Quadruple Assign res None var]
    return False
  Latte.Incr _ (Latte.Ident ident) -> do
    var <- getVarLoc ident
    tell [Quadruple Add var (Const 1) var]
    return False
  Latte.Decr _ (Latte.Ident ident) -> do
    var <- getVarLoc ident
    tell [Quadruple Sub var (Const 1) var]
    return False
  Latte.Ret _ expr -> do
    res <- transExpr expr
    tell [Quadruple Return res None None]
    return True
  Latte.VRet _ -> do
    tell [Quadruple ReturnVoid None None None]
    return True
  Latte.Cond _loc expr stmt -> do
    -- generate labels
    blockLabel <- newLabel
    endLabel <- newLabel
    -- process the condition
    res <- transExpr expr
    tell [Quadruple JumpIf res (Target blockLabel) (Target endLabel)]
    transBlockLabels (makeBlock stmt) blockLabel endLabel
    tell [Quadruple (Label endLabel) None None None]
    return False
  Latte.CondElse _ expr stmt1 stmt2 -> do
    -- generate labels
    block1Label <- newLabel
    block2Label <- newLabel
    endLabel <- newLabel
    -- process the condition
    res <- transExpr expr
    tell [Quadruple JumpIf res (Target block1Label) (Target block2Label)]
    isRet1 <- transBlockLabels (makeBlock stmt1) block1Label endLabel
    isRet2 <- transBlockLabels (makeBlock stmt2) block2Label endLabel
    let isRet = isRet1 && isRet2
    unless isRet $ tellLabel endLabel
    return isRet
  Latte.While _ expr stmt -> do
    bodyLabel <- newLabel
    condLabel <- newLabel
    afterLabel <- newLabel
    tell [Quadruple Jump (Target condLabel) None None]
    transBlockLabels (makeBlock stmt) bodyLabel condLabel
    tellLabel condLabel
    res <- transExpr expr
    tell [Quadruple JumpIf res (Target bodyLabel) (Target afterLabel)]
    -- process more code only if the while loop does not return
    tellLabel afterLabel
    return False
  Latte.SExp _ expr -> do
    transExpr expr
    return False

tellLabel :: LabelName -> Context ()
tellLabel label = tell [Quadruple (Label label) None None None]

makeBlock :: Latte.Stmt -> Latte.Block
makeBlock stmt = case stmt of
  Latte.BStmt _ block -> block
  _ -> Latte.Block (hasPosition stmt) [stmt]

transItem :: Latte.Item -> Context ()
transItem x = case x of
  Latte.NoInit _ (Latte.Ident ident) -> do
    var <- newVar ident
    tell [Quadruple Assign (Const 0) None var]
    return ()
  Latte.Init _ (Latte.Ident ident) expr -> do
    var <- newVar ident
    res <- transExpr expr
    tell [Quadruple Assign res None var]
    return ()

-- | Creates a new variable in the context
-- increases its location if it already exists
newVar :: String -> Context Quadruples.Arg
newVar ident = do
  (Env freeLoc map) <- get
  put $ Env (freeLoc + 1) (Data.Map.insert ident freeLoc map)
  return $ Var freeLoc

newLabel :: Context LabelName
newLabel = do
  (Env freeLoc map) <- get
  put $ Env (freeLoc + 1) map
  return $ "label" ++ show freeLoc

transExpr :: Latte.Expr -> Context Quadruples.Arg
transExpr x = case x of
  Latte.EVar _ (Latte.Ident ident) -> getVarLoc ident
  Latte.ELitInt _ integer -> return $ Const integer
  Latte.ELitTrue _ -> return $ Const 1
  Latte.ELitFalse _ -> return $ Const 0
  Latte.EApp _ (Latte.Ident ident) exprs -> do
    args <- mapM transExpr exprs
    mapM_ (\arg -> tell [Quadruple Put arg None None]) args
    loc <- getFreeLoc
    tell [Quadruple Call (Target ident) None loc]
    return loc
  Latte.EString _ string -> failExp x
  Latte.Neg _ expr -> do
    res <- transExpr expr
    loc <- getFreeLoc
    tell [Quadruple Quadruples.Neg res None loc]
    return loc
  Latte.Not _ expr -> do
    res <- transExpr expr
    loc <- getFreeLoc
    tell [Quadruple Quadruples.Not res None loc]
    return loc
  Latte.EMul _ expr1 mulop expr2 ->
    transBinOp expr1 expr2 (transMulOp mulop)
  Latte.EAdd _ expr1 addop expr2 ->
    transBinOp expr1 expr2 (transAddOp addop)
  Latte.ERel _ expr1 relop expr2 ->
    transBinOp expr1 expr2 (transRelOp relop)
  Latte.EAnd _ expr1 expr2 ->
    transBinOp expr1 expr2 And
  Latte.EOr _ expr1 expr2 ->
    transBinOp expr1 expr2 Or

transBinOp :: Latte.Expr -> Latte.Expr -> Op -> Context Quadruples.Arg
transBinOp expr1 expr2 op = do
  res1 <- transExpr expr1
  res2 <- transExpr expr2
  loc <- getFreeLoc
  tell [Quadruple op res1 res2 loc]
  return loc

getFreeLoc :: Context Quadruples.Arg
getFreeLoc = do
  (Env freeLoc map) <- get
  put $ Env (freeLoc + 1) map
  return $ Var freeLoc

failExp :: Show a => a -> Context Quadruples.Arg
failExp x = fail $ "Undefined case: " ++ show x

transAddOp :: Latte.AddOp -> Op
transAddOp x = case x of
  Latte.Plus _ -> Add
  Latte.Minus _ -> Sub

transMulOp :: Latte.MulOp -> Op
transMulOp x = case x of
  Latte.Times _ -> Mul
  Latte.Div _ -> Quadruples.Div
  Latte.Mod _ -> Quadruples.Mod

transRelOp :: Latte.RelOp -> Op
transRelOp x = case x of
  Latte.LTH _ -> Lt
  Latte.LE _ -> Le
  Latte.GTH _ -> Gt
  Latte.GE _ -> Ge
  Latte.EQU _ -> Eq
  Latte.NE _ -> Neq

-- | Returns the labels where the operation jumps to.
jumpLabels :: Quadruple -> [LabelName]
jumpLabels quadruple = case quadruple of
  Quadruple Jump (Target label) None None -> [label]
  Quadruple JumpIf _ (Target label1) (Target label2) -> [label1, label2]
  _ -> []

-- | Return the location of the variable
-- Creates the variable if needed.
getVarLoc :: String -> Context Quadruples.Arg
getVarLoc ident = do
  (Env freeLoc map) <- get
  case Data.Map.lookup ident map of
    Just loc -> return $ Var loc
    Nothing -> do
      fnData <- ask
      -- check if the variable is an argument
      case Data.Map.lookup ident (funcArgs fnData) of
        Just stackLoc -> do
          tell [Quadruple Get (Const stackLoc) None (Var freeLoc)]
          put $ Env (freeLoc + 1) (Data.Map.insert ident freeLoc map)
          return $ Var freeLoc
        Nothing -> do
          -- decalre the variable
          put $ Env (freeLoc + 1) (Data.Map.insert ident freeLoc map)
          return $ Var freeLoc
