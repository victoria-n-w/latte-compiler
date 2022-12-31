module Quadruples where

import Control.Monad.RWS
import Data.Data
import Data.Foldable
import Data.Map
import Data.Maybe
import Data.Set
import Latte.Abs
import Latte.ErrM
import Text.Printf (printf)

-- module which translates code to internal representation

type Loc = Int

data Arg = Var Loc | Const Integer | Mem Int | None | Target LabelName

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
  | Neg
  | And
  | Or
  | Not
  | Eq
  | Neq
  | Lt
  | Gt
  | Le
  | Ge
  | Assign
  | Get
  | Put
  | Call
  | Label LabelName
  | Jump
  | JumpIf
  | Return
  | ReturnVoid
  | Nop
  deriving (Data, Typeable)

instance Show Op where
  show (Label label) = label ++ ":"
  show op = show $ toConstr op

type LabelName = String

data Quadruple = Quadruple
  { op :: Quadruples.Op,
    arg1 :: Quadruples.Arg,
    arg2 :: Quadruples.Arg,
    res :: Quadruples.Arg
  }

instance Show Quadruple where
  show :: Quadruple -> String
  show (Quadruple op arg1 arg2 res) =
    case op of
      Label _ -> show op
      Jump -> printf "\t%s (%s)" (show op) (show arg1)
      JumpIf -> printf "\t%s (%s) (%s) (%s)" (show op) (show arg1) (show arg2) (show res)
      ReturnVoid -> printf "\t%s" (show op)
      Return -> printf "\t%s (%s)" (show op) (show arg1)
      _ -> printf "\t%s <- %s (%s) (%s)" (show res) (show op) (show arg1) (show arg2)

data Env = Env {nextLoc :: Loc, varMap :: Data.Map.Map String Loc}

data QFuncData = QFuncData
  { fnNames :: Data.Set.Set String,
    -- where on the stack the argument of current function is
    funcArgs :: Map String Integer
  }

type Context = RWST QFuncData [Quadruple] Env Err

translate :: Program -> Err [Quadruple]
translate p = do
  (_, _, quadruples) <-
    runRWST
      (transProgram p)
      (QFuncData Data.Set.empty Data.Map.empty)
      (Env 1 Data.Map.empty)
  return quadruples

transProgram :: Latte.Abs.Program -> Context ()
transProgram (Program loc topDefs) =
  mapM_ transTopDef topDefs

failure :: Show a => a -> Context ()
failure x = fail $ "Undefined case: " ++ show x

transTopDef :: Latte.Abs.TopDef -> Context ()
transTopDef x = case x of
  Latte.Abs.FnDef _ type_ (Ident ident) args block -> do
    let argsMap = transArgs args
    tellLabel ident
    res <-
      local (\env -> env {funcArgs = argsMap}) $
        transBlock block
    -- if the function does not return, return void
    unless res $ tell [Quadruple ReturnVoid None None None]

-- | Returns the map of argument names and their location on stack
transArgs :: [Latte.Abs.Arg] -> Map String Integer
transArgs args = Data.Map.fromList $ zip (Prelude.map (\(Arg _ _ (Ident ident)) -> ident) args) [1 ..]

transBlock :: Block -> Context Bool
transBlock (Block _ stmts) = do
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
  -- return to the prievious environment
  put env
  return isRet

transBlockLabels :: Block -> LabelName -> LabelName -> Context Bool
transBlockLabels block inLabel outLabel = do
  tellLabel inLabel
  isRet <- transBlock block
  unless isRet $ tell [Quadruple Jump (Target outLabel) None None]
  return isRet

-- | Translates a statement to a list of quadruples
-- returns true, if the statement is a return statement
transStmt :: Latte.Abs.Stmt -> Context Bool
transStmt x = case x of
  Empty _ -> return False
  BStmt _ block -> transBlock block
  Decl _ type_ items -> do
    mapM_ transItem items
    return False
  Ass _ (Ident ident) expr -> do
    res <- transExpr expr
    var <- getVarLoc ident
    tell [Quadruple Assign res None var]
    return False
  Incr _ (Ident ident) -> do
    var <- getVarLoc ident
    tell [Quadruple Add var (Const 1) var]
    return False
  Decr _ (Ident ident) -> do
    var <- getVarLoc ident
    tell [Quadruple Sub var (Const 1) var]
    return False
  Ret _ expr -> do
    res <- transExpr expr
    tell [Quadruple Return res None None]
    return True
  VRet _ -> do
    tell [Quadruple ReturnVoid None None None]
    return True
  Cond _loc expr stmt -> do
    -- generate labels
    blockLabel <- newLabel
    endLabel <- newLabel
    -- process the condition
    res <- transExpr expr
    tell [Quadruple JumpIf res (Target blockLabel) (Target endLabel)]
    transBlockLabels (makeBlock stmt) blockLabel endLabel
    tell [Quadruple (Label endLabel) None None None]
    return False
  CondElse _ expr stmt1 stmt2 -> do
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
  While _ expr stmt -> do
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
  SExp _ expr -> do
    transExpr expr
    return False

tellLabel :: LabelName -> Context ()
tellLabel label = tell [Quadruple (Label label) None None None]

makeBlock :: Stmt -> Block
makeBlock stmt = case stmt of
  BStmt _ block -> block
  _ -> Block (hasPosition stmt) [stmt]

transItem :: Latte.Abs.Item -> Context ()
transItem x = case x of
  NoInit _ (Ident ident) -> do
    var <- newVar ident
    tell [Quadruple Assign (Const 0) None var]
    return ()
  Init _ (Ident ident) expr -> do
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

transExpr :: Latte.Abs.Expr -> Context Quadruples.Arg
transExpr x = case x of
  EVar _ (Ident ident) -> getVarLoc ident
  ELitInt _ integer -> return $ Const integer
  ELitTrue _ -> return $ Const 1
  ELitFalse _ -> return $ Const 0
  EApp _ (Ident ident) exprs -> do
    args <- mapM transExpr exprs
    mapM_ (\(i, arg) -> tell [Quadruple Put (Const i) arg None]) $ zip [1 ..] args
    loc <- getFreeLoc
    tell [Quadruple Call (Target ident) None loc]
    return loc
  EString _ string -> failExp x
  Latte.Abs.Neg _ expr -> do
    res <- transExpr expr
    loc <- getFreeLoc
    tell [Quadruple Quadruples.Neg res None loc]
    return loc
  Latte.Abs.Not _ expr -> do
    res <- transExpr expr
    loc <- getFreeLoc
    tell [Quadruple Quadruples.Not res None loc]
    return loc
  EMul _ expr1 mulop expr2 ->
    transBinOp expr1 expr2 (transMulOp mulop)
  EAdd _ expr1 addop expr2 ->
    transBinOp expr1 expr2 (transAddOp addop)
  ERel _ expr1 relop expr2 ->
    transBinOp expr1 expr2 (transRelOp relop)
  EAnd _ expr1 expr2 ->
    transBinOp expr1 expr2 And
  EOr _ expr1 expr2 ->
    transBinOp expr1 expr2 Or

transBinOp :: Expr -> Expr -> Op -> Context Quadruples.Arg
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

transAddOp :: Latte.Abs.AddOp -> Op
transAddOp x = case x of
  Latte.Abs.Plus _ -> Add
  Latte.Abs.Minus _ -> Sub

transMulOp :: Latte.Abs.MulOp -> Op
transMulOp x = case x of
  Latte.Abs.Times _ -> Mul
  Latte.Abs.Div _ -> Quadruples.Div
  Latte.Abs.Mod _ -> Quadruples.Mod

transRelOp :: Latte.Abs.RelOp -> Op
transRelOp x = case x of
  Latte.Abs.LTH _ -> Lt
  Latte.Abs.LE _ -> Le
  Latte.Abs.GTH _ -> Gt
  Latte.Abs.GE _ -> Ge
  Latte.Abs.EQU _ -> Eq
  Latte.Abs.NE _ -> Neq

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
