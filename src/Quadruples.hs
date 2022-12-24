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
      JumpIf -> printf "\t%s (%s) (%s)" (show op) (show arg1) (show arg2)
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
    -- fold the arguments, starting with counter 0
    let argsMap = transArgs args
    res <-
      local (\env -> env {funcArgs = argsMap}) $
        transBlock block (Just ident) Nothing
    -- if the function does not return, return void
    unless res $ tell [Quadruple ReturnVoid None None None]

-- | Returns the map of argument names and their location on stack
transArgs :: [Latte.Abs.Arg] -> Map String Integer
transArgs args = Data.Map.fromList $ zip (Prelude.map (\(Arg _ _ (Ident ident)) -> ident) args) [1 ..]

transBlock :: Block -> Maybe LabelName -> Maybe LabelName -> Context Bool
transBlock (Block _ stmts) inLabel outLabel = do
  -- at the beginning of a block, if there is an in label, flag it
  Data.Foldable.forM_ inLabel tellLabel
  isRet <-
    foldM
      ( \ret stmt ->
          if ret
            then return True
            else transStmt stmt
      )
      False
      stmts
  -- at the end of a block, if there is an out label, jump to it
  when (isJust outLabel) $ tell [Quadruple Jump (Target (fromJust outLabel)) None None]
  return isRet

-- | Translates a statement to a list of quadruples
-- returns true, if the statement is a return statement
transStmt :: Latte.Abs.Stmt -> Context Bool
transStmt x = case x of
  Empty _ -> return False
  BStmt _ block -> transBlock block Nothing Nothing
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
    res <- transExpr expr
    blockLabel <- newLabel
    endLabel <- newLabel
    tell [Quadruple JumpIf res (Target blockLabel) (Target endLabel)]
    transBlock (makeBlock stmt) (Just blockLabel) (Just endLabel)
    tell [Quadruple (Label endLabel) None None None]
    return False
  CondElse _ expr stmt1 stmt2 -> do
    res <- transExpr expr
    block1Label <- newLabel
    block2Label <- newLabel
    endLabel <- newLabel
    tell [Quadruple JumpIf res (Target block1Label) (Target block2Label)]
    transBlock (makeBlock stmt1) (Just block1Label) (Just endLabel)
    transBlock (makeBlock stmt2) (Just block2Label) (Just endLabel)
    tellLabel endLabel
    return False
  While _ expr stmt -> do
    condLabel <- newLabel
    tellLabel condLabel
    res <- transExpr expr
    blockLabel <- newLabel
    endLabel <- newLabel
    tell [Quadruple JumpIf res (Target blockLabel) (Target endLabel)]
    transBlock (makeBlock stmt) (Just blockLabel) (Just condLabel)
    tellLabel endLabel
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
    newVar ident
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
    mapM_ (\arg -> tell [Quadruple Put arg None None]) args
    loc <- getFreeLoc
    tell [Quadruple Call (Target ident) None loc]
    return loc
  EString _ string -> failExp x
  Latte.Abs.Neg _ expr -> failExp x
  Latte.Abs.Not _ expr -> failExp x
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
