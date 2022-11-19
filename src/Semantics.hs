module Semantics where

import Latte.Abs

import Control.Monad.RWS
import Data.Map
import Text.Printf

data SResult = Ok | Error [String]

verify:: Program -> SResult
verify program =
  let (_, res) = evalRWS (transProgram program) "top-level" empty in
  case res of
    [] -> Ok
    _ -> Error res

type TypeBinds = Map String ()

type Context = RWS String [String] TypeBinds

failure :: Show a => HasPosition a => a -> Context ()
failure x = do
  fnName <- ask
  let (BNFC'Position l c) = hasPosition x
  tell [printf "%s@%d:%d Undefined case %s" fnName l c (show x) ]

transProgram :: Program -> Context ()
transProgram (Program _ topDefs) =
  mapM_ transTopDef topDefs

transTopDef :: TopDef -> Context ()
transTopDef (FnDef _ type_ (Ident fnName) args block) =
  local (const fnName) $ transBlock block

transArg ::Arg -> Context ()
transArg x = case x of
  Arg _ type_ ident -> failure x

transBlock ::Block -> Context ()
transBlock (Block _ stmts) = do
  env <- get
  mapM_ transStmt stmts
  -- leave the environment unchanged after leaving a code block
  put env

transStmt ::Stmt -> Context ()
transStmt stmt = case stmt of
  Empty _ -> return ()

  BStmt _ block -> transBlock block

  Decl _ type_ items -> mapM_ transItem items

  Ass loc (Ident ident) expr -> do
    env <- get
    transExpr expr
    case Data.Map.lookup ident env of
      Nothing -> msgNotDeclared ident loc
      Just _ -> return ()

  Incr loc (Ident ident) -> do
    env <- get
    case Data.Map.lookup ident env of
      Nothing -> msgNotDeclared ident loc
      Just _ -> return ()

  Decr loc (Ident ident) -> do
    env <- get
    case Data.Map.lookup ident env of
      Nothing -> msgNotDeclared ident loc
      Just _ -> return ()

  Ret _ expr -> transExpr expr

  VRet _ -> return ()

  Cond _ expr stmt ->
    transExpr expr >> transStmt stmt

  CondElse _ expr stmt1 stmt2 -> do
    transExpr expr
    transStmt stmt1
    transStmt stmt2

  While _ expr stmt ->
    transExpr expr >> transStmt stmt

  SExp _ expr -> transExpr expr

transItem ::Item -> Context ()
transItem item = case item of
  NoInit _ (Ident ident) -> do
    env <- get
    put $ insert ident () env
  Init _ (Ident ident) expr -> do
    transExpr expr
    env <- get
    put $ insert ident () env

transType ::Type -> Context ()
transType x = case x of
  Int _ -> failure x
  Str _ -> failure x
  Bool _ -> failure x
  Void _ -> failure x
  Fun _ type_ types -> failure x

transExpr ::Expr -> Context ()
transExpr x = case x of
  EVar loc (Ident ident) -> do
    env <- get
    case Data.Map.lookup ident env of
      Just _ -> return ()
      Nothing -> msgNotDeclared ident loc
  ELitInt _ integer ->
    return ()
  ELitTrue _ -> return ()
  ELitFalse _ -> return ()
  EApp _ ident exprs -> return ()
  EString _ string -> return ()
  Neg _ expr -> transExpr expr
  Not _ expr -> transExpr expr
  EMul _ expr1 mulop expr2 ->
    transExpr expr1 >> transExpr expr2
  EAdd _ expr1 addop expr2 ->
    transExpr expr1 >> transExpr expr2
  ERel _ expr1 relop expr2 ->
    transExpr expr1 >> transExpr expr2
  EAnd _ expr1 expr2 ->
    transExpr expr1 >> transExpr expr2
  EOr _ expr1 expr2 ->
    transExpr expr1 >> transExpr expr2

transAddOp ::AddOp -> Context ()
transAddOp x = case x of
  Plus _ -> failure x
  Minus _ -> failure x

transMulOp ::MulOp -> Context ()
transMulOp x = case x of
  Times _ -> failure x
  Div _ -> failure x
  Mod _ -> failure x

transRelOp ::RelOp -> Context ()
transRelOp x = case x of
  LTH _ -> failure x
  LE _ -> failure x
  GTH _ -> failure x
  GE _ -> failure x
  EQU _ -> failure x
  NE _ -> failure x

msgNotDeclared :: String -> BNFC'Position -> Context ()
msgNotDeclared ident = msgTop $ printf "Variable %s not declared" ident

msgTop :: String -> BNFC'Position -> Context ()
msgTop msg (BNFC'Position l c) = do
  fnName <- ask
  tell [printf "%s @ %d:%d :: %s" fnName l c msg]
