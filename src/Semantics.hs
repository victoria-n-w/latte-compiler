module Semantics where

import Control.Monad.RWS
import Data.Map
import Latte.Abs
import SErr
import Text.Printf

data SResult = Ok | Error [SErr]

verify :: Program -> SResult
verify program =
  let (_, res) = evalRWS (transProgram program) "top-level" empty
   in case res of
        [] -> Ok
        _ -> Error res

type TypeBinds = Map String ()

type Context = RWS String [SErr] TypeBinds

failure :: Show a => HasPosition a => a -> Context ()
failure x = do
  fnName <- ask
  tellErr (hasPosition x) $ NotImplemented $ printf "Undefined case %s" $ show x

transProgram :: Program -> Context ()
transProgram (Program loc topDefs) =
  let fnMap =
        Data.Map.fromList $
          Prelude.map (\(FnDef _ _ (Ident fnName) _ _) -> (fnName, ())) topDefs
   in do
        case Data.Map.lookup "main" fnMap of
          Nothing -> tellErr loc NoMain
          Just _ -> return ()
        put fnMap
        mapM_
          transTopDef
          topDefs

transTopDef :: TopDef -> Context ()
transTopDef (FnDef _ type_ (Ident fnName) args block) = do
  env <- get
  local (const fnName) $
    mapM_ transArg args
      >> transBlock block
  put env

transArg :: Arg -> Context ()
transArg x = case x of
  Arg loc type_ (Ident ident) -> newName loc ident

transBlock :: Block -> Context ()
transBlock (Block _ stmts) = do
  env <- get
  mapM_ transStmt stmts
  -- leave the environment unchanged after leaving a code block
  put env

transStmt :: Stmt -> Context ()
transStmt stmt = case stmt of
  Empty _ -> return ()
  BStmt _ block -> transBlock block
  Decl _ type_ items -> mapM_ transItem items
  Ass loc (Ident ident) expr -> do
    env <- get
    transExpr expr
    case Data.Map.lookup ident env of
      Nothing -> tellErr loc $ VarNotDeclared ident
      Just _ -> return ()
  Incr loc (Ident ident) -> do
    env <- get
    case Data.Map.lookup ident env of
      Nothing -> tellErr loc $ VarNotDeclared ident
      Just _ -> return ()
  Decr loc (Ident ident) -> do
    env <- get
    case Data.Map.lookup ident env of
      Nothing -> tellErr loc $ VarNotDeclared ident
      Just _ -> return ()
  Ret _ expr -> do
    transExpr expr
    return ()
  VRet _ -> return ()
  Cond _ expr stmt ->
    transExpr expr >> transStmt stmt
  CondElse _ expr stmt1 stmt2 -> do
    transExpr expr
    transStmt stmt1
    transStmt stmt2
  While _ expr stmt ->
    transExpr expr >> transStmt stmt
  SExp _ expr -> do
    transExpr expr
    return ()

transItem :: Item -> Context ()
transItem item = case item of
  NoInit loc (Ident ident) ->
    newName loc ident
  Init loc (Ident ident) expr -> do
    transExpr expr
    newName loc ident

newName :: BNFC'Position -> String -> Context ()
newName loc ident = do
  env <- get
  case Data.Map.lookup ident env of
    Just _ -> tellErr loc $ VarRedeclared ident
    Nothing -> put $ insert ident () env

transType :: Type -> Context ()
transType x = case x of
  Int _ -> failure x
  Str _ -> failure x
  Bool _ -> failure x
  Void _ -> failure x
  Fun _ type_ types -> failure x

transExpr :: Expr -> Context (Maybe ())
transExpr x = case x of
  EVar loc (Ident ident) -> do
    env <- get
    case Data.Map.lookup ident env of
      Just _ -> return $ Just ()
      Nothing -> do
        tellErr loc $ VarNotDeclared ident
        return Nothing
  ELitInt _ integer ->
    return Nothing
  ELitTrue _ -> return Nothing
  ELitFalse _ -> return Nothing
  EApp _ ident exprs -> return Nothing
  EString _ string -> return Nothing
  Neg _ expr -> transExpr expr
  Not _ expr -> transExpr expr
  EMul _ expr1 mulop expr2 -> do
    transExpr expr1
    transExpr expr2
  EAdd _ expr1 addop expr2 ->
    transExpr expr1 >> transExpr expr2
  ERel _ expr1 relop expr2 ->
    transExpr expr1 >> transExpr expr2
  EAnd _ expr1 expr2 ->
    transExpr expr1 >> transExpr expr2
  EOr _ expr1 expr2 ->
    transExpr expr1 >> transExpr expr2

transAddOp :: AddOp -> Context ()
transAddOp x = case x of
  Plus _ -> failure x
  Minus _ -> failure x

transMulOp :: MulOp -> Context ()
transMulOp x = case x of
  Times _ -> failure x
  Div _ -> failure x
  Mod _ -> failure x

transRelOp :: RelOp -> Context ()
transRelOp x = case x of
  LTH _ -> failure x
  LE _ -> failure x
  GTH _ -> failure x
  GE _ -> failure x
  EQU _ -> failure x
  NE _ -> failure x

tellErr :: BNFC'Position -> ErrCause -> Context ()
tellErr (BNFC'Position l c) cause = do
  fnName <- ask
  tell [SErr fnName (l, c) cause]
