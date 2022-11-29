module Semantics where

import Control.Monad.RWS
import Control.Monad.Reader
import Data.Map
import Data.Maybe
import Latte.Abs hiding (Int, Str, Bool, Void, Fun)
import SErr
import SType
import Text.Printf
import Control.Monad.Except
import Control.Exception (throw)

data SResult = Ok | Error [SErr]

verify :: Program -> SResult
verify program =
  let (_, res) = evalRWS (transProgram program) (FnLocal "top-level" Void) empty
   in case res of
        [] -> Semantics.Ok
        _ -> Error res

type TypeBinds = Map String SType

type Context = RWS FnLocal [SErr] TypeBinds

failure :: Show a => HasPosition a => a -> Context ()
failure x = do
  fnName <- ask
  tellErr (hasPosition x) $ NotImplemented $ printf "Undefined case %s" $ show x

transProgram :: Program -> Context ()
transProgram (Program loc topDefs) =
  let fnMap =
        Data.Map.fromList $
          Prelude.map (\(FnDef _ type_ (Ident fnName) _ _) -> (fnName, fromBNFC type_)) topDefs
            ++ [("printString", Void), ("printInt", Void), ("readInt", Int), ("readString", Str)]
   in do
        case Data.Map.lookup "main" fnMap of
          Nothing -> tellErr loc NoMain
          Just _ -> pure ()
        put fnMap
        mapM_
          transTopDef
          topDefs

transTopDef :: TopDef -> Context ()
transTopDef (FnDef loc type_ (Ident fnName) args block) = do
  env <- get
  let fnType = fromBNFC type_
  local (const (FnLocal fnName fnType)) $ do
    mapM_ transArg args
    isRet <- transBlock block
    when (fnType /= Void && not isRet) $ tellErr loc NoReturn
  put env

transArg :: Arg -> Context ()
transArg x = case x of
  Arg loc type_ (Ident ident) -> newName loc ident $ fromBNFC type_

transBlock :: Block -> Context Bool
transBlock (Block _ stmts) = do
  env <- get
  res <- mapM transStmt stmts
  -- leave the environment unchanged after leaving a code block
  put env
  pure $ or res

transStmt :: Stmt -> Context Returns
transStmt stmt = case stmt of
  Empty _ -> pure False
  BStmt _ block -> transBlock block
  Decl _ type_ items -> do
    mapM_ (transItem type_) items
    pure False
  Ass loc (Ident ident) expr -> do
    env <- get
    resT <- transExprWr expr
    case Data.Map.lookup ident env of
      Nothing -> do
        tellErr loc $ VarNotDeclared ident
        pure False
      Just t -> do
        checkType loc resT t
        pure False
  Incr loc (Ident ident) -> do
    env <- get
    case Data.Map.lookup ident env of
      Nothing -> tellErr loc $ VarNotDeclared ident
      Just t -> do when (t /= Int) $ tellErr loc $ TypeError t Int
    pure False
  Decr loc (Ident ident) -> do
    env <- get
    case Data.Map.lookup ident env of
      Nothing -> tellErr loc $ VarNotDeclared ident
      Just t -> do when (t /= Int) $ tellErr loc $ TypeError t Int
    pure False
  Ret loc expr -> do
    resT <- transExprWr expr
    FnLocal _ retType <- ask
    checkType loc resT retType
    pure True
  VRet loc -> do
    FnLocal fnName type_ <- ask
    when (type_ /= Void) $ tellErr loc $ ReturnTypeErr type_ Void
    pure True
  Cond loc expr stmt -> do
    resT <- transExprWr expr
    checkType loc resT Bool
    rets <- transStmt stmt
    case expr of
      ELitTrue _ -> pure rets
      _ -> pure False
  CondElse loc expr stmt1 stmt2 -> do
    resT <- transExprWr expr
    checkType loc resT Bool
    ret1 <- transStmt stmt1
    ret2 <- transStmt stmt2
    pure $ ret1 || ret2
  While loc expr stmt -> do
    resT <- transExprWr expr
    checkType loc resT Bool
    transStmt stmt
  SExp _ expr -> do
    transExprWr expr
    pure False

checkType :: BNFC'Position -> ResType -> SType -> Context ()
checkType loc resT t =
  case resT of
    (Just t_) ->
      if t == t_
        then pure ()
        else do
          tellErr loc $ TypeError t_ t
    Nothing -> pure ()

transItem :: Type -> Item -> Context ()
transItem type_ item = case item of
  NoInit loc (Ident ident) ->
    newName loc ident $ fromBNFC type_
  Init loc (Ident ident) expr -> do
    transExprWr expr
    newName loc ident $ fromBNFC type_

newName :: BNFC'Position -> String -> SType -> Context ()
newName loc ident type_ = do
  env <- get
  case Data.Map.lookup ident env of
    Just _ -> tellErr loc $ VarRedeclared ident
    Nothing -> put $ insert ident type_ env


transExprWr :: Expr -> Context (Maybe SType)
transExprWr expr = do
  env <- get
  let res = runReaderT (transExpr expr) env
  case res of
    (Right t) -> pure $ Just t
    (Left (ExpErr loc cause)) -> do
      tellErr loc cause
      pure Nothing

type EContext t = ReaderT TypeBinds (Either ExpErr) t

transExpr :: Expr -> EContext SType
transExpr x = case x of
  EVar loc (Ident ident) -> do
    env <- ask
    case Data.Map.lookup ident env of
      (Just t) -> pure t
      Nothing -> throwError $ ExpErr loc (VarNotDeclared ident)
  ELitInt _ integer ->
    pure Int
  ELitTrue _ ->
    pure Bool
  ELitFalse _ ->
    pure Bool
  EApp loc (Ident ident) exprs -> do
    env <- ask
    case Data.Map.lookup ident env of
      Just t -> pure t
      Nothing -> throwError $ ExpErr loc (VarNotDeclared ident)
  EString _ string -> pure Str
  Neg loc expr -> do
    t <- transExpr expr
    case t of
      Int -> pure Int
      _ -> throwError $ ExpErr loc $ TypeError t Int
  Not loc expr -> do
    t <- transExpr expr
    case t of
      Bool -> pure t
      _ -> throwError $ ExpErr loc $ TypeError t Bool
  EMul loc expr1 mulop expr2 -> do
    t1 <- transExpr expr1
    t2 <- transExpr expr2
    case (t1, t2) of
      (Int, Int) -> pure Int
      _ -> err loc t1 t2
  EAdd loc expr1 addop expr2 -> do
    t1 <- transExpr expr1
    t2 <- transExpr expr2
    case (t1, t2, addop) of
      (Str, Str, Plus _) -> pure Str
      (Int, Int, _) -> pure Int
      _ -> err loc t1 t2
  ERel loc expr1 relop expr2 -> do
    t1 <- transExpr expr1
    t2 <- transExpr expr2
    case (t1, t2, relop) of
      (_, _, EQU _) ->
        if t1 == t2 then pure Bool else err loc t1 t2
      (_, _, NE _) ->
        if t1 == t2 then pure Bool else err loc t1 t2
      (Int, Int, _) -> pure Bool
      _ -> err loc t1 t2
  EAnd loc expr1 expr2 -> do
    t1 <- transExpr expr1
    t2 <- transExpr expr2
    case (t1, t2) of
      (Bool, Bool) -> pure Bool
      _ -> err loc t1 t2
  EOr loc expr1 expr2 -> do
    t1 <- transExpr expr1
    t2 <- transExpr expr2
    case (t1, t2) of
      (Bool, Bool) -> pure Bool
      _ -> err loc t1 t2

err :: BNFC'Position -> SType -> SType -> EContext SType
err loc t1 t2 = do throwError $ ExpErr loc $ TypeError t1 t2

tellErr :: BNFC'Position -> ErrCause -> Context ()
tellErr (BNFC'Position l c) cause = do
  fnLocal <- ask
  tell [SErr (show fnLocal) (l, c) cause]
