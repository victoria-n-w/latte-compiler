module Semantics where

import Control.Monad.RWS
import Data.Map
import Data.Maybe
import Latte.Abs
import SErr
import SType
import Text.Printf

data SResult = Ok | Error [SErr]

verify :: Program -> SResult
verify program =
  let (_, res) = evalRWS (transProgram program) "top-level" empty
   in case res of
        [] -> Ok
        _ -> Error res

type TypeBinds = Map String SType

type Context = RWS String [SErr] TypeBinds

failure :: Show a => HasPosition a => a -> Context ()
failure x = do
  fnName <- ask
  tellErr (hasPosition x) $ NotImplemented $ printf "Undefined case %s" $ show x

transProgram :: Program -> Context ()
transProgram (Program loc topDefs) =
  let fnMap =
        Data.Map.fromList $
          Prelude.map (\(FnDef _ _ (Ident fnName) _ _) -> (fnName, SType.Int)) topDefs
   in do
        case Data.Map.lookup "main" fnMap of
          Nothing -> tellErr loc NoMain
          Just _ -> pure ()
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
  Arg loc type_ (Ident ident) -> newName loc ident $ fromBNFC type_

transBlock :: Block -> Context ()
transBlock (Block _ stmts) = do
  env <- get
  mapM_ transStmt stmts
  -- leave the environment unchanged after leaving a code block
  put env

transStmt :: Stmt -> Context ()
transStmt stmt = case stmt of
  Empty _ -> pure ()
  BStmt _ block -> transBlock block
  Decl _ type_ items -> mapM_ (transItem type_) items
  Ass loc (Ident ident) expr -> do
    env <- get
    resT <- transExpr expr
    case Data.Map.lookup ident env of
      Nothing -> tellErr loc $ VarNotDeclared ident
      Just t -> do
        transResType loc resT t
        pure ()
  Incr loc (Ident ident) -> do
    env <- get
    case Data.Map.lookup ident env of
      Nothing -> tellErr loc $ VarNotDeclared ident
      Just t -> do when (t /= SType.Int) $ tellErr loc $ TypeError t SType.Int
    pure ()
  Decr loc (Ident ident) -> do
    env <- get
    case Data.Map.lookup ident env of
      Nothing -> tellErr loc $ VarNotDeclared ident
      Just t -> do when (t /= SType.Int) $ tellErr loc $ TypeError t SType.Int
    pure ()
  Ret _ expr -> do
    transExpr expr
    pure ()
  VRet _ -> pure ()
  Cond loc expr stmt -> do
    resT <- transExpr expr
    transResType loc resT SType.Bool
    transStmt stmt
  CondElse loc expr stmt1 stmt2 -> do
    resT <- transExpr expr
    transResType loc resT SType.Bool
    transStmt stmt1
    transStmt stmt2
  While loc expr stmt -> do
    resT <- transExpr expr
    transResType loc resT SType.Bool
    transStmt stmt
  SExp _ expr -> do
    transExpr expr
    pure ()

transItem :: Type -> Item -> Context ()
transItem type_ item = case item of
  NoInit loc (Ident ident) ->
    newName loc ident $ fromBNFC type_
  Init loc (Ident ident) expr -> do
    transExpr expr
    newName loc ident $ fromBNFC type_

newName :: BNFC'Position -> String -> SType -> Context ()
newName loc ident type_ = do
  env <- get
  case Data.Map.lookup ident env of
    Just _ -> tellErr loc $ VarRedeclared ident
    Nothing -> put $ insert ident type_ env

transExpr :: Expr -> Context ResType
transExpr x = case x of
  EVar loc (Ident ident) -> do
    env <- get
    let t = Data.Map.lookup ident env
    when (isNothing t) $ tellErr loc $ VarNotDeclared ident
    pure t
  ELitInt _ integer ->
    pure $ Just SType.Int
  ELitTrue _ ->
    pure $ Just SType.Bool
  ELitFalse _ ->
    pure $ Just SType.Bool
  EApp _ ident exprs -> pure Nothing -- TODO func type
  EString _ string -> pure $ Just SType.Str
  Neg loc expr -> do
    resT <- transExpr expr
    transResType loc resT SType.Int
  Not loc expr -> do
    resT <- transExpr expr
    transResType loc resT SType.Bool
  EMul loc expr1 mulop expr2 -> do
    resT1 <- transExpr expr1
    resT2 <- transExpr expr2
    case (resT1, resT2) of
      (Just SType.Int, Just SType.Int) -> pure $ Just SType.Int
      (Just t1, Just t2) -> do
        tellErr loc $ BinOpErr t1 t2
        pure Nothing
      _ -> pure Nothing
  EAdd loc expr1 addop expr2 -> do
    resT1 <- transExpr expr1
    resT2 <- transExpr expr2
    case (resT1, resT2, addop) of
      (Just SType.Str, Just SType.Str, Plus _) ->
        -- strings can be added
        pure $ Just SType.Str
      (Just SType.Int, Just SType.Int, _) ->
        pure $ Just SType.Int
      (Just t1, Just t2, _) -> do
        tellErr loc $ BinOpErr t1 t2
        pure Nothing
      _ -> pure Nothing
  ERel loc expr1 relop expr2 -> do
    resT1 <- transExpr expr1
    resT2 <- transExpr expr2
    case (resT1, resT2, relop) of
      (Just SType.Bool, Just SType.Bool, EQU _) -> pure $ Just SType.Bool
      (Just SType.Bool, Just SType.Bool, NE _) -> pure $ Just SType.Bool
      (Just SType.Int, Just SType.Int, _) -> pure $ Just SType.Bool
      (Just t1, Just t2, _) -> do
        tellErr loc $ BinOpErr t1 t2
        pure Nothing
      _ -> pure Nothing
  EAnd loc expr1 expr2 -> transBoolOp loc expr1 expr2
  EOr loc expr1 expr2 -> transBoolOp loc expr1 expr2

transBoolOp :: BNFC'Position -> Expr -> Expr -> Context ResType
transBoolOp loc expr1 expr2 = do
  resT1 <- transExpr expr1
  resT2 <- transExpr expr2
  case (resT1, resT2) of
    (Just SType.Bool, Just SType.Bool) -> pure $ Just SType.Bool
    (Just t1, Just t2) -> do
      tellErr loc $ BinOpErr t1 t2
      pure Nothing
    _ -> pure Nothing

transResType :: BNFC'Position -> ResType -> SType -> Context ResType
transResType loc resT t =
  case resT of
    (Just t_) ->
      if t == t_
        then pure $ Just t
        else do
          tellErr loc $ TypeError t_ t
          pure Nothing
    Nothing -> pure Nothing

tellErr :: BNFC'Position -> ErrCause -> Context ()
tellErr (BNFC'Position l c) cause = do
  fnName <- ask
  tell [SErr fnName (l, c) cause]
