module Semantics where

import Control.Monad.Except
import Control.Monad.RWS
import Control.Monad.Reader
import Data.Map
import Data.Maybe
import Latte.Abs hiding (Bool, Fun, Int, Str, Void)
import SErr
import SType
import Text.Printf

data SResult = Ok | Error [SErr]

verify :: Program -> SResult
verify program =
  let (_, res) = evalRWS (transProgram program) (Context (FnLocal "top-level" Void) empty 0) empty
   in case res of
        [] -> Semantics.Ok
        _ -> Error res

type TypeBinds = Map String SType

data Context = Context
  { fnLocal :: FnLocal,
    fnDefs :: FnDefs,
    depth :: Int
  }

type FnDefs = Map String FnType

type Env = RWS Context [SErr] TypeBinds

failure :: Show a => HasPosition a => a -> Env ()
failure x = do
  fnName <- ask
  tellErr (hasPosition x) $ NotImplemented $ printf "Undefined case %s" $ show x

transProgram :: Program -> Env ()
transProgram (Program loc topDefs) =
  let fnMap =
        Data.Map.fromList $
          Prelude.map makeFnEntry topDefs
            ++ header
   in do
        case Data.Map.lookup "main" fnMap of
          Nothing -> tellErr loc $ Custom "No entry point: 'main'"
          Just (FnType retT args) -> do
            when (retT /= Int) $ tellErr loc $ Custom $ "Incorrect main return type, should be int, is " ++ show retT
            when (args /= []) $ tellErr loc $ Custom $ "main expects no args, got " ++ show args
        local (\context -> Context (fnLocal context) fnMap 0) $
          mapM_
            transTopDef
            topDefs

header =
  [ ("printString", FnType Void [Str]),
    ("printInt", FnType Void [Int]),
    ("readInt", FnType Int []),
    ("readString", FnType Str [])
  ]

transTopDef :: TopDef -> Env ()
transTopDef (FnDef loc type_ (Ident fnName) args block) = do
  let fnType = fromBNFC type_
  Context _ fnDefs _ <- ask
  local (const (Context (FnLocal fnName fnType) fnDefs 0)) $ do
    mapM_ transArg args
    isRet <- transBlock block
    when (fnType /= Void && not isRet) $ tellErr loc NoReturn
  put empty

transArg :: Arg -> Env ()
transArg x = case x of
  Arg loc type_ (Ident ident) -> newName loc ident $ fromBNFC type_

transBlock :: Block -> Env Bool
transBlock (Block _ stmts) = do
  env <- get
  res <-
    local
      ( \context ->
          -- increment the depth, leave rest of the context unchanged
          Context (fnLocal context) (fnDefs context) (Semantics.depth context + 1)
      )
      $ mapM transStmt stmts
  -- leave the environment unchanged after leaving a code block
  put env
  pure $ or res

transStmt :: Stmt -> Env Returns
transStmt stmt = case stmt of
  Empty _ -> pure False
  BStmt _ block -> transBlock block
  Decl _ type_ items -> do
    mapM_ (transItem type_) items
    pure False
  Ass loc lexpr expr -> do
    env <- get
    resT <- transExprWr expr
    case lexpr of
      LVar _ (Ident ident) -> do
        case Data.Map.lookup ident env of
          Nothing -> tellErr loc $ VarNotDeclared ident
          Just (SType t _) -> checkType loc resT t
      LArr _ (Ident ident) expr' -> do
        case Data.Map.lookup ident env of
          Nothing -> tellErr loc $ VarNotDeclared ident
          Just (SType (SType.Arr t) _) -> do
            checkType loc resT t
            resGet <- transExprWr expr'
            checkType loc resGet Int
          Just (SType t _) -> tellErr loc $ Custom $ printf "Cannot assign to a non-array type %s" $ show t
    pure False
  Incr loc (Ident ident) -> do
    env <- get
    context <- ask
    case Data.Map.lookup ident env of
      Nothing -> tellErr loc $ VarNotDeclared ident
      Just (SType t _) -> do when (t /= Int) $ tellErr loc $ TypeError t Int
    pure False
  Decr loc (Ident ident) -> do
    env <- get
    case Data.Map.lookup ident env of
      Nothing -> tellErr loc $ VarNotDeclared ident
      Just (SType t _) -> do when (t /= Int) $ tellErr loc $ TypeError t Int
    pure False
  Ret loc expr -> do
    resT <- transExprWr expr
    Context (FnLocal _ retType) _ _ <- ask
    checkType loc resT retType
    pure True
  VRet loc -> do
    Context (FnLocal _ type_) _ _ <- ask
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
    case expr of
      ELitTrue _ -> pure ret1
      ELitFalse _ -> pure ret2
      _ -> pure $ ret1 && ret2
  While loc expr stmt ->
    case expr of
      ELitFalse _ -> pure False
      ELitTrue _ -> transStmt stmt
      _ -> do
        resT <- transExprWr expr
        checkType loc resT Bool
        transStmt stmt
        -- return False, since the loop may not execute at all
        pure False
  For loc type_ (Ident ident) expr stmt -> do
    arrT <- transExprWr expr
    case arrT of
      (Just (SType.Arr t)) -> do
        env <- get
        newName loc ident t
        transStmt stmt
        -- return False, since the loop may not execute at all
        -- for example if the array was empty
        put env
        pure ()
      (Just wrongT) -> do
        tellErr loc $ TypeError wrongT (SType.Arr (fromBNFC type_))
      Nothing -> pure ()
    pure False
  SExp _ expr -> do
    transExprWr expr
    pure False

checkType :: BNFC'Position -> Maybe TypeLit -> TypeLit -> Env ()
checkType loc resT t =
  case resT of
    (Just t_) ->
      if t == t_
        then pure ()
        else do
          tellErr loc $ TypeError t_ t
    Nothing -> pure ()

transItem :: Type -> Item -> Env ()
transItem type_ item = case item of
  NoInit loc (Ident ident) ->
    newName loc ident $ fromBNFC type_
  Init loc (Ident ident) expr -> do
    resT <- transExprWr expr
    case resT of
      (Just t) ->
        if t == fromBNFC type_
          then newName loc ident t
          else tellErr loc $ TypeError t (fromBNFC type_)
      _ -> pure ()

newName :: BNFC'Position -> String -> TypeLit -> Env ()
newName loc ident type_ = do
  env <- get
  context <- ask
  let var = Data.Map.lookup ident env
  let fn = Data.Map.lookup ident $ fnDefs context
  let depth = Semantics.depth context
  case (var, fn) of
    (Nothing, Nothing) -> put $ insert ident (SType type_ depth) env
    (Just (SType t varDepth), Nothing) ->
      if varDepth < depth
        then put $ insert ident (SType type_ depth) env
        else tellErr loc $ VarRedeclared ident
    (Nothing, _) -> tellErr loc $ IsAFunction ident

transExprWr :: Expr -> Env (Maybe TypeLit)
transExprWr expr = do
  env <- get
  context <- ask
  let res = runReaderT (transExpr expr) $ ENameMap (fnDefs context) env
  case res of
    (Right t) -> pure $ Just t
    (Left (ExpErr loc cause)) -> do
      tellErr loc cause
      pure Nothing

type EnvExpr t = ReaderT ENameMap (Either ExpErr) t

data ENameMap = ENameMap FnDefs TypeBinds

transExpr :: Expr -> EnvExpr TypeLit
transExpr x = case x of
  EVar loc (Ident ident) -> do
    ENameMap _ env <- ask
    case Data.Map.lookup ident env of
      (Just sType) -> pure $ t sType
      Nothing -> throwError $ ExpErr loc (VarNotDeclared ident)
  ELitInt _ _ ->
    pure Int
  ELitTrue _ ->
    pure Bool
  ELitFalse _ ->
    pure Bool
  EApp loc (Ident ident) exprs -> do
    ENameMap fnDefs _ <- ask
    case Data.Map.lookup ident fnDefs of
      Nothing -> throwError $ ExpErr loc (NoSuchFn ident)
      Just (FnType retType args) -> do
        exprTypes <- mapM transExpr exprs
        if exprTypes == args
          then pure retType
          else throwError $ ExpErr loc $ CallErr ident exprTypes args
  EGet loc (Ident ident) expr -> do
    ENameMap _ env <- ask
    t <- transExpr expr
    when (t /= Int) $ throwError $ ExpErr loc $ TypeError t Int
    case Data.Map.lookup ident env of
      Nothing -> throwError $ ExpErr loc $ VarNotDeclared ident
      Just (SType (SType.Arr t) _) -> do
        pure t
      Just (SType t _) -> throwError $ ExpErr loc $ NotAnArray ident
  ENewArr loc type_ expr -> do
    t <- transExpr expr
    when (t /= Int) $ throwError $ ExpErr loc $ TypeError t Int
    pure $ SType.Arr (fromBNFC type_)
  EMember loc (Ident ident1) (Ident ident2) -> do
    -- check that ident1 is initialized array, and that ident2 is equal to lenght
    ENameMap _ env <- ask
    case Data.Map.lookup ident1 env of
      Nothing -> throwError $ ExpErr loc $ VarNotDeclared ident1
      Just (SType (SType.Arr _) _) -> do
        if ident2 == "length"
          then pure Int
          else throwError $ ExpErr loc $ NoSuchMember ident1 ident2
      Just (SType t _) -> throwError $ ExpErr loc $ NotAnArray ident1
  EString _ _ -> pure Str
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
      _ -> exprTypeErr loc t1 t2
  EAdd loc expr1 addop expr2 -> do
    t1 <- transExpr expr1
    t2 <- transExpr expr2
    case (t1, t2, addop) of
      (Str, Str, Plus _) -> pure Str
      (Int, Int, _) -> pure Int
      _ -> exprTypeErr loc t1 t2
  ERel loc expr1 relop expr2 -> do
    t1 <- transExpr expr1
    t2 <- transExpr expr2
    case (t1, t2, relop) of
      (_, _, EQU _) ->
        if t1 == t2 then pure Bool else exprTypeErr loc t1 t2
      (_, _, NE _) ->
        if t1 == t2 then pure Bool else exprTypeErr loc t1 t2
      (Int, Int, _) -> pure Bool
      _ -> exprTypeErr loc t1 t2
  EAnd loc expr1 expr2 -> do
    t1 <- transExpr expr1
    t2 <- transExpr expr2
    case (t1, t2) of
      (Bool, Bool) -> pure Bool
      _ -> exprTypeErr loc t1 t2
  EOr loc expr1 expr2 -> do
    t1 <- transExpr expr1
    t2 <- transExpr expr2
    case (t1, t2) of
      (Bool, Bool) -> pure Bool
      _ -> exprTypeErr loc t1 t2

exprTypeErr :: BNFC'Position -> TypeLit -> TypeLit -> EnvExpr TypeLit
exprTypeErr loc t1 t2 = do throwError $ ExpErr loc $ TypeError t1 t2

tellErr :: BNFC'Position -> ErrCause -> Env ()
tellErr (BNFC'Position l c) cause = do
  context <- ask
  tell [SErr ((show . fnLocal) context) (l, c) cause]
