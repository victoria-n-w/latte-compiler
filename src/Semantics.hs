module Semantics where

import Control.Monad.Except
import Control.Monad.RWS
import Control.Monad.Reader
import Data.Map
import Data.Maybe
import Latte.Abs (HasPosition (hasPosition))
import Latte.Abs hiding (Bool, Fun, Int, Str, Void)
import SErr
import SType
import Text.Printf
import TopDefs qualified

data SResult = Ok | Error [SErr]

verify :: Program -> SResult
verify program =
  let (_, res) =
        evalRWS
          (transProgram program)
          (Context (FnLocal "top-level" Void) empty empty 0 Nothing)
          empty
   in case res of
        [] -> Semantics.Ok
        _ -> Error res

type TypeBinds = Map String SType

data Context = Context
  { fnLocal :: FnLocal,
    fnDefs :: FnDefs,
    classDefs :: ClassDefs,
    depth :: Int,
    scope :: Maybe ClassName
  }

type FnDefs = Map String FnType

type ClassDefs = Map String ClassDef

type Env = RWS Context [SErr] TypeBinds

failure :: Show a => HasPosition a => a -> Env ()
failure x = do
  fnName <- ask
  tellErr (hasPosition x) $ NotImplemented $ printf "Undefined case %s" $ show x

transProgram :: Program -> Env ()
transProgram (Program loc topDefs) =
  let (fnMap, classMap) = TopDefs.firstPass topDefs
   in do
        case Data.Map.lookup "main" fnMap of
          Nothing -> tellErr loc $ Custom "No entry point: 'main'"
          Just (FnType retT args) -> do
            when (retT /= Int) $ tellErr loc $ Custom $ "Incorrect main return type, should be int, is " ++ show retT
            when (args /= []) $ tellErr loc $ Custom $ "main expects no args, got " ++ show args
        local
          (const $ Context (FnLocal "top-level" Void) (fnMap `union` header) classMap 0 Nothing)
          $ mapM_
            transTopDef
            topDefs

header :: Map String FnType
header =
  fromList
    [ ("printString", FnType Void [Str]),
      ("printInt", FnType Void [Int]),
      ("readInt", FnType Int []),
      ("readString", FnType Str [])
    ]

transTopDef :: TopDef -> Env ()
transTopDef x = case x of
  FnDef loc type_ ident args block -> transFn loc type_ ident args block
  Latte.Abs.ClassDef loc (Ident ident) members -> do
    -- modify the scope in the reader monad
    local (\context -> context {scope = Just ident}) $ do
      mapM_ transMember members

transFn :: BNFC'Position -> Type -> Ident -> [Arg] -> Block -> Env ()
transFn loc type_ (Ident fnName) args block = do
  let fnType = fromBNFC type_
  fns <- asks fnDefs
  local (\context -> context {fnLocal = FnLocal fnName fnType}) $ do
    mapM_ transArg args
    isRet <- transBlock block
    when (fnType /= Void && not isRet) $ tellErr loc NoReturn
  put empty

transMember :: Member -> Env ()
transMember x = case x of
  Attr loc type_ (Ident ident) ->
    transType type_
  Method loc type_ ident args block ->
    transFn loc type_ ident args block

transArg :: Arg -> Env ()
transArg x = case x of
  Arg loc type_ (Ident ident) -> newName loc ident $ fromBNFC type_

transBlock :: Block -> Env Bool
transBlock (Block _ stmts) = do
  env <- get
  res <-
    -- enter a code block, modify only the depth
    local
      (\context -> context {Semantics.depth = Semantics.depth context + 1})
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
  Ass loc lhs expr -> do
    resT <- transMonadWrapper transExpr expr
    varT <- transMonadWrapper transLHS lhs
    checkTypeMaybe loc resT varT
    pure False
  Incr loc lhs -> do
    varT <- transMonadWrapper transLHS lhs
    checkType loc varT Int
    pure False
  Decr loc lhs -> do
    varT <- transMonadWrapper transLHS lhs
    checkType loc varT Int
    pure False
  Ret loc expr -> do
    resT <- transMonadWrapper transExpr expr
    (FnLocal _ retType) <- asks fnLocal
    checkType loc resT retType
    pure True
  VRet loc -> do
    (FnLocal _ type_) <- asks fnLocal
    when (type_ /= Void) $ tellErr loc $ ReturnTypeErr type_ Void
    pure True
  Cond loc expr stmt -> do
    resT <- transMonadWrapper transExpr expr
    checkType loc resT Bool
    rets <- transStmt stmt
    case expr of
      ELitTrue _ -> pure rets
      _ -> pure False
  CondElse loc expr stmt1 stmt2 -> do
    resT <- transMonadWrapper transExpr expr
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
        resT <- transMonadWrapper transExpr expr
        checkType loc resT Bool
        transStmt stmt
        -- return False, since the loop may not execute at all
        pure False
  SExp _ expr -> do
    transMonadWrapper transExpr expr
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

checkTypeMaybe :: BNFC'Position -> Maybe TypeLit -> Maybe TypeLit -> Env ()
checkTypeMaybe loc (Just t1) (Just t2) =
  if t1 == t2
    then pure ()
    else do
      tellErr loc $ TypeError t1 t2
checkTypeMaybe _ _ _ = pure ()

transItem :: Type -> Item -> Env ()
transItem type_ item = do
  transType type_
  case item of
    NoInit loc (Ident ident) ->
      newName loc ident $ fromBNFC type_
    Init loc (Ident ident) expr -> do
      resT <- transMonadWrapper transExpr expr
      case resT of
        (Just t) ->
          if t == fromBNFC type_
            then newName loc ident t
            else tellErr loc $ TypeError t (fromBNFC type_)
        _ -> pure ()

-- | Checks wheter type is valid
-- (if it's a defined class, or a primitive type)
transType :: Type -> Env ()
transType x = case x of
  ClassT loc (Ident ident) -> do
    classes <- asks classDefs
    unless (Data.Map.member ident classes) $
      tellErr (BNFC'Position 0 0) $
        Custom $
          printf "Class %s is not defined" ident
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

-- | A wrapper function
-- adjusts the monads' types to make it easier to use
-- tells the error, should it occur
transMonadWrapper :: (a -> EnvExpr TypeLit) -> a -> Env (Maybe TypeLit)
transMonadWrapper f a = do
  env <- get
  context <- ask
  let res = runReaderT (f a) $ ENameMap (fnDefs context) (classDefs context) env Nothing
  case res of
    (Right t) -> pure $ Just t
    (Left (ExpErr loc cause)) -> do
      tellErr loc cause
      pure Nothing

type EnvExpr t = ReaderT ENameMap (Either ExpErr) t

data ENameMap = ENameMap
  { eFnDefs :: FnDefs,
    eClassDefs :: ClassDefs,
    eTypeBinds :: TypeBinds,
    eScope :: Maybe ClassName -- nothing represents global scope
  }

transExpr :: Expr -> EnvExpr TypeLit
transExpr x = case x of
  EVar loc (Ident ident) -> do
    env <- asks eTypeBinds
    case Data.Map.lookup ident env of
      (Just sType) -> pure $ t sType
      Nothing -> do
        -- check if the variable is a member
        scope <- asks eScope
        case scope of
          (Just name) -> do
            classDefs <- asks eClassDefs
            let classDef = classDefs ! name -- TODO handle error: class might be missing
            case Data.Map.lookup ident (classMembers classDef) of
              (Just t) -> pure t
              Nothing -> throwError $ ExpErr loc $ VarNotDeclared ident
          Nothing -> throwError $ ExpErr loc (VarNotDeclared ident)
  EVarR loc (Ident ident) expr -> do
    -- check if the variable is a class
    env <- asks eTypeBinds
    case Data.Map.lookup ident env of
      (Just (SType (Class name) _)) -> do
        -- change the scope in the reader monad
        local (\c -> c {eScope = Just name}) $
          transExpr expr
      _ -> throwError $ ExpErr loc (NotAClass ident)
  ENew loc enew -> transENew loc enew
  ELitInt _ _ ->
    pure Int
  ELitTrue _ ->
    pure Bool
  ELitFalse _ ->
    pure Bool
  ELitNull loc (Ident ident) -> do
    -- check wheter the class is defined
    classDefs <- asks eClassDefs
    case Data.Map.lookup ident classDefs of
      (Just _) -> pure $ Class ident
      Nothing -> throwError $ ExpErr loc $ Custom $ printf "Class %s is not defined" ident
  EApp loc (Ident ident) exprs -> do
    fnDefs <- asks eFnDefs
    case Data.Map.lookup ident fnDefs of
      Nothing -> throwError $ ExpErr loc (NoSuchFn ident)
      Just (FnType retType args) -> do
        exprTypes <- mapM transExpr exprs
        if exprTypes == args
          then pure retType
          else throwError $ ExpErr loc $ CallErr ident exprTypes args
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

-- | Passes the expression, and returns the type of what it refers to
-- Returns error if the expression is not a valid Left-hand-side (ie. a function call, etc)
transLHS :: Expr -> EnvExpr TypeLit
transLHS x =
  case x of
    EVar loc (Ident ident) -> transExpr x
    EVarR loc (Ident ident) expr -> do
      scope <- asks eScope
      -- check wheter variable is declared locally
      env <- asks eTypeBinds
      case Data.Map.lookup ident env of
        -- if it's a class
        (Just (SType (Class className) _)) -> do
          -- change the scope of the reader
          local (\e -> e {eScope = Just className}) $ transLHS expr
        (Just _) -> throwError $ ExpErr loc $ NotAClass ident
    _ -> throwError $ ExpErr (hasPosition x) $ Custom $ printf "Not a valid LHS: %s" (show x)

-- | Verifiec the constructor expression
transENew :: BNFC'Position -> ENew -> EnvExpr TypeLit
transENew loc x = case x of
  NewClass _ type_ -> do
    case type_ of
      ClassT _ (Ident ident) -> do
        classDefs <- asks eClassDefs
        case Data.Map.lookup ident classDefs of
          (Just _) -> pure $ Class ident
          Nothing -> throwError $ ExpErr loc $ Custom $ printf "Class %s is not defined" ident
      _ -> throwError $ ExpErr loc $ Custom $ printf "Not a valid class type: %s" (show type_)

exprTypeErr :: BNFC'Position -> TypeLit -> TypeLit -> EnvExpr TypeLit
exprTypeErr loc t1 t2 = do throwError $ ExpErr loc $ TypeError t1 t2

tellErr :: BNFC'Position -> ErrCause -> Env ()
tellErr (BNFC'Position l c) cause = do
  context <- ask
  tell [SErr ((show . fnLocal) context) (l, c) cause]
