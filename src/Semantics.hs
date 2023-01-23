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
import TopDefs qualified

data SResult = Ok | Error [SErr]

verify :: Program -> SResult
verify program =
  let (_, res) =
        evalRWS
          (transProgram program)
          (Context (FnLocal "top-level" Void) empty empty 0 Global)
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
    scope :: Scope
  }

data Scope
  = Global
  | Weak String -- variable can both be a member, or defined locally
  | Strong String -- variable has to be a member

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
          (const $ Context (FnLocal "top-level" Void) (fnMap `union` header) classMap 0 Global)
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
    local (\context -> context {scope = Weak ident}) $ do
      mapM_ transMember members
  Latte.Abs.ClassExtend loc (Ident ident) (Ident baseClass) members -> do
    -- verify that the base class exists
    baseClassExists <- asks $ member baseClass . classDefs
    unless baseClassExists $ tellErr loc $ Custom $ "Base class " ++ baseClass ++ " does not exist"
    -- modify the scope in the reader monad
    local (\context -> context {scope = Weak ident}) $ do
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

checkType :: BNFC'Position -> Maybe TypeLit -> TypeLit -> Env Bool
checkType loc resT t = checkTypeMaybe loc resT (Just t)

-- | Verify if the types are the same
-- or if one is a subclass of the other
-- Tell an error if they are not
checkTypeMaybe :: BNFC'Position -> Maybe TypeLit -> Maybe TypeLit -> Env Bool
checkTypeMaybe loc (Just t1) (Just t2) =
  case (t1, t2) of
    (Class class1, Class class2) -> do
      -- check if the classes are the same
      -- or if one inherits from the other
      classDefs <- asks classDefs
      let classEq =
            class1 == class2
              || inheritsFrom classDefs class1 class2
              || inheritsFrom classDefs class2 class1
      unless classEq $ tellErr loc $ TypeError t1 t2
      pure classEq
    (t1, t2) -> do
      let typesEq = t1 == t2
      unless typesEq $ tellErr loc $ TypeError t1 t2
      pure typesEq
checkTypeMaybe _ _ _ = pure False

inheritsFrom :: Map ClassName ClassDef -> ClassName -> ClassName -> Bool
inheritsFrom classMap className1 className2 =
  case Data.Map.lookup className1 classMap of
    (Just (SType.ClassDef _ _ _ (Just baseClass))) ->
      className2 == baseClass || inheritsFrom classMap baseClass className2
    _ -> False

transItem :: Type -> Item -> Env ()
transItem type_ item = do
  transType type_
  case item of
    NoInit loc (Ident ident) ->
      newName loc ident $ fromBNFC type_
    Init loc (Ident ident) expr -> do
      resT <- transMonadWrapper transExpr expr
      typesEq <- checkTypeMaybe loc resT (Just $ fromBNFC type_)
      when typesEq $ newName loc ident $ fromBNFC type_

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
  let newContext = ENameMap (fnDefs context) (classDefs context) env (scope context) (scope context)
  let res = runReaderT (f a) newContext
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
    eScope :: Scope,
    baseScope :: Scope -- the scope in which the expression is evaluated,
    -- doesn't change through the expression
  }

transExpr :: Expr -> EnvExpr TypeLit
transExpr x = case x of
  EVar loc (Ident ident) -> do
    scope <- asks eScope
    case scope of
      (Strong className) -> getInScope className classMembers loc ident
      (Weak className) -> do
        -- first, check if the variable is defined locally
        -- if not, check if it's the class member
        env <- asks eTypeBinds
        case Data.Map.lookup ident env of
          (Just (SType type_ _)) -> pure type_
          Nothing -> getInScope className classMembers loc ident
      Global -> do
        env <- asks eTypeBinds
        case Data.Map.lookup ident env of
          (Just (SType type_ _)) -> pure type_
          Nothing -> throwError $ ExpErr loc $ VarNotDeclared ident
  EApp loc (Ident ident) exprs -> do
    -- check the expression types in the current scope
    scope <- asks eScope
    case scope of
      Strong className -> do
        fnType <- getInScope className classMethods loc ident
        checkCall loc ident fnType exprs
      _ -> do
        -- in case of weak, or global scope, check if function is defined
        fnDefs <- asks eFnDefs
        case Data.Map.lookup ident fnDefs of
          Nothing -> throwError $ ExpErr loc (NoSuchFn ident)
          Just fnType ->
            checkCall loc ident fnType exprs
  EVarR loc (Ident ident) expr ->
    if ident == "self"
      then do
        scope <- asks eScope
        case scope of
          (Strong className) -> throwError $ ExpErr loc $ Custom "Cannot reffer to self in this context"
          (Weak className) ->
            do
              -- change the scope to strong
              local (\context -> context {eScope = Strong className})
              $ transExpr expr
          _ -> throwError $ ExpErr loc $ Custom "self is not defined in this scope"
      else chainScope ident transExpr expr
  EAppR loc (Ident ident) exprs expr -> do
    fnRetT <- transExpr $ EApp loc (Ident ident) exprs
    chainScope' fnRetT (printf "the return value of function %s" ident) transExpr expr
  ENew loc enew -> transENew enew
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
    EVarR loc (Ident ident) expr ->
      chainScope ident transLHS expr
    _ -> throwError $ ExpErr (hasPosition x) $ Custom $ printf "Not a valid LHS: %s" (show x)

getInScope :: ClassName -> (ClassDef -> Map String a) -> BNFC'Position -> String -> EnvExpr a
getInScope className f loc ident = do
  classDefs <- asks eClassDefs
  case Data.Map.lookup className classDefs of
    (Just classDef) -> do
      case Data.Map.lookup ident (f classDef) of
        (Just x) -> pure x
        Nothing ->
          case baseClass classDef of
            (Just baseClassName) -> getInScope baseClassName f loc ident
            Nothing -> throwError $ ExpErr loc $ Custom $ printf "Could not find %s in scope %s" ident className
    Nothing -> throwError $ ExpErr loc $ Custom $ printf "Class %s is not defined" className

-- | Chains the scope of the expression (ie. `ident.x`)
-- Arguments:
--  * ident - name of the chained variable
--  * f - the function to apply in the context with the new scope
--  * x - the expression to apply the function f to
chainScope :: HasPosition a => String -> (a -> EnvExpr b) -> a -> EnvExpr b
chainScope ident f x = do
  scope <- asks eScope
  case scope of
    Strong className -> do
      varT <- getInScope className classMembers (hasPosition x) ident
      chainScope' varT ident f x
    Weak className -> do
      -- first check, it it's a local variable
      env <- asks eTypeBinds
      case Data.Map.lookup ident env of
        (Just (SType t _)) -> do
          chainScope' t ident f x
        Nothing -> do
          -- then check if it's a class member
          varT <- getInScope className classMembers (hasPosition x) ident
          chainScope' varT ident f x
    Global -> do
      -- check if variable is declared
      env <- asks eTypeBinds
      case Data.Map.lookup ident env of
        (Just (SType t _)) -> do
          -- change the scope of the reader
          chainScope' t ident f x
        Nothing -> throwError $ ExpErr (hasPosition x) $ VarNotDeclared ident

-- | Evaluates the function f in the context of the new scope
-- Throws error, if the given type is not a class
-- Arguments:
--  * t - the type of the variable, in which the scope is chained
--  * ident - name of the chained variable, for error messages
--  * f - the function to apply in the context with the new scope
chainScope' :: HasPosition a => TypeLit -> String -> (a -> EnvExpr b) -> a -> EnvExpr b
chainScope' t ident f x = do
  case t of
    Class className -> do
      -- change the scope of the reader
      local (\e -> e {eScope = Strong className}) $ f x
    _ -> throwError $ ExpErr (hasPosition x) $ NotAClass ident t

-- | Verifiec the constructor expression
transENew :: ENew -> EnvExpr TypeLit
transENew x = case x of
  NewClass loc type_ -> do
    case type_ of
      ClassT _ (Ident ident) -> do
        classDefs <- asks eClassDefs
        case Data.Map.lookup ident classDefs of
          (Just _) -> pure $ Class ident
          Nothing -> throwError $ ExpErr loc $ Custom $ printf "Class %s is not defined" ident
      _ -> throwError $ ExpErr loc $ Custom $ printf "Not a valid class type: %s" (show type_)

checkCall :: BNFC'Position -> String -> FnType -> [Expr] -> EnvExpr TypeLit
checkCall loc ident (FnType retType args) exprTypes = do
  baseScope <- asks baseScope
  exprTypes <- local (\e -> e {eScope = baseScope}) $ mapM transExpr exprTypes
  if exprTypes == args
    then pure retType
    else throwError $ ExpErr loc $ CallErr ident exprTypes args

exprTypeErr :: BNFC'Position -> TypeLit -> TypeLit -> EnvExpr TypeLit
exprTypeErr loc t1 t2 = do throwError $ ExpErr loc $ TypeError t1 t2

tellErr :: BNFC'Position -> ErrCause -> Env ()
tellErr (BNFC'Position l c) cause = do
  context <- ask
  tell [SErr ((show . fnLocal) context) (l, c) cause]
