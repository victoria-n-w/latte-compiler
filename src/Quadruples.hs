module Quadruples where

import CTypes
import Control.Monad.RWS
import Control.Monad.Writer
import Data.Data
import Data.Foldable
import Data.List (intercalate)
import Data.Map
import Data.Maybe
import Data.Set
import Distribution.Simple.Program (Program (Program))
import Latte.Abs qualified as Latte
import Latte.ErrM
import Semantics (transENew)
import Text.Printf (printf)
import VirtualMethods (VirtualTable (..))
import VirtualMethods qualified

-- module which translates code to internal representation

data Arg
  = Var Loc
  | Const Integer
  | Mem Loc
  | Global Loc
  | GlobalVar String
  | Null
  deriving (Eq)

type LabelName = String

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
  = BinOp Type Op Arg Arg Loc
  | SingleArgOp Type SingOp Arg Loc
  | CmpBinOp Type CmpOp Arg Arg Loc
  | Assign Type Arg Loc
  | Call Loc Type Arg [(Type, Arg)]
  | Label LabelName
  | Jump LabelName
  | JumpIf Arg LabelName LabelName
  | ReturnVoid
  | Return Type Arg
  | Nop
  | LiteralString Loc String
  | -- src type, dst type, src, dst
    Bitcast Type Type Arg Loc
  | -- type, source, destination, first index, second index
    GetElementPtr Type Loc Loc Arg Arg
  | -- type of what is being loaded, src, dst
    Load Type Loc Loc
  | Store Type Arg Loc

type TopDef = TopDef' [Quadruple]

data VarData = VarData {nextLoc :: Loc, varMap :: Data.Map.Map String (Type, Loc)}

data Scope
  = GlobalScope
  | Weak ClassName
  | Strong ClassName

data Context = Context
  { fnMap :: Data.Map.Map String Type, -- function name -> return type
    classMap :: ClassMap, -- class name -> field name -> (type, index)
    vTablesMap :: Data.Map.Map String VirtualMethods.VirtualTable,
    scope :: Scope, -- current scope
    classPtr :: Maybe Loc
  }

data StructDef = StructDef
  { structName :: String,
    structFields :: [Type]
  }

type Env = RWS Context [Quadruple] VarData

translate ::
  Latte.Program ->
  ( [StructDef],
    [VirtualMethods.VirtualTable],
    [TopDef]
  )
translate (Latte.Program _ topdefs) =
  let (classMap, fnMap) = execRWS (mapM_ firstPass topdefs) () Data.Map.empty
      virtualMap = VirtualMethods.makeVirtualTables classMap
      context =
        Context
          { fnMap = fnMap `Data.Map.union` header,
            classMap = classMap,
            vTablesMap = virtualMap,
            scope = GlobalScope,
            classPtr = Nothing
          }
      topdefs' = execWriter (mapM (transTopDef context) topdefs)
   in ( makeStructDefs classMap,
        -- convert virtualMap to list of virtual tables (map elements)
        Data.Map.elems virtualMap,
        topdefs'
      )

makeStructDefs :: ClassMap -> [StructDef]
makeStructDefs classMap =
  let makeStructDef (name, ClassData _ members _ _) =
        StructDef
          { structName = name,
            -- pointer to the vtable, and then all the members
            structFields = Ptr (Int 8) : Data.Map.elems (Data.Map.map fst members)
          }
   in -- map classMap, using the above function, as key value pairs
      Prelude.map makeStructDef (Data.Map.toList classMap)

-- | Passes through the topdef, collecting classes and functions
-- Saves classes in the state
-- And functions in the writer monad
firstPass :: Latte.TopDef -> RWS () (Data.Map.Map String Type) ClassMap ()
firstPass x = case x of
  Latte.FnDef _ type_ (Latte.Ident ident) _ _ -> tell $ Data.Map.singleton ident $ transType type_
  Latte.ClassDef _ (Latte.Ident ident) members -> do
    let (membersMap, methodsMap) = execRWS (mapM firstPassMember $ zip [1 ..] members) () Data.Map.empty
    modify $
      Data.Map.insert ident $
        ClassData
          methodsMap
          membersMap
          Nothing
          (Data.Map.size membersMap + 1) -- +1 for vtable pointer
  Latte.ClassExtend _ (Latte.Ident ident) (Latte.Ident base) members -> do
    let (membersMap, methodsMap) = execRWS (mapM firstPassMember $ zip [1 ..] members) () Data.Map.empty
    modify $
      Data.Map.insert ident $
        ClassData
          methodsMap
          membersMap
          (Just base)
          (Data.Map.size membersMap + 1) -- +1 for vtable pointer

firstPassMember :: (Int, Latte.Member) -> RWS () FnMap MemberMap ()
firstPassMember (index, Latte.Attr _ type_ (Latte.Ident ident)) = do
  -- modify the state with MemberMap, adding a new member
  -- inserting pair (type, index) into the map
  modify $ Data.Map.insert ident (transType type_, index)
firstPassMember (index, Latte.Method _ type_ (Latte.Ident ident) args block) = do
  -- tell the writer monad with a new function
  -- inserting pair (ident, type) into the map
  tell $
    Data.Map.singleton ident $
      FnData (transType type_) (Prelude.map transArgToType args)

-- | Parses the argument, and returns its type
transArgToType :: Latte.Arg -> Type
transArgToType (Latte.Arg _ type_ _) = transType type_

header :: Map String Type
header =
  Data.Map.fromList
    [ ("printInt", Void),
      ("printString", Void),
      ("readInt", Int 32),
      ("readString", Ptr (Int 8)),
      ("new", Ptr (Int 8))
    ]

transTopDef :: Context -> Latte.TopDef -> Writer [TopDef] ()
transTopDef context x = case x of
  Latte.FnDef _ type_ (Latte.Ident ident) args block ->
    do
      -- initial map, mapping all args to numbers from 1 to n
      let varMap = Data.Map.fromList $ zipWith (curry transArg) [1 ..] args
      let (res, _, quadruples) = runRWS (transBlock block) context (VarData (length args + 1) varMap)
      tell
        [ TopDef'
            { name = ident,
              retType = transType type_,
              args = Data.Map.fromList $ Prelude.map (\(a, b) -> (snd b, fst b)) (Data.Map.toList varMap),
              contents =
                [Label "entry"]
                  ++ quadruples
                  ++ [ReturnVoid | not res]
            }
        ]
  Latte.ClassDef _ (Latte.Ident ident) members ->
    -- TODO parse methods
    return ()
  Latte.ClassExtend _ (Latte.Ident name) (Latte.Ident baseName) members ->
    -- TODO parse methods
    return ()

transMember :: Latte.Member -> Type
transMember (Latte.Attr _ type_ _) = transType type_

transArg :: (Int, Latte.Arg) -> (String, (Type, Loc))
transArg (i, Latte.Arg _ type_ (Latte.Ident ident)) = (ident, (transType type_, i))

transBlock :: Latte.Block -> Env Bool
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

transBlockLabels :: Latte.Block -> LabelName -> LabelName -> Env Bool
transBlockLabels block inLabel outLabel = do
  tellLabel inLabel
  isRet <- transBlock block
  unless isRet $ tell [Jump outLabel]
  return isRet

-- | Translates a statement to a list of quadruples
-- returns true, if the statement is a return statement
transStmt :: Latte.Stmt -> Env Bool
transStmt x = case x of
  Latte.Empty _ -> return False
  Latte.BStmt _ block -> transBlock block
  Latte.Decl _ type_ items -> do
    mapM_ (transItem (transType type_)) items
    return False
  Latte.Ass _ lhs expr -> do
    (_, res) <- transExpr expr
    (t, lhsLoc) <- transChained transLHS lhs
    case lhsLoc of
      Var loc -> tell [Assign t res loc]
      Mem loc -> tell [Store t res loc]
    return False
  Latte.Incr _ lhs -> do
    (t, lhsVar) <- transChained transLHS lhs
    case lhsVar of
      Var loc -> do
        tell [BinOp t Add (Var loc) (Const 1) loc]
      Mem loc -> do
        tmpLoc <- getFreeLoc
        tell [Load t loc tmpLoc]
        tell [BinOp t Add (Var tmpLoc) (Const 1) tmpLoc]
        tell [Store t (Var tmpLoc) loc]
    return False
  Latte.Decr _ lhs -> do
    (t, lhsVar) <- transChained transLHS lhs
    case lhsVar of
      Var loc -> do
        tell [BinOp t Sub (Var loc) (Const 1) loc]
      Mem loc -> do
        tmpLoc <- getFreeLoc
        tell [Load t loc tmpLoc]
        tell [BinOp t Sub (Var tmpLoc) (Const 1) tmpLoc]
        tell [Store t (Var tmpLoc) loc]
    return False
  Latte.Ret _ expr -> do
    (t, res) <- transExpr expr
    tell [Return t res]
    return True
  Latte.VRet _ -> do
    tell [ReturnVoid]
    return True
  Latte.Cond _loc expr stmt ->
    case expr of
      Latte.ELitTrue _ -> transStmt stmt
      Latte.ELitFalse _ -> return False
      _ -> do
        -- generate labels
        ltrue <- newLabel
        endLabel <- newLabel
        -- process the condition
        transBoolShortCircuit expr ltrue endLabel
        transBlockLabels (makeBlock stmt) ltrue endLabel
        tell [Label endLabel]
        return False
  Latte.CondElse _ expr stmt1 stmt2 -> do
    case expr of
      Latte.ELitTrue _ -> transStmt stmt1
      Latte.ELitFalse _ -> transStmt stmt2
      _ -> do
        -- generate labels
        ltrue <- newLabel
        lfalse <- newLabel
        endLabel <- newLabel
        -- process the condition
        transBoolShortCircuit expr ltrue lfalse
        isRet1 <- transBlockLabels (makeBlock stmt1) ltrue endLabel
        isRet2 <- transBlockLabels (makeBlock stmt2) lfalse endLabel
        let isRet = isRet1 && isRet2
        unless isRet $ tellLabel endLabel
        return isRet
  Latte.While _ expr stmt -> do
    case expr of
      Latte.ELitTrue _ -> transStmt stmt
      Latte.ELitFalse _ -> return False
      _ -> do
        ltrue <- newLabel
        condLabel <- newLabel
        afterLabel <- newLabel
        tell [Jump condLabel]
        transBlockLabels (makeBlock stmt) ltrue condLabel
        tellLabel condLabel
        transBoolShortCircuit expr ltrue afterLabel
        -- process more code only if the while loop does not return
        tellLabel afterLabel
        return False
  Latte.SExp _ expr -> do
    transExpr expr
    return False

tellLabel :: LabelName -> Env ()
tellLabel label = tell [Label label]

makeBlock :: Latte.Stmt -> Latte.Block
makeBlock stmt = case stmt of
  Latte.BStmt _ block -> block
  _ -> Latte.Block (Latte.hasPosition stmt) [stmt]

transItem :: Type -> Latte.Item -> Env ()
transItem t x = case x of
  Latte.NoInit _ (Latte.Ident ident) -> do
    var <- newVar t ident
    tell [Assign t (Const 0) var]
    return ()
  Latte.Init _ (Latte.Ident ident) expr -> do
    (_, res) <- transExpr expr
    var <- newVar t ident
    tell [Assign t res var]
    return ()

transType :: Latte.Type -> Type
transType x = case x of
  Latte.Int _ -> Int 32
  Latte.Str _ -> Ptr (Int 8)
  Latte.Bool _ -> Int 1
  Latte.Void _ -> Void
  -- Pass structs by reference
  Latte.ClassT _ (Latte.Ident ident) -> Ptr $ Struct ident

-- | Creates a new variable in the context
-- increases its location if it already exists
newVar :: Type -> String -> Env Loc
newVar t ident = do
  (VarData freeLoc map) <- get
  put $ VarData (freeLoc + 1) (Data.Map.insert ident (t, freeLoc) map)
  return freeLoc

newLabel :: Env LabelName
newLabel = do
  (VarData freeLoc map) <- get
  put $ VarData (freeLoc + 1) map
  return $ "label" ++ show freeLoc

transChained :: (Latte.Expr -> Env (Type, Arg)) -> Latte.Expr -> Env (Type, Arg)
transChained f x = case x of
  Latte.EVarR _ (Latte.Ident ident) expr -> do
    scope <- asks scope
    case scope of
      -- TODO different scopes (chaining)
      GlobalScope -> do
        -- get the variable location from the variables map
        varData <- getVar ident
        case varData of
          (Ptr (Struct classname), loc) -> do
            -- modify the reader, changing the scope and classPtr
            local (\c -> c {scope = Strong classname, classPtr = Just loc}) $
              f expr
  _ -> f x

transLHS :: Latte.Expr -> Env (Type, Arg)
transLHS x = case x of
  Latte.EVar _ (Latte.Ident ident) -> do
    scope <- asks scope
    case scope of
      GlobalScope -> do
        -- get the variable location from the variables map
        (t, loc) <- getVar ident
        return (t, Var loc)
      Weak name -> do
        -- try to get the variable from the variables map
        -- if cannot, get it from the current scope
        varMap <- gets varMap
        case Data.Map.lookup ident varMap of
          Just (t, loc) -> return (t, Var loc)
          Nothing -> getFromNamespace name ident
      Strong name -> getFromNamespace name ident

getFromNamespace :: String -> String -> Env (Type, Arg)
getFromNamespace className varName = do
  -- get the class type
  classType <- asks $ fromJust . Data.Map.lookup className . classMap
  let (t, index) = members classType ! varName
  -- tell the code to get the variable from the class
  res <- getFreeLoc -- store the Mem
  structPtr <- asks $ fromJust . classPtr
  tell [GetElementPtr (Struct className) structPtr res (Const 0) (Const (toInteger index))]
  return (t, Mem res)

transExpr :: Latte.Expr -> Env (Type, Arg)
transExpr x = case x of
  Latte.EVar _ _ -> do
    (t, res) <- transLHS x
    makeRHS t res
  Latte.EVarR {} -> do
    transChained transExpr x
  Latte.ENew _ enew -> Quadruples.transENew enew
  Latte.ELitInt _ integer -> return (Int 32, Const integer)
  Latte.ELitTrue _ -> return (Int 1, Const 1)
  Latte.ELitFalse _ -> return (Int 1, Const 0)
  Latte.EApp _ (Latte.Ident ident) exprs -> do
    scope <- asks scope
    case scope of
      GlobalScope -> transGlobalCall ident exprs
      Weak _ -> transGlobalCall ident exprs
      Strong className ->
        callVirtual className ident exprs
  Latte.ELitNull _ (Latte.Ident ident) ->
    return (Ptr (Struct ident), Null)
  Latte.EString _ string -> do
    loc <- getFreeLoc
    tell [LiteralString loc string]
    return (Ptr (Int 8), Var loc)
  Latte.Neg _ expr -> do
    (t, res) <- transExpr expr
    loc <- getFreeLoc
    tell [SingleArgOp t Neg res loc]
    return (t, Var loc)
  Latte.Not _ expr -> do
    (t, res) <- transExpr expr
    loc <- getFreeLoc
    tell [SingleArgOp t Not res loc]
    return (t, Var loc)
  Latte.EMul _ expr1 mulop expr2 ->
    transBinOp expr1 expr2 (transMulOp mulop)
  Latte.EAdd _ expr1 addop expr2 ->
    transBinOp expr1 expr2 (transAddOp addop)
  Latte.ERel _ expr1 relop expr2 -> do
    (t, res1) <- transExpr expr1
    (_, res2) <- transExpr expr2
    loc <- getFreeLoc
    tell [CmpBinOp t (transRelOp relop) res1 res2 loc]
    return (Int 1, Var loc)
  Latte.EAnd {} ->
    transBoolExpr x
  Latte.EOr {} ->
    transBoolExpr x

transGlobalCall :: String -> [Latte.Expr] -> Env (Type, Arg)
transGlobalCall ident exprs = do
  args <- mapM transExpr exprs
  loc <- getFreeLoc
  -- get the function type (function is already defined)
  fnType <- asks $ fromJust . Data.Map.lookup ident . fnMap
  tell [Call loc fnType (GlobalVar ident) args]
  return (fnType, Var loc)

makeRHS :: Type -> Arg -> Env (Type, Arg)
makeRHS t res =
  case res of
    Var loc -> return (t, res)
    Mem loc -> do
      loc' <- getFreeLoc
      tell [Load t loc loc']
      return (t, Var loc')

makeFnType :: VirtualMethods.FnRef -> Type
makeFnType (VirtualMethods.FnRef _ _ fnData) =
  Ptr (Fn (fnType fnData) (fnArgs fnData))

transENew :: Latte.ENew -> Env (Type, Arg)
transENew x = case x of
  Latte.NewClass _ type_ -> do
    let (Ptr (Struct className)) = transType type_
    -- get the class type
    classType <- asks $ fromJust . Data.Map.lookup className . classMap
    -- get the size of the class
    let size = classSize classType
    -- allocate memory for the class
    loc <- getFreeLoc
    -- call the new function
    tell [Call loc (Ptr (Int 8)) (GlobalVar "new") [(Int 32, Const (4 * toInteger size))]]
    -- conver the i8* to the class type
    loc' <- getFreeLoc
    tell [Bitcast (Ptr (Int 8)) (Ptr (Struct className)) (Var loc) loc']
    -- store the pointer to the vtable, in the 0th position
    vTablePtr <- makeVTablePtr className
    return (Struct className, Var loc')

-- | Creates the pointer to the virtual table
-- casts it to the correct type (i8*)
makeVTablePtr :: ClassName -> Env Loc
makeVTablePtr name = do
  vTable <- asks $ fromJust . Data.Map.lookup name . vTablesMap
  loc <- getFreeLoc
  -- bitcast the vtable to i8*
  tell
    [ Bitcast
        (Ptr (makeVTableType vTable))
        (Ptr (Int 8))
        (GlobalVar (VirtualMethods.virtualName vTable))
        loc
    ]
  return loc

-- | Creates the type of the virtual table
-- that means, a pointer to an array of pointers to i8, of size n
makeVTableType :: VirtualMethods.VirtualTable -> Type
makeVTableType vTable =
  Arr (length (VirtualMethods.virtualTable vTable)) (Ptr (Int 8))

transBinOp :: Latte.Expr -> Latte.Expr -> Op -> Env (Type, Arg)
transBinOp expr1 expr2 op = do
  (t, res1) <- transExpr expr1
  (t, res2) <- transExpr expr2
  loc <- getFreeLoc
  case t of
    Ptr (Int 8) -> do
      -- add two strings using the concat function
      tell [Call loc (Ptr (Int 8)) (GlobalVar "concat") [(t, res1), (t, res2)]]
      return (t, Var loc)
    _ -> do
      tell [BinOp t op res1 res2 loc]
      return (t, Var loc)

getFreeLoc :: Env Loc
getFreeLoc = do
  (VarData freeLoc map) <- get
  put $ VarData (freeLoc + 1) map
  return freeLoc

transAddOp :: Latte.AddOp -> Op
transAddOp x = case x of
  Latte.Plus _ -> Add
  Latte.Minus _ -> Sub

transMulOp :: Latte.MulOp -> Op
transMulOp x = case x of
  Latte.Times _ -> Mul
  Latte.Div _ -> Quadruples.Div
  Latte.Mod _ -> Quadruples.Mod

transBoolExpr :: Latte.Expr -> Env (Type, Arg)
transBoolExpr x = do
  ltrue <- newLabel
  lfalse <- newLabel
  transBoolShortCircuit x ltrue lfalse
  endLabel <- newLabel
  -- location of the result
  loc <- getFreeLoc
  tellLabel ltrue
  tell [Assign (Int 1) (Const 1) loc, Jump endLabel]
  tellLabel lfalse
  tell [Assign (Int 1) (Const 0) loc, Jump endLabel]
  tellLabel endLabel
  return (Int 1, Var loc)

transBoolShortCircuit :: Latte.Expr -> LabelName -> LabelName -> Env ()
transBoolShortCircuit expr ltrue lfalse =
  case expr of
    (Latte.EAnd _ expr1 expr2) -> do
      secondExprLabel <- newLabel
      case expr1 of
        Latte.ERel {} -> do
          (_, res) <- transExpr expr1
          tell [JumpIf res secondExprLabel lfalse]
        _ ->
          -- it's a boolean expression
          -- if true, continue to the second expression
          -- if false, short circuit to lfalse
          transBoolShortCircuit expr1 secondExprLabel lfalse
      tellLabel secondExprLabel
      case expr2 of
        Latte.ERel {} -> do
          (_, res) <- transExpr expr2
          tell [JumpIf res ltrue lfalse]
        _ ->
          -- it's a boolean expression
          transBoolShortCircuit expr2 ltrue lfalse
    (Latte.EOr _ expr1 expr2) -> do
      secondExprLabel <- newLabel
      case expr1 of
        Latte.ERel {} -> do
          (_, res) <- transExpr expr1
          tell [JumpIf res ltrue secondExprLabel]
        _ ->
          -- it's a boolean expression
          -- if true, short circuit to ltrue
          -- evaluate the second epression otherwise
          transBoolShortCircuit expr1 ltrue secondExprLabel
      tellLabel secondExprLabel
      case expr2 of
        Latte.ERel {} -> do
          (_, res) <- transExpr expr2
          tell [JumpIf res ltrue lfalse]
        _ ->
          -- it's a boolean expression
          transBoolShortCircuit expr2 ltrue lfalse
    (Latte.ELitFalse _) -> do
      tell [Jump lfalse]
    (Latte.ELitTrue _) -> do
      tell [Jump ltrue]
    _ -> do
      (_, res) <- transExpr expr
      tell [JumpIf res ltrue lfalse]

transRelOp :: Latte.RelOp -> CmpOp
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
  (Jump label) -> [label]
  (JumpIf _ label1 label2) -> [label1, label2]
  _ -> []

getVar :: String -> Env (Type, Loc)
getVar ident = do
  (VarData _ map) <- get
  return $ map Data.Map.! ident

callVirtual :: ClassName -> String -> [Latte.Expr] -> Env (Type, Arg)
callVirtual className ident exprs = do
  vTableData <- asks $ fromJust . Data.Map.lookup className . vTablesMap
  -- get the index of the function in the virtual table
  let index = virtualMap vTableData ! ident
      vTableSize = length $ virtualMap vTableData
      -- get the index-th element of the list
      fnRef = virtualTable vTableData !! index
      vTableType = makeVTableType vTableData
  -- get the pointer to the virtual table
  structPtr <- asks $ fromJust . classPtr
  vTablePtrI8 <- getFreeLoc
  tell [GetElementPtr (Struct className) structPtr vTablePtrI8 (Const 0) (Const 0)]
  -- load the vTablePtr to memory
  vTablePtrLoaded <- getFreeLoc
  tell [Load (Ptr (Int 8)) vTablePtrI8 vTablePtrLoaded]
  -- bitcast the i8* vtable pointer to the correct type
  vTablePtr <- getFreeLoc
  tell
    [ Bitcast
        (Ptr (Int 8))
        (Ptr vTableType)
        (Var vTablePtrLoaded)
        vTablePtr
    ]
  -- get the index-th element of the vtable
  fnPtrPtr <- getFreeLoc
  tell [GetElementPtr vTableType vTablePtr fnPtrPtr (Const 0) (Const (toInteger index))]
  -- load the pointer to the function
  fnPtr <- getFreeLoc
  tell [Load (Ptr (Int 8)) fnPtrPtr fnPtr]
  -- bitcast the i8* function pointer to the correct type
  fnPtr' <- getFreeLoc
  tell [Bitcast (Ptr (Int 8)) (makeFnType fnRef) (Var fnPtr) fnPtr']
  -- call the function
  let retType = fnType $ VirtualMethods.fnData fnRef
  resLoc <- getFreeLoc
  args <- mapM transExpr exprs
  tell [Call resLoc retType (Var fnPtr') args]
  return (retType, Var resLoc)
