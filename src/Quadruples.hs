module Quadruples where

import Control.Monad.RWS
import Data.Data
import Data.Foldable
import Data.List (intercalate)
import Data.Map
import Data.Maybe
import Data.Set
import Distribution.Simple.Program (Program (Program))
import Latte.Abs qualified as Latte
import Latte.ErrM
import Text.Printf (printf)

-- module which translates code to internal representation

type Loc = Int

data Arg = Var Loc | Const Integer deriving (Eq)

data Type = Int Int | Bool | Void | Ptr Type deriving (Eq)

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
  | Call Loc Type String [(Type, Arg)]
  | Label LabelName
  | Jump LabelName
  | JumpIf Arg LabelName LabelName
  | ReturnVoid
  | Return Type Arg
  | Nop
  | LiteralString Loc String

data TopDef' a = TopDef'
  { name :: String,
    retType :: Type,
    args :: Data.Map.Map Loc Type,
    contents :: a
  }

type TopDef = TopDef' [Quadruple]

data Env = Env {nextLoc :: Loc, varMap :: Data.Map.Map String (Type, Loc)}

type Context = RWS (Data.Map.Map String Type) [Quadruple] Env

translate :: Latte.Program -> [TopDef]
translate (Latte.Program _ topdefs) =
  let fnMap =
        Data.Map.fromList $
          [("printInt", Void), ("printString", Void), ("readInt", Int 32), ("readString", Ptr (Int 8))]
            ++ Prelude.map
              ( \(Latte.FnDef _ type_ (Latte.Ident ident) _ _) ->
                  (ident, transType type_)
              )
              topdefs
   in Prelude.map (transTopDef fnMap) topdefs

transTopDef :: Data.Map.Map String Type -> Latte.TopDef -> TopDef
transTopDef fnMap x = case x of
  Latte.FnDef _ type_ (Latte.Ident ident) args block ->
    do
      -- initial map, mapping all args to numbers from 1 to n
      let varMap = Data.Map.fromList $ zipWith (curry transArg) [1 ..] args
      let (res, _, quadruples) = runRWS (transBlock block) fnMap (Env (length args + 1) varMap)
      TopDef'
        { name = ident,
          retType = transType type_,
          args = Data.Map.fromList $ Prelude.map (\(a, b) -> (snd b, fst b)) (Data.Map.toList varMap),
          contents =
            [Label "entry"]
              ++ quadruples
              ++ [ReturnVoid | not res]
        }

transArg :: (Int, Latte.Arg) -> (String, (Type, Loc))
transArg (i, Latte.Arg _ type_ (Latte.Ident ident)) = (ident, (transType type_, i))

transBlock :: Latte.Block -> Context Bool
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

transBlockLabels :: Latte.Block -> LabelName -> LabelName -> Context Bool
transBlockLabels block inLabel outLabel = do
  tellLabel inLabel
  isRet <- transBlock block
  unless isRet $ tell [Jump outLabel]
  return isRet

-- | Translates a statement to a list of quadruples
-- returns true, if the statement is a return statement
transStmt :: Latte.Stmt -> Context Bool
transStmt x = case x of
  Latte.Empty _ -> return False
  Latte.BStmt _ block -> transBlock block
  Latte.Decl _ type_ items -> do
    mapM_ (transItem (transType type_)) items
    return False
  Latte.Ass _ (Latte.Ident ident) expr -> do
    (_, res) <- transExpr expr
    (t, loc) <- getVar ident
    tell [Assign t res loc]
    return False
  Latte.Incr _ (Latte.Ident ident) -> do
    (t, loc) <- getVar ident
    tell [BinOp t Add (Var loc) (Const 1) loc]
    return False
  Latte.Decr _ (Latte.Ident ident) -> do
    (t, loc) <- getVar ident
    tell [BinOp t Sub (Var loc) (Const 1) loc]
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
        blockLabel <- newLabel
        endLabel <- newLabel
        -- process the condition
        (_, res) <- transExpr expr
        tell [JumpIf res blockLabel endLabel]
        transBlockLabels (makeBlock stmt) blockLabel endLabel
        tell [Label endLabel]
        return False
  Latte.CondElse _ expr stmt1 stmt2 -> do
    case expr of
      Latte.ELitTrue _ -> transStmt stmt1
      Latte.ELitFalse _ -> transStmt stmt2
      _ -> do
        -- generate labels
        block1Label <- newLabel
        block2Label <- newLabel
        endLabel <- newLabel
        -- process the condition
        (_, res) <- transExpr expr
        tell [JumpIf res block1Label block2Label]
        isRet1 <- transBlockLabels (makeBlock stmt1) block1Label endLabel
        isRet2 <- transBlockLabels (makeBlock stmt2) block2Label endLabel
        let isRet = isRet1 && isRet2
        unless isRet $ tellLabel endLabel
        return isRet
  Latte.While _ expr stmt -> do
    case expr of
      Latte.ELitTrue _ -> transStmt stmt
      Latte.ELitFalse _ -> return False
      _ -> do
        bodyLabel <- newLabel
        condLabel <- newLabel
        afterLabel <- newLabel
        tell [Jump condLabel]
        transBlockLabels (makeBlock stmt) bodyLabel condLabel
        tellLabel condLabel
        (_, res) <- transExpr expr
        tell [JumpIf res bodyLabel afterLabel]
        -- process more code only if the while loop does not return
        tellLabel afterLabel
        return False
  Latte.SExp _ expr -> do
    transExpr expr
    return False

tellLabel :: LabelName -> Context ()
tellLabel label = tell [Label label]

makeBlock :: Latte.Stmt -> Latte.Block
makeBlock stmt = case stmt of
  Latte.BStmt _ block -> block
  _ -> Latte.Block (Latte.hasPosition stmt) [stmt]

transItem :: Type -> Latte.Item -> Context ()
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

-- | Creates a new variable in the context
-- increases its location if it already exists
newVar :: Type -> String -> Context Loc
newVar t ident = do
  (Env freeLoc map) <- get
  put $ Env (freeLoc + 1) (Data.Map.insert ident (t, freeLoc) map)
  return freeLoc

newLabel :: Context LabelName
newLabel = do
  (Env freeLoc map) <- get
  put $ Env (freeLoc + 1) map
  return $ "label" ++ show freeLoc

transExpr :: Latte.Expr -> Context (Type, Arg)
transExpr x = case x of
  Latte.EVar _ (Latte.Ident ident) -> do
    (t, loc) <- getVar ident
    return (t, Var loc)
  Latte.ELitInt _ integer -> return (Int 32, Const integer)
  Latte.ELitTrue _ -> return (Int 1, Const 1)
  Latte.ELitFalse _ -> return (Int 1, Const 0)
  Latte.EApp _ (Latte.Ident ident) exprs -> do
    args <- mapM transExpr exprs
    loc <- getFreeLoc
    -- get the function type (function is already defined)
    fnType <- asks (Data.Map.! ident)
    tell [Call loc fnType ident args]
    return (fnType, Var loc)
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
  Latte.EAnd _ expr1 expr2 ->
    transBinOp expr1 expr2 And
  Latte.EOr _ expr1 expr2 ->
    transBinOp expr1 expr2 Or

transBinOp :: Latte.Expr -> Latte.Expr -> Op -> Context (Type, Arg)
transBinOp expr1 expr2 op = do
  (t, res1) <- transExpr expr1
  (t, res2) <- transExpr expr2
  loc <- getFreeLoc
  case t of
    Ptr (Int 8) -> do
      -- add two strings using the concat function
      tell [Call loc (Ptr (Int 8)) "concat" [(t, res1), (t, res2)]]
      return (t, Var loc)
    _ -> do
      tell [BinOp t op res1 res2 loc]
      return (t, Var loc)

getFreeLoc :: Context Loc
getFreeLoc = do
  (Env freeLoc map) <- get
  put $ Env (freeLoc + 1) map
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

getVar :: String -> Context (Type, Loc)
getVar ident = do
  (Env _ map) <- get
  return $ map Data.Map.! ident
