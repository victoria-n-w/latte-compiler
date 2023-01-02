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

data Arg = Var Loc | Const Integer

instance Show Arg where
  show :: Arg -> String
  show (Var loc) = "%" ++ show loc
  show (Const i) = show i

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
  = BinOp Op Arg Arg Loc
  | SingleArgOp SingOp Arg Loc
  | CmpBinOp CmpOp Arg Arg Loc
  | Assign Arg Loc
  | Call Loc String [Arg]
  | Label LabelName
  | Jump LabelName
  | JumpIf Arg LabelName LabelName
  | ReturnVoid
  | Return Arg
  | Nop

instance Show Quadruple where
  show :: Quadruple -> String
  show (BinOp op arg1 arg2 loc) = printf "%s = %s %s %s" (show $ Var loc) (show arg1) (show op) (show arg2)
  show (SingleArgOp op arg loc) = printf "%s = %s %s" (show $ Var loc) (show op) (show arg)
  show (CmpBinOp op arg1 arg2 loc) = printf "%s = %s %s %s" (show $ Var loc) (show arg1) (show op) (show arg2)
  show (Assign arg loc) = printf "%s = %s" (show $ Var loc) (show arg)
  show (Call loc name args) = printf "%s = Call i32 @%s(%s)" (show $ Var loc) name $ intercalate ", " (Prelude.map show args)
  show (Jump label) = printf "Jump %s" label
  show (JumpIf arg label1 label2) = printf "JumpIf %s %s %s" (show arg) label1 label2
  show (Label label) = printf "%s:" label
  show ReturnVoid = "ReturnVoid"
  show (Return arg) = printf "Return %s" (show arg)
  show Nop = "Nop"

data TopDef' a = TopDef'
  { name :: String,
    args :: Data.Set.Set Loc,
    contents :: a
  }

instance Show a => Show (TopDef' a) where
  show :: TopDef' a -> String
  show (TopDef' name args contents) =
    name ++ "(" ++ intercalate ", " (Prelude.map show (Data.Set.toList args)) ++ ") {\n" ++ show contents ++ "\n}"

type TopDef = TopDef' [Quadruple]

data Env = Env {nextLoc :: Loc, varMap :: Data.Map.Map String Loc}

type Context = RWS () [Quadruple] Env

translate :: Latte.Program -> [TopDef]
translate (Latte.Program _ topdefs) =
  Prelude.map transTopDef topdefs

transTopDef :: Latte.TopDef -> TopDef
transTopDef x = case x of
  Latte.FnDef _ type_ (Latte.Ident ident) args block ->
    do
      -- initial map, mapping all args to numbers from 1 to n
      let varMap = Data.Map.fromList $ zip (Prelude.map (\(Latte.Arg _ _ (Latte.Ident ident)) -> ident) args) [1 ..]
      let (res, _, quadruples) = runRWS (transBlock block) () (Env (length args + 1) varMap)
      TopDef'
        { name = ident,
          args = Data.Set.fromList [1 .. length args],
          contents =
            [Label "entry"]
              ++ quadruples
              ++ [ReturnVoid | not res]
        }

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
    mapM_ transItem items
    return False
  Latte.Ass _ (Latte.Ident ident) expr -> do
    res <- transExpr expr
    var <- getVarLoc ident
    tell [Assign res var]
    return False
  Latte.Incr _ (Latte.Ident ident) -> do
    var <- getVarLoc ident
    tell [BinOp Add (Var var) (Const 1) var]
    return False
  Latte.Decr _ (Latte.Ident ident) -> do
    var <- getVarLoc ident
    tell [BinOp Sub (Var var) (Const 1) var]
    return False
  Latte.Ret _ expr -> do
    res <- transExpr expr
    tell [Return res]
    return True
  Latte.VRet _ -> do
    tell [ReturnVoid]
    return True
  Latte.Cond _loc expr stmt -> do
    -- generate labels
    blockLabel <- newLabel
    endLabel <- newLabel
    -- process the condition
    res <- transExpr expr
    tell [JumpIf res blockLabel endLabel]
    transBlockLabels (makeBlock stmt) blockLabel endLabel
    tell [Label endLabel]
    return False
  Latte.CondElse _ expr stmt1 stmt2 -> do
    -- generate labels
    block1Label <- newLabel
    block2Label <- newLabel
    endLabel <- newLabel
    -- process the condition
    res <- transExpr expr
    tell [JumpIf res block1Label block2Label]
    isRet1 <- transBlockLabels (makeBlock stmt1) block1Label endLabel
    isRet2 <- transBlockLabels (makeBlock stmt2) block2Label endLabel
    let isRet = isRet1 && isRet2
    unless isRet $ tellLabel endLabel
    return isRet
  Latte.While _ expr stmt -> do
    bodyLabel <- newLabel
    condLabel <- newLabel
    afterLabel <- newLabel
    tell [Jump condLabel]
    transBlockLabels (makeBlock stmt) bodyLabel condLabel
    tellLabel condLabel
    res <- transExpr expr
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

transItem :: Latte.Item -> Context ()
transItem x = case x of
  Latte.NoInit _ (Latte.Ident ident) -> do
    var <- newVar ident
    tell [Assign (Const 0) var]
    return ()
  Latte.Init _ (Latte.Ident ident) expr -> do
    var <- newVar ident
    res <- transExpr expr
    tell [Assign res var]
    return ()

-- | Creates a new variable in the context
-- increases its location if it already exists
newVar :: String -> Context Loc
newVar ident = do
  (Env freeLoc map) <- get
  put $ Env (freeLoc + 1) (Data.Map.insert ident freeLoc map)
  return freeLoc

newLabel :: Context LabelName
newLabel = do
  (Env freeLoc map) <- get
  put $ Env (freeLoc + 1) map
  return $ "label" ++ show freeLoc

transExpr :: Latte.Expr -> Context Quadruples.Arg
transExpr x = case x of
  Latte.EVar _ (Latte.Ident ident) -> do
    var <- getVarLoc ident
    return $ Var var
  Latte.ELitInt _ integer -> return $ Const integer
  Latte.ELitTrue _ -> return $ Const 1
  Latte.ELitFalse _ -> return $ Const 0
  Latte.EApp _ (Latte.Ident ident) exprs -> do
    args <- mapM transExpr exprs
    loc <- getFreeLoc
    tell [Call loc ident args]
    return $ Var loc
  Latte.EString _ string -> do
    tell [Nop]
    return $ Const 0
  Latte.Neg _ expr -> do
    res <- transExpr expr
    loc <- getFreeLoc
    tell [SingleArgOp Neg res loc]
    return $ Var loc
  Latte.Not _ expr -> do
    res <- transExpr expr
    loc <- getFreeLoc
    tell [SingleArgOp Not res loc]
    return $ Var loc
  Latte.EMul _ expr1 mulop expr2 ->
    transBinOp expr1 expr2 (transMulOp mulop)
  Latte.EAdd _ expr1 addop expr2 ->
    transBinOp expr1 expr2 (transAddOp addop)
  Latte.ERel _ expr1 relop expr2 -> do
    res1 <- transExpr expr1
    res2 <- transExpr expr2
    loc <- getFreeLoc
    tell [CmpBinOp (transRelOp relop) res1 res2 loc]
    return $ Var loc
  Latte.EAnd _ expr1 expr2 ->
    transBinOp expr1 expr2 And
  Latte.EOr _ expr1 expr2 ->
    transBinOp expr1 expr2 Or

transBinOp :: Latte.Expr -> Latte.Expr -> Op -> Context Quadruples.Arg
transBinOp expr1 expr2 op = do
  res1 <- transExpr expr1
  res2 <- transExpr expr2
  loc <- getFreeLoc
  tell [BinOp op res1 res2 loc]
  return $ Var loc

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

-- | Return the location of the variable
-- Creates the variable if needed.
getVarLoc :: String -> Context Loc
getVarLoc ident = do
  (Env freeLoc map) <- get
  case Data.Map.lookup ident map of
    Just loc -> return loc
    Nothing -> do
      -- decalre the variable
      put $ Env (freeLoc + 1) (Data.Map.insert ident freeLoc map)
      return freeLoc
