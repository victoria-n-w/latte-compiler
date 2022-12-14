module Translate where

import Control.Monad.RWS
import Data.Maybe
import Data.Data
import Data.Map
import Latte.Abs
import Latte.ErrM

-- module which translates code to internal representation

type Loc = Int

data Arg = Var Loc | Const Integer | None | Target LabelName
  deriving (Show)

data Op
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  | Neg
  | And
  | Or
  | Not
  | Eq
  | Neq
  | Lt
  | Gt
  | Le
  | Ge
  | Assign
  | Get
  | Put
  | Call
  | Label LabelName
  | Jump
  | JumpIf
  | JumpIfNot
  | Return
  | ReturnVoid
  deriving (Data, Typeable)

instance Show Op where
  show (Label label) = label ++ ":"
  show op = show $ toConstr op

type LabelName = String

data Quadruple = Quadruple
  { op :: Translate.Op,
    arg1 :: Translate.Arg,
    arg2 :: Translate.Arg,
    res :: Translate.Arg
  }

instance Show Quadruple where
  show :: Quadruple -> String
  show (Quadruple op arg1 arg2 res) =
    case op of
      Label _ -> show op
      _ -> "\t" ++ show res ++ " << " ++ show op ++ " " ++ show arg1 ++ " " ++ show arg2

data Env = Env {nextLoc :: Loc, varMap :: Data.Map.Map String Loc}

type Context = RWST () [Quadruple] Env Err

translate :: Program -> Err [Quadruple]
translate p = do
  (_, _, quadruples) <- runRWST (transProgram p) () (Env 1 Data.Map.empty)
  return quadruples

transProgram :: Latte.Abs.Program -> Context ()
transProgram (Program loc topDefs) =
  mapM_ transTopDef topDefs

failure :: Show a => a -> Context ()
failure x = fail $ "Undefined case: " ++ show x

transTopDef :: Latte.Abs.TopDef -> Context ()
transTopDef x = case x of
  Latte.Abs.FnDef _ type_ ident args block -> do
    mapM_ transArg args
    transBlock block Nothing Nothing

transArg :: Latte.Abs.Arg -> Context ()
transArg (Latte.Abs.Arg _ _ ident) = do
  newVar ident
  return ()

transBlock :: Block -> Maybe LabelName -> Maybe LabelName -> Context ()
transBlock (Block _ stmts) inLabel outLabel = do
  -- at the beginning of a block, if there is an in label, flag it
  when (isJust inLabel) $ tellLabel $ fromJust inLabel
  mapM_ transStmt stmts
  -- at the end of a block, if there is an out label, jump to it
  when (isJust outLabel) $ tell [Quadruple Jump (Target (fromJust outLabel)) None None]

transStmt :: Latte.Abs.Stmt -> Context ()
transStmt x = case x of
  Empty _ -> return ()
  BStmt _ block -> transBlock block Nothing Nothing
  Decl _ type_ items -> mapM_ transItem items
  Ass _ ident expr -> do
    res <- transExpr expr
    loc <- newVar ident
    tell [Quadruple Assign res None loc]
  Incr _ ident -> do
    oldLoc <- getVar ident
    newLoc <- newVar ident
    tell [Quadruple Add oldLoc (Const 1) newLoc]
  Decr _ ident -> do
    oldLoc <- getVar ident
    newLoc <- newVar ident
    tell [Quadruple Sub oldLoc (Const 1) newLoc]
  Ret _ expr -> do
    res <- transExpr expr
    tell [Quadruple Return res None None]
  VRet _ -> do
    tell [Quadruple ReturnVoid None None None]
  Cond _loc expr stmt -> do
    res <- transExpr expr
    blockLabel <- newLabel
    endLabel <- newLabel
    tell [Quadruple JumpIf res (Target blockLabel) (Target endLabel)]
    transBlock (makeBlock stmt) (Just blockLabel) (Just endLabel)
    tell [Quadruple (Label endLabel) None None None]
  CondElse _ expr stmt1 stmt2 -> do
    res <- transExpr expr
    block1Label <- newLabel
    block2Label <- newLabel
    endLabel <- newLabel
    tell [Quadruple JumpIf res (Target block1Label) (Target block2Label)]
    transBlock (makeBlock stmt1) (Just block1Label) (Just endLabel)
    transBlock (makeBlock stmt2) (Just block2Label) (Just endLabel)
    tellLabel endLabel
  While _ expr stmt -> do
    condLabel <- newLabel
    tellLabel condLabel
    res <- transExpr expr
    blockLabel <- newLabel
    endLabel <- newLabel
    tell [Quadruple JumpIf res (Target blockLabel) (Target endLabel)]
    transBlock (makeBlock stmt) (Just blockLabel) (Just condLabel)
    tellLabel endLabel
  SExp _ expr -> do
    transExpr expr
    return ()

tellLabel :: LabelName -> Context ()
tellLabel label = tell [Quadruple (Label label) None None None]

makeBlock :: Stmt -> Block
makeBlock stmt = case stmt of
  BStmt _ block -> block
  _ -> Block (hasPosition stmt) [stmt]

transItem :: Latte.Abs.Item -> Context ()
transItem x = case x of
  Latte.Abs.NoInit _ (Ident ident) -> return ()
  Latte.Abs.Init _ ident expr -> do
    res <- transExpr expr
    loc <- newVar ident
    tell [Quadruple Assign res None loc]

newVar :: Ident -> Context Translate.Arg
newVar (Ident ident) = do
  (Env freeLoc env) <- get
  put $ Env (freeLoc + 1) $ Data.Map.insert ident freeLoc env
  return $ Var freeLoc

getVar :: Ident -> Context Translate.Arg
getVar (Ident ident) = do
  (Env _ env) <- get
  case Data.Map.lookup ident env of
    Just loc -> return $ Var loc
    Nothing -> fail $ "Variable " ++ ident ++ " not found"

newLabel :: Context LabelName
newLabel = do
  (Env freeLoc map) <- get
  put $ Env (freeLoc + 1) map
  return $ "label" ++ show freeLoc

transExpr :: Latte.Abs.Expr -> Context Translate.Arg
transExpr x = case x of
  EVar _ ident -> getVar ident
  ELitInt _ integer -> return $ Const integer
  ELitTrue _ -> failExp x
  ELitFalse _ -> failExp x
  EApp _ ident exprs -> failExp x
  EString _ string -> failExp x
  Latte.Abs.Neg _ expr -> failExp x
  Latte.Abs.Not _ expr -> failExp x
  EMul _ expr1 mulop expr2 ->
    transBinOp expr1 expr2 (transMulOp mulop)
  EAdd _ expr1 addop expr2 ->
    transBinOp expr1 expr2 (transAddOp addop)
  ERel _ expr1 relop expr2 ->
    transBinOp expr1 expr2 (transRelOp relop)
  EAnd _ expr1 expr2 ->
    transBinOp expr1 expr2 And
  EOr _ expr1 expr2 ->
    transBinOp expr1 expr2 Or

transBinOp :: Expr -> Expr -> Op -> Context Translate.Arg
transBinOp expr1 expr2 op = do
  res1 <- transExpr expr1
  res2 <- transExpr expr2
  loc <- getFreeLoc
  tell [Quadruple op res1 res2 loc]
  return loc

getFreeLoc :: Context Translate.Arg
getFreeLoc = do
  (Env freeLoc map) <- get
  put $ Env (freeLoc + 1) map
  return $ Var freeLoc

failExp :: Show a => a -> Context Translate.Arg
failExp x = fail $ "Undefined case: " ++ show x

transAddOp :: Latte.Abs.AddOp -> Op
transAddOp x = case x of
  Latte.Abs.Plus _ -> Add
  Latte.Abs.Minus _ -> Sub

transMulOp :: Latte.Abs.MulOp -> Op
transMulOp x = case x of
  Latte.Abs.Times _ -> Mul
  Latte.Abs.Div _ -> Translate.Div
  Latte.Abs.Mod _ -> Translate.Mod

transRelOp :: Latte.Abs.RelOp -> Op
transRelOp x = case x of
  Latte.Abs.LTH _ -> Lt
  Latte.Abs.LE _ -> Le
  Latte.Abs.GTH _ -> Gt
  Latte.Abs.GE _ -> Ge
  Latte.Abs.EQU _ -> Eq
  Latte.Abs.NE _ -> Neq
