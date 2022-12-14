module Translate where

import Control.Monad.RWS
import Data.Map qualified as Data
import Latte.Abs
import Latte.ErrM

-- module which translates code to internal representation

type Loc = Int

data Arg = Var Loc | Const Integer | None
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
  | Label
  | Jump
  | JumpIf
  | JumpIfNot
  | Return
  | ReturnVoid
  deriving (Show)

data Quadruple = Quadruple
  { op :: Translate.Op,
    arg1 :: Translate.Arg,
    arg2 :: Translate.Arg,
    res :: Translate.Arg
  }
  deriving (Show)

-- type Context, which is a RWS monad transform, with
-- - unit reader
-- - map of variables to their addresses
-- - result writer, storing a list of quadruples
-- with internal monad error type
type Context = RWST Loc [Quadruple] (Data.Map String Loc) Err

translate :: Program -> Err [Quadruple]
translate p = do
  (_, _, quadruples) <- runRWST (transProgram p) 1 Data.empty
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
    transBlock block

transArg :: Latte.Abs.Arg -> Context ()
transArg (Latte.Abs.Arg _ _ ident) = do
  newVar ident
  return ()

transBlock :: Latte.Abs.Block -> Context ()
transBlock (Latte.Abs.Block _ stmts) =
  mapM_ transStmt stmts

transStmt :: Latte.Abs.Stmt -> Context ()
transStmt x = case x of
  Empty _ -> return ()
  BStmt _ block -> transBlock block
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
  Cond _ expr stmt -> failure x
  CondElse _ expr stmt1 stmt2 -> failure x
  While _ expr stmt -> failure x
  SExp _ expr -> do
    transExpr expr
    return ()

transItem :: Latte.Abs.Item -> Context ()
transItem x = case x of
  Latte.Abs.NoInit _ (Ident ident) -> return ()
  Latte.Abs.Init _ (Ident ident) expr -> failure x

newVar :: Ident -> Context Translate.Arg
newVar (Ident ident) = do
  env <- get
  freeLoc <- getFreeLoc
  put $ Data.insert ident freeLoc env
  return $ Var freeLoc

getFreeLoc :: Context Loc
getFreeLoc = do
  freeVar <- ask
  local (+ 1) $ return freeVar -- TODO check if it works

getVar :: Ident -> Context Translate.Arg
getVar (Ident ident) = do
  env <- get
  case Data.lookup ident env of
    Just loc -> return $ Var loc
    Nothing -> fail $ "Variable " ++ ident ++ " not found"

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
  tell [Quadruple op res1 res2 (Var loc)]
  return (Var loc)

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
