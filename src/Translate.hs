module Translate where

import Control.Monad.RWS
import Data.Map qualified as Data
import Latte.Abs
import Latte.ErrM

-- module which translates code to internal representation

type Arg = Int

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
type Context = RWST Translate.Arg [Quadruple] (Data.Map String Translate.Arg) Err

translate :: Program -> Err [Quadruple]
translate p = do
  (_, _, quadruples) <- runRWST (transProgram p) 1 Data.empty
  return quadruples

transProgram :: Latte.Abs.Program -> Context ()
transProgram (Program loc topDefs) =
  mapM_ transTopDef topDefs

getFreeArg :: Context Translate.Arg
getFreeArg = do
  freeVar <- ask
  local (+ 1) $ return freeVar -- TODO check if it works

failure :: Show a => a -> Context ()
failure x = fail $ "Undefined case: " ++ show x

transIdent :: Latte.Abs.Ident -> Context ()
transIdent x = case x of
  Latte.Abs.Ident string -> failure x

transTopDef :: Latte.Abs.TopDef -> Context ()
transTopDef x = case x of
  Latte.Abs.FnDef _ type_ ident args block -> do
    mapM_ transArg args
    transBlock block

transArg :: Latte.Abs.Arg -> Context ()
transArg (Latte.Abs.Arg _ _ (Ident ident)) = do
  -- put argument to context
  env <- get
  freeArg <- getFreeArg
  put $ Data.insert ident freeArg env

transBlock :: Latte.Abs.Block -> Context ()
transBlock (Latte.Abs.Block _ stmts) =
  mapM_ transStmt stmts

transStmt :: Latte.Abs.Stmt -> Context ()
transStmt x = case x of
  Empty _ -> return ()
  BStmt _ block -> transBlock block
  Decl _ type_ items -> mapM_ transItem items
  Ass _ ident expr -> do
    transExpr expr
  Incr _ ident -> failure x
  Decr _ ident -> failure x
  Ret _ expr -> failure x
  VRet _ -> failure x
  Cond _ expr stmt -> failure x
  CondElse _ expr stmt1 stmt2 -> failure x
  While _ expr stmt -> failure x
  SExp _ expr -> failure x

transItem :: Latte.Abs.Item -> Context ()
transItem x = case x of
  Latte.Abs.NoInit _ (Ident ident) -> failure x
  Latte.Abs.Init _ (Ident ident) expr -> failure x

transType :: Latte.Abs.Type -> Context ()
transType x = case x of
  Latte.Abs.Int _ -> failure x
  Latte.Abs.Str _ -> failure x
  Latte.Abs.Bool _ -> failure x
  Latte.Abs.Void _ -> failure x
  Latte.Abs.Fun _ type_ types -> failure x

transExpr :: Latte.Abs.Expr -> Context ()
transExpr x = case x of
  Latte.Abs.EVar _ ident -> failure x
  Latte.Abs.ELitInt _ integer -> failure x
  Latte.Abs.ELitTrue _ -> failure x
  Latte.Abs.ELitFalse _ -> failure x
  Latte.Abs.EApp _ ident exprs -> failure x
  Latte.Abs.EString _ string -> failure x
  Latte.Abs.Neg _ expr -> failure x
  Latte.Abs.Not _ expr -> failure x
  Latte.Abs.EMul _ expr1 mulop expr2 -> failure x
  Latte.Abs.EAdd _ expr1 addop expr2 -> failure x
  Latte.Abs.ERel _ expr1 relop expr2 -> failure x
  Latte.Abs.EAnd _ expr1 expr2 -> failure x
  Latte.Abs.EOr _ expr1 expr2 -> failure x

transAddOp :: Latte.Abs.AddOp -> Context ()
transAddOp x = case x of
  Latte.Abs.Plus _ -> failure x
  Latte.Abs.Minus _ -> failure x

transMulOp :: Latte.Abs.MulOp -> Context ()
transMulOp x = case x of
  Latte.Abs.Times _ -> failure x
  Latte.Abs.Div _ -> failure x
  Latte.Abs.Mod _ -> failure x

transRelOp :: Latte.Abs.RelOp -> Context ()
transRelOp x = case x of
  Latte.Abs.LTH _ -> failure x
  Latte.Abs.LE _ -> failure x
  Latte.Abs.GTH _ -> failure x
  Latte.Abs.GE _ -> failure x
  Latte.Abs.EQU _ -> failure x
  Latte.Abs.NE _ -> failure x
