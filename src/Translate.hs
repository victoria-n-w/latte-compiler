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
type Context = RWST () (Data.Map String Int) [Quadruple] Err

translate :: Program -> Err [Quadruple]
translate p = do
  (_, quadruples, _) <- runRWST (transProgram p) () []
  return quadruples

transProgram :: Show a => Latte.Abs.Program' a -> Context ()
transProgram x = case x of
  Latte.Abs.Program _ topdefs -> failure x

failure :: Show a => a -> Context ()
failure x = fail $ "Undefined case: " ++ show x

transIdent :: Latte.Abs.Ident -> Context ()
transIdent x = case x of
  Latte.Abs.Ident string -> failure x

transTopDef :: Show a => Latte.Abs.TopDef' a -> Context ()
transTopDef x = case x of
  Latte.Abs.FnDef _ type_ ident args block -> failure x

transArg :: Show a => Latte.Abs.Arg' a -> Context ()
transArg x = case x of
  Latte.Abs.Arg _ type_ ident -> failure x

transBlock :: Show a => Latte.Abs.Block' a -> Context ()
transBlock x = case x of
  Latte.Abs.Block _ stmts -> failure x

transStmt :: Show a => Latte.Abs.Stmt' a -> Context ()
transStmt x = case x of
  Latte.Abs.Empty _ -> failure x
  Latte.Abs.BStmt _ block -> failure x
  Latte.Abs.Decl _ type_ items -> failure x
  Latte.Abs.Ass _ ident expr -> failure x
  Latte.Abs.Incr _ ident -> failure x
  Latte.Abs.Decr _ ident -> failure x
  Latte.Abs.Ret _ expr -> failure x
  Latte.Abs.VRet _ -> failure x
  Latte.Abs.Cond _ expr stmt -> failure x
  Latte.Abs.CondElse _ expr stmt1 stmt2 -> failure x
  Latte.Abs.While _ expr stmt -> failure x
  Latte.Abs.SExp _ expr -> failure x

transItem :: Show a => Latte.Abs.Item' a -> Context ()
transItem x = case x of
  Latte.Abs.NoInit _ ident -> failure x
  Latte.Abs.Init _ ident expr -> failure x

transType :: Show a => Latte.Abs.Type' a -> Context ()
transType x = case x of
  Latte.Abs.Int _ -> failure x
  Latte.Abs.Str _ -> failure x
  Latte.Abs.Bool _ -> failure x
  Latte.Abs.Void _ -> failure x
  Latte.Abs.Fun _ type_ types -> failure x

transExpr :: Show a => Latte.Abs.Expr' a -> Context ()
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

transAddOp :: Show a => Latte.Abs.AddOp' a -> Context ()
transAddOp x = case x of
  Latte.Abs.Plus _ -> failure x
  Latte.Abs.Minus _ -> failure x

transMulOp :: Show a => Latte.Abs.MulOp' a -> Context ()
transMulOp x = case x of
  Latte.Abs.Times _ -> failure x
  Latte.Abs.Div _ -> failure x
  Latte.Abs.Mod _ -> failure x

transRelOp :: Show a => Latte.Abs.RelOp' a -> Context ()
transRelOp x = case x of
  Latte.Abs.LTH _ -> failure x
  Latte.Abs.LE _ -> failure x
  Latte.Abs.GTH _ -> failure x
  Latte.Abs.GE _ -> failure x
  Latte.Abs.EQU _ -> failure x
  Latte.Abs.NE _ -> failure x
