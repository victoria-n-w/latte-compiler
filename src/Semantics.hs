module Semantics where

import Latte.Abs

import Control.Monad.RWS
import Data.Map
import Text.Printf

data SResult = Ok | Error [String]

verify:: Program -> SResult
verify program =
  let (_, res) = evalRWS (transProgram program) "top-level" empty in
  case res of
    [] -> Ok
    _ -> Error res

type TypeBinds = Map String Type

type Context = RWS String [String] TypeBinds

failure :: Show a => a -> Context ()
failure x = do
  fnName <- ask
  tell [printf "@%s: Undefined case %s" fnName (show x) ]    

transIdent :: Ident -> Context ()
transIdent x = case x of
  Ident string -> failure x

transProgram :: Program -> Context ()
transProgram (Program _ topDefs) = 
  mapM_ transTopDef topDefs

transTopDef :: TopDef -> Context ()
transTopDef (FnDef _ type_ (Ident fnName) args block) =
  local (const fnName) $ transBlock block

transArg ::Arg -> Context ()
transArg x = case x of
  Arg _ type_ ident -> failure x

transBlock ::Block -> Context ()
transBlock (Block _ stmts) = do
  env <- get
  mapM_ transStmt stmts
  -- leave the environment unchanged after leaving a code block
  put env

transStmt ::Stmt -> Context ()
transStmt (Empty _) = return ()

transStmt (BStmt _ block) =
  transBlock block

transStmt (Decl _ type_ items) = return ()

transStmt (Ass _ ident expr) = return ()

transStmt (Incr _ ident) = return ()

transStmt (Decr _ ident) = return ()

transStmt (Ret _ expr) = return ()

transStmt (VRet _) = return ()

transStmt (Cond _ expr stmt) = return ()

transStmt (CondElse _ expr stmt1 stmt2) = return ()

transStmt (While _ expr stmt) = return ()

transStmt (SExp _ expr) = return ()

transItem ::Item -> Context ()
transItem x = case x of
  NoInit _ ident -> failure x
  Init _ ident expr -> failure x

transType ::Type -> Context ()
transType x = case x of
  Int _ -> failure x
  Str _ -> failure x
  Bool _ -> failure x
  Void _ -> failure x
  Fun _ type_ types -> failure x

transExpr ::Expr -> Context ()
transExpr x = case x of
  EVar _ ident -> failure x
  ELitInt _ integer -> failure x
  ELitTrue _ -> failure x
  ELitFalse _ -> failure x
  EApp _ ident exprs -> failure x
  EString _ string -> failure x
  Neg _ expr -> failure x
  Not _ expr -> failure x
  EMul _ expr1 mulop expr2 -> failure x
  EAdd _ expr1 addop expr2 -> failure x
  ERel _ expr1 relop expr2 -> failure x
  EAnd _ expr1 expr2 -> failure x
  EOr _ expr1 expr2 -> failure x

transAddOp ::AddOp -> Context ()
transAddOp x = case x of
  Plus _ -> failure x
  Minus _ -> failure x

transMulOp ::MulOp -> Context ()
transMulOp x = case x of
  Times _ -> failure x
  Div _ -> failure x
  Mod _ -> failure x

transRelOp ::RelOp -> Context ()
transRelOp x = case x of
  LTH _ -> failure x
  LE _ -> failure x
  GTH _ -> failure x
  GE _ -> failure x
  EQU _ -> failure x
  NE _ -> failure x
