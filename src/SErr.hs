module SErr where

import Control.Monad
import Latte.Abs
import SType
import Text.Printf

data ErrCause
  = VarNotDeclared String
  | VarRedeclared String
  | NotImplemented String
  | TypeError SType SType
  | BinOpErr SType SType
  | ReturnTypeErr SType SType
  | NoSuchFn String
  | CallErr String [SType] [SType]
  | NoReturn
  | Custom String

instance Show ErrCause where
  show (VarNotDeclared ident) = "Variable not declared: " ++ ident
  show (VarRedeclared ident) = "Variable redeclared: " ++ ident
  show (NotImplemented what) = "Not implemented: " ++ what
  show (TypeError what expected) = printf "Type error: got %s, expected %s" (show what) (show expected)
  show (BinOpErr t1 t2) = printf "Type error: incorrect types for operation: %s %s" (show t1) (show t2)
  show (ReturnTypeErr what expected) = printf "Type error: incorrect return type: expected %s, got %s" (show expected) (show what)
  show NoReturn = "Possible branch with no returns in a non-void function"
  show (NoSuchFn what) = "No such function: " ++ what
  show (CallErr fnName given expected) = printf "Type error: function %s expected: %s, got %s" fnName (show expected) (show given)
  show (Custom msg) = msg

data SErr = SErr
  { fnName :: String,
    loc :: (Int, Int),
    cause :: ErrCause
  }

instance Show SErr where
  show (SErr fnName (l, c) cause) =
    printf "\ESC[0;31mError:\ESC[0m %s @ %d:%d :: %s\n" fnName l c $ show cause

data ExpErr = ExpErr BNFC'Position ErrCause

data ExprRes t = Ok t | Bad ErrCause BNFC'Position
