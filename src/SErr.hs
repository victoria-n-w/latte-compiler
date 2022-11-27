module SErr where

import SType
import Text.Printf

data ErrCause
  = VarNotDeclared String
  | VarRedeclared String
  | NotImplemented String
  | TypeError SType SType
  | BinOpErr SType SType
  | ReturnTypeErr SType SType
  | NoMain

instance Show ErrCause where
  show (VarNotDeclared ident) = "Variable not declared: " ++ ident
  show (VarRedeclared ident) = "Variable redeclared: " ++ ident
  show (NotImplemented what) = "Not implemented: " ++ what
  show (TypeError what expected) = printf "Type error: got %s, expected %s" (show what) (show expected)
  show (BinOpErr t1 t2) = printf "Type error: incorrect types for operation: %s %s" (show t1) (show t2)
  show (ReturnTypeErr what expected) = printf "Type error: inccorect return type: expected %s, got %s" (show expected) (show what)
  show NoMain = "No entry point: 'main'"

data SErr = SErr
  { fnName :: String,
    loc :: (Int, Int),
    cause :: ErrCause
  }

instance Show SErr where
  show (SErr fnName (l, c) cause) =
    printf "\ESC[0;31mError:\ESC[0m %s @ %d:%d :: %s\n" fnName l c $ show cause
