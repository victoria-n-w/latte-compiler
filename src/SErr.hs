module SErr where

import Text.Printf

data ErrCause
  = VarNotDeclared String
  | VarRedeclared String
  | NotImplemented String

instance Show ErrCause where
  show (VarNotDeclared ident) = "Variable not declared: " ++ ident
  show (VarRedeclared ident) = "Variable redeclared: " ++ ident
  show (NotImplemented what) = "Not implemented: " ++ what

data SErr = SErr
  { fnName :: String,
    loc :: (Int, Int),
    cause :: ErrCause
  }

instance Show SErr where
  show (SErr fnName (l, c) cause) =
    printf "\ESC[0;31mError:\ESC[0m %s @ %d:%d :: %s\n" fnName l c $ show cause
