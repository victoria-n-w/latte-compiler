module SType where

data SType
  = Int
  | Str
  | Bool
  | Void
  deriving (Eq, Show)

type ResType = Maybe SType
