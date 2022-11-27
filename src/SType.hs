module SType where

import Data.Bool qualified as SType
import Latte.Abs

data SType
  = Int
  | Str
  | Bool
  | Void
  deriving (Eq)

instance Show SType where
  show :: SType -> String
  show SType.Int = "int"
  show SType.Str = "string"
  show SType.Bool = "boolean"
  show SType.Void = ""

fromBNFC :: Type -> SType
fromBNFC (Latte.Abs.Int _) = SType.Int
fromBNFC (Latte.Abs.Str _) = SType.Str
fromBNFC (Latte.Abs.Bool _) = SType.Bool

type ResType = Maybe SType
