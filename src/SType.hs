module SType where

import Latte.Abs

data SType
  = Int
  | Str
  | Bool
  | Void
  deriving (Eq, Show)

fromBNFC :: Type -> SType
fromBNFC (Latte.Abs.Int _) = SType.Int
fromBNFC (Latte.Abs.Str _) = SType.Str
fromBNFC (Latte.Abs.Bool _) = SType.Bool

type ResType = Maybe SType
