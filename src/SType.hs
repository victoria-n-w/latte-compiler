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
fromBNFC (Latte.Abs.Void _) = SType.Void

data FnType = FnType
  { ret :: SType,
    args :: [SType]
  }

makeFnEntry :: TopDef -> (String, FnType)
makeFnEntry (FnDef _ type_ (Ident fnName) args _) =
  ( fnName,
    FnType (fromBNFC type_) $
      map (fromBNFC . \(Arg _ t _) -> t) args
  )

data FnLocal = FnLocal
  { fnName :: String,
    retType :: SType
  }

instance Show FnLocal where
  show (FnLocal fnName _) = fnName

type Returns = Bool
