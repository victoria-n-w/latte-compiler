module SType where

import Data.Bool qualified as SType
import Latte.Abs

data TypeLit
  = Int
  | Str
  | Bool
  | Void
  | Arr TypeLit Bool
  deriving (Eq)

instance Show TypeLit where
  show :: TypeLit -> String
  show SType.Int = "int"
  show SType.Str = "string"
  show SType.Bool = "boolean"
  show SType.Void = "(void)"

data SType = SType
  { t :: TypeLit,
    depth :: Int
  }

instance Show SType where
  show = show . t

instance Eq SType where
  (==) a b = t a == t b

fromBNFC :: Latte.Abs.Type -> TypeLit
fromBNFC (Latte.Abs.Int _) = SType.Int
fromBNFC (Latte.Abs.Str _) = SType.Str
fromBNFC (Latte.Abs.Bool _) = SType.Bool
fromBNFC (Latte.Abs.Void _) = SType.Void

data FnType = FnType
  { ret :: TypeLit,
    args :: [TypeLit]
  }

makeFnEntry :: TopDef -> (String, FnType)
makeFnEntry (FnDef _ type_ (Ident fnName) args _) =
  ( fnName,
    FnType (fromBNFC type_) $
      map (fromBNFC . \(Arg _ t _) -> t) args
  )

data FnLocal = FnLocal
  { fnName :: String,
    retType :: TypeLit
  }

instance Show FnLocal where
  show (FnLocal fnName _) = fnName

type Returns = Bool
