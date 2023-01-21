module SType where

import Data.Map qualified as Map
import Latte.Abs

type ClassName = String

data TypeLit
  = Int
  | Str
  | Bool
  | Void
  | Class ClassName
  deriving (Eq)

instance Show TypeLit where
  show :: TypeLit -> String
  show SType.Int = "int"
  show SType.Str = "string"
  show SType.Bool = "boolean"
  show SType.Void = "(void)"
  show (SType.Class name) = name

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
fromBNFC (Latte.Abs.ClassT _ (Latte.Abs.Ident name)) = SType.Class name

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

data ClassDef = ClassDef
  { className :: String,
    classMembers :: Map.Map String TypeLit,
    classMethods :: Map.Map String FnType,
    baseClass :: Maybe ClassName
  }

data FnLocal = FnLocal
  { fnName :: String,
    retType :: TypeLit
  }

instance Show FnLocal where
  show (FnLocal fnName _) = fnName

type Returns = Bool
