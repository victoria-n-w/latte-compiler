module CTypes where

import Data.Map qualified as Map
import Latte.Abs qualified as Latte

data FnData = FnData
  { fnType :: Type,
    fnArgs :: [Type]
  }

type FnMap = Map.Map String FnData

type MemberMap = Map.Map String (Type, Int)

type ClassName = String

data ClassData = ClassData
  { methods :: FnMap,
    members :: MemberMap,
    baseClass :: Maybe ClassName,
    classSize :: Int -- TODO calculate the size correctly
  }

type ClassMap = Map.Map String ClassData

type Loc = Int

data Type
  = Int Int
  | Bool
  | Void
  | Ptr Type
  | Arr Int Type
  | Struct ClassName
  | Fn Type [Type]
  deriving (Eq)

data TopDef' a = TopDef'
  { name :: String,
    retType :: Type,
    args :: Map.Map Loc Type,
    contents :: a
  }
