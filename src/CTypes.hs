module CTypes where

import Data.Map qualified as Map
import Latte.Abs qualified as Latte

type FnMap = Map.Map String Type

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
  deriving (Eq)

data TopDef' a = TopDef'
  { name :: String,
    retType :: Type,
    args :: Map.Map Loc Type,
    contents :: a
  }
