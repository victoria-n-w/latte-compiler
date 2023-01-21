module TopDefs where

import Control.Monad.RWS
import Data.Map qualified as Map
import Latte.Abs qualified as Latte
import SType

firstPass :: [Latte.TopDef] -> (Map.Map String FnType, Map.Map String ClassDef)
firstPass topdefs =
  let ((), classDefs, fnDefs) = runRWS (mapM_ transTopDef topdefs) () Map.empty
   in (fnDefs, classDefs)

transTopDef :: Latte.TopDef -> RWS () (Map.Map String FnType) (Map.Map String ClassDef) ()
transTopDef (Latte.FnDef loc type_ (Latte.Ident fnName) args block) = do
  tell $ Map.singleton fnName $ FnType (fromBNFC type_) $ map (fromBNFC . \(Latte.Arg _ t _) -> t) args
transTopDef (Latte.ClassDef loc (Latte.Ident className) members) =
  let ((), methodsMap, membersMap) = runRWS (mapM_ transMember members) () Map.empty
   in -- modify the map of class definitions
      modify $ Map.insert className $ ClassDef className membersMap methodsMap

transMember :: Latte.Member -> RWS () (Map.Map String TypeLit) (Map.Map String FnType) ()
transMember x = case x of
  Latte.Attr _ type_ (Latte.Ident name) -> do
    tell $ Map.singleton name $ fromBNFC type_
  Latte.Method _ type_ (Latte.Ident name) args _ -> do
    -- modify the state monad
    modify $ Map.insert name $ FnType (fromBNFC type_) $ map (fromBNFC . \(Latte.Arg _ t _) -> t) args
