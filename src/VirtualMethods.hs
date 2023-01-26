module VirtualMethods where

import CTypes
import Control.Monad.RWS
import Data.Map qualified as Map
import Latte.Abs (TopDef' (ClassDef))

data FnRef = FnRef
  { oldName :: String, -- name of the method in the class
    newName :: String -- name of the method in the virtual table
  }

data VirtualTable = VirtualTable
  { virtualTable :: [FnRef],
    virtualMap :: Map.Map String Int -- map from fn name to index in virtual table
  }

makeVirtualTables :: ClassMap -> Map.Map String VirtualTable
makeVirtualTables classMap =
  let classNames = Map.keys classMap
      (tables, _) = execRWS (mapM makeVirtualTable classNames) classMap Map.empty
   in tables

type Context = RWS ClassMap () (Map.Map ClassName VirtualTable)

makeVirtualTable :: ClassName -> Context VirtualTable
makeVirtualTable name = do
  env <- get
  case Map.lookup name env of
    Just table -> pure table
    Nothing -> do
      classData <- asks (Map.! name)
      case baseClass classData of
        Nothing -> do
          let table = makeVirtualMethods name classData
          modify (Map.insert name table)
          pure table
        Just baseName -> do
          baseTable <- makeVirtualTable baseName
          let table = extendVirtualTable name baseTable classData
          modify (Map.insert name table)
          pure table

-- | Returns the virtual table made for the given class,
-- without the base class' methods.
-- appends class name to the method names
makeVirtualMethods :: ClassName -> ClassData -> VirtualTable
makeVirtualMethods className classData =
  let methods' = Map.toList (methods classData)
      virtualTable = map (\(name, _) -> FnRef name (className ++ "_" ++ name)) methods'
      virtualMap = Map.fromList (zip (map oldName virtualTable) [0 ..])
   in VirtualTable virtualTable virtualMap

-- | Extends the virtual table with the methods of the given class.
-- Copies the table for the base class, and appends unique methods to it
-- Overwrites methods with the same name
-- appends class name to the method names
extendVirtualTable :: ClassName -> VirtualTable -> ClassData -> VirtualTable
extendVirtualTable className baseTable classData =
  let baseMethods' =
        map
          ( \(FnRef oldName newName) ->
              if Map.member oldName (methods classData)
                then FnRef oldName (className ++ "_" ++ oldName)
                else FnRef oldName newName
          )
          (virtualTable baseTable)
      -- methods that are not in the base class
      newMethods =
        map
          (\(name, _) -> FnRef name (className ++ "_" ++ name))
          ( filter
              (\(name, _) -> not (Map.member name (virtualMap baseTable)))
              (Map.toList (methods classData))
          )
      -- enumarate both the new methods and the base methods
      virtualMap' =
        Map.fromList $
          zip
            (map oldName (baseMethods' ++ newMethods))
            [0 ..]
   in VirtualTable (baseMethods' ++ newMethods) virtualMap'
