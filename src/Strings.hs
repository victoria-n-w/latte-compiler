module Strings where

import Control.Monad.Reader
import Control.Monad.Writer
import Data.Map qualified as Map
import Data.Set qualified as Set
import Quadruples (Arg (..), LabelName, Loc, Op (..), Quadruple (..), TopDef, TopDef' (..), Type (..))
import SSA qualified

-- | Passes throught the quadruple list
-- looks up all literal strings used
-- and then replaces them with appropriate quadurples
-- that load the string into a register
-- Returns the new quadruple list and a map of strings to locations
trans :: [SSA.TopDef] -> ([SSA.TopDef], Map.Map String Loc)
trans topdefs =
  let stringSet = execWriter (mapM gatherStrings topdefs)
      stringMap = Map.fromList (zip (Set.toList stringSet) [0 ..])
      topdefs' = runReader (mapM transTopDef topdefs) stringMap
   in (topdefs', stringMap)

gatherStrings :: SSA.TopDef -> Writer (Set.Set String) ()
gatherStrings (TopDef' _ _ _ blocks) = mapM_ gatherStringsBlock blocks

gatherStringsBlock :: SSA.SSABlock -> Writer (Set.Set String) ()
gatherStringsBlock (SSA.SSABlock _ qs _ _ _) = mapM_ gatherStringsQuad qs

gatherStringsQuad :: Quadruple -> Writer (Set.Set String) ()
gatherStringsQuad (LiteralString loc str) = tell (Set.singleton str)
gatherStringsQuad _ = return ()

transTopDef :: SSA.TopDef -> Reader (Map.Map String Loc) SSA.TopDef
transTopDef (TopDef' name type_ args blocks) =
  TopDef' name type_ args <$> mapM transBlock blocks

transBlock :: SSA.SSABlock -> Reader (Map.Map String Loc) SSA.SSABlock
transBlock block = do
  qs <- mapM transQuad (SSA.block block)
  return (block {SSA.block = qs})

transQuad :: Quadruple -> Reader (Map.Map String Loc) Quadruple
transQuad (LiteralString loc str) = do
  stringMap <- ask
  let stringLoc = stringMap Map.! str
      strType = Ptr (getStrType str)
  return (Bitcast strType (Ptr (Int 8)) (Global stringLoc) loc)
transQuad q = return q

getStrType :: String -> Type
getStrType str = Arr (length str + 1) (Int 8)
