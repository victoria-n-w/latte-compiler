{-# LANGUAGE LambdaCase #-}

module Postprocess where

import Control.Monad.Writer
import Data.Bifunctor (second)
import Data.Map qualified as Map
import Quadruples (Arg (..), LabelName, Loc, Op (..), Quadruple (..), TopDef' (..), Type (..))
import SSA

postprocess :: [SSA.TopDef] -> [SSA.TopDef]
postprocess = map removeAssignment

-- | Removes assignment operations from the quadruple list.
removeAssignment :: SSA.TopDef -> SSA.TopDef
removeAssignment (TopDef' name type_ args blocks) =
  let remaps = execWriter (mapM gatherRemapBlock blocks)
   in TopDef' name type_ args (map (removeAssignmentBlock remaps) blocks)

removeAssignmentBlock :: VarMap -> SSA.SSABlock -> SSA.SSABlock
removeAssignmentBlock remaps (SSABlock label qs phiMap next prev) =
  let qs' =
        filter
          ( \case
              (Assign {}) -> False
              _ -> True
          )
          qs
   in SSA.rename remaps (SSABlock label qs' phiMap next prev)

gatherRemapBlock :: SSA.SSABlock -> Writer VarMap ()
gatherRemapBlock (SSABlock _ qs _ _ _) = mapM_ gatherRemap qs

type VarMap = Map.Map Loc Arg

gatherRemap :: Quadruple -> Writer VarMap ()
gatherRemap q =
  case q of
    (Assign _ arg res) -> tell (Map.singleton res arg)
    _ -> return ()
