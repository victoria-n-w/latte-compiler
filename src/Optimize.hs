module Optimize where

import Control.Monad.RWS
import Control.Monad.Writer
import Data.Map qualified as Map
import Quadruples (Arg (..), Loc, Op (..), Quadruple (..))
import SSA

optimize :: [SSABlock] -> [SSABlock]
optimize = optAssign

optAssign :: [SSABlock] -> [SSABlock]
optAssign blocks =
  let (blocks', remap) = runWriter $ mapM optAssignBlock blocks
   in map (SSA.rename remap) blocks'

-- | Optimize assignments in a single block
-- writes a map from old locations to new locations
-- returns optimized block
optAssignBlock :: SSABlock -> Writer (Map.Map Loc Loc) SSABlock
optAssignBlock block = do
  (blocks, _) <- foldM optAssignQuadruple ([], Quadruple Nop None None None) (SSA.block block)
  return $ block {SSA.block = blocks}

optAssignQuadruple :: ([Quadruple], Quadruple) -> Quadruple -> Writer (Map.Map Loc Loc) ([Quadruple], Quadruple)
optAssignQuadruple (quadruples, prievious) this =
  case (prievious, this) of
    (Quadruple _ _ _ (Var loc1), Quadruple Assign (Var loc2) _ (Var loc3)) ->
      if loc2 == loc1
        then do
          tell $ Map.singleton loc3 loc2
          return (quadruples, this)
        else return (quadruples ++ [this], this)
    _ -> return (quadruples ++ [this], this)
