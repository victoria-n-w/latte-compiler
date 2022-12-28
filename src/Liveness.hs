module Liveness where

import Control.Monad.State
import Control.Monad.Trans.Writer (runWriter, tell)
import Data.List (intercalate)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Distribution.Compat.Lens (_1)
import Quadruples (Arg (..), LabelName, Loc, Op (..), Quadruple (..))
import SSA (Phi, PhiMap, SSABlock (..))
import Text.Printf (printf)

data Liveness = Liveness
  { inLive :: Set.Set Loc,
    out :: Set.Set Loc,
    kill :: Set.Set Loc,
    use :: Set.Set Loc
  }

instance Show Liveness where
  show :: Liveness -> String
  show (Liveness inLive out kill use) =
    -- show each set as a comma separated list
    printf "in: {%s} out: {%s} kill: {%s} use: {%s}" (showSet inLive) (showSet out) (showSet kill) (showSet use)
    where
      showSet :: Set.Set Loc -> String
      showSet = intercalate "," . map show . Set.toList

data LBlock = LBlock
  { label :: LabelName,
    block :: [Quadruple],
    phiMap :: PhiMap,
    next :: [LabelName],
    previous :: [LabelName],
    liveness :: Liveness
  }

instance Show LBlock where
  show :: LBlock -> String
  show (LBlock label block phiMap next prvs liveness) =
    "---\n"
      ++ label
      ++ ":\n"
      ++ "previous:\n"
      -- add indentation
      ++ unlines (map ("\t" ++) prvs)
      ++ "phi:\n"
      ++ unlines
        ( map
            ( \(loc, phi) ->
                printf
                  "\tphi(%d) = %s"
                  loc
                  $ intercalate
                    ","
                    ( map
                        (\(label, loc) -> printf "%s:%s" label (show loc))
                        (Map.toList phi)
                    )
            )
            (Map.toList phiMap)
        )
      ++ "block:\n"
      ++ unlines (map show block)
      ++ "next:\n"
      ++ unlines (map ("\t" ++) next)
      ++ "liveness:\n"
      ++ "\t"
      ++ show liveness

type LBlockMap = Map.Map LabelName LBlock

analyze :: [SSABlock] -> LBlockMap
analyze blocks =
  let (killedMap, usedMap) = killAndUsedInList blocks
      inMap = Map.fromList $ map (\b -> (SSA.label b, Set.empty)) blocks
      outMap = Map.fromList $ map (\b -> (SSA.label b, Set.empty)) blocks
   in let (inMap', outMap') = solveIter killedMap usedMap inMap outMap blocks
       in Map.fromList $ map (\b -> (SSA.label b, lBlockFromSSABlock b inMap' outMap' killedMap usedMap)) blocks

lBlockFromSSABlock :: SSABlock -> LivenessMap -> LivenessMap -> LivenessMap -> LivenessMap -> LBlock
lBlockFromSSABlock (SSA.SSABlock label block phiMap next prvs) inMap outMap killedMap usedMap =
  let liveness =
        Liveness (inMap Map.! label) (outMap Map.! label) (killedMap Map.! label) (usedMap Map.! label)
   in LBlock label block phiMap next prvs liveness

type LivenessMap = Map.Map LabelName (Set.Set Loc)

solveIter :: LivenessMap -> LivenessMap -> LivenessMap -> LivenessMap -> [SSABlock] -> (LivenessMap, LivenessMap)
solveIter killedMap usedMap inMap outMap blocks =
  let inMap' =
        Map.fromList $
          map
            -- in[b] = (out[b] - kill[b]) + use[b]
            (\b -> (SSA.label b, (outMap Map.! SSA.label b `Set.difference` (killedMap Map.! SSA.label b)) `Set.union` (usedMap Map.! SSA.label b)))
            blocks
      outMap' =
        Map.fromList $
          map
            -- out[b] = union(in[s]) for all s in succ[b]
            (\b -> (SSA.label b, Set.unions $ map (inMap' Map.!) (SSA.next b)))
            blocks
   in if inMap == inMap' && outMap == outMap'
        then (inMap, outMap)
        else solveIter killedMap usedMap inMap' outMap' blocks

-- | Returns a pair:
-- 1. A map of locations that are killed in each block.
-- 2. A map of locations that are used in each block.
killAndUsedInList :: [SSABlock] -> (LivenessMap, LivenessMap)
killAndUsedInList blocks =
  -- run the killedAndUsed function on each block
  -- collect the results to two maps
  let (usedList, killedMap) =
        runWriter $
          mapM
            ( \b -> do
                let (killed, used) = killedAndUsed b
                tell $ Map.singleton (SSA.label b) killed
                return (SSA.label b, used)
            )
            blocks
   in (killedMap, Map.fromList usedList)

-- | Returns a map of locations that are killed and used in a block.
killedAndUsed :: SSABlock -> (Set.Set Loc, Set.Set Loc)
killedAndUsed (SSABlock _ block phiMap _ _) =
  let usedPhi = usedInPhi phiMap
      killedPhi = killedInPhi phiMap
   in -- run the killedAndUsedInBlock function on the block
      -- collect the results to two sets
      let (killed, used) = execState (killedAndUsedInBlock block) (killedPhi, usedPhi)
       in (killed, used)

killedInPhi :: PhiMap -> Set.Set Loc
killedInPhi phiMap = Set.fromList $ map fst $ Map.toList phiMap

usedInPhi :: PhiMap -> Set.Set Loc
usedInPhi phiMap = Set.unions $ map (Set.fromList . map snd . Map.toList) $ Map.elems phiMap

-- | Stores the locations that are killed and used in a block.
killedAndUsedInBlock :: [Quadruple] -> State (Set.Set Loc, Set.Set Loc) ()
killedAndUsedInBlock = mapM_ killedAndUsedInQuad

-- | Stores the locations that are killed and used in a quadruple.
killedAndUsedInQuad :: Quadruple -> State (Set.Set Loc, Set.Set Loc) ()
killedAndUsedInQuad (Quadruple op arg1 arg2 res) = do
  (killed, used) <- get
  let killed' = locSetFromArg res
      used' = locSetFromArg arg1 `Set.union` locSetFromArg arg2
  put (killed `Set.union` killed', used `Set.union` (used' `Set.difference` killed))
  return ()

locSetFromArg :: Arg -> Set.Set Loc
locSetFromArg (Var loc) = Set.singleton loc
locSetFromArg _ = Set.empty
