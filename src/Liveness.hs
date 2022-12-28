module Liveness where

import Control.Monad.State
import Control.Monad.Trans.Writer (runWriter, tell)
import Data.List (intercalate)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Distribution.Compat.Lens (_1)
import Quadruples (Arg (..), LabelName, Loc, Op (..), Quadruple (..))
import SSA (Phi, PhiMap, SSABlock (..))
import Text.Printf (printf)

data LivenessBlock = LivenessBlock
  { inLive :: Map.Map LabelName (Set.Set Loc),
    out :: Set.Set Loc,
    kill :: Set.Set Loc,
    use :: Map.Map LabelName (Set.Set Loc)
  }

instance Show LivenessBlock where
  show :: LivenessBlock -> String
  show (LivenessBlock inLive out kill use) =
    -- show each set as a comma separated list
    printf "in: {%s} out: {%s} kill: {%s} use: {%s}" (showMap inLive) (showSet out) (showSet kill) (showMap use)
    where
      showSet :: Set.Set Loc -> String
      showSet = intercalate "," . map show . Set.toList
      showMap :: Map.Map LabelName (Set.Set Loc) -> String
      showMap = intercalate "," . map (\(label, set) -> printf "%s:{%s}" label (showSet set)) . Map.toList

data LBlock = LBlock
  { label :: LabelName,
    block :: [Quadruple],
    phiMap :: PhiMap,
    next :: [LabelName],
    previous :: [LabelName],
    liveness :: LivenessBlock
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
      -- create InMap of maps of empty sets for each block, for each predecessor
      inMap = Map.fromList $ map (\b -> (SSA.label b, Map.fromList $ map (,Set.empty) (SSA.previous b))) blocks
      outMap = Map.fromList $ map (\b -> (SSA.label b, Set.empty)) blocks
   in let (inMap', outMap') = solveIter killedMap usedMap inMap outMap blocks
       in Map.fromList $ map (\b -> (SSA.label b, lBlockFromSSABlock b inMap' outMap' killedMap usedMap)) blocks

lBlockFromSSABlock :: SSABlock -> InMap -> LivenessMap -> LivenessMap -> InMap -> LBlock
lBlockFromSSABlock (SSA.SSABlock label block phiMap next prvs) inMap outMap killedMap usedMap =
  let liveness =
        LivenessBlock (inMap Map.! label) (outMap Map.! label) (killedMap Map.! label) (usedMap Map.! label)
   in LBlock label block phiMap next prvs liveness

type LivenessMap = Map.Map LabelName (Set.Set Loc)

type InMap = Map.Map LabelName (Map.Map LabelName (Set.Set Loc))

solveIter :: LivenessMap -> InMap -> InMap -> LivenessMap -> [SSABlock] -> (InMap, LivenessMap)
solveIter killedMap usedMap inMap outMap blocks =
  let inMap' =
        Map.fromList $ map (\block -> (SSA.label block, prepareInMap killedMap usedMap outMap block)) blocks
      outMap' = Map.fromList $ map (\block -> (SSA.label block, prepareOutSet killedMap usedMap inMap' block)) blocks
   in if inMap == inMap' && outMap == outMap'
        then (inMap, outMap)
        else solveIter killedMap usedMap inMap' outMap' blocks

prepareInMap :: LivenessMap -> InMap -> LivenessMap -> SSABlock -> Map.Map LabelName (Set.Set Loc)
prepareInMap killedMap usedMap outMap block =
  let label = SSA.label block
   in -- for each predecessor of the block
      -- in[b][p] = (out[b] - kill[b]) + use[b][p]
      Map.fromList
        . map
          ( \p ->
              ( p,
                (outMap Map.! label `Set.difference` (killedMap Map.! label)) `Set.union` ((usedMap Map.! label) Map.! p)
              )
          )
        $ SSA.previous block

prepareOutSet :: LivenessMap -> InMap -> InMap -> SSABlock -> Set.Set Loc
prepareOutSet killedMap usedMap inMap block =
  let label = SSA.label block
   in -- out[b] = union(in[s][b]) for all s in succ[b]
      Set.unions $ map (\next -> inMap Map.! next Map.! label) (SSA.next block)

-- | Returns a pair:
-- 1. A map of locations that are killed in each block.
-- 2. A map of locations that are used in each block.
killAndUsedInList :: [SSABlock] -> (LivenessMap, InMap)
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
killedAndUsed :: SSABlock -> (Set.Set Loc, Map.Map LabelName (Set.Set Loc))
killedAndUsed (SSABlock _ block phiMap _ prvs) =
  let usedPhi = usedInPhi phiMap
      killedPhi = killedInPhi phiMap
   in -- run the killedAndUsedInBlock function on the block
      -- collect the results to two sets
      let (killed, used) = execState (killedAndUsedInBlock block) (killedPhi, Set.empty)
       in -- for each of the previous blocks, create an empty in map
          -- containing the union of the used locations in the phi map and the used locations in the block
          -- if no entry exists for the previous block, use just the locations used in the block
          ( killed,
            Map.fromList $
              map
                ( \prv ->
                    ( prv,
                      Set.union
                        (Data.Maybe.fromMaybe Set.empty (Map.lookup prv usedPhi))
                        used
                    )
                )
                prvs
          )

killedInPhi :: PhiMap -> Set.Set Loc
killedInPhi phiMap = Set.fromList $ map fst $ Map.toList phiMap

usedInPhi :: PhiMap -> Map.Map LabelName (Set.Set Loc)
usedInPhi phiMap =
  -- for each label in the phi map return a set of locations that are used based on that label
  -- use state to collect the results
  let used = execState (mapM_ usedInSinglePhi $ Map.toList phiMap) Map.empty
   in used

usedInSinglePhi :: (Loc, Phi) -> State (Map.Map LabelName (Set.Set Loc)) ()
usedInSinglePhi (_, phi) = do
  mapM_ usedInSinglePhiArg $ Map.toList phi

usedInSinglePhiArg :: (LabelName, Loc) -> State (Map.Map LabelName (Set.Set Loc)) ()
usedInSinglePhiArg (label, loc) = do
  used <- get
  put $ Map.insertWith Set.union label (Set.singleton loc) used


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
