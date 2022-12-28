{-# LANGUAGE LambdaCase #-}

module Liveness where

import Control.Monad.Trans.Writer (runWriter)
import Control.Monad.Writer
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
  let killedMap = Map.fromList $ map (\b -> (SSA.label b, killedInBlock b)) blocks
      usedMap =
        Map.fromList $
          map
            ( \b ->
                let (_, used) = runWriter $ usedInBlock b
                 in (SSA.label b, used)
            )
            blocks
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

-- | Returns a set of locations that are killed in a block.
killedInBlock :: SSABlock -> Set.Set Loc
killedInBlock (SSABlock _ block phiMap next prvs) =
  Set.fromList $
    -- map the result of filtering out assign operations to a list
    map
      -- get the location from the result of assignemnt
      (\case (Quadruple Assign _ _ (Var loc)) -> loc)
      -- filter out all operations that are not assign operations
      (filter (\case (Quadruple Assign _ _ _) -> True; _ -> False) block)
      ++
      -- get all the locations from the phi map
      map fst (Map.toList phiMap)

-- | Returns a set of locations that are used in a block.
usedInBlock :: SSABlock -> Writer (Set.Set Loc) ()
usedInBlock (SSABlock _ block phiMap next prvs) = do
  mapM_ usedInQuad block
  mapM_ usedInPhi (Map.toList phiMap)

usedInQuad :: Quadruple -> Writer (Set.Set Loc) ()
usedInQuad q = do
  case arg1 q of
    Var loc -> tell $ Set.singleton loc
    _ -> return ()
  case arg2 q of
    Var loc -> tell $ Set.singleton loc
    _ -> return ()

usedInPhi :: (Loc, Phi) -> Writer (Set.Set Loc) ()
usedInPhi (_, phi) = do
  mapM_ (\(_, loc) -> tell $ Set.singleton loc) (Map.toList phi)
