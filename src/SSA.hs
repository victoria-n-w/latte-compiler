module SSA where

import Block (Block (..), BlockMap)
import Control.Monad.RWS
import Control.Monad.State
import Control.Monad.Writer
import Data.List (intercalate)
import Data.Map
import Data.Maybe (fromMaybe)
import Distribution.Pretty qualified as SSA
import Quadruples (Arg (..), LabelName, Loc, Op (..), Quadruple (..))
import Text.Printf (printf)

data SSABlock = SSABlock
  { label :: LabelName,
    block :: [Quadruple],
    phiMap :: PhiMap,
    next :: [LabelName],
    prievious :: [LabelName]
  }

instance Show SSABlock where
  show :: SSABlock -> String
  show (SSABlock label block phiMap next prvs) =
    "---\n"
      ++ label
      ++ ":\n"
      ++ "prievious:\n"
      -- add indentation
      ++ unlines (Prelude.map ("\t" ++) prvs)
      ++ "phi:\n"
      ++ unlines
        ( Prelude.map
            ( \(loc, phi) ->
                printf
                  "\tphi(%d) = %s"
                  loc
                  $ intercalate
                    ","
                    ( Prelude.map
                        (\(label, loc) -> printf "%s:%s" label (show loc))
                        (toList phi)
                    )
            )
            (toList phiMap)
        )
      ++ "block:\n"
      ++ unlines (Prelude.map show block)
      ++ "next:\n"
      ++ unlines (Prelude.map ("\t" ++) next)

type PhiMap = Map Loc Phi

type Phi = Map LabelName Loc

-- | Transforms a map of blocks into a map of SSA blocks.
transpose :: BlockMap -> [SSABlock]
transpose m =
  let (_, env, blocks) = runRWS (transMap m) m (Env 0 empty empty)
      phiMap = phis env
   in let blocklist = Prelude.map (buildSSABlock m phiMap) blocks
       in rmRedundantPhi blocklist

buildSSABlock :: BlockMap -> Map LabelName PhiMap -> (LabelName, [Quadruple]) -> SSABlock
buildSSABlock m phiMap (label, quadruples) =
  let prvs = Block.prievious (m ! label)
      next = Block.next (m ! label)
   in SSABlock label quadruples (phiMap ! label) next prvs

data Env = Env
  { freeLoc :: Loc,
    remaps :: Map LabelName (Map Loc Loc),
    phis :: Map LabelName PhiMap
  }

type Context =
  RWS
    BlockMap
    [(LabelName, [Quadruple])]
    Env

transMap :: BlockMap -> Context ()
transMap m =
  mapM_ getRemap (keys m)

getRemap :: LabelName -> Context (Map Loc Loc)
getRemap label = do
  env <- get
  case Data.Map.lookup label (remaps env) of
    Just remap -> return remap
    Nothing -> do
      m <- ask
      (quadruples, remap) <- transBlock (m ! label)
      tell [(label, quadruples)]
      return remap

-- | Transforms a block into an SSA block
-- Returns the transformed block, and the map of remapped locations
transBlock :: Block -> Context ([Quadruple], Map Loc Loc)
transBlock block = do
  env <- get
  let (quadruples, resEnv, phiCandidates) =
        runRWS
          (transQuadruples (Block.block block))
          (prievious block)
          (QEnv (freeLoc env) empty)
  put $
    Env
      (qFreeLoc resEnv)
      (insert (Block.label block) (remap resEnv) (remaps env))
      (phis env)
  newPhis <-
    mapM
      ( \(newLoc, loc) -> do
          newPhi <- makePhi block loc
          return (newLoc, newPhi)
      )
      phiCandidates
  -- modify the phis map in the state
  modify $ \env -> env {phis = insert (Block.label block) (fromList newPhis) (phis env)}
  return (quadruples, remap resEnv)

makePhi :: Block -> Loc -> Context Phi
makePhi block loc = do
  let labels = Block.prievious block
  locations <- mapM (getLoc loc) labels
  return $ fromList $ Prelude.zip labels locations

getLoc :: Loc -> LabelName -> Context Loc
getLoc loc label = do
  remap <- getRemap label
  case Data.Map.lookup loc remap of
    Just loc' -> return loc'
    Nothing -> do
      block <- asks (! label)
      newPhis <- makePhi block loc
      env <- get
      -- create a new variable for the phi
      freeLoc <- gets freeLoc
      oldPhis <- case Data.Map.lookup label (phis env) of
        Just phis -> return phis
        Nothing -> return empty
      -- add a mapping from the old location to the new one
      -- and insert the new phi
      put $
        Env
          (freeLoc + 1)
          (insert label (insert loc freeLoc remap) (remaps env))
          (insert label (insert freeLoc newPhis oldPhis) (phis env))
      return freeLoc

transQuadruples :: [Quadruple] -> QContext [Quadruple]
transQuadruples = mapM transQuadruple

data QEnv = QEnv
  { qFreeLoc :: Loc,
    remap :: Map Loc Loc
  }

type QContext = RWS [LabelName] [(Loc, Loc)] QEnv

transQuadruple :: Quadruple -> QContext Quadruple
transQuadruple (Quadruple op arg1 arg2 res) = do
  arg1' <- transArg arg1
  arg2' <- transArg arg2
  res' <- case res of
    Var loc -> do
      newLoc <- newVar loc
      return $ Var newLoc
    _ -> return res
  return $ Quadruple op arg1' arg2' res'

transArg :: Arg -> QContext Arg
transArg arg =
  case arg of
    Var loc -> do
      (QEnv freeLoc remap) <- get
      case Data.Map.lookup loc remap of
        Just loc' -> return $ Var loc'
        Nothing -> do
          -- tell that we need a phi for this location
          tell [(freeLoc, loc)]
          put $ QEnv (freeLoc + 1) (insert loc freeLoc remap)
          return $ Var freeLoc
    _ -> return arg

newVar :: Loc -> QContext Loc
newVar loc = do
  env <- get
  put $ QEnv (qFreeLoc env + 1) (insert loc (qFreeLoc env) (remap env))
  return $ qFreeLoc env

rmRedundantPhi :: [SSABlock] -> [SSABlock]
rmRedundantPhi blocks =
  let (blocks', phiMap) = runWriter (mapM rmRedundantPhiBlock blocks)
   in Prelude.map (rename phiMap) blocks'

-- | Removes redundant phi functions from the phi map
-- writes removed mappings to the writer monad
rmRedundantPhiBlock :: SSABlock -> Writer (Map Loc Loc) SSABlock
rmRedundantPhiBlock block = do
  phiMap' <- filterM notRedudant (toList (phiMap block))
  return $ SSABlock (SSA.label block) (SSA.block block) (fromList phiMap') (SSA.next block) (SSA.prievious block)

notRedudant :: (Loc, Phi) -> Writer (Map Loc Loc) Bool
notRedudant (loc, phi) = do
  let locs = toList phi
  -- phi is redundant is second argument is the same
  -- for all labels
  if Prelude.all (\(_, loc) -> loc == snd (head locs)) locs
    then do
      tell $ fromList [(loc, snd (head locs))]
      return False
    else return True

-- | Renames all the variables in the block
-- accordingly to the provided map
rename :: Map Loc Loc -> SSABlock -> SSABlock
rename m (SSABlock label block phiMap next prvs) =
  let block' = Prelude.map (renameQuadruple m) block
   in SSABlock label block' (renamePhiMap m phiMap) next prvs

renameQuadruple :: Map Loc Loc -> Quadruple -> Quadruple
renameQuadruple m (Quadruple op arg1 arg2 res) =
  Quadruple op (renameArg m arg1) (renameArg m arg2) res

renameArg :: Map Loc Loc -> Arg -> Arg
renameArg m arg =
  case arg of
    Var loc -> Var (renameLoc m loc)
    _ -> arg

renamePhiMap :: Map Loc Loc -> PhiMap -> PhiMap
renamePhiMap m = Data.Map.map (renamePhi m)

renamePhi :: Map Loc Loc -> Phi -> Phi
renamePhi m = Data.Map.map (renameLoc m)

renameLoc :: Map Loc Loc -> Loc -> Loc
renameLoc m loc =
  Data.Maybe.fromMaybe loc (Data.Map.lookup loc m)
