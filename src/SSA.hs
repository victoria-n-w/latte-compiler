module SSA where

import Block (Block (..), BlockMap)
import Control.Monad.RWS
import Control.Monad.State
import Data.Map
import Quadruples (Arg (..), LabelName, Loc, Op (..), Quadruple (..))
import Text.Printf (printf)

data SSABlock = SSABlock
  { label :: LabelName,
    block :: [Quadruple],
    phi :: [Phi]
  }

instance Show SSABlock where
  show :: SSABlock -> String
  show (SSABlock label block phi) =
    "---\n"
      ++ label
      ++ ":\n"
      ++ "phi:\n"
      ++ unlines (Prelude.map show phi)
      ++ "block:\n"
      ++ unlines (Prelude.map show block)

data Phi = Phi
  { phiLoc :: Loc,
    phiArgs :: [(LabelName, Loc)]
  }

instance Show Phi where
  show :: Phi -> String
  show (Phi loc args) =
    printf "phi %d <- %s" loc (unwords $ Prelude.map showArg args)
    where
      showArg :: (LabelName, Loc) -> String
      showArg (label, loc) = printf "%s %d" label loc

-- | Transforms a map of blocks into a map of SSA blocks.
transpose :: BlockMap -> [SSABlock]
transpose m =
  let (_, env, blocks) = runRWS (transMap m) m (Env 0 empty empty)
      phiMap = phis env
   in Prelude.map (\(label, block) -> SSABlock label block (elems (phiMap ! label))) blocks

data Env = Env
  { freeLoc :: Loc,
    remaps :: Map LabelName (Map Loc Loc),
    phis :: Map LabelName (Map Loc Phi)
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
  phis <- mapM (makePhi block) phiCandidates
  put $
    Env
      (freeLoc env)
      (remaps env)
      ( insert
          (Block.label block)
          (fromList $ Prelude.map (\phi -> (phiLoc phi, phi)) phis)
          (phis env)
      )
  return (quadruples, remap resEnv)

makePhi :: Block -> Loc -> Context Phi
makePhi block loc = do
  let labels = Block.prievious block
  locations <- mapM (getLoc loc) labels
  return $ Phi loc (zip labels locations)

getLoc :: Loc -> LabelName -> Context Loc
getLoc loc label = do
  remap <- getRemap label
  case Data.Map.lookup loc remap of
    Just loc' -> return loc'
    Nothing -> do
      env <- get
      blockMap <- ask
      let block = blockMap ! label
      phi <- makePhi block loc
      put $
        Env
          (freeLoc env)
          (remaps env)
          (insert label (insert loc phi (phis env ! label)) (phis env))

      return loc

transQuadruples :: [Quadruple] -> QContext [Quadruple]
transQuadruples = mapM transQuadruple

data QEnv = QEnv
  { qFreeLoc :: Loc,
    remap :: Map Loc Loc
  }

type QContext = RWS [LabelName] [Loc] QEnv

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
      (QEnv loc remap) <- get
      case Data.Map.lookup loc remap of
        Just loc' -> return $ Var loc'
        Nothing -> do
          loc' <- newVar loc
          -- tell that phi is needed
          tell [loc]
          return $ Var loc'
    _ -> return arg

newVar :: Loc -> QContext Loc
newVar loc = do
  env <- get
  put $ QEnv (qFreeLoc env + 1) (insert loc (qFreeLoc env) (remap env))
  return $ qFreeLoc env
