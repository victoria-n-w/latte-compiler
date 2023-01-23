module SSA where

import Block (Block (..), BlockMap, TopDef)
import Control.Monad.RWS
import Control.Monad.State
import Control.Monad.Writer
import Data.Bifunctor
import Data.List (intercalate)
import Data.Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Distribution.Pretty qualified as SSA
import Quadruples (Arg (..), LabelName, Loc, Op (..), Quadruple (..), TopDef' (TopDef'), Type (..))
import Text.Printf (printf)

data SSABlock = SSABlock
  { label :: LabelName,
    block :: [Quadruple],
    phiMap :: PhiMap,
    next :: [LabelName],
    previous :: [LabelName]
  }

type PhiMap = Map Loc Phi

data Phi = Phi
  { type_ :: Type,
    mapping :: Map LabelName Arg
  }

type TopDef = TopDef' [SSABlock]

transpose :: [Block.TopDef] -> [SSA.TopDef]
transpose = Prelude.map transTopDef

transTopDef :: Block.TopDef -> SSA.TopDef
transTopDef (TopDef' name type_ args block) =
  TopDef' name type_ args (transpose' args block)

-- | Transforms a map of blocks into a map of SSA blocks.
transpose' :: Data.Map.Map Loc Type -> BlockMap -> [SSABlock]
transpose' args m =
  let (_, env, blocks) = runRWS (transMap m) (m, args) (Env (length args + 1) empty empty)
      phiMap = phis env
   in let blocklist = Prelude.map (buildSSABlock m phiMap) blocks
       in blocklist

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
    (BlockMap, Map Loc Type)
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
      (m, _) <- ask
      (quadruples, remap) <- transBlock (m ! label)
      tell [(label, quadruples)]
      return remap

getPhis :: LabelName -> Context PhiMap
getPhis label = do
  env <- get
  case Data.Map.lookup label (phis env) of
    Just phiMap -> return phiMap
    Nothing -> return empty

-- | Transforms a block into an SSA block
-- Returns the transformed block, and the map of remapped locations
transBlock :: Block -> Context ([Quadruple], Map Loc Loc)
transBlock block = do
  env <- get
  (_, args) <- ask
  let (quadruples, resEnv, phiCandidates) =
        runRWS
          (transQuadruples (Block.block block))
          (prepareArgs block args)
          (QEnv (freeLoc env) empty)
  put $
    Env
      (qFreeLoc resEnv)
      (insert (Block.label block) (remap resEnv) (remaps env))
      (phis env)
  newPhis <-
    mapM
      ( \(newLoc, loc, t) -> do
          newPhi <- makePhi block loc t
          return (newLoc, newPhi)
      )
      phiCandidates
  -- modify the phis map in the state
  -- store the old phis in case it was created while calling makePhi
  -- (a cycle in the graph occured)
  oldPhis <- getPhis (Block.label block)
  -- merge the old phis with the new ones
  modify $ \env ->
    env
      { phis =
          insert
            (Block.label block)
            (oldPhis `union` fromList newPhis)
            (phis env)
      }
  return (quadruples, remap resEnv)

makePhi :: Block -> Loc -> Type -> Context Phi
makePhi block loc t = do
  let labels = Block.prievious block
  locations <- mapM (getLoc loc t) labels
  return $ Phi t $ fromList $ Prelude.zip labels $ Prelude.map Var locations

getLoc :: Loc -> Type -> LabelName -> Context Loc
getLoc loc t label = do
  remap <- getRemap label
  case Data.Map.lookup loc remap of
    Just loc' -> return loc'
    Nothing ->
      if label == "entry"
        then do
          -- check if the location is an argument
          (_, args) <- ask
          if Data.Map.member loc args
            then do
              -- if it is, return the location
              return loc
            else getLocRec loc t label
        else getLocRec loc t label

getLocRec :: Loc -> Type -> LabelName -> Context Loc
getLocRec loc t label = do
  block <- asks (\(m, _) -> m ! label)
  newPhis <- makePhi block loc t
  env <- get
  -- create a new variable for the phi
  freeLoc <- gets freeLoc
  oldPhis <- case Data.Map.lookup label (phis env) of
    Just phis -> return phis
    Nothing -> return empty
  -- add a mapping from the old location to the new one
  -- and insert the new phi
  let updatedRemap = remaps env ! label
  put $
    Env
      (freeLoc + 1)
      (insert label (insert loc freeLoc updatedRemap) (remaps env))
      (insert label (insert freeLoc newPhis oldPhis) (phis env))
  return freeLoc

transQuadruples :: [Quadruple] -> QContext [Quadruple]
transQuadruples = mapM transQuadruple

data QEnv = QEnv
  { qFreeLoc :: Loc,
    remap :: Map Loc Loc
  }

-- | If has value, then it means that block can use args
-- The block cannot use args otherwise
type Args = Maybe (Map Loc Type)

prepareArgs :: Block -> Map Loc Type -> Args
prepareArgs block args =
  if Block.label block == "entry"
    then Just args
    else Nothing

type QContext = RWS Args [(Loc, Loc, Type)] QEnv

transQuadruple :: Quadruple -> QContext Quadruple
transQuadruple q =
  case q of
    (BinOp t op arg1 arg2 loc) -> do
      arg1' <- transArg t arg1
      arg2' <- transArg t arg2
      loc' <- newVar loc
      return $ BinOp t op arg1' arg2' loc'
    (SingleArgOp t op arg loc) -> do
      arg' <- transArg t arg
      loc' <- newVar loc
      return $ SingleArgOp t op arg' loc'
    (CmpBinOp t op arg1 arg2 loc) -> do
      arg1' <- transArg t arg1
      arg2' <- transArg t arg2
      loc' <- newVar loc
      return $ CmpBinOp t op arg1' arg2' loc'
    (Assign t arg loc) -> do
      arg' <- transArg t arg
      loc' <- newVar loc
      return $ Assign t arg' loc'
    (Call loc t name args) -> do
      args' <-
        mapM
          ( \(t, arg) -> do
              newArg <- transArg t arg
              return (t, newArg)
          )
          args
      loc' <- newVar loc
      return $ Call loc' t name args'
    (JumpIf arg label1 label2) -> do
      arg' <- transArg (Int 1) arg
      return $ JumpIf arg' label1 label2
    (Return t arg) -> do
      arg' <- transArg t arg
      return $ Return t arg'
    (LiteralString loc str) -> do
      loc' <- newVar loc
      return $ LiteralString loc' str
    (Bitcast t1 t2 arg loc) -> do
      arg' <- transArg t1 arg
      loc' <- newVar loc
      return $ Bitcast t1 t2 arg' loc'
    (GetElementPtr t src dst idx1 idx2) -> do
      src' <- transLoc (Ptr t) src
      dst' <- newVar dst
      idx1' <- transArg (Int 32) idx1
      idx2' <- transArg (Int 32) idx2
      return $ GetElementPtr t src' dst' idx1' idx2'
    (Load t src dst) -> do
      src' <- transLoc (Ptr t) src
      dst' <- newVar dst
      return $ Load t src' dst'
    (Store t src dst) -> do
      src' <- transArg t src
      dst' <- transLoc (Ptr t) dst
      return $ Store t src' dst'
    q -> return q

transArg :: Type -> Arg -> QContext Arg
transArg t arg =
  case arg of
    Var loc -> do
      loc' <- transLoc t loc
      return $ Var loc'
    _ -> return arg

transLoc :: Type -> Loc -> QContext Loc
transLoc t loc = do
  remap <- gets remap
  case Data.Map.lookup loc remap of
    Just loc' -> return loc'
    Nothing -> do
      args <- ask
      case args of
        Nothing -> tellPhiNeeded loc t
        Just args ->
          -- try to find loc in argument set
          if Data.Map.member loc args
            then -- just use the old location
              return loc
            else tellPhiNeeded loc t

tellPhiNeeded :: Loc -> Type -> QContext Loc
tellPhiNeeded loc t = do
  (QEnv freeLoc remap) <- get
  -- tell that we need a phi
  tell [(freeLoc, loc, t)]
  put $ QEnv (freeLoc + 1) (insert loc freeLoc remap)
  return freeLoc

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
rmRedundantPhiBlock :: SSABlock -> Writer (Map Loc Arg) SSABlock
rmRedundantPhiBlock block = do
  phiMap' <- filterM notRedudant (toList (phiMap block))
  return $ SSABlock (SSA.label block) (SSA.block block) (fromList phiMap') (SSA.next block) (SSA.previous block)

notRedudant :: (Loc, Phi) -> Writer (Map Loc Arg) Bool
notRedudant (loc, Phi t phiMap) = do
  let locs = toList phiMap
  -- phi is redundant is second argument is the same
  -- for all labels
  if Prelude.all (\(_, loc) -> loc == snd (head locs)) locs
    then do
      tell $ fromList [(loc, snd (head locs))]
      return False
    else return True

-- | Renames all the variables in the block
-- accordingly to the provided map
rename :: Map Loc Arg -> SSABlock -> SSABlock
rename m (SSABlock label block phiMap next prvs) =
  let block' = Prelude.map (renameQuadruple m) block
   in SSABlock label block' (renamePhiMap m phiMap) next prvs

renameQuadruple :: Map Loc Arg -> Quadruple -> Quadruple
renameQuadruple m q =
  case q of
    (BinOp t op arg1 arg2 res) -> BinOp t op (renameArg m arg1) (renameArg m arg2) res
    (SingleArgOp t op arg res) -> SingleArgOp t op (renameArg m arg) res
    (CmpBinOp t op arg1 arg2 res) -> CmpBinOp t op (renameArg m arg1) (renameArg m arg2) res
    (Assign t arg loc) -> Assign t (renameArg m arg) loc
    (Call loc t label args) -> Call loc t label (Prelude.map (Data.Bifunctor.second (renameArg m)) args)
    (JumpIf arg label1 label2) -> JumpIf (renameArg m arg) label1 label2
    (Return t arg) -> Return t (renameArg m arg)
    (Bitcast t1 t2 arg loc) -> Bitcast t1 t2 (renameArg m arg) loc
    (GetElementPtr t src dst idx1 idx2) -> GetElementPtr t (renameLoc m src) dst (renameArg m idx1) (renameArg m idx2)
    (Load t src dst) -> Load t (renameLoc m src) dst
    (Store t src dst) -> Store t (renameArg m src) (renameLoc m dst)
    _ -> q

renameArg :: Map Loc Arg -> Arg -> Arg
renameArg m arg =
  case arg of
    (Var loc) -> case Data.Map.lookup loc m of
      -- maybe the other variable was also remapped
      Just arg' -> renameArg m arg'
      Nothing -> arg
    _ -> arg

-- | Differs from the renameArg in that it doesn't rename to constants
renameLoc :: Map Loc Arg -> Loc -> Loc
renameLoc m loc =
  case Data.Map.lookup loc m of
    Just (Var loc') -> loc'
    _ -> loc

renamePhiMap :: Map Loc Arg -> PhiMap -> PhiMap
renamePhiMap m = Data.Map.map (renamePhi m)

renamePhi :: Map Loc Arg -> Phi -> Phi
renamePhi m (Phi t phiMap) = Phi t (Data.Map.map (renameArg m) phiMap)
