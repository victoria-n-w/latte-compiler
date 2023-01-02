module SSA where

import Block (Block (..), BlockMap, TopDef)
import Control.Monad.RWS
import Control.Monad.State
import Control.Monad.Writer
import Data.List (intercalate)
import Data.Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Distribution.Pretty qualified as SSA
import Quadruples (Arg (..), LabelName, Loc, Op (..), Quadruple (..), TopDef' (TopDef'))
import Text.Printf (printf)

data SSABlock = SSABlock
  { label :: LabelName,
    block :: [Quadruple],
    phiMap :: PhiMap,
    next :: [LabelName],
    previous :: [LabelName]
  }

instance Show SSABlock where
  show :: SSABlock -> String
  show (SSABlock label block phiMap next prvs) =
    label
      ++ ":\n"
      ++ "\tprevious:\n"
      -- add indentation
      ++ unlines (Prelude.map ("\t\t" ++) prvs)
      ++ "\tphi:\n"
      ++ unlines
        ( Prelude.map
            ( \(loc, phi) ->
                printf
                  "\t\tphi(%d) = %s"
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
      ++ "\tblock:\n"
      ++ unlines (Prelude.map (\q -> "\t\t" ++ show q) block)
      ++ "\tnext:\n"
      ++ unlines (Prelude.map ("\t\t" ++) next)

type PhiMap = Map Loc Phi

type Phi = Map LabelName Loc

type TopDef = TopDef' [SSABlock]

instance Show SSA.TopDef where
  show :: SSA.TopDef -> String
  show (TopDef' name args blocks) =
    printf "function %s(%s){\n" name (intercalate ", " (Prelude.map (\arg -> "%" ++ show arg) (Set.toList args)))
      ++ unlines (Prelude.map show blocks)
      ++ "}\n"

transpose :: [Block.TopDef] -> [SSA.TopDef]
transpose = Prelude.map (\(TopDef' name args block) -> TopDef' name args (transpose' args block))

-- | Transforms a map of blocks into a map of SSA blocks.
transpose' :: Set.Set Loc -> BlockMap -> [SSABlock]
transpose' args m =
  let (_, env, blocks) = runRWS (transMap m) (m, args) (Env (length args + 1) empty empty)
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
    (BlockMap, Set.Set Loc)
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

-- | Transforms a block into an SSA block
-- Returns the transformed block, and the map of remapped locations
transBlock :: Block -> Context ([Quadruple], Map Loc Loc)
transBlock block = do
  env <- get
  (_, args) <- ask
  let (quadruples, resEnv, phiCandidates) =
        runRWS
          (transQuadruples (Block.block block))
          (prievious block, args)
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
      block <- asks (\(m, _) -> m ! label)
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

type QContext = RWS ([LabelName], Set.Set Loc) [(Loc, Loc)] QEnv

transQuadruple :: Quadruple -> QContext Quadruple
transQuadruple q =
  case q of
    (BinOp op arg1 arg2 loc) -> do
      arg1' <- transArg arg1
      arg2' <- transArg arg2
      loc' <- newVar loc
      return $ BinOp op arg1' arg2' loc'
    (SingleArgOp op arg loc) -> do
      arg' <- transArg arg
      loc' <- newVar loc
      return $ SingleArgOp op arg' loc'
    (CmpBinOp op arg1 arg2 loc) -> do
      arg1' <- transArg arg1
      arg2' <- transArg arg2
      loc' <- newVar loc
      return $ CmpBinOp op arg1' arg2' loc'
    (Assign arg loc) -> do
      arg' <- transArg arg
      loc' <- newVar loc
      return $ Assign arg' loc'
    (Call loc name args) -> do
      args' <- mapM transArg args
      loc' <- newVar loc
      return $ Call loc' name args'
    (JumpIf arg label1 label2) -> do
      arg' <- transArg arg
      return $ JumpIf arg' label1 label2
    (Return arg) -> do
      arg' <- transArg arg
      return $ Return arg'
    q -> return q

transArg :: Arg -> QContext Arg
transArg arg =
  case arg of
    Var loc -> do
      (QEnv freeLoc remap) <- get
      case Data.Map.lookup loc remap of
        Just loc' -> return $ Var loc'
        Nothing -> do
          -- try to find loc in argument set
          (m, args) <- ask
          if Set.member loc args
            then -- just use the old location
              return $ Var loc
            else do
              -- tell that we need a phi
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
  return $ SSABlock (SSA.label block) (SSA.block block) (fromList phiMap') (SSA.next block) (SSA.previous block)

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
renameQuadruple m q =
  case q of
    (BinOp op arg1 arg2 res) -> BinOp op (renameArg m arg1) (renameArg m arg2) (renameLoc m res)
    (SingleArgOp op arg res) -> SingleArgOp op (renameArg m arg) (renameLoc m res)
    (CmpBinOp op arg1 arg2 res) -> CmpBinOp op (renameArg m arg1) (renameArg m arg2) (renameLoc m res)
    (Assign arg loc) -> Assign (renameArg m arg) (renameLoc m loc)
    (Call loc label args) -> Call (renameLoc m loc) label (Prelude.map (renameArg m) args)
    (JumpIf arg label1 label2) -> JumpIf (renameArg m arg) label1 label2
    (Return arg) -> Return (renameArg m arg)
    _ -> q

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
