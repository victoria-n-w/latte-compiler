module Assembler where

import Control.Monad.RWS
import Data.List (intercalate)
import Data.Map qualified as Map
import Data.Maybe
import Liveness (LBlock (..), LBlockMap, LiveQuadruple, LiveVars)
import Quadruples (Arg (..), LabelName, Loc, Op (..), Quadruple (..))
import SSA (Phi, PhiMap)
import Text.Printf (printf)

data ASMBlock = ASMBlock
  { label :: LabelName,
    phi :: PhiMap,
    code :: [String],
    prievious :: [LabelName],
    next :: [LabelName]
  }

generateBlocks :: LBlockMap -> Map.Map LabelName ASMBlock
generateBlocks =
  Map.map
    ( \block ->
        let (_, _, res) = runRWS (generateBlock block) () (makeRegisterMap, Memory 0 Map.empty, Map.empty)
         in ASMBlock
              (Liveness.label block)
              (phiMap block)
              res
              (Liveness.prievious block)
              (Liveness.next block)
    )

generateBlock :: LBlock -> Context ()
generateBlock (LBlock _ block phiMap _ _ inLive outLive) = do
  parsePhi phiMap
  mapM_ generateQuadruple block

type Context = RWS () [String] (RegisterMap, Memory, VarMap)

type RegisterMap = Map.Map RegisterName Register

-- | Omit RSP and RBP because they are used for stack operations.
data RegisterName = RAX | RBX | RCX | RDX | RSI | RDI | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
  deriving (Eq, Ord, Show, Enum)

-- | Initializes the map of empty registers.
makeRegisterMap :: RegisterMap
makeRegisterMap = Map.fromList $ Prelude.map (,Empty) [RAX .. R15]

type VarMap = Map.Map Loc VarLoc

data Memory = Memory
  { nextLoc :: Int,
    mmry :: MemMap
  }

type MemMap = Map.Map Int Loc

data VarLoc = Reg RegisterName | InMem Loc
  deriving (Eq, Ord, Show)

generateQuadruple :: LiveQuadruple -> Context ()
generateQuadruple (quad, liveVars) = do
  case Quadruples.res quad of
    Var loc -> do
      assignVariable loc
      return ()
    _ -> return ()
  case quad of
    (Quadruple op arg1 arg2 res) ->
      do
        tell [show quad]

parsePhi :: PhiMap -> Context ()
parsePhi phiMap = do
  mapM_
    ( \(loc, _) ->
        assignVariable loc
    )
    (Map.toList phiMap)

data Register = Full Loc | Empty
  deriving (Eq, Ord, Show)

-- | Try to assign a variable to a register,
-- if there is no free register, assign it to memory.
assignVariable :: Loc -> Context VarLoc
assignVariable loc = do
  freeReg <- getFreeRegister
  case freeReg of
    Just regName -> do
      assignToRegister regName loc
      return $ Reg regName
    Nothing -> do
      memLoc <- assignToMemory loc
      return $ InMem memLoc

getVariableLocation :: Loc -> Context VarLoc
getVariableLocation loc = do
  (_, _, varMap) <- get
  return $ varMap Map.! loc

-- | Returns the freed register name and variable that was assigned to it
-- Precondition: there is a register to spill
getFreeRegister :: Context (Maybe RegisterName)
getFreeRegister = do
  (regMap, _, _) <- get
  let freeRegs = Map.filter (== Empty) regMap
  return $ if Map.null freeRegs then Nothing else Just $ fst $ Map.findMin freeRegs

assignToRegister :: RegisterName -> Loc -> Context ()
assignToRegister regName loc = do
  (regMap, mmry, varMap) <- get
  put (Map.insert regName (Full loc) regMap, mmry, Map.insert loc (Reg regName) varMap)

assignToMemory :: Loc -> Context Int
assignToMemory loc = do
  (regMap, Memory nextLoc memMap, varMap) <- get
  put (regMap, Memory (nextLoc + 1) (Map.insert nextLoc loc memMap), Map.insert loc (InMem nextLoc) varMap)
  return nextLoc

instance Show ASMBlock where
  show :: ASMBlock -> String
  show (ASMBlock label phiMap code prvs next) =
    "---\n"
      ++ label
      ++ ":\n"
      ++ "phi: "
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
                        (Map.toList phi)
                    )
            )
            (Map.toList phiMap)
        )
      ++ "prvs: "
      ++ show prvs
      ++ "\n"
      ++ unlines (map ("\t" ++) code)
      ++ "next: "
      ++ show next
