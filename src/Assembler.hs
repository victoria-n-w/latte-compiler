module Assembler where

import Control.Monad.RWS
import Data.Data
import Data.List (intercalate)
import Data.Map qualified as Map
import Data.Maybe
import Latte.ErrM
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
generateBlock (LBlock label block phiMap _ _ inLive outLive) = do
  tell [label ++ ":"]
  parseInVars inLive
  parsePhi phiMap
  mapM_
    ( \q -> do
        generateQuadruple q
        killDeadVars q
    )
    block

type Context = RWS () [String] (RegisterMap, Memory, VarMap)

type RegisterMap = Map.Map RegisterName Register

-- | Omit RSP and RBP because they are used for stack operations.
data RegisterName = RAX | RBX | RCX | RDX | RSI | RDI | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
  deriving (Eq, Ord, Enum)

instance Show RegisterName where
  show :: RegisterName -> String
  show RAX = "rax"
  show RBX = "rbx"
  show RCX = "rcx"
  show RDX = "rdx"
  show RSI = "rsi"
  show RDI = "rdi"
  show R8 = "r8"
  show R9 = "r9"
  show R10 = "r10"
  show R11 = "r11"
  show R12 = "r12"
  show R13 = "r13"
  show R14 = "r14"
  show R15 = "r15"

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
  case quad of
    Quadruple Add arg1 arg2 (Var res) -> do
      generateOp liveVars arg1 arg2 res "add"
      return ()
    Quadruple Sub arg1 arg2 (Var res) -> do
      generateOp liveVars arg1 arg2 res "sub"
      return ()
    Quadruple Mul arg1 arg2 (Var res) -> do
      generateOp liveVars arg1 arg2 res "imul"
      return ()
    Quadruple Neg arg _ (Var res) -> do
      generateOp liveVars arg (Const (-1)) res "imul"
      return ()
    Quadruple And arg1 arg2 (Var res) -> do
      generateOp liveVars arg1 arg2 res "and"
      return ()
    Quadruple Or arg1 arg2 (Var res) -> do
      generateOp liveVars arg1 arg2 res "or"
      return ()
    Quadruple Not arg _ (Var res) -> do
      generateOp liveVars arg (Const 1) res "xor"
      return ()
    Quadruple Eq arg1 arg2 (Var res) -> do
      generateCmp liveVars arg1 arg2 res "sete"
    Quadruple Neq arg1 arg2 (Var res) -> do
      generateCmp liveVars arg1 arg2 res "setne"
    Quadruple Gt arg1 arg2 (Var res) -> do
      generateCmp liveVars arg1 arg2 res "setg"
    Quadruple Lt arg1 arg2 (Var res) -> do
      generateCmp liveVars arg1 arg2 res "setl"
    Quadruple Ge arg1 arg2 (Var res) -> do
      generateCmp liveVars arg1 arg2 res "setge"
    Quadruple Le arg1 arg2 (Var res) -> do
      generateCmp liveVars arg1 arg2 res "setle"
    -- TODO handle arrays
    Quadruple Assign arg _ (Var loc) -> do
      -- TODO not force lol (?)
      res' <- forceGetRegister
      case arg of
        Var loc -> do
          varLoc <- getVariableLocation loc
          case varLoc of
            Reg regName -> do
              unless (regName == res') $ tell [printf "mov %s, %s" (show res') (show regName)]
            InMem memLoc -> do
              tell [printf "mov %s, [rbp - %d]" (show res') (memLoc * 8)]
        _ -> do
          tell [printf "mov %s, %s" (show res') (show arg)]
    Quadruple Get (Const n) _ (Var res) -> do
      -- get variable from stack location
      resReg <- forceGetRegister
      tell [printf "mov %s, [rbp - %d]" (show resReg) (n * 8)]
    Quadruple Put (Const n) (Var loc) _ ->
      -- save variable to stack location
      -- TODO double check this
      tell [printf "mov [rbp - %d], %d" (n * 8) loc]
    Quadruple (Label label) _ _ _ -> do
      tell [printf "%s:" label]
    Quadruple Jump _ _ label -> do
      tell [printf "jmp %s" (show label)]
    Quadruple JumpIf what label1 label2 -> do
      case what of
        Var loc -> do
          varLoc <- getVariableLocation loc
          case varLoc of
            Reg regName -> do
              tell [printf "cmp %s, 0" (show regName)]
            InMem n -> do
              tell [printf "cmp [rbp - %d], 0" (n * 8)]
          tell [printf "je %s" (show label1)]
          tell [printf "jmp %s" (show label2)]
        Const i -> do
          if i == 0
            then do
              -- condition always false, jump to the second label
              tell [printf "jmp %s" (show label2)]
            else do
              -- condition always true, jump to the first label
              tell [printf "jmp %s" (show label1)]
    Quadruple Return arg _ _ -> do
      case arg of
        Var loc -> do
          varLoc <- getVariableLocation loc
          case varLoc of
            Reg regName -> do
              unless (regName == RAX) $ tell [printf "mov rax, %s" (show regName)]
            InMem memLoc -> do
              tell [printf "mov rax, [rbp - %d]" (memLoc * 8)]
        _ -> do
          tell [printf "mov rax, %s" (show arg)]
      tell ["leave", "ret"]
    Quadruple ReturnVoid _ _ _ -> do
      tell ["leave", "ret"]
    Quadruple Nop _ _ _ -> return ()
    Quadruple op arg1 arg2 res -> do
      tell [printf "%s %s, %s, %s" (show op) (show arg1) (show arg2) (show res)]

generateOp :: LiveVars -> Arg -> Arg -> Loc -> String -> Context RegisterName
generateOp liveVars arg1 arg2 res op = do
  opReg <- case arg1 of
    Var loc -> do
      varLoc <- getVariableLocation loc
      case varLoc of
        Reg regName -> do
          return regName
        InMem memLoc -> do
          opReg' <- forceGetRegister
          tell [printf "mov %s, [rbp - %d]" (show opReg') (memLoc * 8)]
          return opReg'
    _ -> do
      opReg' <- forceGetRegister
      tell [printf "mov %s, %s" (show opReg') (show arg1)]
      return opReg'
  -- if arg1 is live after this instruction, we need to save it
  case arg1 of
    Var loc -> do
      when (loc `elem` liveVars) $ do
        newVarReg <- forceGetRegister
        tell [printf "mov %s, %s" (show newVarReg) (show opReg)]
        assignToRegister newVarReg loc
    _ -> return ()
  assignToRegister opReg res
  -- emit the operation
  case arg2 of
    Var loc -> do
      varLoc <- getVariableLocation loc
      case varLoc of
        Reg regName -> do
          tell [printf "%s %s, %s" op (show opReg) (show regName)]
        InMem memLoc -> do
          tell [printf "%s %s, [rbp - %d]" op (show opReg) (memLoc * 8)]
    _ -> do
      tell [printf "%s %s, %s" op (show opReg) (show arg2)]
  return opReg

generateCmp :: LiveVars -> Arg -> Arg -> Loc -> String -> Context ()
generateCmp liveVars arg1 arg2 res op = do
  opReg <- generateOp liveVars arg1 arg2 res "cmp"
  -- emit the set instruction
  tell [printf "%s %s" op (show opReg)]

killDeadVars :: LiveQuadruple -> Context ()
killDeadVars (_, liveVars) = do
  (_, _, varMap) <- get
  -- filter the list of all variables that are not in liveVars set
  let deadVars = Map.filterWithKey (\loc _ -> loc `notElem` liveVars) varMap
   in mapM_ killVariable (Map.toList deadVars)

killVariable :: (Loc, VarLoc) -> Context ()
killVariable (loc, varLoc) = do
  rmFromVarMap loc
  case varLoc of
    Reg regName -> do
      freeRegister regName
    InMem memLoc -> do
      freeMemory memLoc

rmFromVarMap :: Loc -> Context ()
rmFromVarMap loc = do
  (regMap, mem, varMap) <- get
  put (regMap, mem, Map.delete loc varMap)

freeRegister :: RegisterName -> Context ()
freeRegister regName = do
  (regMap, mem, varMap) <- get
  put (Map.insert regName Empty regMap, mem, varMap)

freeMemory :: Int -> Context ()
freeMemory memLoc = do
  (regMap, mem, varMap) <- get
  put (regMap, mem {mmry = Map.delete memLoc (mmry mem)}, varMap)

parseInVars :: LiveVars -> Context ()
parseInVars liveVars = do
  mapM_
    ( \loc -> do
        assignVariable loc
    )
    liveVars

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

forceGetRegister :: Context RegisterName
-- TODO this is not correct, we need to spill a register
forceGetRegister = do
  (regMap, _, _) <- get
  let freeRegs = Map.filter (== Empty) regMap
  return $ fst $ Map.findMin freeRegs

-- | Returns the freed register name and variable that was assigned to it
-- Precondition: there is a register to spill
getFreeRegister :: Context (Maybe RegisterName)
getFreeRegister = do
  (regMap, _, _) <- get
  let freeRegs = Map.filter (== Empty) regMap
  return $ if Map.null freeRegs then Nothing else Just $ fst $ Map.findMin freeRegs

-- | Performs operation on the State monad
assignToRegister :: RegisterName -> Loc -> Context ()
assignToRegister regName loc = do
  (regMap, mmry, varMap) <- get
  put (Map.insert regName (Full loc) regMap, mmry, Map.insert loc (Reg regName) varMap)

-- | Performs operation on the State monad
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
