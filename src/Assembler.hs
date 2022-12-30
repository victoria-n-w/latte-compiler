module Assembler where

import Data.Map qualified as Map
import Liveness (LBlock (..), LBlockMap, LiveVars)
import Quadruples (Arg (..), LabelName, Loc, Op (..), Quadruple (..))

generateBlocks :: LBlockMap -> Map.Map LabelName ASMBlock
generateBlocks = Map.map generateBlock

generateBlock :: LBlock -> ASMBlock
generateBlock (LBlock label block phiMap next prievious outLiveVars) =
  ASMBlock
    { label = label,
      code = generateCode code,
      prievious = [],
      next = []
    }

data ASMBlock = ASMBlock
  { label :: LabelName,
    code :: [String],
    prievious :: [LabelName],
    next :: [LabelName]
  }

instance Show ASMBlock where
  show :: ASMBlock -> String
  show (ASMBlock label code prvs next) =
    "---\n"
      ++ label
      ++ ":\n"
      ++ "prvs: "
      ++ show prvs
      ++ "\n"
      ++ unlines code
      ++ "next: "
      ++ show next
