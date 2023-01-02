module Block where

import Control.Monad.Writer
import Data.Map
import Latte.ErrM
import Quadruples

data Block = Block
  { label :: LabelName,
    block :: [Quadruple],
    next :: [LabelName],
    prievious :: [LabelName]
  }

instance Show Block where
  show :: Block -> String
  show (Block label block next prvs) =
    "---\n" ++ label ++ ":\n" ++ "prvs: " ++ show prvs ++ "\n" ++ unlines (Prelude.map show block) ++ "next: " ++ show next

type BlockMap = Map LabelName Block

type TopDef = TopDef' BlockMap

transpose :: [Quadruples.TopDef] -> Err [Block.TopDef]
transpose = mapM (\(Quadruples.TopDef' name args block) -> TopDef' name args <$> transpose' block)

transpose' :: [Quadruple] -> Err BlockMap
transpose' q = do
  blockList <- execWriterT $ divideIntoBlocks q
  let blockMap = fromList $ Prelude.map (\b -> (label b, b)) blockList
  return $ fillPrevious blockMap

type BContext = WriterT [Block] Err

-- | Transforms a list of quadruples into a map of blocks.
-- Passes through the list of quadruples once, and creates a new block
-- each block starts at a label, and ends at a jump or return
divideIntoBlocks :: [Quadruple] -> BContext ()
divideIntoBlocks q =
  case q of
    [] -> return ()
    (Label label) : rest -> do
      (block, rest') <- divideBlock [] rest
      tell [Block label block (nextLabels block) []]
      divideIntoBlocks rest'
    _ -> fail "divideIntoBlocks: first quadruple is not a label"

-- | Divides a list of quadruples into a block and the rest of the list.
-- A block ends at a jump or return.
divideBlock :: [Quadruple] -> [Quadruple] -> BContext ([Quadruple], [Quadruple])
divideBlock acc [] = return (acc, [])
divideBlock acc (q : rest) =
  case q of
    Jump _ -> return (acc ++ [q], rest)
    JumpIf {} -> return (acc ++ [q], rest)
    Return _ -> return (acc ++ [q], rest)
    ReturnVoid -> return (acc ++ [q], rest)
    Label _ -> fail "divideBlock: label in the middle of a block"
    _ -> divideBlock (acc ++ [q]) rest

-- | Returns a list of labels that are the targets of jumps in the block.
nextLabels :: [Quadruple] -> [LabelName]
nextLabels q = jumpLabels (last q)

-- | Pass the map of blocks and returns a map of blocks with the
-- previous field filled in.
fillPrevious :: BlockMap -> BlockMap
fillPrevious blockMap = fromList $ Prelude.map (\b -> (label b, b {prievious = previous b})) (elems blockMap)
  where
    previous :: Block -> [LabelName]
    previous b =
      Prelude.map fst $
        Prelude.filter (\(_, l) -> label b `elem` l) $
          Prelude.map (\b -> (label b, next b)) (elems blockMap)
