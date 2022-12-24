module Block where

import Control.Monad.Writer
import Data.Map
import Latte.ErrM
import Quadruples

data Block = Block
  { label :: LabelName,
    block :: [Quadruple],
    next :: [LabelName]
  }

instance Show Block where
  show :: Block -> String
  show (Block label block next) =
    "---\n" ++ label ++ ":\n" ++ unlines (Prelude.map show block) ++ "next: " ++ show next

type BlockMap = Map LabelName Block

transpose :: [Quadruple] -> Err BlockMap
transpose q = do
  res <- execWriterT $ divideIntoBlocks q
  return $ fromList $ Prelude.map (\b -> (label b, b)) res

type BContext = WriterT [Block] Err

-- | Transforms a list of quadruples into a map of blocks.
-- Passes through the list of quadruples once, and creates a new block
-- each block starts at a label, and ends at a jump or return
divideIntoBlocks :: [Quadruple] -> BContext ()
divideIntoBlocks q =
  case q of
    [] -> return ()
    (Quadruple (Label label) None None None) : rest -> do
      (block, rest') <- divideBlock [] rest
      tell [Block label block (nextLabels block)]
      divideIntoBlocks rest'
    _ -> fail "divideIntoBlocks: first quadruple is not a label"

-- | Divides a list of quadruples into a block and the rest of the list.
-- A block ends at a jump or return.
divideBlock :: [Quadruple] -> [Quadruple] -> BContext ([Quadruple], [Quadruple])
divideBlock acc [] = return (acc, [])
divideBlock acc (q : rest) =
  case op q of
    Jump -> return (acc ++ [q], rest)
    JumpIf -> return (acc ++ [q], rest)
    Return -> return (acc ++ [q], rest)
    ReturnVoid -> return (acc ++ [q], rest)
    Label _ -> fail "divideBlock: label in the middle of a block"
    _ -> divideBlock (acc ++ [q]) rest

-- | Returns a list of labels that are the targets of jumps in the block.
nextLabels :: [Quadruple] -> [LabelName]
nextLabels q = jumpLabels (last q)
