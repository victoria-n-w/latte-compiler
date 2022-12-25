module SSA where

import Block (Block (..), BlockMap)
import Control.Monad.State
import Quadruples (LabelName)

-- | Transforms a map of blocks into a map of SSA blocks.
transpose :: BlockMap -> BlockMap
transpose b = b
