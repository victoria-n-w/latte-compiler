module Optimize where

import Control.Monad.RWS
import Control.Monad.Writer
import Data.Map qualified as Map
import Quadruples (Arg (..), Loc, Op (..), Quadruple (..))
import SSA

optimize :: [SSABlock] -> [SSABlock]
optimize = id
