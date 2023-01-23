module Optimize where

import CTypes
import Control.Monad.RWS
import Control.Monad.Writer
import Data.Map qualified as Map
import Quadruples (Arg (..), Op (..), Quadruple (..))
import SSA

optimize :: [SSABlock] -> [SSABlock]
optimize = id
