module Optimize where

import Liveness
import SSA

optBeforeLiveness :: [SSABlock] -> [SSABlock]
optBeforeLiveness = id

optAfterLiveness :: LBlockMap -> LBlockMap
optAfterLiveness = id
