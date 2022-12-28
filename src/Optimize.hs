module Optimize where

import SSA

optBeforeLiveness :: [SSABlock] -> [SSABlock]
optBeforeLiveness = id

optAfterLiveness :: [SSABlock] -> [SSABlock]
optAfterLiveness = id
