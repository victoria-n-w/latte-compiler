module Registers where

import Liveness (LBlock (..), LBlockMap, LiveVars)

allocate :: LBlockMap -> LBlockMap
allocate = id
