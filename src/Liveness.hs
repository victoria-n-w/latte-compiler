module Liveness where

import Data.Set
import Quadruples (Arg (..), LabelName, Loc, Op (..), Quadruple (..))
import SSA

data Liveness = Liveness
  { inLive :: Set Loc,
    out :: Set Loc,
    kill :: Set Loc,
    use :: Set Loc
  }
