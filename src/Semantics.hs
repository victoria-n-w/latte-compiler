module Semantics where

import Latte.Abs
import Latte.ErrM

verify:: Program -> Err Program
verify program = do
    return program
