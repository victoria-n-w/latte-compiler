module Semantics where

import Latte.Abs

import Control.Monad.State
import Control.Monad.Writer
import Data.Map

data Result = Ok | Error [String]

verify:: Program -> Result
verify program =
    Ok

type TypeBinds = Map String Type

data Env = Env {
    typeBinds :: TypeBinds,
    funcName :: String
}

type Context a = StateT Env (WriterT [String] a)
