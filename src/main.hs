module Main where

import Block
import Data.Function ((&))
import Data.List
import Data.Map (elems)
import Latte.Abs
import Latte.ErrM
import Latte.Par
import Optimize qualified
import Quadruples qualified
import SSA qualified
import Semantics qualified
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO

pipeline :: BlockMap -> String
pipeline b =
  SSA.transpose b
    & Optimize.optimize
    & Prelude.map show
    & intercalate "\n"

translate :: Program -> Err String
translate program =
  do
    quadruples <- Quadruples.translate program
    blocks <- Block.transpose quadruples
    return $ pipeline blocks

process :: String -> Err String
process source = do
  program <- pProgram $ myLexer source
  case Semantics.verify program of
    Semantics.Error err ->
      Bad $
        intercalate "\n" $
          map show err
    Semantics.Ok ->
      translate program

main :: IO ()
main = do
  -- open the file specficied by the first command line argument
  -- and read its contents into a string
  [fileName] <- getArgs
  handle <- openFile fileName ReadMode
  source <- hGetContents handle
  case process source of
    Latte.ErrM.Ok res ->
      putStrLn res
    Bad msg -> do
      hPutStrLn stderr msg
      exitFailure
