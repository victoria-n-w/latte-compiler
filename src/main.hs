module Main where

import Block
import Data.List
import Data.Map (elems)
import Latte.Abs
import Latte.ErrM
import Latte.Par
import Quadruples qualified
import SSA qualified
import Semantics qualified
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

translate :: Program -> Err String
translate program = do
  quadruples <- Quadruples.translate program
  blocks <- Block.transpose quadruples
  let ssaBlocks = SSA.transpose blocks
  return $ intercalate "\n" $ map show ssaBlocks

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
  source <- getContents
  case process source of
    Latte.ErrM.Ok res ->
      putStrLn res
    Bad msg -> do
      hPutStrLn stderr msg
      exitFailure
