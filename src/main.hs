module Main where

import Block
import Data.List
import Data.Map (elems)
import Latte.Abs
import Latte.ErrM
import Latte.Par
import Quadruples
import Semantics
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

process :: String -> Err String
process source = do
  program <- pProgram $ myLexer source
  case Semantics.verify program of
    Semantics.Error err ->
      Bad $
        intercalate "\n" $
          map show err
    Semantics.Ok -> do
      quadruples <- Quadruples.translate program
      blocks <- Block.transpose quadruples
      return $ intercalate "\n" $ map show $ elems blocks

main :: IO ()
main = do
  source <- getContents
  case process source of
    Latte.ErrM.Ok res ->
      putStrLn res
    Bad msg -> do
      hPutStrLn stderr msg
      exitFailure
