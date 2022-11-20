module Main where

import Data.List
import Latte.Abs
import Latte.ErrM
import Latte.Par
import Semantics
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

process :: String -> Err String
process source = do
  program <- pProgram $ myLexer source
  case Semantics.verify program of
    Semantics.Ok -> return "OK"
    Semantics.Error err ->
      Bad $
        intercalate "\n" $
          map show err

main :: IO ()
main = do
  source <- getContents
  case process source of
    Latte.ErrM.Ok res ->
      putStrLn res
    Bad msg -> do
      hPutStrLn stderr msg
      exitFailure
