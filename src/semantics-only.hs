module Main where

import Data.List
import Latte.Abs
import Latte.ErrM
import Latte.Par
import Semantics qualified
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO

process :: String -> Err String
process source = do
  program <- pProgram $ myLexer source
  case Semantics.verify program of
    Semantics.Error err ->
      Bad $
        intercalate "\n" $
          map show err
    Semantics.Ok ->
      pure "OK"

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
