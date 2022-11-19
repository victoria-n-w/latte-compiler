module Main where

import Latte.Abs
import Latte.ErrM
import Latte.Par
import Semantics

import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Data.List


process :: String -> Err String
process source = do
    program <- pProgram $ myLexer source
    case Semantics.verify program of
        Semantics.Ok -> return "OK"
        Semantics.Error err -> Bad $ intercalate "\n"
            $ map (\msg -> "\ESC[0;31mError:\ESC[0m " ++ msg ++ "\n") err


main :: IO()
main = do
    source <- getContents
    case process source of
        Latte.ErrM.Ok res ->
            putStrLn res
        Bad msg -> do
            hPutStrLn stderr $ "\ESC[0;31mError:\ESC[0m " ++ msg
            exitFailure
