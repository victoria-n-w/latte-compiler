module Main where

import Latte.Abs
import Latte.ErrM
import Latte.Par
import Semantics

import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)


process :: String -> Err String
process source = do
    program <- pProgram $ myLexer source
    program <- Semantics.verify program
    return "OK"


main :: IO()
main = do
    source <- getContents
    case process source of
        Ok res ->
            putStrLn res
        Bad msg -> do
            hPutStrLn stderr $ "\ESC[0;31mError:\ESC[0m " ++ msg
            exitFailure
