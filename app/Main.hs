module Main where

import Lib
import System.Environment ( getArgs )

main :: IO ()
main = do
    args <- getArgs
    case length args of
        0 -> runRepl
        1 -> evalAndPrint $ args !! 0
        _ -> putStrLn "Program takes only 1 or 0 arguments. If ran sans an \
            \ argument REPL mode is entered. If an argument is given, it's \
            \  evaluated and the result is printed."
