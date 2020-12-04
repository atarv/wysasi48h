module Main where

import           Lib                            ( runOne
                                                , runRepl
                                                )
import           System.Environment             ( getArgs )

main :: IO ()
main = do
    args <- getArgs
    case length args of
        0 -> runRepl
        1 -> runOne $ head args
        _ -> putStrLn help

help :: String
help =
    "Program takes only 1 or 0 arguments. If ran sans an argument REPL mode\ 
    \ is entered. If an argument is given, it is evaluated and the result is\ 
    \ printed."
