module Main where

import Lib
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

main :: IO ()
main = do
    (expr : _) <- getArgs
    putStrLn $ "Input: " <> expr
    let initial = readExpr expr
        evaluated = eval initial
    putStrLn $ "Found value: " <> show initial
    putStrLn $ "Evaluated value: " <> show evaluated
