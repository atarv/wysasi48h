module Main where

import Lib
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

main :: IO ()
main = do
    (expr : _) <- getArgs
    putStrLn $ "Input: " <> expr
    putStrLn $ readExpr expr
