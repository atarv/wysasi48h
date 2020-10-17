module Main where

import Lib
import System.Environment
import Control.Monad (liftM)

main :: IO ()
main = do
    (expr : _) <- getArgs
    let initial = readExpr expr 
    evaluated <- return $ liftM show $ initial >>= eval
    putStrLn $ "Found value >>> " <> show initial
    putStrLn $ "Evaluated value >>> " <> (extractValue $ trapError evaluated)
