module Main where

import           Lib                            ( runOne
                                                , runRepl
                                                )
import           System.Environment             ( getArgs )
import           Data.List.NonEmpty             ( nonEmpty )

main :: IO ()
main = do
    getArgs >>= maybe runRepl runOne . nonEmpty
