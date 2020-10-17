module PrimitivesSpec where

import           Lib
import           Test.Hspec
import           TestHelpers                    ( expect )

-- Shorthand for reading and evaluating expressions
readEval :: String -> Either LispError LispVal
readEval expr = readExpr expr >>= eval

primitivesSpec = parallel $ do
    describe "+ operation" $ do
        it "should throw error if arguments are missing"
            $ expect False
            $ case readEval "(+ 9)" of
                  Right _             -> True
                  Left  (NumArgs 2 _) -> False
        it "should work with multiple arguments"
            $ expect 21
            $ case readEval "(+ 1 2 3 4 5 6)" of
                  Right (Number n) -> n
                  _                -> 0

    describe "type testing operators" $ do
        it "number? should return true on numbers"
            $ expect True
            $ case readEval "(number? 99)" of
                  Right (Bool x) -> x
                  Left  _        -> False
        it "number? should return false for other types, even when coercible"
            $ expect False
            $ case readEval "(number? \"0\")" of
                  Right (Bool x) -> x
                  Left  _        -> False
    describe "comparison operators on numbers" $ do
        it "`=` should evaluate equal values as true"
            $ expect True
            $ case readEval "(= 22 22)" of
                  Right (Bool b) -> b
                  _              -> False
        it "`=` should evaluate unequal values as false"
            $ expect False
            $ case readEval "(= 0 1)" of
                  Right (Bool b) -> b
                  _              -> True


