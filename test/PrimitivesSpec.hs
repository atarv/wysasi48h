module PrimitivesSpec where

import           Lib
import           Test.Hspec

-- Shorthand for reading and evaluating expressions
readEval :: String -> Either LispError LispVal
readEval expr = readExpr expr >>= eval

primitivesSpec = parallel $
    describe "+ operation" $ do
        it "should throw error if arguments are missing"
            $ shouldBe False
            $ case readEval "(+ 9)" of
                Right _ -> True
                Left (NumArgs 2 _) -> False

        it "should work with multiple arguments"
            $ shouldBe True
            $ case readEval "(+ 1 2 3 4 5 6)" of
                  Right (Number 21) -> True
                  _        -> False

