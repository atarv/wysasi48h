module PrimitivesSpec where

import           Lib
import           Test.Hspec
import           Data.Either

primitivesSpec = parallel $ do
    describe "+ operation" $ do
        it "should work with only one argument"
            $ shouldBe True
            $ case eval $ readExpr "(+ 9)" of
                  Number 9 -> True
                  _        -> False
        it "should work with multiple arguments"
            $ shouldBe True
            $ case eval $ readExpr "(+ 1 2 3 4 5 6)" of
                  Number 21 -> True
                  _        -> False

