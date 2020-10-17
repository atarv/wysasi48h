module TestHelpers where

import           Test.Hspec

expect :: (HasCallStack, Show a, Eq a) => a -> a -> Expectation
expect = flip shouldBe
