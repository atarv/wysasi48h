import           Test.Hspec
import           ParserSpec                     ( parserSpec )
import           PrimitivesSpec                 ( primitivesSpec )

main :: IO ()
main = hspec $ do
    parserSpec
    primitivesSpec
