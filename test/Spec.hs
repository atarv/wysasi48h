import           Lib
import           Test.Hspec
import           Data.Either
import           ParserSpec                     ( parserSpec )
import           PrimitivesSpec                 ( primitivesSpec )

main :: IO ()
main = hspec $ do
    parserSpec
    primitivesSpec
