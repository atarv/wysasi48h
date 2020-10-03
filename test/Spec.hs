import           Lib
import           Test.Hspec
import           Data.Either

main :: IO ()
main = hspec $ do
    describe "readLispVal : Boolean" $ do
        it "should parse true (#t)" $ do
            let Right (Bool x) = readLispVal "#t"
            x `shouldBe` True
        it "should parse false (#f)" $ do
            let Right (Bool x) = readLispVal "#f"
            x `shouldBe` False
    describe "readLispVal : Atom" $ do
        it "should parse valid atom" $ do
            let Right (Atom x) = readLispVal "fooBar"
            x `shouldBe` "fooBar"
        it "should not parse atoms beginning with a number"
            $ shouldBe False
            $ case readLispVal "1Foo" of
                  Right (Atom x) -> True
                  _              -> False
    describe "readLispVal : String" $ do
        it "should parse empty string"
            $ shouldBe True
            $ case readLispVal "\"\"" of
                  Right (String s) -> s == ""
                  _                -> False
        it "should parse escaped characters"
            $ shouldBe True
            $ case readLispVal "\"\\\"\\t\\n\\\\\"" of
                  Right (String s) -> s == "\"\t\n\\"
                  _                -> False
        it "should not parse strings with missing delimiter"
            $ shouldBe False
            $ case readLispVal "\"" of
                  Right (String s) -> True
                  _                -> False
    describe "readLispVal : Character" $ do
        it "should parse named newline"
            $ shouldBe True
            $ case readLispVal "#\\newline" of
                  Right (Character c) -> c == '\n'
                  _                   -> False
        it "should parse named space"
            $ shouldBe True
            $ case readLispVal "#\\space" of
                  Right (Character c) -> c == ' '
                  _                   -> False
        it "should parse characters"
            $ shouldBe True
            $ case readLispVal "#\\(" of
                  Right (Character c) -> c == '('
                  _                   -> False
    describe "readLispVal : Number" $ do
        it "should parse base 10 numbers"
            $ shouldBe 123
            $ case readLispVal "123" of
                Right (Number x) -> x
                _ -> 0
        it "should parse explicit base 10 numbers"
            $ shouldBe 987654321
            $ case readLispVal "987654321" of
                Right (Number x) -> x
                _ -> 0
        it "should parse binary numbers"
            $ shouldBe 9
            $ case readLispVal "#b1001" of
                Right (Number x) -> x
                _ -> 0
        it "should parse octal numbers"
            $ shouldBe 16434824
            $ case readLispVal "#o76543210" of
                Right (Number x) -> x
                _ -> 0
        it "should parse hexadecimal numbers with mixed case"
            $ shouldBe 2882400144
            $ case readLispVal "#xAbcDEF90" of
                Right (Number x) -> x
                _ -> 0
        



