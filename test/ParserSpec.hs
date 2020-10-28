module ParserSpec where

import           Lib
import           Test.Hspec
import           TestHelpers                    ( expect )

parserSpec = parallel $ do
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
            $ expect False
            $ case readLispVal "1Foo" of
                  Right (Atom _) -> True
                  _              -> False
    describe "readLispVal : String" $ do
        it "should parse empty string"
            $ expect True
            $ case readLispVal "\"\"" of
                  Right (String s) -> s == ""
                  _                -> False
        it "should parse escaped characters"
            $ expect True
            $ case readLispVal "\"\\\"\\t\\n\\\\\"" of
                  Right (String s) -> s == "\"\t\n\\"
                  _                -> False
        it "should not parse strings with missing delimiter"
            $ expect False
            $ case readLispVal "\"" of
                  Right (String _) -> True
                  _                -> False
    describe "readLispVal : Character" $ do
        it "should parse named newline"
            $ expect True
            $ case readLispVal "#\\newline" of
                  Right (Character c) -> c == '\n'
                  _                   -> False
        it "should parse named space"
            $ expect True
            $ case readLispVal "#\\space" of
                  Right (Character c) -> c == ' '
                  _                   -> False
        it "should parse characters" $ expect True $ case readLispVal "#\\(" of
            Right (Character c) -> c == '('
            _                   -> False
    describe "readLispVal : Number" $ do
        it "should parse base 10 numbers"
            $ expect 123
            $ case readLispVal "123" of
                  Right (Number x) -> x
                  _                -> 0
        it "should parse explicit base 10 numbers"
            $ expect 987654321
            $ case readLispVal "987654321" of
                  Right (Number x) -> x
                  _                -> 0
        it "should parse binary numbers"
            $ expect 9
            $ case readLispVal "#b1001" of
                  Right (Number x) -> x
                  _                -> 0
        it "should parse octal numbers"
            $ expect 16434824
            $ case readLispVal "#o76543210" of
                  Right (Number x) -> x
                  _                -> 0
        it "should parse hexadecimal numbers with mixed case"
            $ expect 2882400144
            $ case readLispVal "#xAbcDEF90" of
                  Right (Number x) -> x
                  _                -> 0
    describe "parseLispVal : List" $ do
        it "should parse empty list" $ expect True $ case readLispVal "()" of
            Right (List xs) -> null xs
            _               -> False
        it "should parse a basic list"
            $ expect [1, 2, 3]
            $ case readLispVal "(1 2  3)" of
                  Right (List [Number a, Number b, Number c]) -> [a, b, c]
                  _ -> []
        it "should parse nested lists"
            $ expect True
            $ case readLispVal "(1 (2 3))" of
                  Right (List [Number a, List [Number b, Number c]]) ->
                      a == 1 && b == 2 && c == 3
                  _ -> False
        it "should parse lists consisting of different types of elements"
            $ expect True
            $ case readLispVal "(#t #\\$ \"test\")" of
                  Right (List [Bool b, Character c, String s]) ->
                      b && c == '$' && s == "test"
                  _ -> False
    describe "parseLispVal : DottedList"
        $ it "should parse dotted list"
        $ expect True
        $ case readLispVal "(a b . 3)" of
              Right (DottedList [Atom a, Atom b] (Number c)) ->
                  a == "a" && b == "b" && c == 3
              _ -> False
