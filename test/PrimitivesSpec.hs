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
    describe "if-clause " $ do
        it "should evaluate second argument if condition is true"
            $ expect 2
            $ case readEval "(if (< 2 3) (+ 1 1) 0)" of
                  Right (Number n) -> n
                  _                -> -1
        it "should evaluate third argument if condition is false"
            $ expect 0
            $ case readEval "(if (> 2 3) (+ 1 1) 0)" of
                  Right (Number n) -> n
                  _                -> -1
    describe "list primitives - car" $ do
        it "should pick the first argument of a list"
            $ expect 1
            $ case readEval "(car '(1 2 3))" of
                  Right (Number n) -> n
                  _                -> -1
        it "should work on lists with only one element"
            $ expect 9
            $ case readEval "(car '(9))" of
                  Right (Number n) -> n
                  _                -> -1
        it "should throw an error on non list argument"
            $ expect "pair"
            $ case readEval "(car '1)" of
                  Right _                  -> ""
                  Left  (TypeMismatch t _) -> t
    describe "list primitives - cdr" $ do
        it "should pick the rest/tail argument of a list"
            $ expect [2, 3]
            $ case readEval "(cdr '(1 2 3))" of
                  Right (List [Number a, Number b]) -> [a, b]
                  _ -> []
        it "should return nil on list of one element"
            $ expect True
            $ case readEval "(cdr '(9))" of
                  Right (List []) -> True
                  _               -> False
        it "should throw an error on non list argument"
            $ expect "pair"
            $ case readEval "(cdr '1)" of
                  Right _                  -> ""
                  Left  (TypeMismatch t _) -> t
    describe "list primitives - cons" $ do
        it "should construct a one element list from one element and nil"
            $ expect [1]
            $ case readEval "(cons 1 '())" of
                  Right (List [Number n]) -> [n]
                  _                       -> []
        it "should prepend first argument to the list as second argument"
            $ expect [1, 2]
            $ case readEval "(cons 1 '(2))" of
                  Right (List [Number a, Number b]) -> [a, b]
                  _ -> []
        it "should prepend first argument to the head of a dotted list"
            $ expect ([1, 2], 3)
            $ case readEval "(cons 1 '(2 . 3))" of
                  Right (DottedList [Number a, Number b] (Number c)) ->
                      ([a, b], c)
                  _ -> ([], 0)
        it "should form a dotted list of two non-list arguments"
            $ expect (['a'], 'b')
            $ case readEval "(cons #\\a #\\b)" of
                  Right (DottedList [Character a] (Character b)) -> ([a], b)
                  _ -> ([], '\0')
    describe "equvality tests - eqv?, eq?" $ do
        -- testing only `eqv?` since it shares the same implementation as `eq?` 
        it "should successfully evaluate booleans (trues)"
            $ expect True
            $ case readEval "(eqv? #t #t)" of
                  Right (Bool b) -> b
                  _              -> False
        it "should successfully evaluate boolean falses"
            $ expect True
            $ case readEval "(eqv? #f #f)" of
                  Right (Bool b) -> b
                  _              -> False
        it "should successfully evaluate unequal booleans"
            $ expect False
            $ case readEval "(eqv? #t #f)" of
                  Right (Bool b) -> b
                  _              -> False
        it "should successfully compare equivalence of lists"
            $ expect True
            $ case
                  readEval
                      "(eqv? '(1 \"ok\" '(#\\a #\\b)) '(1 \"ok\" '(#\\a #\\b)))"
              of
                  Right (Bool b) -> b
                  _              -> False
        it "should successfully compare unequivalence of lists"
            $ expect False
            $ case
                  readEval
                      "(eqv? '(1 \"ok\" '(#\\a #\\b)) '(1 \"ok\" '(#\\a #\\c)))"
              of
                  Right (Bool b) -> b
                  _              -> False
        it "should successfully compare equivalence of dotted lists"
            $ expect True
            $ case readEval "(eqv? '(1 2 . #\\w) '(1 2 . #\\w))" of
                  Right (Bool b) -> b
                  _              -> False
