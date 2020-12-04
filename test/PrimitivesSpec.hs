module PrimitivesSpec where

import           Lib
import           Test.Hspec
import           TestHelpers                    ( expect )
import           Data.IORef

testEnv :: IO Env
testEnv = newIORef []

-- Shorthand for reading and evaluating expressions
readEval :: String -> IO String
readEval expr = do
    env <- testEnv
    evalString env expr

primitivesSpec = parallel $ do
    describe "+ operation" $ do
        it "should throw error if arguments are missing"
            $   readEval "(+ 9)"
            >>= expect "Expected 2 args, found values: 9"
        it "should work with multiple arguments"
            $   readEval "(+ 1 2 3 4 5 6)"
            >>= expect "21"
    describe "type testing operators" $ do
        it "number? should return true on numbers"
            $   readEval "(number? 99)"
            >>= expect "#t"
        it "number? should return false for other types, even when coercible"
            $   readEval "(number? \"0\")"
            >>= expect "#f"
    describe "comparison operators on numbers" $ do
        it "`=` should evaluate equal values as true"
            $   readEval "(= 22 22)"
            >>= expect "#t"
        it "`=` should evaluate unequal values as false"
            $   readEval "(= 0 1)"
            >>= expect "#f"
    describe "if-clause " $ do
        it "should evaluate second argument if condition is true"
            $   readEval "(if (< 2 3) (+ 1 1) 0)"
            >>= expect "2"
        it "should evaluate third argument if condition is false"
            $   readEval "(if (> 2 3) (+ 1 1) 0)"
            >>= expect "0"
    describe "list primitives - car" $ do
        it "should pick the first argument of a list"
            $   readEval "(car '(1 2 3))"
            >>= expect "1"
        it "should work on lists with only one element"
            $   readEval "(car '(9))"
            >>= expect "9"
        it "should throw an error on non list argument"
            $   readEval "(car '1)"
            >>= expect "Invalid type: expected pair, found 1"
    describe "list primitives - cdr" $ do
        it "should pick the rest/tail argument of a list"
            $   readEval "(cdr '(1 2 3))"
            >>= expect "(2 3)"
        it "should return nil on list of one element"
            $   readEval "(cdr '(9))"
            >>= expect "()"
        it "should throw an error on non list argument"
            $   readEval "(cdr '1)"
            >>= expect "Invalid type: expected pair, found 1"
    describe "list primitives - cons" $ do
        it "should construct a one element list from one element and nil"
            $   readEval "(cons 1 '())"
            >>= expect "(1)"
        it "should prepend first argument to the list as second argument"
            $   readEval "(cons 1 '(2))"
            >>= expect "(1 2)"
        it "should prepend first argument to the head of a dotted list"
            $   readEval "(cons 1 '(2 . 3))"
            >>= expect "(1 2 . 3)"
        it "should form a dotted list of two non-list arguments"
            $   readEval "(cons #\\a #\\b)"
            >>= expect "(#\\a . #\\b)"
    describe "equality tests (strict) - eqv?, eq?" $ do
        -- testing only `eqv?` since it shares the same implementation as `eq?` 
        it "should successfully evaluate booleans (trues)"
            $   readEval "(eqv? #t #t)"
            >>= expect "#t"
        it "should successfully evaluate boolean falses"
            $   readEval "(eqv? #f #f)"
            >>= expect "#t"
        it "should successfully evaluate unequal booleans"
            $   readEval "(eqv? #t #f)"
            >>= expect "#f"
        it "should successfully compare equivalence of lists"
            $   readEval
                    "(eqv? '(1 \"ok\" '(#\\a #\\b)) '(1 \"ok\" '(#\\a #\\b)))"
            >>= expect "#t"
        it "should successfully compare unequivalence of lists"
            $   readEval
                    "(eqv? '(1 \"ok\" '(#\\a #\\b)) '(1 \"ok\" '(#\\a #\\c)))"
            >>= expect "#f"
        it "should successfully compare equivalence of dotted lists"
            $   readEval "(eqv? '(1 2 . #\\w) '(1 2 . #\\w))"
            >>= expect "#t"
    describe "loose equality test - equal?" $ do
        it "should consider numbers that convert to same string as equals"
            $   readEval "(equal? \"2\" 2)"
            >>= expect "#t"
        it "should consider unequal values unequal even after conversion"
            $   readEval "(equal? \"2\" 0)"
            >>= expect "#f"
        it "should consider booleans that convert to same string as equals"
            $   readEval "(equal? \"#f\" #f)"
            >>= expect "#t"
        it "should work on lists"
            $   readEval "(equal? '(1 2 3) '(1 2 \"3\"))"
            >>= expect "#t"
        it "should work on equal nested lists"
            $   readEval "(equal? '(\"#t\" 2 '(3 3)) '(#t 2 '(\"3\" 3)))"
            >>= expect "#t"
        it "should work on unequal nested lists"
            $   readEval "(equal? '(1 2 '(3 3)) '(1 2 '(\"4\" 3)))"
            >>= expect "#f"
    describe "condition expression -  cond" $ do
        it "should evaluate first true clause"
            $   readEval
                    (  "(cond ((= 1 2) \"not this\") "
                    <> "((= 1 1) \"this\") "
                    <> "(else \"not this either\"))"
                    )
            >>= expect "\"this\""
        it "should evaluate else clause if no clause before it evaluates true"
            $   readEval "(cond ((= 1 2) \"not this\") (else \"this\"))"
            >>= expect "\"this\""
        it "should throw an error, if no clause is true"
            $   readEval "(cond ((= 1 2) \"not this\") (#f \"nor this\"))"
            >>= expect "no true condition found: ()" -- TODO: this is suboptimal
