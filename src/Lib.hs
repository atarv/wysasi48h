module Lib where
import           Text.ParserCombinators.Parsec
                                         hiding ( spaces )
import           Control.Monad                  ( liftM )

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

escapedCharSet :: [Char]
escapedCharSet = "tn\\\"fr"

escapedChar :: Parser Char
escapedChar = do
    char '\\'
    c <- oneOf escapedCharSet
    return $ case c of
        't'   -> '\t'
        'n'   -> '\n'
        'f'   -> '\f'
        'r'   -> '\r'
        other -> other

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many (escapedChar <|> anyChar)
    char '"'
    return $ String x

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest  <- many (letter <|> digit <|> symbol)
    let atom = first : rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

parseNumber :: Parser LispVal
-- parseNumber = liftM (Number . read) $ many1 digit
-- ex 1.1 do notation
-- parseNumber = do
--     digits <- many1 digit
--     return $ Number . read $ digits
-- ex 1.2 bind
parseNumber = many1 digit >>= return . Number . read


spaces :: Parser ()
spaces = skipMany1 space


parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left  err -> "No match: " ++ show err
    Right val -> "Found value"
