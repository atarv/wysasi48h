module Lib where
import           Text.ParserCombinators.Parsec
                                         hiding ( spaces )
import           Control.Monad                  ( liftM )
import           Numeric                        ( readOct
                                                , readHex
                                                , readDec
                                                , readInt
                                                )
import qualified Data.Char                     as Char

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             deriving (Show)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

escapedChar :: Parser Char
escapedChar = do
    char '\\'
    c <- oneOf "\\\"fnrt"
    return $ case c of
        'f'   -> '\f'
        'n'   -> '\n'
        'r'   -> '\r'
        't'   -> '\t'
        other -> other

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many $ escapedChar <|> noneOf "\\\""
    char '"'
    return $ String x

parseCharacter :: Parser LispVal
parseCharacter = do
    try $ string "#\\"
    value <- try (string "space" <|> string "newline") <|> do
        x <- anyChar
        notFollowedBy alphaNum
        return [x]
    return $ Character $ case map Char.toLower value of
        "space"   -> ' '
        "newline" -> '\n'
        [x]       -> x

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest  <- many (letter <|> digit <|> symbol)
    return . Atom $ first : rest

parseBool :: Parser LispVal
parseBool = do
    char '#'
    (char 't' >> (return $ Bool True)) <|> (char 'f' >> (return $ Bool False))

parseNumber :: Parser LispVal
parseNumber = base10 <|> base10Explicit <|> binary <|> octal <|> hexadecimal
  where
    binary = do
        try $ string "#b"
        binaryDigits <- many1 $ oneOf "01"
        let readBin = readInt 2 (\c -> c == '0' || c == '1') (\i -> read [i])
            [(result, _)] = readBin binaryDigits
        return $ Number result
    octal = do
        try $ string "#o"
        octalDigits <- many1 octDigit
        let [(result, _)] = readOct octalDigits
        return $ Number $ result
    base10         = many1 digit >>= return . Number . read
    base10Explicit = do
        try $ string "#d"
        base10
    hexadecimal = do
        try $ string "#x"
        hexDigits <- many1 hexDigit
        let [(result, _)] = readHex hexDigits
        return $ Number $ result

spaces :: Parser ()
spaces = skipMany1 space

parseExpr :: Parser LispVal
parseExpr =
    parseAtom <|> parseString <|> parseNumber <|> parseCharacter <|> parseBool

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left  err -> "No match: " ++ show err
    Right val -> "Found value: " <> show val

readLispVal :: String -> Either ParseError LispVal
readLispVal input = parse parseExpr "lisp" input
