module Lib
    ( readLispVal
    , readExpr
    , LispVal(..)
    , eval
    )
where

import           Text.ParserCombinators.Parsec
                                         hiding ( spaces )
import           Control.Monad                  ( liftM )
import           Numeric                        ( readOct
                                                , readHex
                                                , readDec
                                                , readInt
                                                )
import qualified Data.Char                     as Char
import           Text.Read                      ( readMaybe )
import           Data.List                      ( intercalate )

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char

instance Show LispVal where
    show (String    content) = mconcat ["\"", content, "\""]
    show (Atom      name   ) = name
    show (Number    n      ) = show n
    show (Bool      True   ) = "#t"
    show (Bool      False  ) = "#f"
    show (List      xs     ) = "(" <> unwordsList xs <> ")"
    show (Character c      ) = "#\\" <> [c]
    show (DottedList head tail) =
        "(" <> unwordsList head <> " . " <> show tail <> ")"

unwordsList :: [LispVal] -> String
unwordsList = intercalate " " . fmap show

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

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
    char '`'
    x <- parseExpr
    return $ List [Atom "quasiquote", x]

parseUnQuote :: Parser LispVal
parseUnQuote = do
    char ','
    x <- parseExpr
    return $ List [Atom "unquote", x]

parseUnQuoteSplicing :: Parser LispVal
parseUnQuoteSplicing = do
    char ','
    char '@'
    x <- parseExpr
    return $ List [Atom "unquote-splicing", x]

spaces :: Parser ()
spaces = skipMany1 space

parseExpr :: Parser LispVal
parseExpr =
    parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseCharacter
        <|> parseQuoted
        <|> parseBool
        <|> parseQuasiQuoted
        <|> parseUnQuote
        <|> parseUnQuoteSplicing
        <|> do
                char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left  err -> String $ "No match: " <> show err
    Right val -> val

readLispVal :: String -> Either ParseError LispVal
readLispVal input = parse parseExpr "lisp" input

eval :: LispVal -> LispVal
eval val@(String    _                  ) = val
eval val@(Number    _                  ) = val
eval val@(Bool      _                  ) = val
eval val@(Character _                  ) = val
eval (    List      [Atom "quote", val]) = val
eval (    List      (Atom func : args) ) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives =
    [
    -- Integer arithmetic
      ("+"       , numericBinOp (+))
    , ("-"       , numericBinOp (-))
    , ("*"       , numericBinOp (*))
    , ("/"       , numericBinOp div)
    , ("mod"     , numericBinOp mod)
    , ("quotient", numericBinOp quot)
    , ( "remainder"
      , numericBinOp rem
      )

    -- Type testing
    , ("number?" , unaryOp isNumber)
    , ("string?" , unaryOp isString)
    , ("list?"   , unaryOp isList)
    , ("boolean?", unaryOp isBoolean)
    , ("char?"   , unaryOp isChar)
    , ("symbol?" , unaryOp isSymbol)
    ]
  where
    numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
    numericBinOp op params = Number $ foldl1 op $ map unpackNum params

    unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
    unaryOp f [x] = f x

isNumber :: LispVal -> LispVal
isNumber (Number _) = Bool True
isNumber _          = Bool False

isString :: LispVal -> LispVal
isString (String _) = Bool True
isString _          = Bool False

isList :: LispVal -> LispVal
isList (List _) = Bool True
isList _        = Bool False

isBoolean :: LispVal -> LispVal
isBoolean (Bool _) = Bool True
isBoolean _        = Bool False

isChar :: LispVal -> LispVal
isChar (Character _) = Bool True
isChar _             = Bool False

isSymbol :: LispVal -> LispVal
isSymbol (Atom _) = Bool True -- are other values than atoms symbols?
isSymbol _        = Bool False

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
-- -- conversion from other types
-- unpackNum (String s) = case readMaybe s of
--     Just x  -> x
--     Nothing -> 0
-- unpackNum (List [x]) = unpackNum x
unpackNum _          = 0
