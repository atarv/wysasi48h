module Lib
    ( readLispVal
    , readExpr
    , LispVal(..)
    , LispError(..)
    , eval
    , extractValue
    , trapError
    )
where

import           Text.ParserCombinators.Parsec
                                         hiding ( spaces )
import           Control.Monad                  ( liftM2 )
import           Control.Monad.Except           ( MonadError()
                                                , throwError
                                                , catchError
                                                )
import           Numeric                        ( readOct
                                                , readHex
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

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where
    show (NumArgs expected found) =
        "Expected "
            <> show expected
            <> " args, found values: "
            <> unwordsList found
    show (TypeMismatch expected found) =
        "Invalid type: expected " <> expected <> ", found " <> show found
    show (Parser  parseErr              ) = "Parse error at " <> show parseErr
    show (Default message               ) = "Error: " <> message
    show (UnboundVar     message varname) = message <> ": " <> varname
    show (BadSpecialForm message form   ) = message <> ": " <> show form
    show (NotFunction    message func   ) = message <> ": " <> show func

type ThrowsError = Either LispError

unwordsList :: [LispVal] -> String
unwordsList = unwords . fmap show

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
    (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

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
        return $ Number result
    base10         = Number . read <$>  many1 digit
    base10Explicit = do
        try $ string "#d"
        base10
    hexadecimal = do
        try $ string "#x"
        hexDigits <- many1 hexDigit
        let [(result, _)] = readHex hexDigits
        return $ Number result

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

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

trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: Either a b -> b
extractValue (Right val) = val
-- (omitted) Left represents programmer error as this should be used only after
-- `catchError`

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

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left  err -> throwError $ Parser err
    Right val -> return val

readLispVal :: String -> Either ParseError LispVal
readLispVal = parse parseExpr "lisp"

eval :: LispVal -> ThrowsError LispVal
eval val@(String    _                  ) = return val
eval val@(Number    _                  ) = return val
eval val@(Bool      _                  ) = return val
eval val@(Character _                  ) = return val
eval (    List      [Atom "quote", val]) = return val
eval (    List      (Atom func : args) ) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe
    (throwError $ NotFunction "Unrecognized primitive function args" func)
    ($ args)
    (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
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
    , ( "symbol?"
      , unaryOp isSymbol
      )

    -- Symbol handling
    , ("symbol->string", unaryOp symbolToString)
    , ( "string->symbol"
      , unaryOp stringToSymbol
      )

    -- Comparison operators
    , ("="        , numBoolBinOp (==))
    , ("<"        , numBoolBinOp (<))
    , (">"        , numBoolBinOp (>))
    , ("/="       , numBoolBinOp (/=))
    , (">="       , numBoolBinOp (>=))
    , ("<="       , numBoolBinOp (<=))
    , ("string=?" , strBoolBinOp (==))
    , ("string<?" , strBoolBinOp (<))
    , ("string>?" , strBoolBinOp (>))
    , ("string<=?", strBoolBinOp (<=))
    , ( "string>=?"
      , strBoolBinOp (>=)
      )

    -- Logic operators
    , ("&&", boolBoolBinOp (&&))
    , ("||", boolBoolBinOp (||))
    ]

numericBinOp
    :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinOp _ [] = throwError $ NumArgs 2 []
numericBinOp _ singleArg@[_] = throwError $ NumArgs 2 singleArg
numericBinOp op args =  Number . foldl1 op <$> mapM unpackNum args

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp op [x]  = return $ op x
unaryOp _  args = throwError $ NumArgs 1 args

-- Binary operation resulting in boolean value after unpacking values
boolBinOp
    :: (LispVal -> ThrowsError a)
    -> (a -> a -> Bool)
    -> [LispVal]
    -> ThrowsError LispVal
boolBinOp unpacker op [left, right] =
    -- Unpack arguments and apply supplied operation on them
    Bool <$> liftM2 op (unpacker left) (unpacker right)
boolBinOp _ _ args = throwError $ NumArgs 2 args

boolBoolBinOp :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinOp = boolBinOp unpackBool

strBoolBinOp :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinOp = boolBinOp unpackStr

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

symbolToString :: LispVal -> LispVal
symbolToString (Atom s) = String s -- TODO: what is the standard casing?
symbolToString _        = Bool False

stringToSymbol :: LispVal -> LispVal
stringToSymbol (String s) = Atom s
stringToSymbol _          = Bool False

numBoolBinOp :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinOp = boolBinOp unpackNum

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum string@(String n) =
    let parsed = reads n
    in  if null parsed
            then throwError $ TypeMismatch "number" string
            else return $ fst $ head parsed
unpackNum (List [x]) = unpackNum x
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr notStr     = throwError $ TypeMismatch "string" notStr
