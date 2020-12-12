{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards           #-}
module Lib
    ( LispError(..)
    , LispVal(..)
    , Env
    , eval
    , evalAndPrint
    , extractValue
    , readExpr
    , readLispVal
    , runRepl
    , runOne
    , trapError
    , evalString
    , primitiveBindings
    )
where

import           Text.ParserCombinators.Parsec
                                         hiding ( spaces )
import           Control.Monad                  ( liftM2 )
import           Control.Monad.Except           ( MonadError(..)
                                                , throwError
                                                , catchError
                                                , ExceptT
                                                , runExceptT
                                                , liftIO
                                                )
import           Numeric                        ( readOct
                                                , readHex
                                                , readInt
                                                )
import qualified Data.Char                     as Char
import           System.IO                      ( stdout
                                                , hFlush
                                                )
import           Data.IORef
import           Data.Functor                   ( (<&>) )
import           Data.Maybe                     ( isNothing
                                                , isJust
                                                )

-- | Lisp's primitive datatypes
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func { params :: [String]
                    , vararg :: Maybe String
                    , body :: [LispVal]
                    , closure :: Env
                    }

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
    show (PrimitiveFunc _) = "<primitive>"
    show Func {..}         = mconcat
        [ "(lambda ("
        , unwords $ map show params
        , case vararg of
            Nothing  -> ""
            Just arg -> " . " <> arg
        , ") ...)"
        ]

-- | Errors that might happen when parsing and evaluating an expression
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

-- | Shorthand type for error results
type ThrowsError = Either LispError

-- | Unpacker 'unpacks' a lisp value to Haskell value or throws an error
data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

-- | Represents program's environment (defined variables and functions and so
-- on)
type Env = IORef [(String, IORef LispVal)]

type IOThrowsError = ExceptT LispError IO

-- Parsing --

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
    base10         = Number . read <$> many1 digit
    base10Explicit = do
        try $ string "#d"
        base10
    hexadecimal = do
        try $ string "#x"
        hexDigits <- many1 hexDigit
        let [(result, _)] = readHex hexDigits
        return $ Number result

parseList :: Parser LispVal
parseList = List <$> parseExpr `sepBy` spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- parseExpr `endBy` spaces
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

-- | Trap error and turn it into it's string value
trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: Either a b -> b
extractValue (Right val) = val
-- (omitted) Left represents programmer error as this should be used only after
-- when argument is known to be Right

-- | Parse lisp expression
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

-- | Read raw lisp expression from string
readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left  err -> throwError $ Parser err
    Right val -> return val

-- | Read a lisp value from string
readLispVal :: String -> Either ParseError LispVal
readLispVal = parse parseExpr "lisp"

-- | Evaluate a lisp expression
eval :: Env -> LispVal -> IOThrowsError LispVal
eval _   val@(String    _ ) = return val
eval _   val@(Number    _ ) = return val
eval _   val@(Bool      _ ) = return val
eval _   val@(Character _ ) = return val
eval env (    Atom      id) = getVar env id
eval env (List [Atom "if", predicate, consequence, alternative]) = do
    result <- eval env predicate
    case result of
        Bool False -> eval env alternative
        Bool True  -> eval env consequence
        badArg     -> throwError $ TypeMismatch "boolean" badArg
eval env (List (Atom "cond" : clauses)) = cond env clauses
eval _   (List [Atom "quote", val]    ) = return val
-- Set variable value
eval env (List [Atom "set!", Atom var, form]) =
    eval env form >>= setVar env var
-- Defining a variable
eval env (List [Atom "define", Atom var, form]) =
    eval env form >>= defineVar env var
-- Defining functions (named)
eval env (List (Atom "define" : List (Atom var : params) : body)) =
    makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body))
    = makeVarArgs varargs env params body >>= defineVar env var
-- Defining functions (anonymous)
eval env (List (Atom "lambda" : List params : body)) =
    makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
    makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
    makeVarArgs varargs env [] body
-- Function application
eval env (List (function : args)) = do
    func    <- eval env function -- function is evaluated
    argVals <- mapM (eval env) args -- each argument is evaluated
    apply func argVals -- then arguments are applied to function
-- Error
eval _ badForm =
    throwError $ BadSpecialForm "Unrecognized special form" badForm

-- | Apply arguments to function
apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply Func {..} args = if numberOf params /= numberOf args && isNothing vararg
    then throwError $ NumArgs (numberOf params) args
    else
        liftIO (bindVars closure $ zip params args)
        >>= bindVarArgs vararg
        >>= evalBody
  where
    -- arguments that haven't been bound
    remainingArgs = drop (length params) args
    numberOf      = toInteger . length
    evalBody env = last <$> mapM (eval env) body
    bindVarArgs arg env = case arg of
        Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
        Nothing      -> return env

makeFunc
    :: (Monad m, Show a) => Maybe String -> Env -> [a] -> [LispVal] -> m LispVal
makeFunc varargs env params body = return $ Func { body    = body
                                                 , closure = env
                                                 , vararg  = varargs
                                                 , params  = map show params
                                                 }

makeNormalFunc :: (Monad m, Show a) => Env -> [a] -> [LispVal] -> m LispVal
makeNormalFunc = makeFunc Nothing

makeVarArgs :: (Monad m, Show a) => a -> Env -> [a] -> [LispVal] -> m LispVal
makeVarArgs = makeFunc . Just . show

-- Primitive functions --

-- | Some primitive operations of lisp
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
    , ("string>=?", strBoolBinOp (>=))
    , ("eqv?"     , eqv)
    , ("eq?"      , eqv)
    , ( "equal?"
      , equal
      )

    -- Logic operators
    , ("&&", boolBoolBinOp (&&))
    , ( "||"
      , boolBoolBinOp (||)
      )

    -- List primitives
    , ("car" , car)
    , ("cdr" , cdr)
    , ("cons", cons)
    ]

-- | Helper function for making primitive functions that are binary numeric
-- operators
numericBinOp
    :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinOp _  []            = throwError $ NumArgs 2 []
numericBinOp _  singleArg@[_] = throwError $ NumArgs 2 singleArg
numericBinOp op args          = Number . foldl1 op <$> mapM unpackNum args

-- | Helper function for making unary lisp primitives
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
unpackNum s@(String n) =
    let parsed = reads n
    in  if null parsed
            then throwError $ TypeMismatch "number" s
            else return $ fst $ head parsed
unpackNum (List [x]) = unpackNum x
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (    Bool   b) = return b
unpackBool arg@(String s) = case s of
    "#t" -> return True
    "#f" -> return False
    _    -> throwError $ TypeMismatch "boolean" arg
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr notStr     = throwError $ TypeMismatch "string" notStr

car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)        ] = return x
car [DottedList (x : _) _] = return x
car [badArg              ] = throwError $ TypeMismatch "pair" badArg
car badArgList             = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)        ] = return $ List xs
cdr [DottedList [_     ] x] = return $ DottedList [] x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg               ] = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x , List []            ] = return $ List [x]
cons [x , List xs            ] = return $ List $ x : xs
cons [x , DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2                 ] = return $ DottedList [x1] x2
cons badArgList                = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool      a, Bool b     ] = return $ Bool $ a == b
eqv [Number    a, Number b   ] = return $ Bool $ a == b
eqv [String    a, String b   ] = return $ Bool $ a == b
eqv [Character a, Character b] = return $ Bool $ a == b
eqv [Atom      a, Atom b     ] = return $ Bool $ a == b
eqv [DottedList xs x, DottedList ys y] =
    eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [List xs, List ys] = return $ Bool $ (length xs == length ys) && all
    (\(Right (Bool b)) -> b)
    (zipWith (\x y -> eqv [x, y]) xs ys)
eqv [_, _]     = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

equal :: [LispVal] -> ThrowsError LispVal
equal [List [], List []] = return $ Bool True
equal [List xs, List ys] = do
    let unLispBool (Bool b) = b
        bools = zipWith (\x y -> unLispBool <$> equal [x, y]) xs ys
    Bool <$> foldl1 (liftM2 (&&)) bools
equal [DottedList xs x, DottedList ys y] = Bool <$> liftM2
    (&&)
    (unLispBool <$> equal (List <$> [xs, ys]))
    (unLispBool <$> equal [x, y])
    where unLispBool (Bool b) = b
equal args@[a, b] = do
    primitiveEquals <- or <$> mapM
        (unpackEquals a b)
        [AnyUnpacker unpackBool, AnyUnpacker unpackNum, AnyUnpacker unpackStr]
    eqvEquals <- eqv args
    return $ Bool (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgs = throwError $ NumArgs 2 badArgs

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals x y (AnyUnpacker unpacker) =
    liftM2 (==) (unpacker x) (unpacker y) `catchError` const (return False)

cond :: Env -> [LispVal] -> IOThrowsError LispVal
cond env clauses = if null clauses
    then throwError $ BadSpecialForm "no true condition found" (List clauses)
    else case head clauses of
        List [Atom "else", expr] -> eval env expr
        List [test       , expr] -> do
            result <- eval env test
            case result of
                Bool b -> if b then eval env expr else cond env $ tail clauses
                badArg -> throwError $ TypeMismatch "boolean" badArg

-- Read-evaluate-print-loop and IO handling --

-- | Print argument and flush so it's guaranteed that the output shows up
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

-- | Read user input. Argument is prompt text that shows before user input.
readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

-- | Parse and evaluate string as an expression. Errors are trapped to show them
-- without exiting REPL.
evalString :: Env -> String -> IO String
evalString env expr =
    runIOThrows $ fmap show $ liftThrows (readExpr expr) >>= eval env


-- | Evaluate expression (from string) and print it's result
evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

-- | Repeat action until predicate is satisfied with prompt's result
until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ predicate prompt action = do
    result <- prompt
    if predicate result
        then return () -- exit loop
        else action result >> until_ predicate prompt action

-- | Start REPL
runRepl :: IO ()
runRepl = do
    env <- primitiveBindings
    until_ (== "quit") (readPrompt "Lisp>>>") (evalAndPrint env)

-- | Evaluate and print one expression
runOne :: String -> IO ()
runOne expr = do
    env <- primitiveBindings
    evalAndPrint env expr

-- | Initialize empty environment
nullEnv :: IO Env
nullEnv = newIORef []

-- | Initialize environment with primitive functions bound
primitiveBindings :: IO Env
primitiveBindings = nullEnv
    >>= flip bindVars (map makePrimitiveFunc primitives)
    where makePrimitiveFunc = fmap PrimitiveFunc

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left  err) = throwError err
liftThrows (Right val) = return val

-- | Run action and return it's result, whether an error or a proper result
runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) <&> extractValue

-- | Check that variable is defined in environment
isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef <&> isJust . lookup var

-- | Get variable from environment
getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Getting an unbound variable" var)
          (liftIO . readIORef)
          (lookup var env)

-- | Set variable's value (if defined)
setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
    env <- liftIO $ readIORef envRef
    maybe throwUnboundVar -- if value is not defined
          writeValue      -- if value is defined
          (lookup var env)
    return value
  where
    throwUnboundVar = throwError $ UnboundVar "Setting an unbound variable" var
    writeValue      = liftIO . (`writeIORef` value)

-- | Define a new variable or set it's value if already defined
defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
    isAlreadyDefined <- liftIO $ isBound envRef var
    if isAlreadyDefined
        then setVar envRef var value >> return value
        else liftIO $ do
            valueRef <- newIORef value
            env      <- readIORef envRef
            writeIORef envRef $ (var, valueRef) : env
            return value

-- | Define multiple variables at once
bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where
    extendEnv bindings env = fmap (++ env) (mapM addBinding bindings)
    addBinding (var, value) = do
        ref <- newIORef value
        return (var, ref)
