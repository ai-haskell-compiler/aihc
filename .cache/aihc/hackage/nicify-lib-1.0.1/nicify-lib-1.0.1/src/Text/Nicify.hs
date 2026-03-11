{-# LANGUAGE Haskell2010
    , DeriveDataTypeable
 #-}
{-# OPTIONS
    -Wall
    -fno-warn-unused-do-bind
    -fno-warn-name-shadowing
 #-}

module Text.Nicify (
    nicify,
    X (..),
    parseX,
    printX,
    printX'
  ) where


import Data.Data
import Data.Functor.Identity
import Text.Parsec


data X = XString String
       | XCurly [X]
       | XBrackets [X]
       | XAnything String
       | XSep
    deriving (Show, Read, Eq, Data, Typeable)


nicify :: String -> String
nicify = printX . parseX

type Parser a = ParsecT String () Identity a

parseX :: String -> [X]
parseX = either (const []) id . runParser (many rData) () "-"

printX :: [X] -> String
printX = printX' ""

printX' :: String -> [X] -> String
printX' indent = (indent ++) . concatMap (prettify indent)

prettify :: String -> X -> String
prettify indent x = case x of
    XAnything s -> s
    XString s -> '\"' : foldr stringify "\"" s
    XSep -> ",\n" ++ indent
    XCurly [] -> "{}"
    XBrackets [] -> "[]"
    XCurly x -> "{\n" ++ indent' ++ foldr (++) ('\n' : indent ++ "}") (map (prettify indent') x)
    XBrackets x -> "[\n" ++ indent' ++ foldr (++) ('\n' : indent ++ "]") (map (prettify indent') x)
  where
    indent' = indent ++ "    "
    stringify c cs = case c of
        '\"' -> "\\\"" ++ cs
        '\\' -> "\\\\" ++ cs
        char -> char : cs

rData, rSep, rAnything, rCurly, rBrackets, rString :: Parser X

rData = rString <|> rCurly <|> rBrackets <|> rSep <|> rAnything

rSep = do
    char ','
    spaces
    return XSep

rAnything = do
    str <- many1 (noneOf "\"{}[],")
    return $ XAnything str

rCurly = do
    char '{'
    str <- many rData
    char '}'
    return $ XCurly str

rBrackets = do
    char '['
    str <- many rData
    char ']'
    return $ XBrackets str

rString = do
    char '\"'
    str <- many (noneOf "\\\"" <|> escape)
    char '\"'
    return $ XString str

escape :: Parser Char
escape = do
    char '\\'
    c <- anyChar
    case c of
        'n' -> return '\n'
        'r' -> return '\r'
        't' -> return '\t'
        any -> return any

