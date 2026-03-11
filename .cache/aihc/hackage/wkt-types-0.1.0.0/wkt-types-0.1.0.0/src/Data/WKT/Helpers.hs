{-# LANGUAGE OverloadedStrings#-}

module Data.WKT.Helpers (module Data.WKT.Helpers) where
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Attoparsec.Text (Parser, skipSpace)
import Control.Applicative ((<|>))

generateZMString :: Maybe a -> Maybe a -> Text
generateZMString z' m'
    |isJust z' && isJust m' = " ZM "
    |isJust z' = " Z "
    |isJust m' = " M "
    |otherwise = " "

showP :: Show a => a -> String
showP = ("(" <>) . (<> ")") . show

allPairs :: [a] -> [(a,a)]
allPairs [] = []
allPairs [_] = []
allPairs (x1:x2:xs) = (x1,x2) : allPairs (x2:xs)

-- Parsers
zmParser :: Parser (Text, Text)
zmParser = do
    skipSpace
    zFlag <- "Z" <|> "z" <|> ""
    mFlag <- "M" <|> "m" <|> ""
    skipSpace
    return (zFlag, mFlag)