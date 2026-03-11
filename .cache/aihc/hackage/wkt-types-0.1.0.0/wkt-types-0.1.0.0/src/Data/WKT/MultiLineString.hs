{-# LANGUAGE OverloadedStrings#-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}

module Data.WKT.MultiLineString (module Data.WKT.MultiLineString) where

import Data.WKT.Classes
import Data.WKT.LineString
import Data.List (intercalate)
import Data.WKT.Helpers (showP, generateZMString, zmParser)
import Data.WKT.Point
import Data.Text (pack, Text)
import Data.Attoparsec.Text (Parser, skipSpace, asciiCI, parseOnly)
import Control.Applicative ((<|>))

newtype MultiLineString a where
  MultiLineString :: [LineString a] -> MultiLineString a
  deriving (Functor, Eq)

instance Show a => Show (MultiLineString a) where
    show (MultiLineString lineStrings) = intercalate ", " lines'
        where
            lines' = showP <$> lineStrings

instance Show a => ToWKT (MultiLineString a) where
    toWKT multiLineString = "MultiLineString" <> zmString <> "(" <> pack (show multiLineString) <> ")"
        where
            (MultiLineString lineStrings) = multiLineString
            (LineString firstLine) = head lineStrings
            first = head firstLine
            z' = z first
            m' = m first

            zmString = generateZMString z' m'

instance Valid (MultiLineString a) where
    isValid (MultiLineString lines') = all isValid lines'

instance FromWKT MultiLineString where
    fromWKT = either (error . show) id . parseOnly wktParser

instance ParseableFromWKT MultiLineString where
    wktParser = do
        skipSpace
        _ <- asciiCI "MULTILINESTRING"
        (zFlag, mFlag) <- zmParser
        _ <- "("
        parseMultiLineString zFlag mFlag
        
parseMultiLineString :: Text -> Text -> Parser (MultiLineString Double)
parseMultiLineString zFlag mFlag = MultiLineString <$> lineStringParser zFlag mFlag
            where
                lineStringParser zFlag' mFlag' = do
                    skipSpace
                    _ <- "("
                    newLineString <- parseLineString zFlag' mFlag'
                    skipSpace                    
                    closing <- ")" <|> ""
                    if closing /= "" then
                        return [newLineString]
                    else
                        (newLineString :) <$> (skipSpace *> "," *> lineStringParser zFlag' mFlag')
