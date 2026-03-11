{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings#-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}


module Data.WKT.Point (module Data.WKT.Point) where
import Data.WKT.Classes
import Data.Maybe (isJust)
import Data.Attoparsec.Text
    ( asciiCI,
      skipSpace,
      double,
      parseOnly, Parser )
import Data.WKT.Helpers (zmParser)

import Data.Text (pack, Text)

data Point a = Point{
    x :: a,
    y :: a,
    z :: Maybe a,
    m :: Maybe a
}
    deriving (Eq, Ord, Functor)
instance Show a => Show (Point a) where
    show (Point {x,y,z,m}) = pointValue
        where
            -- TODO: check if it's okay to have . or if I must always e.
            x' = show x
            y' = show y
            z' = maybe "" ((" " <>). show) z
            m' = maybe "" ((" " <>). show) m
            pointValue = x' <> " " <> y' <> z' <> m'

instance Show a => ToWKT (Point a) where
    toWKT point = "Point" <> zmString <> "(" <> pack (show point) <> ")"
        where
            Point{z,m} = point
            zmString
                |isJust z && isJust m = " ZM "
                |isJust z = " Z "
                |isJust m = " M "
                |otherwise = " "

-- Just here for completeness.
instance Valid (Point a) where
    isValid (Point {}) = True

instance FromWKT Point where
    fromWKT = either (error . show) id . parseOnly wktParser
instance ParseableFromWKT Point where
    wktParser = do
        skipSpace
        _ <- asciiCI "POINT"
        (zFlag, mFlag) <- zmParser
        _ <- "("
        parsePoint zFlag mFlag

parsePoint :: Text -> Text -> Parser (Point Double)
parsePoint zFlag mFlag = do
    skipSpace
    x' <- double
    skipSpace
    y' <- double
    skipSpace
    z' <-
        if zFlag /= "" then do
            z' <- double
            skipSpace
            return $ Just z'
        else
            return Nothing
    m' <-
        if mFlag /= "" then do
            m' <- double
            skipSpace
            return $ Just m'
        else
            return Nothing
    return Point{x = x',y = y', z = z', m = m'}

-- Helpers
pointDimension :: Point a -> Int
pointDimension (Point _ _ z' m')
    | isJust m' = 4
    | isJust z' = 3
    | otherwise = 2