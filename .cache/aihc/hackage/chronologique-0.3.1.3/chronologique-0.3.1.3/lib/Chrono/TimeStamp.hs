--
-- Time to manipulate time
--
-- Copyright © 2013-2017 Operational Dynamics Consulting, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Chrono.TimeStamp
(
    TimeStamp(..)
  , getCurrentTimeNanoseconds

  , ISO8601_Precise(..)
) where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Aeson.Encoding (string)
import Data.Maybe
import Data.Int (Int64)
import Data.Hourglass
import qualified Data.Text as T
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M
import Data.Vector.Unboxed
import GHC.Generics
import Time.System

import Chrono.Formats

--
-- | Number of nanoseconds since the Unix epoch.
--
-- The Show instance displays the TimeStamp as seconds with the nanosecond
-- precision expressed as a decimal amount after the interger, ie:
--
-- >>> t <- getCurrentTimeNanoseconds
-- >>> show t
-- 2014-07-31T23:09:35.274387031Z
--
-- However this doesn't change the fact the underlying representation counts
-- nanoseconds since epoch:
--
-- >>> show $ unTimeStamp t
-- 1406848175274387031
--
-- There is a Read instance that is reasonably accommodating.
--
-- >>> read "2014-07-31T13:05:04.942089001Z" :: TimeStamp
-- 2014-07-31T13:05:04.942089001Z
--
-- >>> read "1406811904.942089001" :: TimeStamp
-- 2014-07-31T13:05:04.942089001Z
--
-- >>> read "1406811904" :: TimeStamp
-- 2014-07-31T13:05:04.000000000Z
--
-- In case you're wondering, the valid range of nanoseconds that fits into the
-- underlying Int64 is:
--
-- >>> show $ minBound :: TimeStamp
-- 1677-09-21T00:12:43.145224192Z
--
-- >>> show $ maxBound :: TimeStamp
-- 2262-04-11T23:47:16.854775807Z
--
-- so in a quarter millenium's time, yes, you'll have the Y2262 Problem.
-- Haskell code from today will, of course, still be running, so in the mid
-- Twenty-Third century you will need to replace this implementation with
-- something else.
--
newtype TimeStamp = TimeStamp {
    unTimeStamp :: Int64
} deriving (Eq, Ord, Enum, Num, Real, Integral, Bounded, Generic)

{-
    Hourglass works by sending types in and out of the Timeable and Time
    typeclasses. They're not particularly easy to work with, but they're a
    prerequisite for using timePrint
-}

instance Timeable TimeStamp where
    timeGetElapsedP :: TimeStamp -> ElapsedP
    timeGetElapsedP (TimeStamp ticks) =
      let
        (s,ns) = divMod ticks 1000000000
      in
        ElapsedP (Elapsed (Seconds (s))) (NanoSeconds (ns))

instance Time TimeStamp where
    timeFromElapsedP :: ElapsedP -> TimeStamp
    timeFromElapsedP (ElapsedP (Elapsed (Seconds seconds)) (NanoSeconds nanoseconds)) =
      let
        s  = fromIntegral seconds :: Int64
        ns = fromIntegral nanoseconds
      in
        TimeStamp $! (s * 1000000000) + ns


instance Show TimeStamp where
    show t = timePrint ISO8601_Precise t

instance Read TimeStamp where
    readsPrec _ s = maybeToList $ (,"") <$> parseInput s

parseInput :: String -> Maybe TimeStamp
parseInput = fmap reduceDateTime . parse
  where
    parse :: String -> Maybe DateTime
    parse x =
            timeParse ISO8601_Precise x
        <|> timeParse ISO8601_Seconds x
        <|> timeParse ISO8601_DateAndTime x -- from hourglass
        <|> timeParse ISO8601_Date x        -- from hourglass
        <|> timeParse Posix_Precise x
        <|> timeParse Posix_Micro x
        <|> timeParse Posix_Milli x
        <|> timeParse Posix_Seconds x

    reduceDateTime :: DateTime -> TimeStamp
    reduceDateTime = timeFromElapsedP . timeGetElapsedP

--
-- | Get the current system time, expressed as a 'TimeStamp' (which is to
-- say, number of nanoseconds since the Unix epoch).
--
getCurrentTimeNanoseconds :: IO TimeStamp
getCurrentTimeNanoseconds = do
    p <- timeCurrentP
    return $! convertToTimeStamp p

convertToTimeStamp :: ElapsedP -> TimeStamp
convertToTimeStamp = timeFromElapsedP

{-
    Ideally both the existing G.Vector Vector Int64 and M.MVector MVector
    Int64 instances would be sufficient to support automatically deriving
    those things for TimeStamp. But, *gaboom* somewhere down in GHC.Prim;
    writing this gumpf out manually did the trick, and satisifies the
    superclass requirements to derive an Unbox instance.
-}

newtype instance MVector s TimeStamp = MV_TimeStamp (MVector s Int64)
newtype instance Vector TimeStamp = V_TimeStamp (Vector Int64)

instance G.Vector Vector TimeStamp where
    basicUnsafeFreeze (MV_TimeStamp v) = V_TimeStamp <$> G.basicUnsafeFreeze v
    basicUnsafeThaw (V_TimeStamp v) = MV_TimeStamp <$> G.basicUnsafeThaw v
    basicLength (V_TimeStamp v) = G.basicLength v
    basicUnsafeSlice j k (V_TimeStamp v) = V_TimeStamp $ G.basicUnsafeSlice j k v
    basicUnsafeIndexM (V_TimeStamp v) i = TimeStamp <$> G.basicUnsafeIndexM v i
    {-# INLINE basicUnsafeFreeze #-}
    {-# INLINE basicUnsafeThaw #-}
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicUnsafeIndexM #-}

instance M.MVector MVector TimeStamp where
    basicLength (MV_TimeStamp v) = M.basicLength v
    basicUnsafeSlice j k (MV_TimeStamp v) = MV_TimeStamp $ M.basicUnsafeSlice j k v
    basicOverlaps (MV_TimeStamp a) (MV_TimeStamp b) = M.basicOverlaps a b
    basicUnsafeNew n = MV_TimeStamp <$> M.basicUnsafeNew n
    basicInitialize (MV_TimeStamp v) = M.basicInitialize v
    basicUnsafeRead (MV_TimeStamp v) i = TimeStamp <$> M.basicUnsafeRead v i
    basicUnsafeWrite (MV_TimeStamp v) i (TimeStamp a) = M.basicUnsafeWrite v i a
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicOverlaps #-}
    {-# INLINE basicUnsafeNew #-}
    {-# INLINE basicInitialize #-}
    {-# INLINE basicUnsafeRead #-}
    {-# INLINE basicUnsafeWrite #-}

deriving instance Unbox TimeStamp

{-
    JSON encoding and decoding
-}

instance ToJSON TimeStamp where
    toEncoding = string . timePrint ISO8601_Precise

instance FromJSON TimeStamp where
    parseJSON (String value) =
      let
        str = T.unpack value
        result = parseInput str
      in
        case result of
            Just t  -> pure t
            Nothing -> fail "Unable to parse input as a TimeStamp"

    parseJSON (invalid) = typeMismatch "TimeStamp" invalid

