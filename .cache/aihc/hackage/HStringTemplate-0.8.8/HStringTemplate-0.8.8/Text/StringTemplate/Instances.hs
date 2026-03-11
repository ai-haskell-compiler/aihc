{-# LANGUAGE CPP, FlexibleInstances, OverlappingInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

module Text.StringTemplate.Instances() where
import Text.StringTemplate.Classes

import qualified Data.Map as M
import Numeric
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Ratio
import Data.Array
import Data.Maybe
import qualified Data.Foldable as F
import Data.Time
import Data.Void
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

#if !MIN_VERSION_time(1,5,0)
import System.Locale (defaultTimeLocale)
#endif


{--------------------------------------------------------------------
  Additional instances for items that may be set as StringTemplate
  attributes. The code should provide examples of how to proceed.
--------------------------------------------------------------------}

--Basics
instance ToSElem () where
    toSElem _ = STR ""

instance ToSElem Void where
    toSElem = absurd

instance ToSElem Char where
    toSElem = STR . (:[])
    toSElemList = STR

instance ToSElem LB.ByteString where
    toSElem = BS

instance ToSElem B.ByteString where
    toSElem = BS . LB.fromChunks . (:[])

instance ToSElem LT.Text where
    toSElem = TXT

instance ToSElem T.Text where
    toSElem = TXT . LT.fromStrict

instance ToSElem Bool where
    toSElem True = STR ""
    toSElem _ = SNull

instance (ToSElem a) => ToSElem (Maybe a) where
    toSElem (Just x) = toSElem x
    toSElem _ = SNull

instance (ToSElem a) => ToSElem (M.Map String a) where
    toSElem = SM . fmap toSElem

instance (ToSElem a) => ToSElem [a] where
    toSElem = toSElemList

instance (ToSElem a, Ix i) => ToSElem (Array i a) where
   toSElem = toSElem . elems

instance (ToSElem a, F.Foldable t) => ToSElem (t a) where
    toSElem = toSElemList . F.toList

--Numbers
instance StringTemplateShows Float where
    stringTemplateShow = flip showFloat ""
    stringTemplateFormattedShow = flip flip [] . showGFloat . fmap fst . listToMaybe . reads
instance ToSElem Float where
    toSElem = stShowsToSE

instance StringTemplateShows Double where
    stringTemplateShow = flip showFloat ""
    stringTemplateFormattedShow = flip flip [] . showGFloat . fmap fst . listToMaybe . reads
instance ToSElem Double where
    toSElem = stShowsToSE

instance ToSElem Int where
    toSElem = STR . show

instance ToSElem Integer where
    toSElem = STR . show

instance (Integral a, Show a) => ToSElem (Ratio a) where
    toSElem = STR . show

--Dates and Times

instance StringTemplateShows Day where
    stringTemplateShow = show
    stringTemplateFormattedShow = formatTime defaultTimeLocale
instance ToSElem Day where
    toSElem = stShowsToSE

instance StringTemplateShows LocalTime where
    stringTemplateShow = show
    stringTemplateFormattedShow = formatTime defaultTimeLocale
instance ToSElem LocalTime where
    toSElem = stShowsToSE

instance StringTemplateShows TimeOfDay where
    stringTemplateShow = show
    stringTemplateFormattedShow = formatTime defaultTimeLocale
instance ToSElem TimeOfDay where
    toSElem = stShowsToSE

instance StringTemplateShows UTCTime where
    stringTemplateShow = show
    stringTemplateFormattedShow = formatTime defaultTimeLocale
instance ToSElem UTCTime where
    toSElem = stShowsToSE

instance StringTemplateShows TimeZone where
    stringTemplateShow = show
    stringTemplateFormattedShow = formatTime defaultTimeLocale
instance ToSElem TimeZone where
    toSElem = stShowsToSE

instance StringTemplateShows ZonedTime where
    stringTemplateShow = show
    stringTemplateFormattedShow = formatTime defaultTimeLocale
instance ToSElem ZonedTime where
    toSElem = stShowsToSE

t2map :: [SElem a] -> SElem a
t2map = SM . M.fromList . zip (map show [(0::Int)..])

instance (ToSElem a, ToSElem b) => ToSElem (a, b) where
   toSElem (a,b) = t2map [toSElem a, toSElem b]
instance (ToSElem a, ToSElem b, ToSElem c) => ToSElem (a, b, c) where
   toSElem (a,b,c) = t2map [toSElem a, toSElem b, toSElem c]
instance (ToSElem a, ToSElem b, ToSElem c, ToSElem d) => ToSElem (a, b, c, d) where
   toSElem (a,b,c,d) = t2map [toSElem a, toSElem b, toSElem c, toSElem d]
instance (ToSElem a, ToSElem b, ToSElem c, ToSElem d, ToSElem e) => ToSElem (a, b, c, d, e) where
   toSElem (a,b,c,d,e) = t2map [toSElem a, toSElem b, toSElem c, toSElem d, toSElem e]
instance (ToSElem a, ToSElem b, ToSElem c, ToSElem d, ToSElem e, ToSElem f) => ToSElem (a, b, c, d, e, f) where
   toSElem (a,b,c,d,e, f) = t2map [toSElem a, toSElem b, toSElem c, toSElem d, toSElem e, toSElem f]
instance (ToSElem a, ToSElem b, ToSElem c, ToSElem d, ToSElem e, ToSElem f, ToSElem g) => ToSElem (a, b, c, d, e, f, g) where
   toSElem (a,b,c,d,e,f,g) = t2map [toSElem a, toSElem b, toSElem c, toSElem d, toSElem e, toSElem f, toSElem g]
instance (ToSElem a, ToSElem b, ToSElem c, ToSElem d, ToSElem e, ToSElem f, ToSElem g, ToSElem h) => ToSElem (a, b, c, d, e, f, g, h) where
   toSElem (a,b,c,d,e,f,g,h) = t2map [toSElem a, toSElem b, toSElem c, toSElem d, toSElem e, toSElem f, toSElem g, toSElem h]
instance (ToSElem a, ToSElem b, ToSElem c, ToSElem d, ToSElem e, ToSElem f, ToSElem g, ToSElem h, ToSElem i) => ToSElem (a, b, c, d, e, f, g, h, i) where
   toSElem (a,b,c,d,e,f,g,h,i) = t2map [toSElem a, toSElem b, toSElem c, toSElem d, toSElem e, toSElem f, toSElem g, toSElem h, toSElem i]
instance (ToSElem a, ToSElem b, ToSElem c, ToSElem d, ToSElem e, ToSElem f, ToSElem g, ToSElem h, ToSElem i, ToSElem j) => ToSElem (a, b, c, d, e, f, g, h, i, j) where
   toSElem (a,b,c,d,e,f,g,h,i,j) = t2map [toSElem a, toSElem b, toSElem c, toSElem d, toSElem e, toSElem f, toSElem g, toSElem h, toSElem i, toSElem j]
