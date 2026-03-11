{-# LANGUAGE FlexibleInstances, OverlappingInstances, UndecidableInstances, Rank2Types, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
--------------------------------------------------------------------
-- | Generic Instance for ToSElem using standard Data.Generic libraries.
--------------------------------------------------------------------}

module Text.StringTemplate.GenericStandard() where
import qualified Data.Map as M
import Text.StringTemplate.Classes
import Text.StringTemplate.Instances()
import Data.Generics.Basics
import Data.Generics.Aliases
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
-- import qualified System.Time as OldTime
-- import System.Locale
-- import Data.Time
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

gToSElem :: forall a b.(Data a, Stringable b) => a -> SElem b
gToSElem = (\x ->
            case (map stripInitUnder (constrFields . toConstr $ x)) of
              [] -> LI (STR (showConstr (toConstr x)) :
                        gmapQ gToSElem x)
              fs -> SM (M.fromList (zip fs (gmapQ gToSElem x)))
           )
           `ext1Q` (\t -> case t of (Just x) -> gToSElem x; _ -> SNull)
           `ext1Q` (SM . fmap gToSElem)
           `ext1Q` (LI . map gToSElem)
           -- `extQ` (toSElem :: OldTime.CalendarTime -> SElem b)
           -- `extQ` (toSElem :: OldTime.TimeDiff -> SElem b)
           -- `extQ` (toSElem :: TimeOfDay -> SElem b)
           -- `extQ` (toSElem :: UTCTime -> SElem b)
           -- `extQ` (toSElem :: TimeZone -> SElem b)
           -- `extQ` (toSElem :: ZonedTime -> SElem b)
           -- `extQ` (toSElem :: Day -> SElem b)
           -- `extQ` (toSElem :: LocalTime -> SElem b)
           `extQ` (toSElem :: Char -> SElem b)
           `extQ` (toSElem :: LB.ByteString -> SElem b)
           `extQ` (toSElem :: B.ByteString -> SElem b)
           `extQ` (toSElem :: LT.Text -> SElem b)
           `extQ` (toSElem :: T.Text -> SElem b)
           `extQ` (toSElem :: Bool -> SElem b)
           `extQ` (toSElem :: Float -> SElem b)
           `extQ` (toSElem :: Double -> SElem b)
           `extQ` (toSElem :: Int -> SElem b)
           `extQ` (toSElem :: Integer -> SElem b)
           `extQ` (toSElem :: String -> SElem b)


instance Data a => ToSElem a
    where toSElem = gToSElem

stripInitUnder :: String -> String
stripInitUnder ('_':s) = stripInitUnder s
stripInitUnder s       = s
