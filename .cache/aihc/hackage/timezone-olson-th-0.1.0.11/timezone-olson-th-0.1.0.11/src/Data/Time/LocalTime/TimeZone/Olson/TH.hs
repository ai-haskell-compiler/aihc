{-# LANGUAGE TemplateHaskell #-}
-- |
-- Load TimeZoneSeries from an Olson file at compile time.
--  
-- For example:
--
-- > myTimeZoneSeries :: TimeZoneSeries
-- > myTimeZoneSeries = $(loadTZFile "/usr/share/zoneinfo/Europe/Stockholm")

module Data.Time.LocalTime.TimeZone.Olson.TH 
  (
    loadTZFile
  ) where       

import Data.Ratio                          (numerator,
                                            denominator)
import Data.Time.LocalTime.TimeZone.Olson  (getTimeZoneSeriesFromOlsonFile)
import Data.Time.LocalTime.TimeZone.Series (TimeZoneSeries(..))
import Data.Time.LocalTime                 (TimeZone(..))
import Data.Time                           (UTCTime(..),
                                            Day(..),
                                            DiffTime,
                                            secondsToDiffTime)
import Language.Haskell.TH                 (Q,
                                            runIO,
                                            Exp(..),
                                            mkName,
                                            Lit(..),
                                            litE,
                                            integerL)

-- | Make a splice of a TimeZoneSeries from an Olson file.
loadTZFile :: FilePath -- ^ Path to the Olson file.
           -> Q Exp
loadTZFile zf = 
  mkTZS =<< (runIO $ getTimeZoneSeriesFromOlsonFile zf)

-- | Make a splice of a TimeZoneSeries.    
mkTZS :: TimeZoneSeries -- ^ The TimeZoneSeries to be spliced
      -> Q Exp    
mkTZS (TimeZoneSeries def tlist) = [| TimeZoneSeries $(litTimeZone def) $(mkList tlist) |]   
  
mkList :: [(UTCTime,TimeZone)] 
       -> Q Exp  
mkList l = [| $(fmap ListE $ mapM mkPair l) |]    
    
mkPair :: (UTCTime,TimeZone) 
       -> Q Exp           
mkPair (t,tz) = [| ($(litUTCTime t),$(litTimeZone tz)) |]
    
litUTCTime :: UTCTime 
           -> Q Exp  
litUTCTime (UTCTime (ModifiedJulianDay d) s) = 
  [| UTCTime (ModifiedJulianDay $(litInteger d)) 
             (secondsToDiffTime $(litInteger $ diffTimeToInteger s)) |]

litInteger :: Integer
           -> Q Exp
litInteger = litE . integerL

diffTimeToInteger :: DiffTime 
                  -> Integer
diffTimeToInteger s =
  let r = toRational s
      n = numerator r
      d = denominator r in
  (n `div` d)

litTimeZone :: TimeZone 
            -> Q Exp
litTimeZone (TimeZone m s n) = 
  [| TimeZone $(litInteger $ toInteger m)
              $(return $ ConE $ mkName $ show s)
              $(litE $ StringL n) |]
        