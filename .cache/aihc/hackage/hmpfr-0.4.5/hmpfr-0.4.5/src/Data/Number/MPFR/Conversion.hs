{-|
    Module      :  Data.Number.MPFR.Conversion
    Description :  wrappers for conversion functions
    Copyright   :  (c) Aleš Bizjak
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  non-portable

  Conversion from basic MPFR back to basic Haskell types. 
  See <http://www.mpfr.org/mpfr-current/mpfr.html#Conversion-Functions> for
  documentation on particular functions.
-}

module Data.Number.MPFR.Conversion where

import Data.Number.MPFR.Internal
import Data.Number.MPFR.Comparison(isZero)
import Data.Number.MPFR.Misc

import Data.List (isInfixOf)

toDouble       :: RoundMode -> MPFR -> Double
toDouble r mp1 = (realToFrac . unsafePerformIO) go
    where go = with mp1 $ \p -> mpfr_get_d p ((fromIntegral . fromEnum) r)

toDouble2exp     :: RoundMode -> MPFR -> (Double, Int)
toDouble2exp r mp1 = unsafePerformIO go 
    where go = with mp1 $ \p1 ->
                   alloca $ \p2 -> do
                       r1 <- mpfr_get_d_2exp p2 p1 ((fromIntegral . fromEnum) r)
                       r2 <- peek p2
                       return (realToFrac r1, fromIntegral r2)
                      
toInt     :: RoundMode -> MPFR -> Int
toInt r mp1 = (fromIntegral . unsafePerformIO) go
    where go = with mp1 $ \p -> mpfr_get_si p ((fromIntegral . fromEnum) r)

toWord       :: RoundMode -> MPFR -> Word
toWord r mp1 = (fromIntegral . unsafePerformIO) go
    where go = with mp1 $ \p -> mpfr_get_ui p ((fromIntegral . fromEnum) r)


mpfrToString           :: RoundMode
                       -> Word -- ^ number of decimals
                       -> Word -- ^ base
                       -> MPFR -> (String, Exp)
mpfrToString r n b mp1 = unsafePerformIO go 
    where go = with mp1 $ \p1 ->
                 alloca $ \p2 -> do
                     p3 <- mpfr_get_str nullPtr p2 (fromIntegral b) (fromIntegral n) p1 ((fromIntegral . fromEnum) r)
                     r1 <- peekCString p3 
                     r2 <- peek p2
                     mpfr_free_str p3
                     return (r1, r2)

fitsULong     :: RoundMode -> MPFR -> Bool
fitsULong r d = withMPFRF d r mpfr_fits_ulong_p /= 0 

fitsSLong     :: RoundMode -> MPFR -> Bool
fitsSLong r d = withMPFRF d r mpfr_fits_slong_p /= 0 

fitsUInt     :: RoundMode -> MPFR -> Bool
fitsUInt r d = withMPFRF d r mpfr_fits_uint_p /= 0 

fitsSInt     :: RoundMode -> MPFR -> Bool
fitsSInt r d = withMPFRF d r mpfr_fits_sint_p /= 0 

fitsUShort     :: RoundMode -> MPFR -> Bool
fitsUShort r d = withMPFRF d r mpfr_fits_ushort_p /= 0 

fitsSShort     :: RoundMode -> MPFR -> Bool
fitsSShort r d = withMPFRF d r mpfr_fits_sshort_p /= 0 

-- TODO
decompose   :: MPFR -> (Integer, Exp)
decompose d@(MP p _ e _) | e == expInf  = error "Don't know how to decompose Infinity"
                         | e == expNaN  = error "Don't know how to decompose NaN"
                         | e == expZero = (0, 0) 
                         | otherwise    = (dm, e - sh)
    where dm = getMantissa d
          sh =  fromIntegral (Prelude.ceiling (fromIntegral p / fromIntegral bitsPerMPLimb :: Double) * bitsPerMPLimb)

-- | Output a string in base 10 rounded to Near in exponential form.
toStringExp       :: Word -- ^ number of digits
                  -> MPFR -> String
toStringExp dec d | isInfixOf "NaN" ss = "NaN"
                  | isInfixOf "Inf" ss = s ++ "Infinity"
                  | isZero d = "0"
                  | e > 0              = 
                      s ++ if Prelude.floor prec <= dec
                           then 
                               take e ss ++ 
                               let bt = backtrim (drop e ss)
                               in if null bt 
                                  then "" 
                                  else '.' : bt
                           else head ss : '.' :
                                let bt = (backtrim . tail) ss 
                                in (if null bt then "0" else bt) 
                                   ++ "e" ++ show (pred e)
                  | otherwise = 
                      s ++ (head ss : '.' : 
                               (let bt = (backtrim . tail) ss in
                                if null bt then "0" 
                                else bt )
                               ++ "e" ++ show (pred e))
                    where (str, e') = mpfrToString Near n 10 d
                          e = fromIntegral e'
                          n        = max dec 5
                          (s, ss) = case head str of
                                      '-' -> ("-", tail str)
                                      _   -> ("" , str)
                          backtrim = reverse . dropWhile (== '0') . reverse 
                          prec = logBase 10 2 * fromIntegral (getExp d) :: Double

-- | Output a string in base 10 rounded to Near. The difference from @toStringExp@ is that
-- it won't output in exponential form if it is sensible to do so.
toString       :: Word -> MPFR -> String
toString dec d | isInfixOf "NaN" ss = "NaN"
               | isInfixOf "Inf" ss = s ++ "Infinity"
               | otherwise          = 
                   s ++ case compare 0 e of
                          LT -> take e ss ++ 
                                (let bt = all (== '0') (drop e ss) 
                                 in if bt then "" else '.' : drop e ss)
                                ++ (if fromIntegral n - e < 0 
                                    then 'e' : show (e - fromIntegral n) 
                                    else "")
                          GT -> let ee = fromIntegral dec + e in 
                                if ee <= 0 then "0" else 
                                   head ss : '.' : (backtrim . tail . take ee) ss
                                            ++ "e" ++ show (pred e)
                          EQ -> "0." ++ let bt = all (== '0') ss 
                                        in if bt then "0" else ss
                  where (str, e') = mpfrToString Near n 10 d
                        n        = max dec 5
                        e = fromIntegral e'
                        (s, ss) = case head str of
                                    '-' -> ("-", tail str)
                                    _   -> ("" , str)
                        backtrim = reverse . dropWhile (== '0') . reverse 

instance Show MPFR where
    show = toStringExp 16
