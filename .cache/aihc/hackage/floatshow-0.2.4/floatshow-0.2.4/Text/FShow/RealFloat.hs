-- |
-- Module:      Text.FShow.RealFloat
-- Copyright:   (c) 2011 Daniel Fischer
-- Licence:     BSD3
-- Maintainer:  Daniel Fischer
-- Stability:   experimental
-- Portability: non-portable (GHC extensions)
--
-- Faster 'String' representations for floating point types.
-- The code is largely taken from code in "GHC.Float" and the 'Show'
-- instance of 'Integer' in "GHC.Num" to get the sequence of digits.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Text.FShow.RealFloat
    ( FShow(..)
    , fshows
    , DispFloat(..)
    , fshowFloat
    , fshowEFloat
    , fshowFFloat
    , fshowGFloat
    , Double7(..)
    , FullDouble(..)
    , Float7(..)
    , FullFloat(..)
    ) where

import GHC.Show
import GHC.Read
import GHC.Float (showSignedFloat)
import Text.FShow.RealFloat.Internals

-- | Class for specifying display parameters. The type @a@
--   is supposed to be an IEEE-ish (real) floating-point
--   type with floating-point radix 2, such that the mantissa
--   returned by 'decodeFloat' satisfies
--
-- @
--   2^('binExp' x) <= 'fst' ('decodeFloat' x) < 2^('binExp' x + 1)
-- @
--
--   for @x > 0@, so @'binExp' x = 'floatDigits' x - 1@.
--   The number of decimal digits that may be required is calculated
--   with the formula
--
-- @
--   'decDigits' x = 2 + 'floor' ('floatDigits' x * 'logBase' 10 2).
-- @
--
--   The default implementation uses an approximation of
--   @'logBase' 10 2@ sufficient for mantissae of up to
--   several thousand bits. Nevertheless, hardcoding
--   the values in instance declarations may yield
--   better performance.
class (RealFloat a) => DispFloat a where
  -- | The number of decimal digits that may be needed to
  --   uniquely determine a value of type @a@.
  --   For faster conversions which need not satisfy
  --
  -- @
  --   x == 'read' ('fshow' x)
  -- @
  --
  --   a smaller value can be given.
  decDigits     :: a -> Int
  decDigits x   = 2 + (8651*(floatDigits x)) `quot` 28738
  -- | The base 2 logarithm of the mantissa returned by
  --   @'decodeFloat' x@ for @x > 0@.
  binExp        :: a -> Int
  binExp x      = floatDigits x - 1

instance DispFloat Double where
  decDigits _   = 17
  binExp _      = 52

instance DispFloat Float where
  decDigits _   = 9
  binExp _      = 23

-- | newtype wrapper for 'Double'. The 'Show' (and 'FShow') instance
--   displays numbers rounded to seven significant digits.
newtype Double7 = D7 Double
  deriving (Eq, Ord, Num, Fractional, Real, RealFrac, Floating, RealFloat)

instance DispFloat Double7 where
  decDigits _ = 7
  binExp    _ = 52

instance Show Double7 where
  showsPrec p   = showSignedFloat fshowFloat p

instance FShow Double7 where
  fshowsPrec p  = showSignedFloat fshowFloat p
  fshowList     = showList__ (fshowsPrec 0)

-- | newtype wrapper for 'Double'. The 'Show' (and 'FShow') instance
--   displays all significant digits.
newtype FullDouble = FD { unFD :: Double }
  deriving (Eq, Ord, Num, Fractional, Real, RealFrac, Floating, RealFloat)

instance DispFloat FullDouble where
  decDigits (FD x)  = case decodeFloat x of
                        (_,e) -> case ((53+e)*8651) `quot` 28738 of
                                   q | e >= 0    -> q+2
                                     | e > (-53) -> q+1-e
                                     | otherwise -> q-e
  binExp _          = 52

instance Show FullDouble where
  showsPrec p = showSignedFloat fshowFloat p

instance FShow FullDouble where
  fshowsPrec p = showSignedFloat fshowFloat p
  fshowList     = showList__ (fshowsPrec 0)

instance Read FullDouble where
  readPrec = fmap FD readPrec
  readListPrec = readListPrecDefault

-- | newtype wrapper for 'Float'. The 'Show' (and 'FShow') instance
--   displays numbers rounded to seven significant digits.
newtype Float7 = F7 Float
  deriving (Eq, Ord, Num, Fractional, Real, RealFrac, Floating, RealFloat)

instance DispFloat Float7 where
  decDigits _ = 7
  binExp    _ = 23

instance Show Float7 where
  showsPrec p   = showSignedFloat fshowFloat p

instance FShow Float7 where
  fshowsPrec p  = showSignedFloat fshowFloat p
  fshowList     = showList__ (fshowsPrec 0)

-- | newtype wrapper for 'Double'. The 'Show' (and 'FShow') instance
--   displays all significant digits.
newtype FullFloat = FF { unFF :: Float }
  deriving (Eq, Ord, Num, Fractional, Real, RealFrac, Floating, RealFloat)

instance DispFloat FullFloat where
  decDigits (FF x)  = case decodeFloat x of
                        (_,e) -> case ((24+e)*8651) `quot` 28738 of
                                   q | e >= 0    -> q+2
                                     | e > (-24) -> q+1-e
                                     | otherwise -> q-e
  binExp _          = 23

instance Show FullFloat where
  showsPrec p = showSignedFloat fshowFloat p

instance FShow FullFloat where
  fshowsPrec p = showSignedFloat fshowFloat p
  fshowList     = showList__ (fshowsPrec 0)

instance Read FullFloat where
  readPrec = fmap FF readPrec
  readListPrec = readListPrecDefault

{-
    The code below is a minor modification of code from GHC.Float
    and Numeric from the base package. The GHC Licence is included
    in the package root.
-}

-- | A duplicate of the 'Show' class.
class FShow a where
  fshow             :: a -> String
  fshowsPrec        :: Int -> a -> ShowS
  fshowList         :: [a] -> ShowS
  fshow x           = fshowsPrec 0 x ""
  fshowsPrec _ x s  = fshow x ++ s
  fshowList xs s    = showList__ fshows xs s

-- | Same as @'shows'@, but using an 'FShow' instance.
fshows :: FShow a => a -> ShowS
fshows x = showString (fshow x)

instance FShow Double where
  fshowsPrec p  = showSignedFloat fshowFloat p
  fshowList     = showList__ (fshowsPrec 0)

instance FShow Float where
  fshowsPrec p  = showSignedFloat fshowFloat p
  fshowList     = showList__ (fshowsPrec 0)

instance (FShow a) => FShow [a] where
  fshowsPrec _ = fshowList

{-# SPECIALISE fshowFloat ::
        Float   -> ShowS,
        Float7  -> ShowS,
        Double7 -> ShowS,
        Double  -> ShowS #-}
-- | Show a signed 'DispFloat' value to full precision
-- using standard decimal notation for arguments whose absolute value lies
-- between @0.1@ and @9,999,999@, and scientific notation otherwise.
-- Analogous to @'showFloat'@ from "GHC.Float".
fshowFloat :: (DispFloat a) => a -> ShowS
fshowFloat x  =  showString (formatFloat FFGeneric Nothing x)

{-# SPECIALISE fshowEFloat ::
        Maybe Int -> Float   -> ShowS,
        Maybe Int -> Float7  -> ShowS,
        Maybe Int -> Double7 -> ShowS,
        Maybe Int -> Double  -> ShowS #-}
{-# SPECIALISE fshowFFloat ::
        Maybe Int -> Float   -> ShowS,
        Maybe Int -> Float7  -> ShowS,
        Maybe Int -> Double7 -> ShowS,
        Maybe Int -> Double  -> ShowS #-}
{-# SPECIALISE fshowGFloat ::
        Maybe Int -> Float   -> ShowS,
        Maybe Int -> Float7  -> ShowS,
        Maybe Int -> Double7 -> ShowS,
        Maybe Int -> Double  -> ShowS #-}

-- | Show a signed 'DispFloat' value
-- using scientific (exponential) notation (e.g. @2.45e2@, @1.5e-3@).
--
-- In the call @'fshowEFloat' digs val@, if @digs@ is 'Nothing',
-- the value is shown to full precision; if @digs@ is @'Just' d@,
-- then @'max' 1 d@ digits after the decimal point are shown.
-- Analogous to @'showEFloat'@ from "Numeric".
fshowEFloat    :: (DispFloat a) => Maybe Int -> a -> ShowS
fshowEFloat d x =  showString (formatFloat FFExponent d x)

-- | Show a signed 'DispFloat' value
-- using standard decimal notation (e.g. @245000@, @0.0015@).
--
-- In the call @'fshowFFloat' digs val@, if @digs@ is 'Nothing',
-- the value is shown to full precision; if @digs@ is @'Just' d@,
-- then @'max' 0 d@ digits after the decimal point are shown.
-- Analogous to @'showFFloat'@ from "Numeric".
fshowFFloat    :: (DispFloat a) => Maybe Int -> a -> ShowS
fshowFFloat d x =  showString (formatFloat FFFixed d x)

-- | Show a signed 'DispFloat' value
-- using standard decimal notation for arguments whose absolute value lies
-- between @0.1@ and @9,999,999@, and scientific notation otherwise.
--
-- In the call @'fshowGFloat' digs val@, if @digs@ is 'Nothing',
-- the value is shown to full precision; if @digs@ is @'Just' d@,
-- then @'max' 1 d@ digits after the decimal point are shown.
-- Analogous to @'showGFloat'@ from "Numeric".
fshowGFloat    :: (DispFloat a) => Maybe Int -> a -> ShowS
fshowGFloat d x =  showString (formatFloat FFGeneric d x)

{-
Code duplication ahead. The below code is - with minor modifications -
replicated in Text.FShow.Raw.
Yuck!
But reusing that interface here costs too much performance here, so
this is staying.
'Tis a library, it needn't be pretty, it's gotta be fast.
-}

data FFFormat = FFExponent | FFFixed | FFGeneric

{-# SPECIALISE formatFloat :: FFFormat -> Maybe Int -> Double -> String,
                              FFFormat -> Maybe Int -> Float -> String,
                              FFFormat -> Maybe Int -> Double7 -> String,
                              FFFormat -> Maybe Int -> Float7 -> String
  #-}
formatFloat :: DispFloat a => FFFormat -> Maybe Int -> a -> String
formatFloat fmt decs x
    | isNaN x                   = "NaN"
    | isInfinite x              = if x < 0 then "-Infinity" else "Infinity"
    | x < 0 || isNegativeZero x = '-':doFmt fmt (fltDigs (-x))
    | otherwise                 = doFmt fmt (fltDigs x)
      where
        fltDigs 0 = ([0],0)
        fltDigs y = uncurry (posToDigits (decDigits y) (binExp y)) (decodeFloat y)
        fluff :: [Int] -> [Int]
        fluff [] = [0]
        fluff xs = xs

        doFmt format (is, e) =
          case format of
            FFGeneric ->
              doFmt (if e < (-1) || e > 6 then FFExponent else FFFixed) (is,e)
            FFExponent ->
              case decs of
                Nothing ->
                  let show_e' = show (e+ei)
                      (ei,(d:ds)) = roundToS (decDigits x) is
                  in case is of
                       [0] -> "0.0e0"
                       _ -> i2D d : '.' : map i2D (fluff ds) ++ ('e' : show_e')
                Just dec ->
                  let dec' = max dec 1 in
                  case is of
                    [0] -> '0' :'.' : take dec' (repeat '0') ++ "e0"
                    _ -> let (ei,is') = roundTo (dec'+1) is
                             (d:ds') = map i2D (if ei == 0 then is' else init is')
                         in d:'.':ds' ++ 'e':show (e+ei)
            FFFixed ->
              let mk0 ls = case ls of { "" -> "0" ; _ -> ls} in
              case decs of
                Nothing ->
                  let (ei, is') = roundToS (decDigits x) is
                      e' = e+1+ei
                      ds = map i2D is'
                  in case is of
                       [0] -> "0.0"
                       _ | e' <= 0 -> "0." ++ replicate (-e') '0' ++ ds
                         | otherwise ->
                           let f 0 s    rs  = mk0 (reverse s) ++ '.':mk0 rs
                               f n s    ""  = f (n-1) ('0':s) ""
                               f n s (r:rs) = f (n-1) (r:s) rs
                           in f e' "" ds
                Just dec ->
                  let dec' = max dec 0
                      e' = e+1
                  in
                  if e' >= 0 then
                     let (ei,is') = roundTo (dec' + e') is
                         (ls,rs)  = splitAt (e'+ei) (map i2D is')
                     in mk0 ls ++ (if null rs then "" else '.':rs)
                  else
                     let (ei,is') = roundTo dec' (replicate (-e') 0 ++ is)
                         d:ds' = map i2D (if ei == 0 then 0:is' else is')
                     in d : (if null ds' then "" else '.':ds')

roundToS :: Int -> [Int] -> (Int,[Int])
roundToS d is =
    case f d is of
      x@(0,_) -> x
      (1,xs)  -> (1, 1:xs)
      _       -> error "roundToS: bad Value"
  where
    f _ []          = (0, [])
    f 0 (x:_)       = (if x < 5 then 0 else 1, [])
    f n (i:xs)
      | i' == 10    = (1,prep 0 ds)
      | otherwise   = (0,prep i' ds)
        where
          prep 0 [] = []
          prep a bs = a:bs
          (c,ds)    = f (n-1) xs
          i'        = c + i

roundTo :: Int -> [Int] -> (Int,[Int])
roundTo d is =
    case f d is of
      x@(0,_) -> x
      (1,xs)  -> (1, 1:xs)
      _       -> error "roundTo: bad Value"
  where
    f n []          = (0, replicate n 0)
    f 0 (x:_)       = (if x < 5 then 0 else 1, [])
    f n [i]         = (if i < 5 then 0 else 1, replicate n 0)
    f n (i:xs)
      | i' == 10    = (1,0:ds)
      | otherwise   = (0,i':ds)
        where
          (c,ds)    = f (n-1) xs
          i'        = c + i
