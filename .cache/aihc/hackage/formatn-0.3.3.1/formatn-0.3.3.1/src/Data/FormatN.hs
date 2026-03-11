{-# LANGUAGE OverloadedStrings #-}

-- | Text formatting of 'Double's.
--
-- In particular, the library provides functionality to calculate and display a fixed number of <https://en.wikipedia.org/wiki/Significant_figures significant figures> for a variety of different number formatting styles.
--
--
-- Some similar libraries that may be better suited for different use cases include:
--
-- Flexible formatters. These libraries provide more flexibility around formatting options, but do not have a concept of significance:
--
-- - <https://hackage.haskell.org/package/base-4.16.0.0/docs/Text-Printf.html Text.Printf> and <https://hackage.haskell.org/package/base-4.16.0.0/docs/Numeric.html#v:showFFloat Numeric> in base.
-- - <https://hackage.haskell.org/package/formatting Formatting>
-- - <https://hackage.haskell.org/package/vformat-0.9.0.0 vformat: A Python str.format() like formatter>
--
-- <https://hackage.haskell.org/package/text-format text-format> has similar functionality but is not native haskell and I wanted to do some tweaking to defaults. It's probably safer and faster.
--
-- <https://hackage.haskell.org/package/rounded rounded> seems to be much more about doing computation taking rounding into account, compared with the much simpler task of pretty printing a number.
--
-- This library could have just provided an ability to compute a significant figure version of a number and then use these other libraries, but the round trip (from Double to SigFig to Double) introduces errors (eg the least significant figure goes from being a '4' to a '3999999' via float maths).
--
-- formatn is used in the <https://hackage.haskell.org/package/chart-svg chart-svg> library to automate consistent number formatting across different scales.
module Data.FormatN
  ( -- * Usage
    -- $setup

    -- * SigFig
    SigFig (..),
    SigFigSign (..),
    toSigFig,
    fromSigFig,
    isZero,
    incSigFig,
    decSigFig,

    -- * Format Styles
    FormatStyle (..),
    precStyle,
    commaPrecStyle,
    FStyle (..),

    -- * SigFig formatters
    fixedSF,
    exptSF,
    exptSFWith,
    decimalSF,
    commaSF,
    dollarSF,
    percentSF,
    formatSF,

    -- * Double formatters
    format,
    formatOrShow,
    fixed,
    expt,
    exptWith,
    decimal,
    prec,
    comma,
    commaPrec,
    dollar,
    percent,

    -- * List Modifiers
    majorityStyle,
    formats,
    formatsSF,
    decSigFigs,
    lpads,
    distinguish,

    -- * FormatN
    FormatN (..),
    defaultFormatN,
    formatN,
    formatNs,
  )
where

import Data.Bifunctor
import Data.Bool
import Data.Containers.ListUtils (nubOrd)
import Data.Data
import Data.Foldable
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Ord
import Data.Text (Text, pack)
import Data.Text qualified as Text
import GHC.Generics hiding (prec)
import Numeric
import Prelude hiding (exponent)

-- $setup
-- >>> import Data.FormatN
-- >>> xs = [(-1),0,1,1.01,1.02,1.1,1.2]
-- >>> fixed (Just 2) <$> xs
-- ["-1.00","0.00","1.00","1.01","1.02","1.10","1.20"]
-- >>> decimal (Just 2) <$> xs
-- ["-1.0","0.0","1.0","1.0","1.0","1.1","1.2"]
-- >>> decimal (Just 3) . (1e-3*) <$> xs
-- ["-0.00100","0.00","0.00100","0.00101","0.00102","0.00110","0.00120"]
-- >>> comma (Just 3) . (1e3*) <$> xs
-- ["-1,000","0.00","1,000","1,010","1,020","1,100","1,200"]
--
-- 'formats' is useful when you want a consistent textual style across a list of numbers:
--
-- >>> formats True False (const DecimalStyle) (Just 2) $ (1e-3*) <$> xs
-- ["-0.0010"," 0.0000"," 0.0010"," 0.0010"," 0.0010"," 0.0011"," 0.0012"]
--
-- Using significant figures actually changes numbers - numbers that were slightly different end up being (and looking like) the same. 'distinguish' increases the number of significant figures to compensate for this effect.
--
-- >>> distinguish 4 True False (const DecimalStyle) (Just 2) xs
-- ["-1.00"," 0.00"," 1.00"," 1.01"," 1.02"," 1.10"," 1.20"]

-- | Decomposition of a Double into the components that are needed to determine significant figure formatting.
--
-- Eliding type changes, the relationship between a Double and a SigFig is:
--
-- \[
--   x == sign * figures * 10^{exponent}
-- \]
data SigFig = SigFig
  { -- | sign
    sfSign :: SigFigSign,
    -- | significant figures expressed as an Integer
    sfFigures :: Integer,
    -- | the power of 10 exponent given figures.
    sfExponent :: Int
  }
  deriving (Eq, Show)

-- | Sign component
data SigFigSign = SigFigNeg | SigFigPos deriving (Eq, Show)

sfsign :: SigFigSign -> String
sfsign s = bool "" "-" (s == SigFigNeg)

-- | Note that zero can still be represented in a SigFig way, so that we can distinguish between something that starts off as zero, and something that ends up as zero via rounding.
--
-- >>> isZero (SigFig SigFigPos 0 (-3))
-- True
isZero :: SigFig -> Bool
isZero (SigFig _ i _) = i == 0

-- | convert from a Double to a 'SigFig'
--
-- >>> toSigFig (Just 2) 1234
-- SigFig {sfSign = SigFigPos, sfFigures = 12, sfExponent = 2}
--
-- > toSigFig Nothing . fromSigFig <==> id
-- > toSigFig (Just x) . fromSigFig . toSigFig (Just x) <==> toSigFig (Just x)
--
-- prop> \x -> let (SigFig s fs e) = toSigFig Nothing x in let x' = ((if (s==SigFigNeg) then (-1.0) else 1.0) * fromIntegral fs * 10.0**fromIntegral e) in (x==0 || abs (x/x'-1) < 1e-6)
--
-- Checks for a valid number of significant figures and turns it off on a silly number.
--
-- >>> toSigFig Nothing 1234
-- SigFig {sfSign = SigFigPos, sfFigures = 1234, sfExponent = 0}
--
-- >>> toSigFig (Just (-3)) 1234
-- SigFig {sfSign = SigFigPos, sfFigures = 1234, sfExponent = 0}
toSigFig :: Maybe Int -> Double -> SigFig
toSigFig n x = SigFig s fs' expo'
  where
    n' = maybe Nothing (\sf -> bool (Just sf) Nothing (sf < 1)) n
    (s, (floatfs, floate)) = bool (SigFigPos, floatToDigits 10 x) (SigFigNeg, floatToDigits 10 (-x)) (x < 0)
    -- floatToDigits 10 0 == ([0],0) floatToDigits 10 1 == ([1],1)
    floate' = bool floate (floate + 1) (x == 0)
    nsig = fromMaybe (length floatfs) n'
    -- pad with extra zeros if less figures than requested
    (floatfs', e) =
      bool
        (floatfs, floate' - length floatfs)
        (floatfs <> replicate (nsig - length floatfs) 0, floate' - nsig)
        (length floatfs < nsig)
    (fs0, fs1) = splitAt nsig floatfs'
    -- reconstitute number to get rounding right at the least significance point
    fs =
      round $
        (fromIntegral $ foldl' (\x' a -> x' * 10 + a) 0 fs0 :: Double)
          + fromIntegral (foldl' (\x' a -> x' * 10 + a) 0 fs1) / (10.0 ^ (length fs1 :: Int))
    -- rounding can bump significant figures by 1 eg 99(.9999) ==> 100
    (fs', expo) =
      bool
        (fs, e + length floatfs' - nsig)
        (fs `div` 10, e + length floatfs' - nsig + 1)
        (length (show fs) > nsig)
    -- zero fix
    expo' = bool expo 0 (fs' == 0 && expo > 0)

-- | convert from a 'SigFig' to a Double
--
-- >>> fromSigFig (SigFig SigFigPos 12 2)
-- 1200.0
fromSigFig :: SigFig -> Double
fromSigFig (SigFig s fs e) = bool 1 (-1) (s == SigFigNeg) * fromIntegral fs * 10 ** fromIntegral e

-- | increase significant figures
--
-- >>> incSigFig 1 (SigFig SigFigPos 1 0)
-- SigFig {sfSign = SigFigPos, sfFigures = 10, sfExponent = -1}
incSigFig :: Int -> SigFig -> SigFig
incSigFig n (SigFig s fs e) = SigFig s (fs * (10 ^ max 0 n)) (e - n)

-- | decrease significant figures, if possible.
--
-- >>> decSigFig 1 (SigFig SigFigPos 100 0)
-- Just (SigFig {sfSign = SigFigPos, sfFigures = 10, sfExponent = 1})
--
-- >>> decSigFig 1 (SigFig SigFigPos 123 0)
-- Nothing
decSigFig :: Int -> SigFig -> Maybe SigFig
decSigFig n (SigFig s fs e) =
  bool
    Nothing
    (Just (SigFig s (fs `div` (10 ^ n)) (e + n)))
    (fs `mod` (10 ^ n) == 0 && n > 0)

-- The natural exponent to format with
eSF :: SigFig -> Int
eSF (SigFig _ fs e) = e + length (show fs) - 1

-- | expt format for a SigFig
fixedSF :: Maybe Int -> SigFig -> Text
fixedSF n sf = fixed n (fromSigFig sf)

-- | expt format for a SigFig
exptSF :: SigFig -> Text
exptSF (SigFig s i e) = pack $ sfsign s <> sfTextDot <> "e" <> show eText
  where
    sfTextDot
      | length sfText == 1 = sfText
      | otherwise = take 1 sfText <> "." <> drop 1 sfText
    sfText = bool (show i) (replicate (max 1 (1 - e)) '0') (i == 0)
    eText = e + length sfText - 1

-- | expt format for a SigFig, with an exponent override
--
-- >>> exptSFWith (Just 1) (toSigFig (Just 1) 1)
-- "0.1e1"
-- >>> exptSFWith (Just 0) (toSigFig (Just 1) 1)
-- "1e0"
-- >>> exptSFWith (Just (-1)) (toSigFig (Just 1) 1)
-- "10e-1"
exptSFWith :: Maybe Int -> SigFig -> Text
exptSFWith eover (SigFig s i e) = pack (sfsign s) <> posDecimalSF i (e - e') <> "e" <> pack (show e')
  where
    e' = fromMaybe (bool (e + length (show i) - 1) 0 (i == 0)) eover

-- Formatting the positive component in decimal style
posDecimalSF :: Integer -> Int -> Text
posDecimalSF xs e = pack t
  where
    xs' = show xs
    nsf = length xs'
    extrasf = bool (-(e + nsf)) (-(e + nsf)) (xs == 0)
    oversf = length xs' + e
    t
      | e >= 0 = bool (xs' <> replicate e '0') xs' (xs == 0)
      | e <= -nsf = "0." <> replicate extrasf '0' <> xs'
      | otherwise = take oversf xs' <> "." <> drop oversf xs'

-- | comma format for a SigFig
maybeCommaSF :: Bool -> SigFig -> Text
maybeCommaSF doCommas (SigFig s xs e) = pack (sfsign s) <> bool id addcommas doCommas (posDecimalSF xs e)
  where
    addcommas =
      uncurry (<>)
        . first (Text.reverse . Text.intercalate "," . Text.chunksOf 3 . Text.reverse)
        . Text.breakOn "."

-- | comma format for a SigFig
commaSF :: SigFig -> Text
commaSF = maybeCommaSF True

-- | decimal format for a SigFig
decimalSF :: SigFig -> Text
decimalSF = maybeCommaSF False

-- | percent format for a SigFig
percentSF :: (SigFig -> Text) -> SigFig -> Text
percentSF f (SigFig s figs e) = (<> "%") $ f (SigFig s figs (e + 2))

-- | dollar format for a SigFig
dollarSF :: (SigFig -> Text) -> SigFig -> Text
dollarSF f sf =
  case sfSign sf of
    SigFigNeg -> "-" <> dollarSF f (SigFig SigFigPos (sfFigures sf) (sfExponent sf))
    SigFigPos -> "$" <> f sf

-- * specific number formats

-- | Format to x decimal places with no significant figure rounding.
--
-- >>> fixed (Just 2) 100
-- "100.00"
-- >>> fixed (Just 2) 0.001
-- "0.00"
fixed :: Maybe Int -> Double -> Text
fixed n x = pack $ showFFloat n x ""

-- | Format in exponential style, maybe with significant figure rounding.
--
-- >>> expt Nothing 1245
-- "1.245e3"
-- >>> expt (Just 3) 1245
-- "1.24e3"
-- >>> expt (Just 3) 0.1245
-- "1.24e-1"
-- >>> expt (Just 2) 0
-- "0.0e0"
expt :: Maybe Int -> Double -> Text
expt n x = exptSF (toSigFig n x)

-- | Format in exponential style, with the suggested exponent.
--
-- >>> exptWith (Just 2) Nothing 1245
-- "12.45e2"
-- >>> exptWith (Just 6) (Just 3) 1245
-- "0.00124e6"
exptWith :: Maybe Int -> Maybe Int -> Double -> Text
exptWith n' n x = exptSFWith n' (toSigFig n x)

-- | Format in decimal style, and maybe round to n significant figures.
--
-- >>> decimal Nothing 1.2345e-2
-- "0.012345"
-- >>> decimal (Just 2) 0.012345
-- "0.012"
-- >>> decimal (Just 2) 12345
-- "12000"
decimal :: Maybe Int -> Double -> Text
decimal n x = decimalSF (toSigFig n x)

-- | Format with US-style commas
--
-- >>> comma (Just 3) 1234567
-- "1,230,000"
comma :: Maybe Int -> Double -> Text
comma n x = commaSF (toSigFig n x)

-- | Adjust format to a percent.
--
-- >>> percent commaSF (Just 3) 0.1234
-- "12.3%"
-- >>> percent decimalSF (Just 1) 0.1234
-- "10%"
percent :: (SigFig -> Text) -> Maybe Int -> Double -> Text
percent f n x = percentSF f (toSigFig n x)

-- | Adjust format to dollar style.
--
-- >>> dollar commaSF (Just 3) 1234
-- "$1,230"
-- >>> dollar (fixedSF (Just 2)) (Just 2) 0.01234
-- "$0.01"
dollar :: (SigFig -> Text) -> Maybe Int -> Double -> Text
dollar f n x = dollarSF f (toSigFig n x)

-- | Data type representing styles of formatting
data FormatStyle
  = -- | 1000 1 0.001
    DecimalStyle
  | -- | 1e3 1e0 1e-3
    ExponentStyle (Maybe Int)
  | -- | 1,000 1 0.001
    CommaStyle
  | -- | 1000.00 1.00 0.00
    FixedStyle Int
  | -- | 100,000% 100% 0.1%
    PercentStyle
  | -- | \$1,000 $1 $0.001
    DollarStyle
  deriving (Show, Eq, Ord)

-- | DecimalStyle between 0.001 and a million and ExponentStyle outside this range.
precStyle :: Double -> FormatStyle
precStyle x
  | x == 0 = DecimalStyle
  | abs x < 0.001 = ExponentStyle (Just (eSF (toSigFig Nothing x)))
  | abs x > 1e6 = ExponentStyle (Just (eSF (toSigFig Nothing x)))
  | otherwise = DecimalStyle

-- | CommaStyle above a thousand but below a million, DecimalStyle between 0.001 and a thousand and ExponentStyle outside this range.
commaPrecStyle :: Double -> FormatStyle
commaPrecStyle x
  | x == 0 = CommaStyle
  | abs x < 0.001 = ExponentStyle (Just (eSF (toSigFig Nothing x)))
  | abs x > 1e6 = ExponentStyle (Just (eSF (toSigFig Nothing x)))
  | otherwise = CommaStyle

-- | Data type representing styles of formatting dependent on the number
data FStyle
  = FSDecimal
  | FSExponent (Maybe Int)
  | FSComma
  | FSFixed Int
  | FSPercent
  | FSDollar
  | FSPrec
  | FSCommaPrec
  | FSNone
  deriving (Eq, Show, Generic, Data)

-- | Compute the majority (modal) FormatStyle so a list of numbers can all have the same formatting
--
-- Also equalises the exponent to the majority for exponent style.
--
-- >>> commaPrecStyle <$> [0,5e6,1e7,2e7]
-- [CommaStyle,ExponentStyle (Just 6),ExponentStyle (Just 7),ExponentStyle (Just 7)]
-- >>> majorityStyle commaPrecStyle [0,5e6,1e7,2e7]
-- ExponentStyle (Just 7)
majorityStyle :: (Double -> FormatStyle) -> [Double] -> FormatStyle
majorityStyle s xs = maj'
  where
    maj = fromMaybe CommaStyle (major (neutralExpStyle . s <$> xs))
    maj' = bool maj (ExponentStyle (fromMaybe Nothing expXs)) (maj == ExponentStyle Nothing)
    neutralExpStyle (ExponentStyle _) = ExponentStyle Nothing
    neutralExpStyle x = x

    expXs = major [x | (ExponentStyle x) <- s <$> xs]

major :: (Ord a) => [a] -> Maybe a
major xs = fst <$> listToMaybe (List.sortOn (Down . snd) $ Map.toList $ Map.fromListWith (+) ((,1 :: Integer) <$> xs))

-- | format a number according to a FormatStyle and significant figures
--
-- >>> format CommaStyle (Just 2) 1234
-- "1,200"
format :: FormatStyle -> Maybe Int -> Double -> Text
format fs n x = bool (go x) ("-" <> go (-x)) (x < 0)
  where
    go x' = case fs of
      DecimalStyle -> decimal n x'
      ExponentStyle n' -> exptWith n' n x'
      CommaStyle -> comma n x'
      FixedStyle n' -> fixed (Just n') x'
      PercentStyle -> percent commaSF n x'
      DollarStyle -> dollar commaSF n x'

-- | format a SigFig according to a style
--
-- >>> formatSF CommaStyle (toSigFig (Just 2) 1234)
-- "1,200"
-- >>> formatSF CommaStyle (SigFig SigFigPos 0 1)
-- "0"
-- >>> formatSF CommaStyle (SigFig SigFigPos 0 (-1))
-- "0.0"
formatSF :: FormatStyle -> SigFig -> Text
formatSF fs x = case fs of
  DecimalStyle -> decimalSF x
  ExponentStyle n' -> exptSFWith n' x
  CommaStyle -> commaSF x
  FixedStyle n -> fixed (Just n) (fromSigFig x)
  PercentStyle -> percentSF commaSF x
  DollarStyle -> dollarSF commaSF x

-- | Format between 0.001 and 1000000 using decimal style and exponential style outside this range.
--
-- >>> prec (Just 2) 0.00234
-- "0.0023"
-- >>> prec (Just 2) 0.000023
-- "2.3e-5"
-- >>> prec (Just 2) 123
-- "120"
-- >>> prec (Just 2) 123456
-- "120000"
-- >>> prec (Just 2) 1234567
-- "1.2e6"
prec :: Maybe Int -> Double -> Text
prec n x = format (precStyle x) n x

-- | Format using comma separators for numbers above 1,000 but below 1 million, otherwise use prec style.
--
-- >>> commaPrec (Just 3) 1234
-- "1,230"
-- >>> commaPrec (Just 3) 1234567
-- "1.23e6"
commaPrec :: Maybe Int -> Double -> Text
commaPrec n x = format (commaPrecStyle x) n x

-- | Consistently format a list of numbers,using the minimum number of decimal places or minimum exponent.
--
-- >>> formats True True precStyle (Just 1) [0,0.5,1,2]
-- ["0.0","0.5","1.0","2.0"]
--
-- Note how the presence of 0.5 in the example above changes the format of all numbers. Without it:
--
-- >>> formats True True precStyle (Just 1) [0,1,2]
-- ["0","1","2"]
--
-- >>> formats False True precStyle (Just 1) $ ((-1)*) <$> [0,0.5,1,2]
-- ["0.0","-0.5","-1.0","-2.0"]
-- >>> formats True True commaPrecStyle (Just 1) $ ((-1000)*) <$> [0,0.5,1,2]
-- ["     0","  -500","-1,000","-2,000"]
-- >>> formats True True commaPrecStyle (Just 1) $ ((1e6)*) <$> [0,0.5,1,2]
-- ["        0","  500,000","1,000,000","2,000,000"]
-- >>> formats True True commaPrecStyle (Just 1) $ ((1e6)*) <$> [0.9,2,3]
-- ["0.9e6","2.0e6","3.0e6"]
-- >>> formats True True commaPrecStyle (Just 1) $ ((1e-6)*) <$> [0,0.5,1,2]
-- ["0.0e-6","0.5e-6","1.0e-6","2.0e-6"]
-- >>> formats True True commaPrecStyle (Just 1) $ ((1e-3)*) <$> [0,0.5,1,2]
-- ["0.0000","0.0005","0.0010","0.0020"]
-- >>> formats True False (const (ExponentStyle Nothing)) (Just 2) [0..4]
-- ["0.0e0","1.0e0","2.0e0","3.0e0","4.0e0"]
-- >>> formats True True (const (ExponentStyle Nothing)) (Just 2) [0..4]
-- ["0e0","1e0","2e0","3e0","4e0"]
formats ::
  -- | left pad to the largest text length
  Bool ->
  -- | Try and reduce excess right-hand zeros
  Bool ->
  -- | style
  (Double -> FormatStyle) ->
  -- | significant figures requested
  Maybe Int ->
  -- | list of numbers
  [Double] ->
  [Text]
formats lpad rcut s n0 xs =
  formatsFromSF lpad s $
    bool id decSigFigs rcut (formatsSF n0 xs)

-- | Consistently convert a list of numbers to 'SigFig's, using the minimum natural exponent of the list.
formatsSF ::
  -- | significant figures requested
  Maybe Int ->
  -- | list of numbers
  [Double] ->
  [SigFig]
formatsSF n0 xs = sigs'
  where
    sigs = toSigFig n0 <$> xs
    minexp = minimum (sfExponent <$> filter (not . isZero) sigs)
    sigs' = (\x -> bool (incSigFig (sfExponent x - minexp) x) (SigFig SigFigPos 0 minexp) (isZero x)) <$> sigs

formatsFromSF ::
  -- | left pad to the largest text length
  Bool ->
  -- | style
  (Double -> FormatStyle) ->
  -- | list of numbers
  [SigFig] ->
  [Text]
formatsFromSF lpad s sigs = bool fsigs (lpads fsigs) lpad
  where
    maj = majorityStyle s (fromSigFig <$> sigs)
    fsigs = formatSF maj <$> sigs

-- | Decrease the SigFig figure of a list of SigFigs without loss of precision, if possible. This has the effect of removing right zeros in decimal representations.
decSigFigs :: [SigFig] -> [SigFig]
decSigFigs xs = bool xs (decSigFigs xs') (all isJust decXs)
  where
    decXs = decSigFig 1 <$> xs
    xs' = catMaybes decXs

-- | Add spaces to the left of a text representation so that all elements have the same length.
lpads :: [Text] -> [Text]
lpads ts = (\x -> mconcat (replicate (maxl - Text.length x) " ") <> x) <$> ts
  where
    maxl = maximum $ Text.length <$> ts

-- | Provide formatted text for a list of numbers so that they are just distinguished.
--
-- For example, __@distinguish 4 commaPrecStyle (Just 2)@__ means use as much significant figures as is needed for the numbers to be distinguished on rendering (up to 4+2=6), but with at least 2 significant figures.
--
-- The difference between this and 'formats' can be seen in these examples:
--
-- >>> formats True True commaPrecStyle (Just 2) [0,1,1.01,1.02,1.1,1.2]
-- ["0.0","1.0","1.0","1.0","1.1","1.2"]
-- >>> distinguish 4 True True commaPrecStyle (Just 2) [0,1,1.01,1.02,1.1,1.2]
-- ["0.00","1.00","1.01","1.02","1.10","1.20"]
--
-- A common occurence is that significant figures being increased to enable textual uniqueness results in excess right zeros (after a decimal place). Consider:
--
-- >>> formats True False commaPrecStyle (Just 1) [0, 0.5, 1, 1.5, 2]
-- ["0.0","0.5","1.0","2.0","2.0"]
--
-- Note that formats seeks With 1.5 rounding up to 2, the distinguish algorithm will increase the number of sigfigs to 2:
--
-- >>> distinguish 4 True False commaPrecStyle (Just 1) [0, 0.5, 1, 1.5, 2]
-- ["0.00","0.50","1.00","1.50","2.00"]
--
-- The format can be simplified further by removing the excess right zeros from each formatted number:
--
-- >>> distinguish 4 True True commaPrecStyle (Just 2) [0, 0.5, 1, 1.5, 2]
-- ["0.0","0.5","1.0","1.5","2.0"]
distinguish ::
  -- | maximum number of iterations
  Int ->
  -- | left pad to the largest text length
  Bool ->
  -- | try and reduce excess right zero pads
  Bool ->
  -- | style
  (Double -> FormatStyle) ->
  -- | significant figures requested
  Maybe Int ->
  -- | list of numbers
  [Double] ->
  [Text]
distinguish maxi pad cutr f n xs =
  case n of
    Nothing -> formats pad cutr f Nothing xs
    Just n0 -> loopSF n0
  where
    loopSF n' = bool (loopSF (1 + n')) s (s == nubOrd s || n' > maxi)
      where
        s = formats pad cutr f (Just n') xs

-- | Wrapper for the various formatting options.
--
-- >>> defaultFormatN
-- FormatN {fstyle = FSCommaPrec, sigFigs = Just 2, maxDistinguishIterations = 4, addLPad = True, cutRightZeros = True}
data FormatN
  = FormatN
  { fstyle :: FStyle,
    sigFigs :: Maybe Int,
    maxDistinguishIterations :: Int,
    addLPad :: Bool,
    cutRightZeros :: Bool
  }
  deriving (Eq, Show, Generic, Data)

-- | The official FormatN
defaultFormatN :: FormatN
defaultFormatN = FormatN FSCommaPrec (Just 2) 4 True True

-- | run a 'FormatN'
--
-- >>> formatN defaultFormatN 1234
-- "1,200"
formatN :: FormatN -> Double -> Text
formatN fn x = case fstyle fn of
  FSDecimal -> format DecimalStyle (sigFigs fn) x
  (FSExponent n) -> format (ExponentStyle n) (sigFigs fn) x
  FSComma -> format CommaStyle (sigFigs fn) x
  (FSFixed n) -> format (FixedStyle n) (sigFigs fn) x
  FSPercent -> format PercentStyle (sigFigs fn) x
  FSDollar -> format DollarStyle (sigFigs fn) x
  FSPrec -> format (precStyle x) (sigFigs fn) x
  FSCommaPrec -> format (commaPrecStyle x) (sigFigs fn) x
  FSNone -> pack (show x)

-- | Consistently format a list of numbers via using 'distinguish'.
--
-- >>> formatNs defaultFormatN [0,1,1.01,1.02,1.1,1.2]
-- ["0.00","1.00","1.01","1.02","1.10","1.20"]
formatNs :: FormatN -> [Double] -> [Text]
formatNs (FormatN FSDecimal sf maxi pad cutr) x = distinguish maxi pad cutr (const DecimalStyle) sf x
formatNs (FormatN (FSExponent n) sf maxi pad cutr) x = distinguish maxi pad cutr (const (ExponentStyle n)) sf x
formatNs (FormatN FSComma sf maxi pad cutr) x = distinguish maxi pad cutr (const CommaStyle) sf x
formatNs (FormatN (FSFixed n) sf maxi pad cutr) x = distinguish maxi pad cutr (const (FixedStyle n)) sf x
formatNs (FormatN FSPercent sf maxi pad cutr) x = distinguish maxi pad cutr (const PercentStyle) sf x
formatNs (FormatN FSDollar sf maxi pad cutr) x = distinguish maxi pad cutr (const DollarStyle) sf x
formatNs (FormatN FSPrec sf maxi pad cutr) x = distinguish maxi pad cutr precStyle sf x
formatNs (FormatN FSCommaPrec sf maxi pad cutr) x = distinguish maxi pad cutr commaPrecStyle sf x
formatNs (FormatN FSNone _ _ pad _) x = bool id lpads pad $ pack . show <$> x

-- | Format with the shorter of show and a style.
--
-- >>> format (ExponentStyle Nothing) Nothing 0
-- "0e0"
-- >>> formatOrShow (ExponentStyle Nothing) Nothing 0
-- "0"
formatOrShow :: FormatStyle -> Maybe Int -> Double -> Text
formatOrShow f n x = bool (bool f' (pack s') (Text.length (pack s') < Text.length f')) "0" (x < 1e-6 && x > -1e-6)
  where
    f' = format f n x
    s' = show x
