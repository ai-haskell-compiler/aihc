{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main ( main ) where

import           Data.Char
import           Data.Int
import           Data.List
import           Data.String
import           GHC.Generics
import           Numeric               hiding (showHex, showOct)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           Text.Format


main :: IO ()
main = hspec $ do
  describe "ArgKey" $ do
    it "read and show empty arg key" $
      let key = Index (-1) in show key == "" && key == read ""
    prop "read and show none empy arg key" $ \key -> checkKey key (show key)

  describe "ArgFmt" $ do
    prop "read, show and pretty" $ \fmt ->
      let raw = prettyArgFmt fmt in fmt{fmtRaw=raw} == read raw

  describe "Format" $ do
    prop "normal way" $
      uncurry (==) . formatOrdered "{0:c}{1:g}{2:g}{3:d}{4:d}{5:d}{6:s}"
    prop "all keys are omitted" $
      uncurry (==) . formatOrdered "{:c}{:g}{:g}{:d}{:d}{:d}{:s}"
    prop "some keys are omitted" $
      uncurry (==) . formatOrdered "{:c}{:g}{:g}{:d}{:d}{5:d}{6:s}"
    prop "keys are disordered" $ uncurry (==) . formatDisordered
    prop "arg format is omitted" $
      uncurry (==) . formatOrdered "{:c}{}{}{}{}{}{}"
    prop "generic" $ uncurry (==) . formatGeneric
    it "escaped brace" $
      format "{{" == ['{'] && format "}}" == ['}']
    prop "alignment and padding" $ \(align, PadChar c, n) ->
      uncurry (==) (formatAlign align c $ max 3 n)
    prop "number sign" $ \(sign, n) -> uncurry (==) (formatSign sign n)
    prop "number alternate form" $
      \(alter, n) -> uncurry (==) (formatAlter alter n)
    prop "number sign aware" $ \n -> uncurry (==) (formatSignAware n)
    prop "max width" $ \cs -> format "{:.10s}" cs == take 10 (cs :: String)
    prop "number separation" $
      \(sep, alter, n) -> uncurry (==) (formatSep sep alter n)
    prop "number precision" $
      \(NonNegative p, n) -> uncurry (==) (formatPrec p n)


instance Arbitrary ArgKey where
  arbitrary = sized $ genKey . min 10 . max 1
    where
      indexedKey = (Index . getNonNegative) <$> arbitrary
      namedKey = sized $ \n ->
        Name <$> (sequence $ replicate (max 1 n) arbitraryUnicodeChar)

      genKey 1 = oneof [indexedKey, namedKey]
      genKey n = Nest <$> (genKey 1) <*> (genKey (n - 1))


instance Arbitrary FmtAlign where
  arbitrary = elements [AlignLeft, AlignRight, AlignCenter, AlignSign]


instance Arbitrary FmtSign where
  arbitrary = elements [SignPlus, SignMinus, SignSpace]


instance Arbitrary FmtNumSep where
  arbitrary = elements [NumSepDash, NumSepComma]


instance Arbitrary ArgFmt where
  arbitrary = do
      (fmtPad, fmtAlign) <- genAlign
      fmtSign <- oneof [return SignNone, arbitrary]
      fmtAlternate <- choose (True, False)
      fmtSignAware <- choose (True, False)
      fmtWidth <- genWidth
      fmtNumSep <- oneof [return NumSepNone, arbitrary]
      fmtPrecision <- genWidth
      fmtSpecs <- getSpecs <$> arbitrary
      return $ let fmtRaw = "" in ArgFmt{..}
    where
      genAlign :: Gen (Char, FmtAlign)
      genAlign = do
        align <- oneof [return AlignNone, arbitrary]
        pad <- if align == AlignNone then return ' '
                                     else arbitraryPrintableChar
        return (pad, align)

      genWidth :: Gen (Either Int ArgKey)
      genWidth = oneof [ (Left . getNonNegative) <$> arbitrary
                       , Right <$> arbitrary
                       ]


newtype Specs = Specs { getSpecs :: String } deriving Show

instance Arbitrary Specs where
  arbitrary = do
    cs <- elements [ "", "s", "%Y-%m-%d", "b", "c", "d", "o", "x", "X"
                   , "e", "E", "f", "F", "g", "G", "%"
                   ]
    return $ Specs cs


newtype EmptyArgKey = EmptyArgKey { getEmptyArgKey :: ArgKey } deriving Show

instance Arbitrary EmptyArgKey where
  arbitrary = return $ EmptyArgKey $ Index (-1)


data BasicArgs = BasicArgs { char    :: Char
                           , double  :: Double
                           , float   :: Float
                           , integer :: Integer
                           , int     :: Int
                           , word    :: Word
                           , string  :: String
                           } deriving (Generic, Show)

instance FormatArg BasicArgs

instance Arbitrary BasicArgs where
  arbitrary = BasicArgs <$> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary


data Color = Red | Yellow | Blue deriving (Generic, Show)

instance FormatArg Color


data Flag = Flag Color Int Int deriving (Generic, Show)

instance FormatArg Flag


data Country = Country { _name :: String
                       , _flag :: Flag
                       } deriving (Generic, Show)

instance FormatArg Country where
  formatArg = genericFormatArg $ defaultOptions { fieldLabelModifier = drop 1 }

instance Arbitrary Country where
  arbitrary = Country <$> genName <*> genFlag
    where
      genName = (:[]) <$> elements ['A'..'Z']
      genFlag = Flag <$> genColor <*> arbitrary <*> arbitrary
      genColor = elements [Red, Yellow, Blue]


data PadChar = PadChar { getPadChar :: Char } deriving Show

instance Arbitrary PadChar where
  arbitrary = do
    c <- suchThat arbitraryUnicodeChar $ not . (`elem` ['{', '}'])
    return $ PadChar c


data Alternate = Bin | Oct | Hex | HexU deriving (Show, Eq)

instance Arbitrary Alternate where
  arbitrary = elements [Bin, Oct, Hex, HexU]


mkSign :: (Num a, Ord a) => a -> String -> String -> String
mkSign n minus other = if n < 0 then minus else other


showBin, showOct, showHex :: Int -> String
showBin n = showIntAtBase 2 intToDigit (abs n) ""
showOct n = showIntAtBase 8 intToDigit (abs n) ""
showHex n = showIntAtBase 16 intToDigit (abs n) ""


checkKey :: ArgKey -> String -> Bool
checkKey k@(Index _) cs = show k == cs && k == read cs
checkKey k@(Name _)  cs = show k == cs && k == read cs
checkKey (Nest k1 k2) cs =
  let (cs1, cs2) = splitAt (length (show k1)) cs
  in checkKey k1 cs1 && head cs2 == '!' && checkKey k2 (tail cs2)


formatOrdered :: Format -> BasicArgs -> (String, String)
formatOrdered fs (BasicArgs{..}) =
    ( format fs char double float integer int word string
    , char : double' ++ float' ++ integer' ++ int' ++ word' ++ string
    )
  where
    double' = showGFloat (Just 6) double ""
    float' = showGFloat (Just 6) float ""
    integer' = show integer
    int' = show int
    word' = show word


formatDisordered :: BasicArgs -> (String, String)
formatDisordered (BasicArgs{..}) =
    ( format fs char double float integer int word string
    , string ++ integer' ++ float' ++ double' ++ int' ++ word' ++ [char]
    )
  where
    double' = showGFloat (Just 6) double ""
    float' = showGFloat (Just 6) float ""
    integer' = show integer
    int' = show int
    word' = show word
    fs = "{6:s}{3:d}{2:g}{1:g}{4:d}{5:d}{0:c}"


formatGeneric :: Country -> (String, String)
formatGeneric country@(Country{_flag=(Flag color width height), ..}) =
  ( format1 "{name} {flag!0} {flag!1} {flag!2}" country
  , format "{} {} {} {}" _name color width height
  )

formatAlign :: FmtAlign -> Char -> Int -> (String, String)
formatAlign align c n =
    ( format (fromString $ "{:" ++ [c] ++ (mkFmt align n) ++ "}") (-42 :: Int)
    , padding align (n - 3)
    )
  where
    mkFmt :: FmtAlign -> Int -> String
    mkFmt AlignLeft   n = '<' : (show n)
    mkFmt AlignRight  n = '>' : (show n)
    mkFmt AlignCenter n = '^' : (show n)
    mkFmt AlignSign   n = '=' : (show n)

    padding AlignLeft   n = "-42" ++ replicate n c
    padding AlignRight  n = replicate n c ++ "-42"
    padding AlignCenter n =
      let ps1 = replicate (n `div` 2) c
          ps2 = replicate (n - (n `div` 2)) c
      in ps1 ++ "-42" ++ ps2
    padding AlignSign   n = "-" ++ replicate n c ++ "42"


formatSign :: FmtSign -> Int -> (String, String)
formatSign SignMinus n = (format "{:-}" n, show n)
formatSign SignPlus  n = (format "{:+}" n, (mkSign n "" "+") ++ (show n))
formatSign SignSpace n = (format "{:< d}" n, (mkSign n "" " ") ++ (show n))


formatAlter :: Alternate -> Int -> (String, String)
formatAlter Bin n = (format "{:#b}" n, (mkSign n "-" "") ++ "0b" ++ (showBin n))
formatAlter Oct n = (format "{:#o}" n, (mkSign n "-" "") ++ "0o" ++ (showOct n))
formatAlter Hex n = (format "{:#x}" n, (mkSign n "-" "") ++ "0x" ++ (showHex n))
formatAlter HexU n =
  ( format "{:#X}" n, (mkSign n "-" "") ++ "0X" ++ (map toUpper $ showHex n))


formatSignAware :: Int8 -> (String, String)
formatSignAware n =
  let cs = if n == (-128) then "128" else show $ abs n
      len = length cs + if n < 0 then 1 else 0
  in (format "{:010}" n, (mkSign n "-" "") ++ (replicate (10 - len) '0') ++ cs)


formatSep :: FmtNumSep -> Maybe Alternate -> Int -> (String, String)
formatSep sep alter n =
    ( format (fromString ("{:" ++ (mkFmt alter) ++ "}")) n
    , (mkSign n "-" "") ++ (intercalate [sepc] $ split altern $ showIt alter n)
    )
  where
    sepc = if sep == NumSepDash then '_' else ','
    altern = maybe 3 (const 4) alter

    showIt :: Maybe Alternate -> Int -> String
    showIt (Just Bin) = showBin . abs
    showIt (Just Oct) = showOct . abs
    showIt (Just _)   = showHex . abs
    showIt  _         = show . abs

    mkFmt :: Maybe Alternate -> String
    mkFmt (Just Bin) = mkFmt Nothing ++ "b"
    mkFmt (Just Oct) = mkFmt Nothing ++ "o"
    mkFmt (Just _)   = mkFmt Nothing ++ "x"
    mkFmt _          = [sepc]

    splitAtEnd :: Int -> [a] -> ([a], [a])
    splitAtEnd n cs =
      let (cs1, cs2) = splitAt n (reverse cs) in (reverse cs2, reverse cs1)

    split :: Int -> [a] -> [[a]]
    split _ [] = []
    split n cs = let (cs1, cs2) = splitAtEnd n cs in split n cs1 ++ [cs2]


formatPrec :: Int -> Double -> (String, String)
formatPrec p n =
  ( format (fromString $ "{:." ++ (if p == 0 then "" else show p) ++ "}") n
  , showGFloat (Just p) n ""
  )
