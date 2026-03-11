{-# LANGUAGE CPP               #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

import Criterion      ( Benchmark, bench, bgroup, env, nf )
import Criterion.Main ( defaultMain )

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.String          as S
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as LT

import qualified "string-interpolate" Data.String.Interpolate            as SI
import qualified "string-interpolate" Data.String.Interpolate.Conversion as SI
import qualified "interpolate" Data.String.Interpolate.IsString          as I
import           "formatting" Formatting                                 ( (%) )
import qualified "formatting" Formatting                                 as F
import qualified "formatting" Formatting.ShortFormatters                 as F
import qualified "neat-interpolation" NeatInterpolation                  as NI

import Control.DeepSeq

import Test.QuickCheck

#ifdef EXTENDED_BENCHMARKS
import "Interpolation" Data.String.Interpolation                as N
import "interpolatedstring-perl6" Text.InterpolatedString.Perl6 as P
#endif

type SIInterpolatable str flag =
  ( SI.IsCustomSink str ~ flag
  , SI.InterpSink flag str
  , SI.Interpolatable flag str str
  , SI.Interpolatable flag Int str
  , SI.Interpolatable flag Bool str
  )

type AllInterpolatable str flag =
  ( SIInterpolatable str flag
  , Show str
  , S.IsString str
  , Monoid str
  )

--------------------
-- string-interpolate
--------------------

singleInterpSI :: SIInterpolatable str flag => str -> str
singleInterpSI str = [SI.i|A fine day to die, #{str}.|]

multiInterpSI :: SIInterpolatable str flag => (Int, str, Bool) -> str
multiInterpSI (x, y, z) = [SI.i| foo #{x} bar #{y} baz #{z} quux |]

--------------------
-- interpolate
--------------------

singleInterpI :: (Show str, S.IsString str) => str -> str
singleInterpI str = [I.i|A fine day to die, #{str}.|]

multiInterpI :: (Show str, S.IsString str) => (Int, str, Bool) -> str
multiInterpI (x, y, z) = [I.i| foo #{x} bar #{y} baz #{z} quux |]

--------------------
-- formatting
--------------------

stringF :: String -> String
stringF = F.formatToString ("A fine day to die, " % F.s % ".")

multiStringF :: (Int, String, Bool) -> String
multiStringF (x, y, z) =
  F.formatToString (" foo " % F.d % " bar " % F.s % " baz " % F.sh % " quux ") x y z

textF :: T.Text -> T.Text
textF = F.sformat ("A fine day to die, " % F.st % ".")

multiTextF :: (Int, T.Text, Bool) -> T.Text
multiTextF (x, y, z) =
  F.sformat (" foo " % F.d % " bar " % F.st % " baz " % F.sh % " quux ") x y z

lazyTextF :: LT.Text -> LT.Text
lazyTextF = F.format ("A find day to die, " % F.t % ".")

multiLazyTextF :: (Int, LT.Text, Bool) -> LT.Text
multiLazyTextF (x, y, z) =
  F.format (" foo " % F.d % " bar " % F.t % " baz " % F.sh % " quux ") x y z

--------------------
-- neat-interpolation
--------------------

textNI :: T.Text -> T.Text
textNI t = [NI.text|A fine day to die, $t.|]

multiTextNI :: (Int, T.Text, Bool) -> T.Text
multiTextNI (x, y, z) =
  let x' = T.pack $ show x
      z' = T.pack $ show z
  in [NI.text| foo $x' bar $y baz $z' quux |]

#ifdef EXTENDED_BENCHMARKS

--------------------
-- Interpolation
--------------------

singleInterpN :: (Monoid str, S.IsString str) => str -> str
singleInterpN t = [str|A fine day to die, $t$.|]

multiInterpN ::(Monoid str, S.IsString str) => (Int, str, Bool) -> str
multiInterpN (x, y, z) = [str| foo $:x$ bar $y$ baz $:z$ quux |]

--------------------
-- interpolatedstring-perl6
--------------------

singleInterpP :: (Monoid str, S.IsString str) => str -> str
singleInterpP t = [qc|A fine day to die, {t}.|]

multiInterpP :: (Monoid str, S.IsString str) => (Int, str, Bool) -> str
multiInterpP (x, y, z) = [qc| foo {x} bar {y} baz {z} quux |]

#endif

--------------------
-- BENCHMARK GROUPS
--------------------

singleInterpBenches :: AllInterpolatable str flag
                    => [(String, (str -> str))]
singleInterpBenches =
  [ ("string-interpolate"      , singleInterpSI)
  , ("interpolate"             , singleInterpI)
#ifdef EXTENDED_BENCHMARKS
  , ("interpolatedstring-perl6", singleInterpP)
  , ("Interpolation"           , singleInterpN)
#endif
  ]

multiInterpBenches :: AllInterpolatable str flag
                   => [(String, ((Int, str, Bool) -> str))]
multiInterpBenches =
  [ ("string-interpolate"      , multiInterpSI)
  , ("interpolate"             , multiInterpI)
#ifdef EXTENDED_BENCHMARKS
  , ("interpolatedstring-perl6", multiInterpP)
  , ("Interpolation"           , multiInterpN)
#endif
  ]

main :: IO ()
main = defaultMain $
  [ benches @String "Small Strings Bench" "William" $
      singleInterpBenches ++
        [ ("formatting", stringF) ]
  , benches @T.Text "Small Text Bench" "William" $
      singleInterpBenches ++
        [ ("formatting"        , textF)
        , ("neat-interpolation", textNI)
        ]
  , benches @LT.Text "Small Lazy Text Bench" "William" $
      singleInterpBenches ++
        [ ("formatting", lazyTextF) ]
  , benches @B.ByteString "Small ByteStrings Bench" "William" $
      singleInterpBenches
  , benches @LB.ByteString "Small Lazy ByteStrings Bench" "William" $
      singleInterpBenches
  , benches @String "Multiple Interpolations String Bench" (42, "CATALLAXY", True) $
      multiInterpBenches ++
        [ ("formatting", multiStringF) ]
  , benches @T.Text "Multiple Interpolations Text Bench" (42, "CATALLAXY", True) $
      multiInterpBenches ++
        [ ("formatting"        , multiTextF)
        , ("neat-interpolation", multiTextNI)
        ]
  , benches @LT.Text "Multiple Interpolations Lazy Text Bench" (42, "CATALLAXY", True) $
      multiInterpBenches ++
        [ ("formatting", multiLazyTextF) ]
  , benches @B.ByteString "Multiple Interpolations ByteString Bench" (42, "CATALLAXY", True) $
      multiInterpBenches
  , benches @LB.ByteString "Multiple Interpolations Lazy ByteString Bench" (42, "CATALLAXY", True) $
      multiInterpBenches
  , env largeishText $ \ ~t -> benches @T.Text "Largeish Text Bench" t $
      singleInterpBenches ++
        [ ("formatting"        , textF)
        , ("neat-interpolation", textNI)
        ]
  , env largeishLazyText $ \ ~lt -> benches @LT.Text "Largeish Lazy Text Bench" lt $
      singleInterpBenches ++
        [ ("formatting", lazyTextF) ]
  , env largeishByteString $ \ ~bs -> benches @B.ByteString "Largeish ByteString Bench" bs $
      singleInterpBenches
  , env largeishLazyByteString $ \ ~lbs -> benches @LB.ByteString "Largeish Lazy ByteString Bench" lbs $
      singleInterpBenches
  ]

largeishText :: IO T.Text
largeishText =
  generate $ T.pack <$> Prelude.take 100000 <$> infiniteListOf arbitrary

largeishLazyText :: IO LT.Text
largeishLazyText =
  generate $ LT.pack <$> Prelude.take 100000 <$> infiniteListOf arbitrary

largeishByteString :: IO B.ByteString
largeishByteString =
  generate $ B.pack <$> Prelude.take 100000 <$> infiniteListOf arbitrary

largeishLazyByteString :: IO LB.ByteString
largeishLazyByteString =
  generate $ LB.pack <$> Prelude.take 100000 <$> infiniteListOf arbitrary

--------------------
-- BENCHMARK UTIL
--------------------

benches :: forall b a. NFData b => String -> a -> [(String, a -> b)] -> Benchmark
benches groupname arg fs = bgroup groupname (fmap benchF fs)
  where benchF (bname, f) = bench bname $ nf f arg
