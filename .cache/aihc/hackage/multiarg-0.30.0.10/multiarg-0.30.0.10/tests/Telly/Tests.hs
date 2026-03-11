{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE TemplateHaskell #-}
module Telly.Tests where

import Multiarg.Examples.Telly
import Test.QuickCheck
import Control.Applicative
import Ernie
import Multiarg.Internal
import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.QuickCheck

tests :: TestTree
tests = $(testGroupGenerator)

-- | Generates any option.
option :: Gen Telly
option = oneof
  [ return Empty
  , Single <$> optArg
  , Double <$> optArg <*> optArg
  , Triple <$> optArg <*> optArg <*> optArg

  , return Zero
  , One <$> optArg
  , Two <$> optArg <*> optArg
  , Three <$> optArg <*> optArg <*> optArg

  , return Cero
  , Uno <$> optArg
  , Dos <$> optArg <*> optArg
  , Tres <$> optArg <*> optArg <*> optArg
  ]

tellyToNestedList :: Telly -> [[String]]
tellyToNestedList telly = case telly of
  PosArg s -> [[s]]
  Empty -> long "empty" [] ++ short 'e' []
  Single s -> long "single" [s] ++ short 's' [s]
  Double s1 s2 -> long "double" [s1, s2] ++ short 'd' [s1, s2]
  Triple s1 s2 s3 -> long "triple" [s1, s2, s3]
    ++ short 't' [s1, s2, s3]

  Zero -> short '0' []
  One s -> short '1' [s]
  Two s1 s2 -> short '2' [s1, s2]
  Three s1 s2 s3 -> short '3' [s1, s2, s3]

  Cero -> long "cero" []
  Uno s -> long "uno" [s]
  Dos s1 s2 -> long "dos" [s1, s2]
  Tres s1 s2 s3 -> long "tres" [s1, s2, s3]

tellyToStrings :: Telly -> Gen [String]
tellyToStrings = pickItem . tellyToNestedList

validTellyStrings :: Gen ([Telly], [String])
validTellyStrings = do
  unneededStopper <- arbitrary
  (start, end) <- interspersedLine option PosArg
  let startStrings = map tellyToNestedList start
      endStrings = map tellyToNestedList end
  startList <- fmap concat $ mapM pickItem startStrings
  endList <- fmap concat $ mapM pickItem endStrings
  let endList'
        | null end && not unneededStopper = endList
        | otherwise = "--" : endList
  return (start ++ end, startList ++ endList')

prop_parseStringsYieldsTellies
  = forAll validTellyStrings $ \(tellies, strings) ->
  let ParsedCommandLine ls _
        = parseCommandLinePure optSpecs PosArg strings
  in map Right tellies === ls

prop_parseStringsYieldsNoEndError
  = forAll validTellyStrings $ \(_, strings) ->
  let ParsedCommandLine _ mayOpt
        = parseCommandLinePure optSpecs PosArg strings
  in mayOpt === Nothing
