{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Main where

import Language.Haskell.TH.DeepStrict.Golden
import Test.Tasty
import Data.Proxy
import Data.HashMap.Strict (HashMap)
import Data.Tree (Tree)
import GHC.Exts

data StrictList a = SCons !a !(StrictList a) | SNill

data Tricky = Tricky !Int (HashMap Int Int)

newtype Identity# :: forall (r :: RuntimeRep). TYPE r -> TYPE r where
  MkIdentity# :: forall (r :: RuntimeRep) (a :: TYPE r). a -> Identity# a

data family Strict x
data instance Strict (a, b) = StrictPair !a !b
data instance Strict (Maybe a) = StrictJust !a | StrictNothing

type family Strict' x
type instance Strict' [x] = StrictList x

data EmbeddedDataFam = EmbeddedDataFam !Int !(Strict (Char, Int))
data EmbeddedDataFamFail = EmbeddedDataFamFail !Int !(Strict (Char, Maybe Int))

$(pure [])

main :: IO ()
main = defaultMain spec

spec :: TestTree
spec = testGroup "golden tests"
  [ testGroup "simple types"
    [ $(testType =<< [t|Bool|])
    , $(testType =<< [t|Maybe Bool|])
    , $(testType =<< [t|Int|])
    , $(testType =<< [t|Integer|])
    , $(testType =<< [t|Rational|])
    , $(testType =<< [t|[Bool]|])
    , $(testType =<< [t|(Bool, Bool)|])
    , $(testType =<< [t|()|])
    , $(testType =<< [t|Proxy Bool|])
    , $(testType =<< [t|StrictList ()|])
    , $(testType =<< [t|Tree Bool|])
    , $(testType =<< [t| HashMap () ()|])
    ]
  , testGroup "unlifted"
    [ $(testType =<< [t|Int#|])
    , $(testType =<< [t|ByteArray#|])
    , $(testType =<< [t|Identity# Int#|])
    , $(testType =<< [t|Identity# (Maybe Int)|])
    , $(testType =<< [t|(# Int#, Int# #)|])
    , $(testType =<< [t| Identity# (# Int#, Int# #)|])
    , $(testType =<< [t| (# Int# | Int# #)|])
    , $(testType =<< [t| (# Int# | Maybe Int #)|])
    ]
  , testGroup "regresion tests"
    [ $(testType =<< [t|Tricky|]) ]
  , testGroup "data families"
    [ $(testType =<< [t|Strict (Int, Int)|])
    , $(testType =<< [t|Strict (Maybe Char)|])
    , $(testType =<< [t|EmbeddedDataFam|])
    , $(testType =<< [t|EmbeddedDataFamFail|])
    , $(testType =<< [t|Strict' [Int]|])
    ]
  ]
