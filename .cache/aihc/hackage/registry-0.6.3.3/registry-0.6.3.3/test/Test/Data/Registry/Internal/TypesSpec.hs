{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.Data.Registry.Internal.TypesSpec where

import Data.List.NonEmpty
import Data.Registry.Internal.Types
import qualified Type.Reflection as R
import Protolude as P

import Test.Tasty.Extensions

test_specialized_context_order = prop "there are preferable specializations than others in a given context" $ do
  let c1 = Context (fmap (\t -> (t, Nothing)) $ [f, e, d, c, b, a])
  let s1 = specializationRange c1 (Specialization (a :| [c]) (UntypedValue $ createValue A))
  let s2 = specializationRange c1 (Specialization (a :| [e]) (UntypedValue $ createValue A))
  let s3 = specializationRange c1 (Specialization (c :| [f]) (UntypedValue $ createValue A))
  let s4 = specializationRange c1 (Specialization (b :| [f]) (UntypedValue $ createValue A))
  let s5 = specializationRange c1 (Specialization (pure c) (UntypedValue $ createValue A))
  let s6 = specializationRange c1 (Specialization (pure f) (UntypedValue $ createValue A))

  (s2 < s1) === True
  (s3 < s1) === True
  (s4 < s1) === True
  (s3 < s2) === True
  (s3 < s4) === True
  (s1 < s5) === True
  (s6 < s5) === True

data A = A deriving (Eq, Show)

data B = B deriving (Eq, Show)

data C = C deriving (Eq, Show)

data D = D deriving (Eq, Show)

data E = E deriving (Eq, Show)

data F = F deriving (Eq, Show)

a = R.someTypeRep $ R.typeOf A

b = R.someTypeRep $ R.typeOf B

c = R.someTypeRep $ R.typeOf C

d = R.someTypeRep $ R.typeOf D

e = R.someTypeRep $ R.typeOf E

f = R.someTypeRep $ R.typeOf F
