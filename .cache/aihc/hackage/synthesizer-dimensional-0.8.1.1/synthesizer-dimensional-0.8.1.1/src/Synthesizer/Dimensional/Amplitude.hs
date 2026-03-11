module Synthesizer.Dimensional.Amplitude where

import qualified Number.DimensionTerm        as DN
-- import qualified Algebra.DimensionTerm       as Dim

{- |
Can be used as amplitude value for enumeration types
such as 'Bool' and 'Ordering'
and other types, where a numeric amplitude makes no sense.
It is essential in 'Synthesizer.Dimensional.Causal.Process.T'.
It would be a bad idea to omit the @Abstract@ parameter
in dimensional causal processes
since the correspondence between amplitude type and sample type would be lost.
-}
data Abstract = Abstract

newtype Numeric amp = Numeric amp

instance Functor Numeric where
   fmap f (Numeric amp) = Numeric $ f amp

type Dimensional v y = Numeric (DN.T v y)

{- |
@Flat y@ is quite the same as @Dimensional Dim.Scalar y@
but in some cases it allows a little more efficient processing.
It should not be mixed up with @Abstract@.
@Flat y@ is reserved for numeric amplitudes.
-}
data Flat y = Flat

{- |
This class is used to make 'Synthesizer.Dimensional.Causal.Process.mapAmplitude'
both flexible and a bit safe.
Its instances are dimensional numbers 'Numeric' and 'Abstract'.
It should not be necessary to add more instances.
-}
class C amp where

instance C Abstract where

instance C (Flat y) where

instance C (Numeric amp) where

-- instance Dim.C v => C (DN.T v y) where

{- |
This class is used for 'Synthesizer.Dimensional.Rate.Cut.append'
and 'Synthesizer.Dimensional.Amplitude.Displacement.map'
that expect that the amplitude value
does carry not more information than that expressed by the type.
It should not be necessary to add more instances.
-}
class Primitive amp where
   primitive :: amp

instance Primitive Abstract where
   primitive = Abstract

instance Primitive (Flat y) where
   primitive = Flat
