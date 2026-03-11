{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}
module Data.Array.Comfort.Shape (
   C(..),
   Indexed(..),
   InvIndexed(..), messageIndexFromOffset, assertIndexFromOffset,
   Static(..),
   Pattern(..),
   AppendSemigroup(..),
   AppendMonoid(..),

   requireCheck,
   CheckSingleton(..),
   Checking(..),
   Result(..),
   runChecked,
   runUnchecked,
   assert,
   throwOrError,

   Zero(Zero),
   ZeroBased(..), zeroBasedSplit,
   OneBased(..),

   Range(..),
   Shifted(..),
   Enumeration(..),
   Deferred(..), DeferredIndex(..), deferIndex, revealIndex,

   (::+)(..),

   Square(..), cartesianFromSquare,
   Cube(..), cartesianFromCube,

   Triangular(..), Lower(Lower), Upper(Upper),
   LowerTriangular, UpperTriangular,
   lowerTriangular, upperTriangular,
   triangleSize, triangleRoot,

   Simplex(..),
   SimplexAscending, simplexAscending,
   SimplexDescending, simplexDescending,
   Ascending,
   Descending,
   SimplexOrder(..),
   SimplexOrderC,
   AllDistinct(..),
   SomeRepetitive(..),
   Collision(..),
   CollisionC,

   Cyclic(..),

   NestedTuple(..),
   AccessorTuple(..),
   StaticTuple(..),
   Element(..),
   TupleAccessor,
   TupleIndex,

   ElementIndex,
   ElementTuple(..),
   indexTupleFromShape,

   Record(..),
   FieldIndex,
   indexRecordFromShape,

   Constructed,
   ConsIndex,
   Construction,
   construct,
   consIndex,
   ) where

import qualified Data.Array.Comfort.Shape.Set as ShapeSet
import Data.Array.Comfort.Shape.Utility (messageIndexFromOffset, isRight)

import qualified Foreign.Storable.Newtype as Store
import Foreign.Storable
         (Storable, sizeOf, alignment, poke, peek, pokeElemOff, peekElemOff)
import Foreign.Ptr (Ptr, castPtr)

import qualified GHC.Arr as Ix

import qualified Control.Monad.Trans.Writer as MW
import qualified Control.Monad.Trans.State as MS
import qualified Control.Monad.HT as Monad
import qualified Control.Applicative.HT as App
import qualified Control.Applicative.Backwards as Back
import Control.DeepSeq (NFData, rnf)
import Control.Monad (liftM)
import Control.Applicative (Applicative, pure, (<*>), (<$>))
import Control.Applicative (Const(Const, getConst))
import Control.Functor.HT (void)

import qualified Data.Functor.Classes as FunctorC
import qualified Data.Traversable as Trav
import qualified Data.Foldable as Fold
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.NonEmpty as NonEmpty
import qualified Data.List.Match as Match
import qualified Data.List.HT as ListHT
import qualified Data.List as List
import Data.Traversable (Traversable)
import Data.Foldable (Foldable)
import Data.Functor.Identity (Identity(Identity), runIdentity)
import Data.Monoid (Sum(Sum, getSum))
import Data.Function.HT (compose2)
import Data.Tagged (Tagged(Tagged, unTagged))
import Data.Complex (Complex((:+)), realPart, imagPart)
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Map (Map)
import Data.Set (Set)
import Data.List.HT (tails)
import Data.Tuple.HT (mapFst, mapSnd, swap, fst3, snd3, thd3)
import Data.Eq.HT (equating)

import Text.Printf (printf)


{- $setup
>>> import qualified Data.Array.Comfort.Shape as Shape
>>> import qualified Data.IntMap as IntMap
>>> import qualified Data.IntSet as IntSet
>>> import qualified Data.Map as Map
>>> import qualified Data.Set as Set
>>> import Data.Array.Comfort.Shape ((::+)((::+)))
>>>
>>> import Test.ChasingBottoms.IsBottom (isBottom)
>>> import Control.DeepSeq (rnf)
-}


data Checked
data Unchecked

class Checking check where
   data Result check a
   switchCheck :: f Checked -> f Unchecked -> f check

data CheckSingleton check where
   Checked :: CheckSingleton Checked
   Unchecked :: CheckSingleton Unchecked

autoCheck :: (Checking check) => CheckSingleton check
autoCheck = switchCheck Checked Unchecked

checkFromResult :: (Checking check) => Result check a -> CheckSingleton check
checkFromResult _ = autoCheck

withCheck ::
   (Checking check) =>
   (CheckSingleton check -> Result check a) -> Result check a
withCheck f = f autoCheck

requireCheck :: CheckSingleton check -> Result check a -> Result check a
requireCheck _ = id


instance Checking Checked where
   newtype Result Checked a = CheckedResult {getChecked :: Either String a}
   switchCheck f _ = f

runChecked :: String -> Result Checked a -> a
runChecked name (CheckedResult m) =
   either (error . (("Shape." ++ name ++ ": ") ++)) id m

instance Checking Unchecked where
   newtype Result Unchecked a = UncheckedResult {getUnchecked :: a}
   switchCheck _ f = f

runUnchecked :: Result Unchecked a -> a
runUnchecked = getUnchecked


throw :: String -> Result Checked a
throw = CheckedResult . Left

throwOrError :: (Checking check) => String -> Result check a
throwOrError msg = withCheck $ \check ->
   case check of
      Checked -> throw msg
      Unchecked -> error msg

assert :: (Checking check) => String -> Bool -> Result check ()
assert msg cond = withCheck $ \check ->
   case check of
      Unchecked -> UncheckedResult ()
      Checked -> if cond then pure () else throw msg


instance (Checking check, Eq a) => Eq (Result check a) where
   a0 == b0 =
      case (checkFromResult a0, a0, b0) of
         (Checked, CheckedResult a, CheckedResult b)  ->  a==b
         (Unchecked, UncheckedResult a, UncheckedResult b)  ->  a==b

instance (Checking check) => Functor (Result check) where
   fmap f m =
      case (checkFromResult m, m) of
         (Checked, CheckedResult e) -> CheckedResult $ fmap f e
         (Unchecked, UncheckedResult a) -> UncheckedResult $ f a

instance (Checking check) => Applicative (Result check) where
   pure a = withCheck $ \check ->
      case check of
         Checked -> CheckedResult $ Right a
         Unchecked -> UncheckedResult a
   f<*>a =
      case (checkFromResult a, f, a) of
         (Checked, CheckedResult ff, CheckedResult fa) ->
            CheckedResult $ ff<*>fa
         (Unchecked, UncheckedResult xf, UncheckedResult xa) ->
            UncheckedResult $ xf xa

instance (Checking check) => Monad (Result check) where
   return = pure
   a >>= b =
      case (checkFromResult a, a) of
         (Checked, CheckedResult e) -> CheckedResult $ getChecked . b =<< e
         (Unchecked, UncheckedResult x) -> b x


{- |
Shape types, that is, instances of 'C', that are also instance of 'Eq',
must have proper 'Eq' instances,
otherwise evil memory corruption will occur.
At least, it must hold @sh0 == sh1  ==>  Shape.size sh0 == Shape.size sh1@.
-}
class C sh where
   {-
   This is the counterpart to 'Ix.rangeSize'.
   We do not support a counterpart to 'Ix.unsafeRangeSize' anymore.
   First, there is hardly any speed advantage
   of using 'Ix.unsafeRangeSize' instead of 'Ix.rangeSize'.
   Second, I do not know of an 'Ix' instance
   where 'Ix.rangeSize' and 'Ix.unsafeRangeSize' differ.
   -}
   size :: sh -> Int

class C sh => Indexed sh where
   {-# MINIMAL indices, (unifiedOffset|unifiedSizeOffset) #-}
   type Index sh
   -- Ix.range
   indices :: sh -> [Index sh]
   -- Ix.index
   offset :: sh -> Index sh -> Int
   offset sh = runChecked "offset" . unifiedOffset sh
   -- Ix.unsafeIndex
   uncheckedOffset :: sh -> Index sh -> Int
   uncheckedOffset sh = getUnchecked . unifiedOffset sh
   unifiedOffset :: (Checking check) => sh -> Index sh -> Result check Int
   unifiedOffset sh = snd $ unifiedSizeOffset sh
   -- Ix.inRange
   inBounds :: sh -> Index sh -> Bool
   inBounds sh = isRight . getChecked . unifiedOffset sh

   sizeOffset :: sh -> (Int, Index sh -> Int)
   sizeOffset sh = (size sh, offset sh)
   uncheckedSizeOffset :: sh -> (Int, Index sh -> Int)
   uncheckedSizeOffset sh = (size sh, uncheckedOffset sh)
   unifiedSizeOffset ::
      (Checking check) => sh -> (Int, Index sh -> Result check Int)
   unifiedSizeOffset sh = (size sh, unifiedOffset sh)

class Indexed sh => InvIndexed sh where
   {-# MINIMAL unifiedIndexFromOffset #-}
   {- |
   It should hold @indexFromOffset sh k == indices sh !! k@,
   but 'indexFromOffset' should generally be faster.
   -}
   indexFromOffset :: sh -> Int -> Index sh
   indexFromOffset sh = runChecked "indexFromOffset" . unifiedIndexFromOffset sh
   uncheckedIndexFromOffset :: sh -> Int -> Index sh
   uncheckedIndexFromOffset sh = getUnchecked . unifiedIndexFromOffset sh
   unifiedIndexFromOffset ::
      (Checking check) => sh -> Int -> Result check (Index sh)

assertIndexFromOffset ::
   (Checking check) => String -> Int -> Bool -> Result check ()
assertIndexFromOffset name k cond = assert (messageIndexFromOffset name k) cond

class (C sh, Eq sh) => Static sh where
   static :: sh

{-
We need superclass Indexed for Index type function.
But this disables the sensible instance Pattern Zero.
-}
class (Indexed sh) => Pattern sh where
   type DataPattern sh x
   indexPattern :: (Index sh -> x) -> sh -> DataPattern sh x




{- |
We cannot use 'Semigroup'
because 'Semigroup' instances for '()' and '(a,b)' are already defined in a way,
that is incompatible for our needs.
-}
class (C sh) => AppendSemigroup sh where
   append :: sh -> sh -> sh

class (AppendSemigroup sh) => AppendMonoid sh where
   empty :: sh


data Zero = Zero
   deriving (Eq, Ord, Show)

instance C Zero where
   size Zero = 0

instance Static Zero where
   static = Zero

{-
missing superclass Indexed

instance Pattern Zero where
   type DataPattern Zero x = ()
   indexPattern _ Zero = ()
-}

instance AppendSemigroup Zero where
   append Zero Zero = Zero

instance AppendMonoid Zero where
   empty = Zero


instance C () where
   size () = 1

{- |
>>> Shape.indices ()
[()]
-}
instance Indexed () where
   type Index () = ()
   indices () = [()]
   unifiedOffset () () = pure 0
   inBounds () () = True

instance InvIndexed () where
   unifiedIndexFromOffset () k = assertIndexFromOffset "()" k (k==0)

instance Static () where
   static = ()

instance Pattern () where
   type DataPattern () x = x
   indexPattern extend = extend


{- |
'ZeroBased' denotes a range starting at zero and has a certain length.

>>> Shape.indices (Shape.ZeroBased (7::Int))
[0,1,2,3,4,5,6]
-}
newtype ZeroBased n = ZeroBased {zeroBasedSize :: n}
   deriving (Eq, Show)

instance Functor ZeroBased where
   fmap f (ZeroBased n) = ZeroBased $ f n

instance Applicative ZeroBased where
   pure = ZeroBased
   ZeroBased f <*> ZeroBased n = ZeroBased $ f n

instance (NFData n) => NFData (ZeroBased n) where
   rnf (ZeroBased n) = rnf n

instance (Storable n) => Storable (ZeroBased n) where
   sizeOf = Store.sizeOf zeroBasedSize
   alignment = Store.alignment zeroBasedSize
   peek = Store.peek ZeroBased
   poke = Store.poke zeroBasedSize

instance (Integral n) => C (ZeroBased n) where
   size (ZeroBased len) = fromIntegral len

instance (Integral n) => Indexed (ZeroBased n) where
   type Index (ZeroBased n) = n
   indices (ZeroBased len) = takeWhile (<len) $ iterate (+1) 0
   unifiedOffset (ZeroBased len) = unifiedOffset $ Shifted 0 len
   inBounds (ZeroBased len) ix = 0<=ix && ix<len

instance (Integral n) => InvIndexed (ZeroBased n) where
   unifiedIndexFromOffset (ZeroBased len) k0 = do
      let k = fromIntegral k0
      assertIndexFromOffset "ZeroBased" k0 $ 0<=k && k<len
      pure k

zeroBasedSplit :: (Real n) => n -> ZeroBased n -> ZeroBased n ::+ ZeroBased n
zeroBasedSplit n (ZeroBased m) =
   if n<0
      then error "Shape.zeroBasedSplit: negative number of elements"
      else let k = min n m in ZeroBased k ::+ ZeroBased (m-k)

instance (Integral n) => AppendSemigroup (ZeroBased n) where
   append (ZeroBased n) (ZeroBased m) = ZeroBased (n+m)

instance (Integral n) => AppendMonoid (ZeroBased n) where
   empty = ZeroBased 0


instance (Integral n) => Pattern (ZeroBased n) where
   type DataPattern (ZeroBased n) x = n -> x
   indexPattern extend (ZeroBased _n) = extend


{- |
'OneBased' denotes a range starting at one and has a certain length.

>>> Shape.indices (Shape.OneBased (7::Int))
[1,2,3,4,5,6,7]
-}
newtype OneBased n = OneBased {oneBasedSize :: n}
   deriving (Eq, Show)

instance Functor OneBased where
   fmap f (OneBased n) = OneBased $ f n

instance Applicative OneBased where
   pure = OneBased
   OneBased f <*> OneBased n = OneBased $ f n

instance (NFData n) => NFData (OneBased n) where
   rnf (OneBased n) = rnf n

instance (Storable n) => Storable (OneBased n) where
   sizeOf = Store.sizeOf oneBasedSize
   alignment = Store.alignment oneBasedSize
   peek = Store.peek OneBased
   poke = Store.poke oneBasedSize

instance (Integral n) => C (OneBased n) where
   size (OneBased len) = fromIntegral len

instance (Integral n) => Indexed (OneBased n) where
   type Index (OneBased n) = n
   indices (OneBased len) = takeWhile (<=len) $ iterate (+1) 1
   unifiedOffset (OneBased len) = unifiedOffset $ Shifted 1 len
   inBounds (OneBased len) ix = 0<ix && ix<=len

instance (Integral n) => InvIndexed (OneBased n) where
   unifiedIndexFromOffset (OneBased len) k0 = do
      let k = fromIntegral k0
      assertIndexFromOffset "OneBased" k0 $ 0<=k && k<len
      pure $ 1+k

instance (Integral n) => AppendSemigroup (OneBased n) where
   append (OneBased n) (OneBased m) = OneBased (n+m)

instance (Integral n) => AppendMonoid (OneBased n) where
   empty = OneBased 0


{- |
'Range' denotes an inclusive range like
those of the Haskell 98 standard @Array@ type from the @array@ package.
E.g. the shape type @(Range Int32, Range Int64)@
is equivalent to the ix type @(Int32, Int64)@ for @Array@s.

>>> Shape.indices (Shape.Range (-5) (5::Int))
[-5,-4,-3,-2,-1,0,1,2,3,4,5]
>>> Shape.indices (Shape.Range (-1,-1) (1::Int,1::Int))
[(-1,-1),(-1,0),(-1,1),(0,-1),(0,0),(0,1),(1,-1),(1,0),(1,1)]
-}
data Range n = Range {rangeFrom, rangeTo :: n}
   deriving (Eq, Show)

instance Functor Range where
   fmap f (Range from to) = Range (f from) (f to)

instance (NFData n) => NFData (Range n) where
   rnf (Range from to) = rnf (from,to)

instance (Ix.Ix n) => C (Range n) where
   size (Range from to) = Ix.rangeSize (from,to)

instance (Ix.Ix n) => Indexed (Range n) where
   type Index (Range n) = n
   indices (Range from to) = Ix.range (from,to)
   offset (Range from to) ix = Ix.index (from,to) ix
   uncheckedOffset (Range from to) ix = Ix.unsafeIndex (from,to) ix
   unifiedOffset (Range from to) ix = do
      assert "Shape.Range: index out of range" $ Ix.inRange (from,to) ix
      return $ Ix.unsafeIndex (from,to) ix
   inBounds (Range from to) ix = Ix.inRange (from,to) ix

-- pretty inefficient when we rely solely on Ix
instance (Ix.Ix n) => InvIndexed (Range n) where
   unifiedIndexFromOffset (Range from to) k = do
      assertIndexFromOffset "Range" k $ 0<=k && k < Ix.rangeSize (from,to)
      return $ Ix.range (from,to) !! k

-- cf. sample-frame:Stereo
instance Storable n => Storable (Range n) where
   {-# INLINE sizeOf #-}
   {-# INLINE alignment #-}
   {-# INLINE peek #-}
   {-# INLINE poke #-}
   sizeOf ~(Range l r) = sizeOf l + mod (- sizeOf l) (alignment r) + sizeOf r
   alignment ~(Range l _) = alignment l
   poke p (Range l r) =
      let q = castToElemPtr p
      in  poke q l >> pokeElemOff q 1 r
   peek p =
      let q = castToElemPtr p
      in  Monad.lift2 Range (peek q) (peekElemOff q 1)


{- |
'Shifted' denotes a range defined by the start index and the length.

>>> Shape.indices (Shape.Shifted (-4) (8::Int))
[-4,-3,-2,-1,0,1,2,3]
-}
data Shifted n = Shifted {shiftedOffset, shiftedSize :: n}
   deriving (Eq, Show)

instance Functor Shifted where
   fmap f (Shifted from to) = Shifted (f from) (f to)

instance (NFData n) => NFData (Shifted n) where
   rnf (Shifted from to) = rnf (from,to)

instance (Integral n) => C (Shifted n) where
   size (Shifted _offs len) = fromIntegral len

instance (Integral n) => Indexed (Shifted n) where
   type Index (Shifted n) = n
   indices (Shifted offs len) =
      map snd $
      takeWhile ((>0) . fst) $
      zip
         (iterate (subtract 1) len)
         (iterate (1+) offs)
   unifiedOffset (Shifted offs len) ix = do
      assert
         (printf "Shape.Shifted %d: array index too small (%d vs %d)"
            (toInteger offs) (toInteger offs) (toInteger ix))
         (ix>=offs)
      let k = ix-offs
      assert
         (printf "Shape.Shifted %d: array index too big (%d vs %d)"
            (toInteger offs) (toInteger k) (toInteger len))
         (k<len)
      return $ fromIntegral k
   inBounds (Shifted offs len) ix = offs <= ix && ix < offs+len

instance (Integral n) => InvIndexed (Shifted n) where
   unifiedIndexFromOffset (Shifted offs len) k0 = do
      let k = fromIntegral k0
      assertIndexFromOffset "Shifted" k0 $ 0<=k && k<len
      return $ offs+k

-- cf. sample-frame:Stereo
instance Storable n => Storable (Shifted n) where
   {-# INLINE sizeOf #-}
   {-# INLINE alignment #-}
   {-# INLINE peek #-}
   {-# INLINE poke #-}
   sizeOf ~(Shifted l n) = sizeOf l + mod (- sizeOf l) (alignment n) + sizeOf n
   alignment ~(Shifted l _) = alignment l
   poke p (Shifted l n) =
      let q = castToElemPtr p
      in  poke q l >> pokeElemOff q 1 n
   peek p =
      let q = castToElemPtr p
      in  Monad.lift2 Shifted (peek q) (peekElemOff q 1)


{-# INLINE castToElemPtr #-}
castToElemPtr :: Ptr (f a) -> Ptr a
castToElemPtr = castPtr



{- |
'Enumeration' denotes a shape of fixed size
that is defined by 'Enum' and 'Bounded' methods.
For correctness it is necessary that the 'Enum' and 'Bounded' instances
are properly implemented.
Automatically derived instances are fine.

>>> Shape.indices (Shape.Enumeration :: Shape.Enumeration Ordering)
[LT,EQ,GT]
-}
data Enumeration n = Enumeration
   deriving (Eq, Show)

instance NFData (Enumeration n) where
   rnf Enumeration = ()

instance (Enum n, Bounded n) => C (Enumeration n) where
   size sh = intFromEnum sh maxBound - intFromEnum sh minBound + 1

instance (Enum n, Bounded n) => Indexed (Enumeration n) where
   type Index (Enumeration n) = n
   indices sh = [asEnumType sh minBound .. asEnumType sh maxBound]
   unifiedOffset sh ix = pure $ fromEnum ix - intFromEnum sh minBound
   inBounds _sh _ix = True

instance (Enum n, Bounded n) => InvIndexed (Enumeration n) where
   unifiedIndexFromOffset sh k = do
      let minBnd = intFromEnum sh minBound
      assertIndexFromOffset "Enumeration" k $
         0<=k && k <= intFromEnum sh maxBound - minBnd
      return $ toEnum $ minBnd + k

asEnumType :: Enumeration n -> n -> n
asEnumType Enumeration = id

intFromEnum :: (Enum n) => Enumeration n -> n -> Int
intFromEnum Enumeration = fromEnum

instance (Enum n, Bounded n) => Static (Enumeration n) where
   static = Enumeration

instance Storable (Enumeration n) where
   {-# INLINE sizeOf #-}
   {-# INLINE alignment #-}
   {-# INLINE peek #-}
   {-# INLINE poke #-}
   sizeOf ~Enumeration = 0
   alignment ~Enumeration = 1
   poke _p Enumeration = return ()
   peek _p = return Enumeration


instance (Ord n) => C (Set n) where
   size = Set.size

{- |
You can use an arbitrary 'Set' of indices as shape.
The array elements are ordered according to the index order in the 'Set'.

An @Array (Set k) a@ is isomorphic to a @Map k a@,
however it is missing most 'Map' operations like @insert@, @delete@ and @union@.
An @Array (Set k, Set j) a@ has a cartesian structure
and thus is not isomorphic to @Map (k,j) a@.
This means, if the array has two elements with indices @(k0,j0)@ and @(k1,j1)@
it has also an element with index @(k0,j1)@.

Disadvantage is that combinators of different Set indexed arrays
have to compare whole sets.
However, the Set implementation may have low-level optimizations
for pointer equality.

>>> Shape.indices (Set.fromList "comfort")
"cfmort"
-}
instance (Ord n) => Indexed (Set n) where
   type Index (Set n) = n
   indices = Set.toAscList
   unifiedOffset sh ix = withCheck $ \check ->
      case check of
         Unchecked -> pure $ ShapeSet.uncheckedOffset sh ix
         Checked ->
            case ShapeSet.offset sh ix of
               Just k -> pure k
               Nothing ->
                  throw "Shape.Set: array index not member of the index set"
   inBounds = flip Set.member

instance (Ord n) => InvIndexed (Set n) where
   unifiedIndexFromOffset sh k = withCheck $ \check ->
      case check of
         Unchecked -> pure $ ShapeSet.uncheckedIndexFromOffset sh k
         Checked ->
            case ShapeSet.indexFromOffset sh k of
               Just ix -> pure ix
               Nothing -> throw $ messageIndexFromOffset "Set" k



instance C IntSet where
   size = IntSet.size

{- |
>>> Shape.indices (IntSet.fromList [3,1,4,1,5,9,2,6,5,3])
[1,2,3,4,5,6,9]
-}
instance Indexed IntSet where
   type Index IntSet = Int
   indices = IntSet.toAscList
   unifiedOffset set ix =
      case IntSet.splitMember ix set of
         (less, hit, _) -> do
            assert "Shape.IntSet: array index not member of the index set" hit
            pure $ IntSet.size less
   inBounds = flip IntSet.member

instance InvIndexed IntSet where
   unifiedIndexFromOffset sh =
      let m = IntMap.fromList $ zip [0..] $ indices sh
      in \k ->
         case IntMap.lookup k m of
            Nothing -> throwOrError "Shape.IntSet.offset: unknown key"
            Just ix -> pure ix



{- |
Concatenate many arrays according to the shapes stored in a 'Map'.
-}
instance (Ord k, C shape) => C (Map k shape) where
   size = Fold.sum . Map.map size

{- |
The implementations of 'offset' et.al.
are optimized for frequent calls with respect to the same shape.

>>> Shape.indices $ fmap Shape.ZeroBased $ Map.fromList [('b', (0::Int)), ('a', 5), ('c', 2)]
[('a',0),('a',1),('a',2),('a',3),('a',4),('c',0),('c',1)]
-}
instance (Ord k, Indexed shape) => Indexed (Map k shape) where
   type Index (Map k shape) = (k, Index shape)
   indices =
      Fold.fold . Map.mapWithKey (\k shape -> map ((,) k) $ indices shape)
   unifiedOffset m =
      let ms = fmap unifiedSizeOffset m
          mu = snd $
            Trav.mapAccumL (\l (sz,getOffset) -> (l + sz, (l,getOffset))) 0 ms
      in \(k,ix) ->
         case Map.lookup k mu of
            Nothing -> throwOrError "Shape.Map.offset: unknown key"
            Just (l,getOffset) -> (l+) <$> getOffset ix
   inBounds m (k,ix) = Fold.any (flip inBounds ix) $ Map.lookup k m

   unifiedSizeOffset = mapSizeOffset . fmap unifiedSizeOffset

{-# INLINE mapSizeOffset #-}
mapSizeOffset ::
   (Checking check, Ord k, Num i) =>
   Map k (i, ix -> Result check i) -> (i, (k, ix) -> Result check i)
mapSizeOffset ms =
   (Fold.sum $ Map.map fst ms,
    let mu = snd $
         Trav.mapAccumL (\l (sz,offs) -> (l + sz, fmap (l+) . offs)) 0 ms
    in \(k,ix) ->
         maybe
            (throwOrError "Shape.Map.sizeOffset: unknown key")
            ($ix)
            (Map.lookup k mu))

instance (Ord k, InvIndexed shape) => InvIndexed (Map k shape) where
   unifiedIndexFromOffset m i =
      (\xs ->
         case xs of
            (_u,ix):_ -> ix
            [] -> throwOrError $ messageIndexFromOffset "Map" i) $
      dropWhile (\(u,_ix) -> u<=i) $ snd $
      List.mapAccumL
         (\l (k,sh) ->
            let u = l + size sh
            in (u, (u, (,) k <$> unifiedIndexFromOffset sh (i-l)))) 0 $
      Map.toAscList m



{- |
Concatenate many arrays according to the shapes stored in a 'IntMap'.
-}
instance (C shape) => C (IntMap shape) where
   size = Fold.sum . IntMap.map size

{- |
The implementations of 'offset' et.al.
are optimized for frequent calls with respect to the same shape.

>>> Shape.indices $ IntMap.fromList [(2, Set.fromList "abc"), (0, Set.fromList "a"), (1, Set.fromList "d")]
[(0,'a'),(1,'d'),(2,'a'),(2,'b'),(2,'c')]
-}
instance (Indexed shape) => Indexed (IntMap shape) where
   type Index (IntMap shape) = (Int, Index shape)
   indices =
      Fold.fold . IntMap.mapWithKey (\k shape -> map ((,) k) $ indices shape)
   unifiedOffset m =
      let ms = fmap unifiedSizeOffset m
          mu = snd $
            Trav.mapAccumL (\l (sz,getOffset) -> (l + sz, (l,getOffset))) 0 ms
      in \(k,ix) ->
         case IntMap.lookup k mu of
            Nothing -> throwOrError "Shape.IntMap.offset: unknown key"
            Just (l,getOffset) -> (l+) <$> getOffset ix
   inBounds m (k,ix) = Fold.any (flip inBounds ix) $ IntMap.lookup k m

   unifiedSizeOffset = intMapSizeOffset . fmap unifiedSizeOffset

{-# INLINE intMapSizeOffset #-}
intMapSizeOffset ::
   (Checking check, Num i) =>
   IntMap (i, ix -> Result check i) -> (i, (Int, ix) -> Result check i)
intMapSizeOffset ms =
   (Fold.sum $ IntMap.map fst ms,
    let mu = snd $
         Trav.mapAccumL (\l (sz,offs) -> (l + sz, fmap (l+) . offs)) 0 ms
    in \(k,ix) ->
         maybe
            (throwOrError "Shape.IntMap.sizeOffset: unknown key")
            ($ix)
            (IntMap.lookup k mu))

-- ToDo: can be sped up using IntMap.lookupLT for containers>=0.5
instance (InvIndexed shape) => InvIndexed (IntMap shape) where
   unifiedIndexFromOffset m i =
      (\xs ->
         case xs of
            (_u,ix):_ -> ix
            [] -> throwOrError $ messageIndexFromOffset "IntMap" i) $
      dropWhile (\(u,_ix) -> u<=i) $ snd $
      List.mapAccumL
         (\l (k,sh) ->
            let u = l + size sh
            in (u, (u, (,) k <$> unifiedIndexFromOffset sh (i-l)))) 0 $
      IntMap.toAscList m



{- |
This data type wraps another array shape.
Its index type is a wrapped 'Int'.
The advantages are:
No conversion forth and back 'Int' and @Index sh@.
You can convert once using 'deferIndex' and 'revealIndex'
whenever you need your application specific index type.
No need for e.g. @Storable (Index sh)@, because 'Int' is already 'Storable'.
You get 'Indexed' and 'InvIndexed' instances
without the need for an 'Index' type.
The disadvantage is:
A deferred index should be bound to a specific shape, but this is not checked.
That is, you may obtain a deferred index for one shape
and accidentally abuse it for another shape without a warning.

Example:

>>> :{
   let sh2 = (Shape.ZeroBased (2::Int), Shape.ZeroBased (2::Int)) in
   let sh3 = (Shape.ZeroBased (3::Int), Shape.ZeroBased (3::Int)) in
   (Shape.offset sh3 $ Shape.indexFromOffset sh2 3,
    Shape.offset (Shape.Deferred sh3) $
      Shape.indexFromOffset (Shape.Deferred sh2) 3)
:}
(4,3)
-}
newtype Deferred sh = Deferred sh
   deriving (Eq, Show)

{- |
'DeferredIndex' has an 'Ord' instance
that is based on the storage order in memory.
This way, you can put 'DeferredIndex' values
in a 'Set' or use them as keys in a 'Map'
even if @Index sh@ has no 'Ord' instance.
The downside is, that the ordering of @DeferredIndex sh@
may differ from the one of @Index sh@.
-}
newtype DeferredIndex sh = DeferredIndex Int
   deriving (Eq, Ord, Show)

instance (NFData sh) => NFData (Deferred sh) where
   rnf (Deferred sh) = rnf sh

instance (C sh) => C (Deferred sh) where
   size (Deferred sh) = size sh

instance (C sh) => Indexed (Deferred sh) where
   type Index (Deferred sh) = DeferredIndex sh
   indices (Deferred sh) = map DeferredIndex $ take (size sh) [0 ..]
   unifiedOffset (Deferred sh) (DeferredIndex k) = withCheck $ \check ->
      case check of
         Checked -> unifiedOffset (ZeroBased $ size sh) k
         Unchecked -> pure k
   unifiedSizeOffset (Deferred sh) =
      mapSnd (\offs (DeferredIndex k) -> offs k) $
      unifiedSizeOffset (ZeroBased $ size sh)
   inBounds (Deferred sh) (DeferredIndex k) =
      inBounds (ZeroBased $ size sh) k

instance (C sh) => InvIndexed (Deferred sh) where
   indexFromOffset (Deferred sh) k =
      DeferredIndex $ indexFromOffset (ZeroBased $ size sh) k
   uncheckedIndexFromOffset _sh = DeferredIndex
   unifiedIndexFromOffset (Deferred sh) k = withCheck $ \check ->
      case check of
         Unchecked -> pure $ DeferredIndex k
         Checked ->
            DeferredIndex <$> unifiedIndexFromOffset (ZeroBased $ size sh) k

deferIndex :: (Indexed sh, Index sh ~ ix) => sh -> ix -> DeferredIndex sh
deferIndex sh ix = DeferredIndex $ offset sh ix

revealIndex :: (InvIndexed sh, Index sh ~ ix) => sh -> DeferredIndex sh -> ix
revealIndex sh (DeferredIndex ix) = indexFromOffset sh ix

instance (Static sh) => Static (Deferred sh) where
   static = Deferred static

instance Storable (DeferredIndex sh) where
   {-# INLINE sizeOf #-}
   {-# INLINE alignment #-}
   {-# INLINE peek #-}
   {-# INLINE poke #-}
   sizeOf (DeferredIndex k) = sizeOf k
   alignment (DeferredIndex k) = alignment k
   poke p (DeferredIndex k) = poke (castPtr p) k
   peek p = fmap DeferredIndex $ peek (castPtr p)



instance (C sh) => C (Tagged s sh) where
   size (Tagged sh) = size sh

instance (Indexed sh) => Indexed (Tagged s sh) where
   type Index (Tagged s sh) = Tagged s (Index sh)
   indices (Tagged sh) = map Tagged $ indices sh
   unifiedOffset (Tagged sh) = unifiedOffset sh . unTagged
   unifiedSizeOffset (Tagged sh) =
      mapSnd (. unTagged) $ unifiedSizeOffset sh
   inBounds (Tagged sh) (Tagged k) = inBounds sh k

instance (InvIndexed sh) => InvIndexed (Tagged s sh) where
   unifiedIndexFromOffset (Tagged sh) k =
      Tagged <$> unifiedIndexFromOffset sh k

instance (Static sh) => Static (Tagged s sh) where
   static = Tagged static

instance (Pattern sh) => Pattern (Tagged s sh) where
   type DataPattern (Tagged s sh) x = DataPattern sh x
   indexPattern extend (Tagged sh) = indexPattern (extend . Tagged) sh



instance (C sh0, C sh1) => C (sh0,sh1) where
   size (sh0,sh1) = size sh0 * size sh1

{- |
Row-major composition of two dimensions.

>>> Shape.indices (Shape.ZeroBased (3::Int), Shape.ZeroBased (3::Int))
[(0,0),(0,1),(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2)]
-}
instance (Indexed sh0, Indexed sh1) => Indexed (sh0,sh1) where
   type Index (sh0,sh1) = (Index sh0, Index sh1)
   indices (sh0,sh1) = Monad.lift2 (,) (indices sh0) (indices sh1)
   unifiedOffset (sh0,sh1) =
      (unifiedOffset sh0 . fst)
      `combineOffset`
      (mapSnd (.snd) $ unifiedSizeOffset sh1)
   unifiedSizeOffset (sh0,sh1) =
      (mapSnd (.fst) $ unifiedSizeOffset sh0)
      `combineSizeOffset`
      (mapSnd (.snd) $ unifiedSizeOffset sh1)
   inBounds (sh0,sh1) (ix0,ix1) = inBounds sh0 ix0 && inBounds sh1 ix1

instance (InvIndexed sh0, InvIndexed sh1) => InvIndexed (sh0,sh1) where
   unifiedIndexFromOffset (sh0,sh1) k = do
      let (rix0,ix1) =
            runInvIndex k $ App.lift2 (,) (pickLastIndex sh0) (pickIndex sh1)
      ix0 <- rix0
      return (ix0,ix1)

instance (Static sh0, Static sh1) => Static (sh0,sh1) where
   static = (static, static)

instance
   (AppendSemigroup sh0, C sh1, Eq sh1) =>
      AppendSemigroup (sh0,sh1) where
   append (sh0a,sh1a) (sh0b,sh1b) =
      if sh1a == sh1b
         then (append sh0a sh0b, sh1a)
         else error $ "Shape.append: column shapes mismatch"

instance (Pattern sh0, Pattern sh1) => Pattern (sh0,sh1) where
   type DataPattern (sh0,sh1) x = PatternRecord sh0 (DataPattern sh1 x)
   indexPattern extend (sh0,sh1) =
      PatternRecord $
         indexPattern (\i -> indexPattern (\j -> extend (i,j)) sh1) sh0


instance (C sh0, C sh1, C sh2) => C (sh0,sh1,sh2) where
   size (sh0,sh1,sh2) = size sh0 * size sh1 * size sh2

instance (Indexed sh0, Indexed sh1, Indexed sh2) => Indexed (sh0,sh1,sh2) where
   type Index (sh0,sh1,sh2) = (Index sh0, Index sh1, Index sh2)
   indices (sh0,sh1,sh2) =
      Monad.lift3 (,,) (indices sh0) (indices sh1) (indices sh2)
   unifiedOffset (sh0,sh1,sh2) =
      (unifiedOffset sh0 . fst3)
      `combineOffset`
      (mapSnd (.snd3) $ unifiedSizeOffset sh1)
      `combineSizeOffset`
      (mapSnd (.thd3) $ unifiedSizeOffset sh2)
   unifiedSizeOffset (sh0,sh1,sh2) =
      (mapSnd (.fst3) $ unifiedSizeOffset sh0)
      `combineSizeOffset`
      (mapSnd (.snd3) $ unifiedSizeOffset sh1)
      `combineSizeOffset`
      (mapSnd (.thd3) $ unifiedSizeOffset sh2)
   inBounds (sh0,sh1,sh2) (ix0,ix1,ix2) =
      inBounds sh0 ix0 && inBounds sh1 ix1 && inBounds sh2 ix2

instance
   (InvIndexed sh0, InvIndexed sh1, InvIndexed sh2) =>
      InvIndexed (sh0,sh1,sh2) where
   unifiedIndexFromOffset (sh0,sh1,sh2) k = do
      let (rix0,ix1,ix2) =
            runInvIndex k $
            App.lift3 (,,) (pickLastIndex sh0) (pickIndex sh1) (pickIndex sh2)
      ix0 <- rix0
      return (ix0,ix1,ix2)

instance (Static sh0, Static sh1, Static sh2) => Static (sh0,sh1,sh2) where
   static = (static, static, static)

instance
   (AppendSemigroup sh0, C sh1, Eq sh1, C sh2, Eq sh2) =>
      AppendSemigroup (sh0,sh1,sh2) where
   append (sh0a,sh1a,sh2a) (sh0b,sh1b,sh2b) =
      if sh1a == sh1b &&  sh2a == sh2b
         then (append sh0a sh0b, sh1a, sh2a)
         else error $ "Shape.append: column shapes mismatch"

runInvIndex :: s -> Back.Backwards (MS.State s) a -> a
runInvIndex k = flip MS.evalState k . Back.forwards

pickLastIndex ::
   (Checking check, InvIndexed sh) =>
   sh -> Back.Backwards (MS.State Int) (Result check (Index sh))
pickLastIndex sh =
   Back.Backwards $ MS.gets $ unifiedIndexFromOffset sh

pickIndex :: (InvIndexed sh) => sh -> Back.Backwards (MS.State Int) (Index sh)
pickIndex sh =
   fmap (uncheckedIndexFromOffset sh) $
   Back.Backwards $ MS.state $ \k -> swap $ divMod k $ size sh



infixr 7 `combineOffset`, `combineSizeOffset`

{-# INLINE combineOffset #-}
combineOffset ::
   (Applicative f, Num a) =>
   (ix -> f a) -> (a, ix -> f a) -> (ix -> f a)
combineOffset offset0 (size1,offset1) ix =
   offset0 ix |* size1 |+| offset1 ix

{-# INLINE combineSizeOffset #-}
combineSizeOffset ::
   (Applicative f, Num a) =>
   (a, ix -> f a) -> (a, ix -> f a) -> (a, ix -> f a)
combineSizeOffset (size0,offset0) (size1,offset1) =
   (size0*size1, \ix -> offset0 ix |* size1 |+| offset1 ix)



{- |
'Square' is like a Cartesian product,
but it is statically asserted that both dimension shapes match.

>>> Shape.indices $ Shape.Square $ Shape.ZeroBased (3::Int)
[(0,0),(0,1),(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2)]
-}
newtype Square sh = Square {squareSize :: sh}
   deriving (Eq, Show)

cartesianFromSquare :: Square sh -> (sh,sh)
cartesianFromSquare (Square sh) = (sh,sh)

instance Functor Square where
   fmap f (Square sh) = Square $ f sh

instance Applicative Square where
   pure = Square
   Square f <*> Square sh = Square $ f sh

instance (NFData sh) => NFData (Square sh) where
   rnf (Square sh) = rnf sh

instance (Storable sh) => Storable (Square sh) where
   sizeOf = Store.sizeOf squareSize
   alignment = Store.alignment squareSize
   peek = Store.peek Square
   poke = Store.poke squareSize

instance (C sh) => C (Square sh) where
   size (Square sh) = size sh ^ (2::Int)

instance (Indexed sh) => Indexed (Square sh) where
   type Index (Square sh) = (Index sh, Index sh)
   indices (Square sh) = indices (sh,sh)
   unifiedSizeOffset (Square sh) =
      let szo = unifiedSizeOffset sh
      in mapSnd (.fst) szo `combineSizeOffset` mapSnd (.snd) szo
   inBounds (Square sh) = inBounds (sh,sh)

instance (InvIndexed sh) => InvIndexed (Square sh) where
   unifiedIndexFromOffset (Square sh) =
      unifiedIndexFromOffset (sh,sh)

newtype PatternRecord sh a = PatternRecord (DataPattern sh a)

instance (Pattern sh) => Pattern (Square sh) where
   -- Would require UndecidableInstances
   -- type DataPattern (Square sh) x = DataPattern sh (DataPattern sh x)

   type DataPattern (Square sh) x = PatternRecord sh (DataPattern sh x)
   indexPattern extend (Square sh) =
      PatternRecord $
         indexPattern (\i -> indexPattern (\j -> extend (i,j)) sh) sh



{- |
'Cube' is like a Cartesian product,
but it is statically asserted that both dimension shapes match.

>>> Shape.indices $ Shape.Cube $ Shape.ZeroBased (2::Int)
[(0,0,0),(0,0,1),(0,1,0),(0,1,1),(1,0,0),(1,0,1),(1,1,0),(1,1,1)]
-}
newtype Cube sh = Cube {cubeSize :: sh}
   deriving (Eq, Show)

cartesianFromCube :: Cube sh -> (sh,sh,sh)
cartesianFromCube (Cube sh) = (sh,sh,sh)

instance Functor Cube where
   fmap f (Cube sh) = Cube $ f sh

instance Applicative Cube where
   pure = Cube
   Cube f <*> Cube sh = Cube $ f sh

instance (NFData sh) => NFData (Cube sh) where
   rnf (Cube sh) = rnf sh

instance (Storable sh) => Storable (Cube sh) where
   sizeOf = Store.sizeOf cubeSize
   alignment = Store.alignment cubeSize
   peek = Store.peek Cube
   poke = Store.poke cubeSize

instance (C sh) => C (Cube sh) where
   size (Cube sh) = size sh ^ (3::Int)

instance (Indexed sh) => Indexed (Cube sh) where
   type Index (Cube sh) = (Index sh, Index sh, Index sh)
   indices (Cube sh) = indices (sh,sh,sh)
   unifiedSizeOffset (Cube sh) =
      let szo = unifiedSizeOffset sh
      in mapSnd (.fst3) szo
         `combineSizeOffset`
         mapSnd (.snd3) szo
         `combineSizeOffset`
         mapSnd (.thd3) szo
   inBounds (Cube sh) = inBounds (sh,sh,sh)

instance (InvIndexed sh) => InvIndexed (Cube sh) where
   unifiedIndexFromOffset (Cube sh) =
      unifiedIndexFromOffset (sh,sh,sh)



data Lower = Lower deriving (Eq, Show)
data Upper = Upper deriving (Eq, Show)

class TriangularPart part where
   switchTriangularPart :: f Lower -> f Upper -> f part
instance TriangularPart Lower where switchTriangularPart f _ = f
instance TriangularPart Upper where switchTriangularPart _ f = f

getConstAs :: c -> Const a c -> a
getConstAs _ = getConst

caseTriangularPart :: (TriangularPart part) => part -> a -> a -> a
caseTriangularPart part lo up =
   getConstAs part $ switchTriangularPart (Const lo) (Const up)

{- |
>>> Shape.indices $ Shape.Triangular Shape.Upper $ Shape.ZeroBased (3::Int)
[(0,0),(0,1),(0,2),(1,1),(1,2),(2,2)]
>>> Shape.indices $ Shape.Triangular Shape.Lower $ Shape.ZeroBased (3::Int)
[(0,0),(1,0),(1,1),(2,0),(2,1),(2,2)]
-}
data Triangular part size =
   Triangular {
      triangularPart :: part,
      triangularSize :: size
   } deriving (Show)

newtype Equal part = Equal {getEqual :: part -> part -> Bool}

equalPart :: (TriangularPart part) => part -> part -> Bool
equalPart = getEqual $ switchTriangularPart (Equal (==)) (Equal (==))

instance (TriangularPart part, Eq size) => Eq (Triangular part size) where
   x==y  =  compose2 equalPart triangularPart x y && equating triangularSize x y

type LowerTriangular = Triangular Lower
type UpperTriangular = Triangular Upper

lowerTriangular :: size -> LowerTriangular size
lowerTriangular = Triangular Lower

upperTriangular :: size -> UpperTriangular size
upperTriangular = Triangular Upper

-- cf. Data.Bifunctor.Flip
newtype Flip f b a = Flip {getFlip :: f a b}

instance
      (TriangularPart part, NFData size) => NFData (Triangular part size) where
   rnf (Triangular part sz) =
      rnf
         (flip getFlip part $
            switchTriangularPart (Flip $ \Lower -> ()) (Flip $ \Upper -> ()),
          sz)

instance (TriangularPart part, C size) => C (Triangular part size) where
   size (Triangular _part sz) = triangleSize $ size sz

instance
   (TriangularPart part, Indexed size) =>
      Indexed (Triangular part size) where
   type Index (Triangular part size) = (Index size, Index size)

   indices (Triangular part sz) =
      let ixs = indices sz
      in concat $
         caseTriangularPart part
            (zipWith (\cs r -> map ((,) r) cs)
               (NonEmpty.tail $ NonEmpty.inits ixs) ixs)
            (zipWith (\r cs -> map ((,) r) cs) ixs $ tails ixs)

   unifiedSizeOffset (Triangular part sz) =
      let (n, getOffset) = unifiedSizeOffset sz
      in (triangleSize n, \(rs,cs) -> do
         r <- getOffset rs
         c <- getOffset cs
         assert "Shape.Triangular.sizeOffset: wrong array part" $
            compareIndices part r c
         return $ triangleOffset part n (r,c))

   inBounds (Triangular part sz) ix@(r,c) =
      inBounds (sz,sz) ix
      &&
      let getOffset = offset sz
      in compareIndices part (getOffset r) (getOffset c)

triangleOffset :: TriangularPart part => part -> Int -> (Int, Int) -> Int
triangleOffset part n (r,c) =
   caseTriangularPart part
      (triangleSize r + c)
      (triangleSize n - triangleSize (n-r) + c-r)

compareIndices :: (TriangularPart part, Ord a) => part -> a -> a -> Bool
compareIndices part = caseTriangularPart part (>=) (<=)

instance
   (TriangularPart part, InvIndexed size) =>
      InvIndexed (Triangular part size) where

   unifiedIndexFromOffset (Triangular part sz) k =
      let n = size sz in
      App.mapPair (unifiedIndexFromOffset sz, unifiedIndexFromOffset sz) $
       caseTriangularPart part
         (let r = floor (triangleRootDouble k)
          in (r, k - triangleSize r))
         (let triSize = triangleSize n
              rr = ceiling (triangleRootDouble (triSize-k))
              r = n - rr
          in (r, k+r - (triSize - triangleSize rr)))

triangleSize :: Int -> Int
triangleSize n = div (n*(n+1)) 2

{-
n*(n+1)/2 = m
n^2 + n - 2m = 0
n = -1/2 + sqrt(1/4+2m)
  = (sqrt(8m+1) - 1) / 2
-}
triangleRoot :: Floating a => a -> a
triangleRoot sz = (sqrt (8*sz+1)-1)/2

triangleRootDouble :: Int -> Double
triangleRootDouble = triangleRoot . fromIntegral


instance
   (TriangularPart part, Static size) =>
      Static (Triangular part size) where
   static = Triangular autoPart static

autoPart :: (TriangularPart part) => part
autoPart = runIdentity $ switchTriangularPart (Identity Lower) (Identity Upper)



{- |
Simplex is a generalization of 'Triangular' to more than two dimensions.
Indices are tuples of fixed size
with elements ordered in ascending, strictly ascending,
descending or strictly descending order.
\"Order\" refers to the index order in 'indices'.
In order to avoid confusion we suggest that the order of 'indices'
is consistent with '<='.

Obviously, 'offset' implements ranking
and 'indexFromOffset' implements unranking
of combinations (in the combinatorial sense)
with or without repetitions.

>>> Shape.indices $ Shape.simplexAscending (replicate 3 Shape.AllDistinct) $ Shape.ZeroBased (4::Int)
[[0,1,2],[0,1,3],[0,2,3],[1,2,3]]
>>> Shape.indices $ Shape.simplexAscending (replicate 3 Shape.SomeRepetitive) $ Shape.ZeroBased (3::Int)
[[0,0,0],[0,0,1],[0,0,2],[0,1,1],[0,1,2],[0,2,2],[1,1,1],[1,1,2],[1,2,2],[2,2,2]]
>>> Shape.indices $ Shape.simplexAscending [Shape.Repetitive,Shape.Distinct,Shape.Repetitive] $ Shape.ZeroBased (4::Int)
[[0,0,1],[0,0,2],[0,0,3],[0,1,2],[0,1,3],[0,2,3],[1,1,2],[1,1,3],[1,2,3],[2,2,3]]
>>> Shape.indices $ Shape.simplexAscending [Shape.Repetitive,Shape.Distinct,Shape.Distinct] $ Shape.ZeroBased (4::Int)
[[0,0,1],[0,0,2],[0,0,3],[0,1,2],[0,1,3],[0,2,3],[1,1,2],[1,1,3],[1,2,3],[2,2,3]]

>>> Shape.indices $ Shape.simplexDescending (replicate 3 Shape.AllDistinct) $ Shape.ZeroBased (4::Int)
[[2,1,0],[3,1,0],[3,2,0],[3,2,1]]
>>> Shape.indices $ Shape.simplexDescending (replicate 3 Shape.SomeRepetitive) $ Shape.ZeroBased (3::Int)
[[0,0,0],[1,0,0],[1,1,0],[1,1,1],[2,0,0],[2,1,0],[2,1,1],[2,2,0],[2,2,1],[2,2,2]]
>>> Shape.indices $ Shape.simplexDescending [Shape.Repetitive,Shape.Distinct,Shape.Repetitive] $ Shape.ZeroBased (4::Int)
[[1,1,0],[2,1,0],[2,2,0],[2,2,1],[3,1,0],[3,2,0],[3,2,1],[3,3,0],[3,3,1],[3,3,2]]
>>> Shape.indices $ Shape.simplexDescending [Shape.Repetitive,Shape.Distinct,Shape.Distinct] $ Shape.ZeroBased (4::Int)
[[1,1,0],[2,1,0],[2,2,0],[2,2,1],[3,1,0],[3,2,0],[3,2,1],[3,3,0],[3,3,1],[3,3,2]]
-}
data Simplex order coll f size =
   Simplex {
      simplexOrder :: SimplexOrder order,
      simplexDimension :: f coll,
      simplexSize :: size
   }

data Ascending
data Descending
data SimplexOrder order where
   Ascending :: SimplexOrder Ascending
   Descending :: SimplexOrder Descending

instance Eq (SimplexOrder order) where
   Ascending == Ascending = True
   Descending == Descending = True

instance Show (SimplexOrder order) where
   show Ascending = "Ascending"
   show Descending = "Descending"

type SimplexAscending = Simplex Ascending
type SimplexDescending = Simplex Descending

simplexAscending :: f coll -> size -> SimplexAscending coll f size
simplexAscending = Simplex Ascending

simplexDescending :: f coll -> size -> SimplexDescending coll f size
simplexDescending = Simplex Descending

isAscending :: SimplexOrder order -> Bool
isAscending Ascending = True
isAscending Descending = False

class SimplexOrderC order where
instance SimplexOrderC Ascending where
instance SimplexOrderC Descending where

data AllDistinct = AllDistinct deriving (Show, Eq)
data SomeRepetitive = SomeRepetitive deriving (Show, Eq)
data Collision = Distinct | Repetitive deriving (Show, Eq, Ord, Enum)

class CollisionC coll where repetitionAllowed :: coll -> Bool
instance CollisionC AllDistinct where repetitionAllowed AllDistinct = False
instance CollisionC SomeRepetitive where repetitionAllowed SomeRepetitive = True
instance CollisionC Collision where
   repetitionAllowed Distinct = False
   repetitionAllowed Repetitive = True

instance
   (SimplexOrderC order, Show coll, FunctorC.Show1 f, Show size) =>
      Show (Simplex order coll f size) where
   showsPrec p (Simplex order d sz) =
      showParen (p>10) $
         showString "Simplex " .
         shows order .
         showString " " .
         FunctorC.showsPrec1 11 d .
         showString " " .
         showsPrec 11 sz

instance
   (SimplexOrderC order, CollisionC coll, Traversable f, C size) =>
      C (Simplex order coll f size) where
   size (Simplex _order d sz) =
      let ds = Fold.toList d
          rep = length $ filter repetitionAllowed $ laxInit ds
      in simplexLayoutSize (length ds) (size sz + rep)

laxInit :: [a] -> [a]
laxInit xs = Match.take (drop 1 xs) xs

simplexLayoutSize :: Integral i => Int -> i -> i
simplexLayoutSize d n =
   case drop d $ binomials n of
      [] -> 0
      m:_ -> m

-- cf. package combinatorial
binomials :: Integral a => a -> [a]
binomials n =
   scanl (\acc (num,den) -> div (acc*num) den) 1
         (zip [n, pred n ..] [1..n])

foldLength :: (Foldable f) => f a -> Int
foldLength = length . Fold.toList

instance
   (SimplexOrderC order, CollisionC coll,
    Traversable f, FunctorC.Eq1 f, Indexed size) =>
      Indexed (Simplex order coll f size) where
   type Index (Simplex order coll f size) = f (Index size)
   indices (Simplex order d sz) =
      flip MS.evalStateT (indices sz) $
      Trav.traverse
         (if isAscending order
             then chooseIndexAscending
             else chooseIndexDescending)
         d
   inBounds (Simplex order d sz) =
      let getOffset = offset sz in \ix ->
      let ixs = Fold.toList ix in
         all (inBounds sz) ixs &&
         FunctorC.eq1 (void d) (void ix) &&
         isMonotonic order (Fold.toList d) (map getOffset ixs)
   unifiedSizeOffset (Simplex order d sz) =
      let (n, getOffset) = unifiedSizeOffset sz in
      let dInt = foldLength d
          prep = prepareSimplexIndexingOrder order d n in
      (simplexLayoutSize dInt (fst prep),
          -- cf. Combinatorics.chooseRank
          \ixf -> do
            ks <- Trav.traverse getOffset $ Fold.toList ixf
            assert
               "Shape.Simplex.offset: simplex and index structure mismatch"
               (FunctorC.eq1 (void d) (void ixf))
            assert
               "Shape.Simplex.offset: index elements not monotonic"
               (isMonotonic order (Fold.toList d) ks)
            return $
               simplexOffset order dInt
                  (mapSnd (map snd . Fold.toList) prep) ks)

simplexOffset ::
   (Integral i) => SimplexOrder order -> Int -> (i, [(Int, i)]) -> [i] -> i
simplexOffset order d (nsum,cis) ks =
   case order of
      Ascending ->
         simplexLayoutSize d nsum - 1
         -
         sum (zipWith (\k (x,y) -> simplexLayoutSize x (y-k)) ks cis)
      Descending ->
         sum (zipWith (\k (x,y) -> simplexLayoutSize x (y+k)) ks cis)

isMonotonic ::
   (CollisionC coll) => SimplexOrder order -> [coll] -> [Int] -> Bool
isMonotonic order cs =
   and
   .
   (if isAscending order
      then
         ListHT.mapAdjacent
            (\(c,x) (_,y) -> if repetitionAllowed c then x<=y else x<y)
      else
         ListHT.mapAdjacent
            (\(c,x) (_,y) -> if repetitionAllowed c then x>=y else x>y))
   .
   zip cs

chooseIndexAscending, chooseIndexDescending ::
   (CollisionC coll) => coll -> MS.StateT [a] [] a

chooseIndexAscending coll =
   MS.StateT $ \as -> zip as $
      (if repetitionAllowed coll then NonEmpty.flatten else NonEmpty.tail) $
      NonEmpty.tails as

chooseIndexDescending coll =
   MS.StateT $ \as -> zip as $
      (if repetitionAllowed coll then NonEmpty.tail else NonEmpty.flatten) $
      NonEmpty.inits as

instance
   (SimplexOrderC order, CollisionC coll,
    Traversable f, FunctorC.Eq1 f, InvIndexed size) =>
      InvIndexed (Simplex order coll f size) where
   unifiedIndexFromOffset (Simplex order d sh) =
      let n = size sh in
      let (nSum,deco) = prepareSimplexIndexingOrder order d n in
      let dInt = foldLength d in \k ->
      maybe
         (throwOrError $ messageIndexFromOffset "Simplex" k)
         (Trav.traverse (unifiedIndexFromOffset sh) . snd) $
      if isAscending order
         then
            mapAccumLM
               (\(a,k0) (db,(x,y)) ->
                  case dropWhile ((<0) . snd) $
                        map (\bi -> (bi, k0 - simplexLayoutSize x (y-bi))) $
                        takeWhile (<n) $ iterate (1+) a of
                     [] -> Nothing
                     (b,k1):_ -> Just ((b+db, k1), b))
               (0, simplexLayoutSize dInt nSum - 1 - k)
               deco
         else
            mapAccumLM
               (\(a,k0) (db,(x,y)) ->
                  case dropWhile ((<0) . snd) $
                        map (\bi -> (bi, k0 - simplexLayoutSize x (y+bi))) $
                        takeWhile (>=0) $ iterate (subtract 1) a of
                     [] -> Nothing
                     (b,k1):_ -> Just ((b-db, k1), b))
               (n,k)
               deco

mapAccumLM ::
   (Traversable t, Monad m) => (a -> b -> m (a, c)) -> a -> t b -> m (a, t c)
mapAccumLM f a0 xs =
   liftM swap $
   MS.runStateT
      (Trav.mapM (\b -> MS.StateT $ \a -> liftM swap $ f a b) xs) a0


prepareSimplexIndexingOrder ::
   (Traversable t, Num i, CollisionC coll) =>
   SimplexOrder order -> t coll -> Int -> (Int, t (Int, (i, Int)))
prepareSimplexIndexingOrder order d n =
   if isAscending order
      then mapFst (1+) $ prepareSimplexIndexing d (n-1)
      else mapFst (n+) $ prepareSimplexIndexing d 0

prepareSimplexIndexing ::
   (Traversable t, Num i, CollisionC coll) =>
   t coll -> Int -> (Int, t (Int, (i, Int)))
prepareSimplexIndexing d n =
   let ((_,(_,nSum)), deco) =
         Trav.mapAccumR
            (\(c0,(x,y)) ci ->
               let c1 = fromEnum (ci&&c0)
                   p = (x+1,y+c1)
               in ((True,p),(1-c1,p)))
            (False,(0,n))
            (fmap repetitionAllowed d)
   in (nSum, deco)



{- |
'Cyclic' is a shape, where the indices wrap around at the array boundaries.
E.g.

prop> let shape = Shape.Cyclic (10::Int) in Shape.offset shape (-1) == Shape.offset shape 9

This also means that there are multiple indices
that address the same array element.

>>> Shape.indices (Shape.Cyclic (7::Int))
[0,1,2,3,4,5,6]
-}
newtype Cyclic n = Cyclic {cyclicSize :: n}
   deriving (Eq, Show)

instance Functor Cyclic where
   fmap f (Cyclic n) = Cyclic $ f n

instance Applicative Cyclic where
   pure = Cyclic
   Cyclic f <*> Cyclic n = Cyclic $ f n

instance (NFData n) => NFData (Cyclic n) where
   rnf (Cyclic n) = rnf n

instance (Storable n) => Storable (Cyclic n) where
   sizeOf = Store.sizeOf cyclicSize
   alignment = Store.alignment cyclicSize
   peek = Store.peek Cyclic
   poke = Store.poke cyclicSize

instance (Integral n) => C (Cyclic n) where
   size (Cyclic len) = fromIntegral len

instance (Integral n) => Indexed (Cyclic n) where
   type Index (Cyclic n) = n
   indices (Cyclic len) = indices $ ZeroBased len
   unifiedOffset (Cyclic len) ix = pure $ fromIntegral $ mod ix len
   inBounds (Cyclic len) _ix = len>0

instance (Integral n) => InvIndexed (Cyclic n) where
   unifiedIndexFromOffset (Cyclic len) k0 = do
      let k = fromIntegral k0
      assertIndexFromOffset "Cyclic" k0 $ 0<=k && k<len
      return k



infixr 5 ::+

{- |
Row-major composition of two dimensions.

>>> Shape.indices (Shape.ZeroBased (3::Int) ::+ Shape.Range 'a' 'c')
[Left 0,Left 1,Left 2,Right 'a',Right 'b',Right 'c']
-}
data sh0::+sh1 = sh0::+sh1
   deriving (Eq, Show)

instance (NFData sh0, NFData sh1) => NFData (sh0::+sh1) where
   rnf (sh0::+sh1) = rnf (sh0,sh1)

instance (C sh0, C sh1) => C (sh0::+sh1) where
   size (sh0::+sh1) = size sh0 + size sh1

instance (Indexed sh0, Indexed sh1) => Indexed (sh0::+sh1) where
   type Index (sh0::+sh1) = Either (Index sh0) (Index sh1)
   indices (sh0::+sh1) = map Left (indices sh0) ++ map Right (indices sh1)
   unifiedOffset (sh0::+sh1) =
      let (n0,getOffset0) = unifiedSizeOffset sh0
          getOffset1 = unifiedOffset sh1
      in \ix ->
         case ix of
            Left ix0 -> getOffset0 ix0
            Right ix1 -> (n0 +) <$> getOffset1 ix1
   unifiedSizeOffset (sh0::+sh1) =
      let (n0, getOffset0) = unifiedSizeOffset sh0
          (n1, getOffset1) = unifiedSizeOffset sh1
      in (n0+n1, either getOffset0 (fmap (n0+) . getOffset1))
   inBounds (sh0::+sh1) = either (inBounds sh0) (inBounds sh1)

instance (InvIndexed sh0, InvIndexed sh1) => InvIndexed (sh0::+sh1) where
   unifiedIndexFromOffset (sh0::+sh1) =
      let pivot = size sh0
      in \k ->
         if k < pivot
            then Left <$> unifiedIndexFromOffset sh0 k
            else Right <$> unifiedIndexFromOffset sh1 (k-pivot)

instance (Static sh0, Static sh1) => Static (sh0::+sh1) where
   static = static::+static

instance (Pattern sh0, Pattern sh1) => Pattern (sh0::+sh1) where
   type DataPattern (sh0::+sh1) x = DataPattern sh0 x ::+ DataPattern sh1 x
   indexPattern extend (sh0::+sh1) =
      indexPattern (extend . Left) sh0 ::+ indexPattern (extend . Right) sh1


infixl 7 |*
infixl 6 |+|

(|*) :: (Functor f, Num a) => f a -> a -> f a
f|*a = fmap (*a) f

(|+|) :: (Applicative f, Num a) => f a -> f a -> f a
(|+|) = App.lift2 (+)



{- |
Shape for arrays that hold elements
that can alternatively be stored in nested tuples.
-}
newtype NestedTuple ixtype tuple = NestedTuple {getNestedTuple :: tuple}
   deriving (Eq, Show)

data TupleAccessor
data TupleIndex

newtype Element = Element Int
   deriving (Eq, Show)

instance NFData Element where
   rnf (Element k) = rnf k


class ElementTuple tuple where
   type DataTuple tuple x
   indexTupleA ::
      (Applicative f) => (Element -> f a) -> tuple -> f (DataTuple tuple a)

tupleSize :: (ElementTuple tuple) => tuple -> Int
tupleSize =
   getSum . MW.execWriter . indexTupleA (\x -> MW.tell (Sum 1) >> return x)

indexTuple ::
   (ElementTuple tuple) => (Element -> a) -> tuple -> DataTuple tuple a
indexTuple extend = runIdentity . indexTupleA (Identity . extend)

{- |
>>> rnf (Shape.NestedTuple (Shape.Element 1, Shape.Element 2))
()
>>> rnf (Shape.NestedTuple (Shape.Element 1, (Shape.Element 2, Shape.Element 3)))
()
>>> isBottom $ rnf (Shape.NestedTuple (Shape.Element undefined, Shape.Element 2))
True
>>> isBottom $ rnf (Shape.NestedTuple (Shape.Element undefined, (Shape.Element 2, Shape.Element 3)))
True
>>> isBottom $ rnf (Shape.NestedTuple (Shape.Element 1, (Shape.Element undefined, Shape.Element 3)))
True
>>> isBottom $ rnf (Shape.NestedTuple (Shape.Element 1, (Shape.Element 2, Shape.Element undefined)))
True
-}
instance (ElementTuple tuple) => NFData (NestedTuple ixtype tuple) where
   rnf (NestedTuple tuple) =
      execStrictUnitWriter $ indexTupleA ((StrictUnitWriter$!) . rnf) tuple

data StrictUnitWriter a = StrictUnitWriter a

execStrictUnitWriter :: StrictUnitWriter a -> ()
execStrictUnitWriter (StrictUnitWriter _) = ()

instance Functor StrictUnitWriter where
   fmap f (StrictUnitWriter a) = StrictUnitWriter $ f a

instance Applicative StrictUnitWriter where
   pure = StrictUnitWriter
   StrictUnitWriter f <*> StrictUnitWriter a = StrictUnitWriter $ f a

instance Monad StrictUnitWriter where
   return = pure
   StrictUnitWriter a >>= k = k a



class (ElementTuple tuple) => AccessorTuple tuple where
   tupleAccessors :: tuple -> [tuple -> Element]

class (ElementTuple tuple, Eq tuple) => StaticTuple tuple where
   staticTuple :: MS.State Element tuple


instance ElementTuple () where
   type DataTuple () x = ()
   indexTupleA _ () = pure ()

instance AccessorTuple () where
   tupleAccessors () = []

instance StaticTuple () where
   staticTuple = return ()


instance ElementTuple Element where
   type DataTuple Element x = x
   indexTupleA extend = extend

instance AccessorTuple Element where
   tupleAccessors _ = [id]

instance StaticTuple Element where
   staticTuple = do
      ix <- MS.get
      MS.modify (\(Element k) -> Element (k+1))
      return ix


instance (ElementTuple a, ElementTuple b) => ElementTuple (a,b) where
   type DataTuple (a,b) x = (DataTuple a x, DataTuple b x)
   indexTupleA extend (a,b) =
      App.lift2 (,) (indexTupleA extend a) (indexTupleA extend b)

instance (AccessorTuple a, AccessorTuple b) => AccessorTuple (a,b) where
   tupleAccessors (a,b) =
      map (.fst) (tupleAccessors a) ++ map (.snd) (tupleAccessors b)

instance (StaticTuple a, StaticTuple b) => StaticTuple (a,b) where
   staticTuple = App.lift2 (,) staticTuple staticTuple


instance
   (ElementTuple a, ElementTuple b, ElementTuple c) =>
      ElementTuple (a,b,c) where
   type DataTuple (a,b,c) x = (DataTuple a x, DataTuple b x, DataTuple c x)
   indexTupleA extend (a,b,c) =
      App.lift3 (,,)
         (indexTupleA extend a) (indexTupleA extend b) (indexTupleA extend c)

instance
   (AccessorTuple a, AccessorTuple b, AccessorTuple c) =>
      AccessorTuple (a,b,c) where
   tupleAccessors (a,b,c) =
      map (.fst3) (tupleAccessors a) ++
      map (.snd3) (tupleAccessors b) ++
      map (.thd3) (tupleAccessors c)

instance
   (StaticTuple a, StaticTuple b, StaticTuple c) =>
      StaticTuple (a,b,c) where
   staticTuple = App.lift3 (,,) staticTuple staticTuple staticTuple


instance
   (ElementTuple a, ElementTuple b, ElementTuple c, ElementTuple d) =>
      ElementTuple (a,b,c,d) where
   type DataTuple (a,b,c,d) x =
         (DataTuple a x, DataTuple b x, DataTuple c x, DataTuple d x)
   indexTupleA extend (a,b,c,d) =
      App.lift4 (,,,)
         (indexTupleA extend a) (indexTupleA extend b)
         (indexTupleA extend c) (indexTupleA extend d)

instance
   (AccessorTuple a, AccessorTuple b, AccessorTuple c, AccessorTuple d) =>
      AccessorTuple (a,b,c,d) where
   tupleAccessors (a,b,c,d) =
      map (.(\(i,_,_,_) -> i)) (tupleAccessors a) ++
      map (.(\(_,i,_,_) -> i)) (tupleAccessors b) ++
      map (.(\(_,_,i,_) -> i)) (tupleAccessors c) ++
      map (.(\(_,_,_,i) -> i)) (tupleAccessors d)

instance
   (StaticTuple a, StaticTuple b, StaticTuple c, StaticTuple d) =>
      StaticTuple (a,b,c,d) where
   staticTuple = App.lift4 (,,,) staticTuple staticTuple staticTuple staticTuple


instance (ElementTuple a) => ElementTuple (Complex a) where
   type DataTuple (Complex a) x = Complex (DataTuple a x)
   indexTupleA extend (a:+b) =
      App.lift2 (:+) (indexTupleA extend a) (indexTupleA extend b)

instance (AccessorTuple a, RealFloat a) => AccessorTuple (Complex a) where
   tupleAccessors (a:+b) =
      map (.realPart) (tupleAccessors a) ++ map (.imagPart) (tupleAccessors b)

instance (StaticTuple a) => StaticTuple (Complex a) where
   staticTuple = App.lift2 (:+) staticTuple staticTuple


instance (ElementTuple tuple) => C (NestedTuple ixtype tuple) where
   size (NestedTuple tuple) = tupleSize tuple

instance (StaticTuple tuple) => Static (NestedTuple ixtype tuple) where
   static = NestedTuple $ MS.evalState staticTuple $ Element 0

-- requires FlexibleInstances
instance (AccessorTuple tuple) => Indexed (NestedTuple TupleAccessor tuple) where
   type Index (NestedTuple TupleAccessor tuple) = tuple -> Element
   indices (NestedTuple tuple) = tupleAccessors tuple
   unifiedOffset (NestedTuple tuple) ix =
      case ix tuple of Element k -> return k



newtype ElementIndex tuple = ElementIndex Int
   deriving (Eq, Ord, Show)

instance (ElementTuple tuple) => Indexed (NestedTuple TupleIndex tuple) where
   type Index (NestedTuple TupleIndex tuple) = ElementIndex tuple
   indices (NestedTuple tuple) =
      map ElementIndex $ take (tupleSize tuple) [0..]
   unifiedOffset (NestedTuple _tuple) (ElementIndex k) = return k

instance (ElementTuple tuple) => Pattern (NestedTuple TupleIndex tuple) where
   type DataPattern (NestedTuple TupleIndex tuple) x = DataTuple tuple x
   indexPattern extend (NestedTuple tuple) =
      let elemIx :: tuple -> Element -> ElementIndex tuple
          elemIx _ (Element k) = ElementIndex k
      in indexTuple (extend . elemIx tuple) tuple

indexTupleFromShape ::
   (ElementTuple tuple) =>
   NestedTuple TupleIndex tuple -> DataTuple tuple (ElementIndex tuple)
indexTupleFromShape = indexPattern id




nextCounter :: MS.State Int Int
nextCounter = do k <- MS.get; MS.put (k+1); return k

{- |
Shape for arrays that hold elements
that can alternatively be stored in a 'Traversable' record.
-}
newtype Record f = Record {getRecord :: f Element}

instance (Foldable f) => Eq (Record f) where
   Record sh0 == Record sh1  =  Fold.toList sh0 == Fold.toList sh1
{-
instance (Eq (f Element)) => Eq (Record f) where
   Record sh0 == Record sh1  =  sh0 == sh1
-}

newtype FieldIndex (f :: * -> *) = FieldIndex Int
   deriving (Eq, Show)

instance (Foldable f) => C (Record f) where
   size = foldLength . getRecord

instance (Applicative f, Traversable f) => Static (Record f) where
   static =
      Record $ flip MS.evalState 0 $ Trav.sequence $
      pure (fmap Element nextCounter)

instance (Foldable f) => Indexed (Record f) where
   type Index (Record f) = FieldIndex f
   indices (Record xs) = map FieldIndex $ Match.take (Fold.toList xs) [0..]
   unifiedOffset (Record _xs) (FieldIndex k) = return k

indexRecordFromShape ::
   (Traversable f) =>
   Record f -> f (FieldIndex f)
indexRecordFromShape (Record xs) = fmap (\(Element k) -> FieldIndex k) xs



{- |
Dynamically build a shape and its indices in the 'Construction' monad.
-}
newtype Constructed tag = Constructed {constructedSize :: Int}
   deriving (Eq, Show)

newtype ConsIndex tag = ConsIndex Int
   deriving (Eq, Show)

newtype Construction tag a = Construction (MS.State Int a)

instance Functor (Construction tag) where
   fmap f (Construction m) = Construction $ fmap f m

instance Applicative (Construction tag) where
   pure = Construction . pure
   Construction f  <*>  Construction a = Construction $ f<*>a

instance Monad (Construction tag) where
   return = pure
   Construction am  >>=  k  =
      Construction $ am >>= \a -> case k a of Construction bm -> bm

construct :: Construction tag a -> (Constructed tag, a)
construct (Construction m) =
   case MS.runState m 0 of (a, sz) -> (Constructed sz, a)

consIndex :: Construction tag (ConsIndex tag)
consIndex = Construction $ fmap ConsIndex nextCounter


instance C (Constructed tag) where
   size = constructedSize

instance Indexed (Constructed tag) where
   type Index (Constructed tag) = ConsIndex tag
   indices (Constructed len) = map ConsIndex $ take len [0..]
   unifiedOffset (Constructed len) =
      let f = unifiedOffset (ZeroBased len) in \(ConsIndex k) -> f k
   inBounds (Constructed len) (ConsIndex ix) = inBounds (ZeroBased len) ix

instance InvIndexed (Constructed tag) where
   unifiedIndexFromOffset (Constructed len) =
      fmap ConsIndex . unifiedIndexFromOffset (ZeroBased len)
