{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE EmptyDataDecls #-}
module Data.FixedLength (
   T, Position, List, Length,
   Index, Zero, Succ(Stop, Succ),
   toList, showsPrec,
   map, zipWith, sequenceA, repeat,
   index, update, indices, indicesInt, numFromIndex, indexFromNum,
   GE1, GE2, GE3, GE4, GE5, GE6, GE7, GE8,
   i0, i1, i2, i3, i4, i5, i6, i7,
   fromFixedList, toFixedList,
   (!:), end, singleton,
   viewL, switchL, head, tail, switchEnd,
   Curried, curry, uncurry,
   ConsAll, NumberOfArguments, ResultSize, ResultElement, consAll,
   minimum, maximum,
   ) where

import qualified Type.Data.Num.Unary as Unary
import Type.Data.Num.Unary.Literal (U1)
import Type.Data.Num.Unary (Positive, Natural, switchNat)

import qualified Foreign.Storable.FixedArray as StoreArray
import qualified Foreign.Storable.Traversable as StoreTrav
import Foreign.Storable (Storable, sizeOf, alignment, poke, peek)

import qualified Data.NonEmpty as NonEmpty
import qualified Data.Empty as Empty

import qualified Control.Applicative as App
import qualified Data.Traversable as Trav
import qualified Data.Foldable as Fold
import qualified Data.Functor.Classes as FunctorC
import qualified Data.List as List
import Control.Applicative (Applicative, liftA2)
import Data.Traversable (Traversable, foldMapDefault)
import Data.Foldable (Foldable, foldMap)
import Data.Functor.Classes (Eq1, eq1, Show1, showsPrec1)
import Data.Maybe (Maybe(Nothing, Just))
import Data.List ((++))
import Data.Word (Word)

import Data.Function (($), (.))
import Data.Bool (Bool(False, True))
import Data.Ord (Ord, Ordering(LT,EQ,GT), compare, (>))
import Data.Eq (Eq, (==))

import Text.Show.HT (concatS)

import qualified Prelude as P
import Prelude (Functor, fmap, Show, ShowS, Int, (+), (-), error)


type family List n :: * -> *
type instance List Unary.Zero = Empty.T
type instance List (Unary.Succ n) = NonEmpty.T (List n)

type family Length (f :: * -> *)
type instance Length Empty.T = Unary.Zero
type instance Length (NonEmpty.T f) = Unary.Succ (Length f)


newtype T n a = Cons (List n a)

fromFixedList :: List n a -> T n a
fromFixedList = Cons

toFixedList :: T n a -> List n a
toFixedList (Cons xs) = xs


end :: T Unary.Zero a
end = Cons Empty.Cons

infixr 5 !:

(!:) :: a -> T n a -> T (Unary.Succ n) a
x !: Cons xs = Cons $ NonEmpty.Cons x xs

viewL :: T (Unary.Succ n) a -> (a, T n a)
viewL (Cons (NonEmpty.Cons x xs)) = (x, Cons xs)

switchL :: (a -> T n a -> b) -> (T (Unary.Succ n) a -> b)
switchL f = P.uncurry f . viewL

switchEnd :: b -> T Unary.Zero a -> b
switchEnd x (Cons Empty.Cons) = x


newtype WithPos a b n = WithPos {runWithPos :: T n a -> b}

withPos ::
   (Positive n) =>
   (forall m. Natural m => T (Unary.Succ m) a -> b) -> T n a -> b
withPos f = runWithPos $ Unary.switchPos (WithPos f)

head :: (Positive n) => T n a -> a
head = withPos (P.fst . viewL)

tail :: T (Unary.Succ n) a -> T n a
tail = P.snd . viewL

singleton :: a -> T U1 a
singleton x = x!:end

minimum :: (Positive n, Ord a) => T n a -> a
minimum = withPos (NonEmpty.minimum . switchL NonEmpty.cons)

maximum :: (Positive n, Ord a) => T n a -> a
maximum = withPos (NonEmpty.maximum . switchL NonEmpty.cons)



instance (Natural n, Eq a) => Eq (T n a) where
   (==)  =  eq1

instance (Natural n) => Eq1 (T n) where
   liftEq eq xs ys  =  Fold.and $ zipWith eq xs ys


showsPrec :: (Natural n, Show a) => Int -> T n a -> ShowS
showsPrec = showsPrec1

liftShowsPrec_ :: (Natural n) => (Int -> a -> ShowS) -> Int -> T n a -> ShowS
liftShowsPrec_ showsPrec_ p =
   P.showParen (p>5) . concatS .
   List.intersperse (P.showString "!:") .
   (++ [P.showString "end"]) .
   List.map (showsPrec_ 6) . toList

instance (Natural n, Show a) => Show (T n a) where
   showsPrec = showsPrec1

instance (Natural n) => Show1 (T n) where
   liftShowsPrec showsPrec_ _showsList = liftShowsPrec_ showsPrec_


toList :: (Natural n) => T n a -> [a]
toList = Fold.toList


newtype Map a b n = Map {runMap :: T n a -> T n b}

map :: Natural n => (a -> b) -> T n a -> T n b
map f =
   runMap $
   switchNat
      (Map $ switchEnd end)
      (Map $ switchL $ \x xs -> f x !: map f xs)


newtype Sequence f a n = Sequence {runSequence :: T n (f a) -> f (T n a)}

sequenceA :: (Applicative f, Natural n) => T n (f a) -> f (T n a)
sequenceA =
   runSequence $
   switchNat
      (Sequence $ switchEnd $ App.pure end)
      (Sequence $ switchL $ \x xs -> liftA2 (!:) x $ sequenceA xs)


newtype Repeat a n = Repeat {runRepeat :: T n a}

repeat :: Natural n => a -> T n a
repeat a =
   runRepeat $
   switchNat
      (Repeat end)
      (Repeat $ a !: repeat a)


newtype Zip a b c n = Zip {runZip :: T n a -> T n b -> T n c}

zipWith :: Natural n => (a -> b -> c) -> T n a -> T n b -> T n c
zipWith f =
   runZip $
   switchNat
      (Zip $ switchEnd $ switchEnd end)
      (Zip $ switchL $ \a as -> switchL $ \b bs -> f a b !: zipWith f as bs)


instance Natural n => Functor (T n) where
   fmap = map

instance Natural n => Foldable (T n) where
   foldMap = foldMapDefault

instance Natural n => Traversable (T n) where
   sequenceA = sequenceA

instance Natural n => Applicative (T n) where
   pure = repeat
   f <*> x = zipWith ($) f x


type family Position n
type instance Position Unary.Zero = Zero
type instance Position (Unary.Succ n) = Succ (Position n)

data Zero
data Succ pos = Stop | Succ pos deriving (Eq, Ord, Show)

instance Eq Zero where _==_ = True
instance Ord Zero where compare _ _ = EQ


newtype Index n = Index (Position n)

instance (Natural n) => Show (Index n) where
   show i = 'i' : P.show (numFromIndex i)

unpackSucc :: Index (Unary.Succ n) -> Succ (Index n)
unpackSucc (Index n1) =
   case n1 of
      Stop -> Stop
      Succ n0 -> Succ $ Index n0


newtype Update a n = Update {runUpdate :: Index n -> T n a -> T n a}

update :: Natural n => (a -> a) -> Index n -> T n a -> T n a
update f =
   runUpdate $
   switchNat
      (Update $ \ _ xs -> xs)
      (Update $ \pos0 -> switchL $ \x xs ->
         case unpackSucc pos0 of
            Stop -> f x !: xs
            Succ pos1 -> x !: update f pos1 xs)

newtype PeekIndex a n = PeekIndex {runIndex :: Index n -> T n a -> a}

index :: Natural n => Index n -> T n a -> a
index =
   runIndex $
   switchNat
      (PeekIndex $ \ _ {- Zero -} -> switchEnd $ error "impossible index")
      (PeekIndex $ \pos0 -> switchL $ \x xs ->
          case unpackSucc pos0 of
             Stop -> x
             Succ pos1 -> index pos1 xs)

newtype Indices n = Indices {runIndices :: T n (Index n)}

indices :: Natural n => T n (Index n)
indices =
   runIndices $
   switchNat
      (Indices end)
      (Indices $ i0 !: map succ indices)

indicesInt :: Natural n => T n Int
indicesInt =
   NonEmpty.init $ NonEmpty.scanl (+) 0 $ App.pure 1


newtype NumFromIndex n = NumFromIndex {runNumFromIndex :: Index n -> Word}

numFromIndex :: Natural n => Index n -> Word
numFromIndex =
   runNumFromIndex $
   switchNat
      (NumFromIndex $ \_ -> error "numFromIndex")
      (NumFromIndex $ \n ->
         case unpackSucc n of
            Stop -> 0
            Succ m -> 1 + numFromIndex m)


newtype
   IndexFromNum n = IndexFromNum {runIndexFromNum :: Word -> Maybe (Index n)}

indexFromNum :: Natural n => Word -> Maybe (Index n)
indexFromNum =
   runIndexFromNum $
   switchNat
      (IndexFromNum $ \ _k -> Nothing)
      (IndexFromNum $ \k ->
         if k==0 then Just i0 else fmap succ $ indexFromNum (k-1))


newtype Compare a n = Compare {runCompare :: Index n -> Index n -> a}

instance (Natural n) => Eq (Index n) where
   (==) =
      runCompare $
      switchNat
         (Compare $ \_ _ -> error "equalIndex")
         (Compare $ \i j ->
            case (unpackSucc i, unpackSucc j) of
               (Succ k, Succ l) -> k == l
               (Stop, Stop) -> True
               _ -> False)

instance (Natural n) => Ord (Index n) where
   compare =
      runCompare $
      switchNat
         (Compare $ \_ _ -> error "compareIndex")
         (Compare $ \i j ->
            case (unpackSucc i, unpackSucc j) of
               (Succ k, Succ l) -> compare k l
               (Stop, Stop) -> EQ
               (Stop, Succ _) -> LT
               (Succ _, Stop) -> GT)


type GE1 n = Unary.Succ n
type GE2 n = Unary.Succ (GE1 n)
type GE3 n = Unary.Succ (GE2 n)
type GE4 n = Unary.Succ (GE3 n)
type GE5 n = Unary.Succ (GE4 n)
type GE6 n = Unary.Succ (GE5 n)
type GE7 n = Unary.Succ (GE6 n)
type GE8 n = Unary.Succ (GE7 n)

succ :: Index n -> Index (Unary.Succ n)
succ (Index n) = Index (Succ n)

i0 :: Index (GE1 n); i0 = Index Stop
i1 :: Index (GE2 n); i1 = succ i0
i2 :: Index (GE3 n); i2 = succ i1
i3 :: Index (GE4 n); i3 = succ i2
i4 :: Index (GE5 n); i4 = succ i3
i5 :: Index (GE6 n); i5 = succ i4
i6 :: Index (GE7 n); i6 = succ i5
i7 :: Index (GE8 n); i7 = succ i6



type family Curried n a b
type instance Curried Unary.Zero a b = b
type instance Curried (Unary.Succ n) a b = a -> Curried n a b

newtype Curry a b n = Curry {runCurry :: (T n a -> b) -> Curried n a b}

curry :: (Unary.Natural n) => (T n a -> b) -> Curried n a b
curry =
   runCurry $
   Unary.switchNat
      (Curry $ ($ end))
      (Curry $ \f a -> curry $ \xs -> f (a!:xs))

newtype Uncurry a b n = Uncurry {runUncurry :: Curried n a b -> T n a -> b}

uncurry :: (Unary.Natural n) => Curried n a b -> T n a -> b
uncurry =
   runUncurry $
   Unary.switchNat
      (Uncurry switchEnd)
      (Uncurry $ \f -> switchL (\x -> uncurry (f x)))


class ConsAll f where
   type NumberOfArguments f
   type ResultSize f
   type ResultElement f
   consAux ::
      (NumberOfArguments f ~ m, ResultSize f ~ n, ResultElement f ~ a) =>
      (T m a -> T n a) -> f

instance ConsAll (T n a) where
   type NumberOfArguments (T n a) = Unary.Zero
   type ResultSize (T n a) = n
   type ResultElement (T n a) = a
   consAux f = f end

instance (a ~ ResultElement f, ConsAll f) => ConsAll (a -> f) where
   type NumberOfArguments (a->f) = Unary.Succ (NumberOfArguments f)
   type ResultSize (a->f) = ResultSize f
   type ResultElement (a->f) = ResultElement f
   consAux f x = consAux (f . (x!:))

consAll ::
   (ConsAll f, ResultSize f ~ n, NumberOfArguments f ~ n, Natural n) => f
consAll = consAux P.id


undefinedElem :: T n a -> a
undefinedElem _ = error "touched element by accident"

instance (Natural n, Storable a) => Storable (T n a) where
   sizeOf xs = StoreArray.sizeOfArray (P.length $ toList xs) (undefinedElem xs)
   alignment = alignment . undefinedElem
   peek = StoreTrav.peekApplicative
   poke = StoreTrav.poke
