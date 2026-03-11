{- |
Word with fixed length lists constructed from 'NonEmpty' and 'Empty' types.
-}
module Data.FixedLengthList where

import qualified Data.NonEmptyPrivate as NonEmpty
import qualified Data.Empty as Empty
import Data.NonEmptyPrivate (uncurrier, currier)



type T0 = Empty.T
type T1 = NonEmpty.T T0
type T2 = NonEmpty.T T1
type T3 = NonEmpty.T T2
type T4 = NonEmpty.T T3
type T5 = NonEmpty.T T4
type T6 = NonEmpty.T T5
type T7 = NonEmpty.T T6
type T8 = NonEmpty.T T7
type T9 = NonEmpty.T T8

type Func0 a b = b
type Func1 a b = a -> Func0 a b
type Func2 a b = a -> Func1 a b
type Func3 a b = a -> Func2 a b
type Func4 a b = a -> Func3 a b
type Func5 a b = a -> Func4 a b
type Func6 a b = a -> Func5 a b
type Func7 a b = a -> Func6 a b
type Func8 a b = a -> Func7 a b
type Func9 a b = a -> Func8 a b



uncurry0 :: Func0 a b -> T0 a -> b
uncurry0 = Empty.switch

uncurry1 :: Func1 a b -> T1 a -> b
uncurry1 = uncurrier uncurry0

uncurry2 :: Func2 a b -> T2 a -> b
uncurry2 = uncurrier uncurry1

uncurry3 :: Func3 a b -> T3 a -> b
uncurry3 = uncurrier uncurry2

uncurry4 :: Func4 a b -> T4 a -> b
uncurry4 = uncurrier uncurry3

uncurry5 :: Func5 a b -> T5 a -> b
uncurry5 = uncurrier uncurry4

uncurry6 :: Func6 a b -> T6 a -> b
uncurry6 = uncurrier uncurry5

uncurry7 :: Func7 a b -> T7 a -> b
uncurry7 = uncurrier uncurry6

uncurry8 :: Func8 a b -> T8 a -> b
uncurry8 = uncurrier uncurry7

uncurry9 :: Func9 a b -> T9 a -> b
uncurry9 = uncurrier uncurry8



curry0 :: (T0 a -> b) -> Func0 a b
curry0 f = f Empty.Cons

curry1 :: (T1 a -> b) -> Func1 a b
curry1 = currier curry0

curry2 :: (T2 a -> b) -> Func2 a b
curry2 = currier curry1

curry3 :: (T3 a -> b) -> Func3 a b
curry3 = currier curry2

curry4 :: (T4 a -> b) -> Func4 a b
curry4 = currier curry3

curry5 :: (T5 a -> b) -> Func5 a b
curry5 = currier curry4

curry6 :: (T6 a -> b) -> Func6 a b
curry6 = currier curry5

curry7 :: (T7 a -> b) -> Func7 a b
curry7 = currier curry6

curry8 :: (T8 a -> b) -> Func8 a b
curry8 = currier curry7

curry9 :: (T9 a -> b) -> Func9 a b
curry9 = currier curry8



consAll0 :: Func0 a (T0 a)
consAll0 = curry0 id

consAll1 :: Func1 a (T1 a)
consAll1 = curry1 id

consAll2 :: Func2 a (T2 a)
consAll2 = curry2 id

consAll3 :: Func3 a (T3 a)
consAll3 = curry3 id

consAll4 :: Func4 a (T4 a)
consAll4 = curry4 id

consAll5 :: Func5 a (T5 a)
consAll5 = curry5 id

consAll6 :: Func6 a (T6 a)
consAll6 = curry6 id

consAll7 :: Func7 a (T7 a)
consAll7 = curry7 id

consAll8 :: Func8 a (T8 a)
consAll8 = curry8 id

consAll9 :: Func9 a (T9 a)
consAll9 = curry9 id
