-- | Deterministic and probabilistic values

module Numeric.Probability.Distribution where

import Numeric.Probability.Show (showR)
import qualified Numeric.Probability.Shape as Shape

import qualified Control.Functor.HT as FuncHT
import Control.Applicative (Applicative(..))
import Control.Monad (liftM, liftM2, join, )

import qualified Data.Foldable as Fold
import qualified Data.List.HT as ListHT
import qualified Data.Map  as Map
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Tuple.HT (mapFst, )
import Data.Ord.HT (comparing, )
import Data.Eq.HT (equating, )

import Prelude hiding (map, filter)


-- * Events
type Event a = a -> Bool

oneOf :: Eq a => [a] -> Event a
oneOf = flip elem

just :: Eq a => a -> Event a
just = (==)



-- * Distributions

{- |
Probability disribution

The underlying data structure is a list.
Unfortunately we cannot use a more efficient data structure
because the key type must be of class 'Ord',
but the 'Monad' class does not allow constraints for result types.
The Monad instance is particularly useful
because many generic monad functions make sense here,
monad transformers can be used
and the monadic design allows to simulate probabilistic games in an elegant manner.

We have the same problem like making "Data.Set" an instance of 'Monad',
see <http://www.randomhacks.net/articles/2007/03/15/data-set-monad-haskell-macros>

If you need efficiency, you should remove redundant elements by 'norm'.
'norm' converts to 'Data.Map' and back internally
and you can hope that the compiler fuses the lists with the intermediate Map structure.

The defined monad is equivalent to
@WriterT (Product prob) [] a@.
See <http://www.randomhacks.net/articles/2007/02/21/refactoring-probability-distributions>.
-}
newtype T prob a = Cons {decons :: [(a,prob)]}

certainly :: Num prob => a -> T prob a
certainly x = Cons [(x,1)]

instance Num prob => Monad (T prob) where
  return   = certainly
  d >>= f  = Cons [(y,q*p) | (x,p) <- decons d, (y,q) <- decons (f x)]

instance Num prob => Applicative (T prob) where
  pure     = certainly
  fm <*> m = Cons [(f x,q*p) | (f,p) <- decons fm, (x,q) <- decons m]

{-
Dist cannot be an instance of MonadPlus,
because there is no mzero
(it would be an empty list of events, but their probabilities do not sum up to 1)
and thus it breaks the normalization for the >>= combinator.
See for instance the Boys example:

   do f <- family
      guard (existsBoy f)
      return f

mplus is not associative because we have to normalize the sum of probabilities to 1.

instance MonadPlus Dist where
  mzero      = Cons []
  mplus d d' =
     if isZero d || isZero d'
       then mzero
       else unfoldD $ choose 0.5 d d'

isZero :: Dist a -> Bool
isZero (Cons d) = null d
-}


instance Functor (T prob) where
  fmap f (Cons d) = Cons [(f x,p) | (x,p) <- d]



errorMargin :: RealFloat prob => prob
errorMargin =
   let eps = 10 / fromInteger (floatRadix eps) ^ floatDigits eps
   in  eps

{- |
Check whether two distributions are equal when neglecting rounding errors.
We do not want to put this into an 'Eq' instance,
since it is not exact equivalence
and it seems to be too easy to mix it up with @liftM2 (==) x y@.
-}
approx :: (RealFloat prob, Ord a) =>
   T prob a -> T prob a ->
   Bool
approx (Cons xs) (Cons ys) =
   let (xse, xsp) = unzip (norm' xs)
       (yse, ysp) = unzip (norm' ys)
   in  xse == yse &&
       all (\p -> abs p < errorMargin) (zipWith (-) xsp ysp)


-- ** Auxiliary functions for constructing and working with distributions
lift :: (Num prob) => ([(a,prob)] -> [(b,prob)]) -> T prob a -> T prob b
lift f  = Cons . f . decons

size :: T prob a -> Int
size = length . decons

check :: (RealFloat prob, Show prob) => T prob a -> T prob a
check (Cons d) =
   if abs (1-sumP d) < errorMargin
     then Cons d
     else error ("Illegal distribution: total probability = "++show (sumP d))

-- | can fail because of rounding errors, better use 'fromFreqs'
cons :: (RealFloat prob, Show prob) => [(a,prob)] -> T prob a
cons = check . Cons

sumP :: Num prob => [(a,prob)] -> prob
sumP = sum . List.map snd

sortP :: Ord prob => [(a,prob)] -> [(a,prob)]
sortP = List.sortBy (comparing snd)

sortPDesc :: Ord prob => [(a,prob)] -> [(a,prob)]
sortPDesc = List.sortBy (flip $ comparing snd)

sortElem :: Ord a => [(a,prob)] -> [(a,prob)]
sortElem = List.sortBy (comparing fst)


-- ** Normalization = grouping
norm :: (Num prob, Ord a) => T prob a -> T prob a
norm = lift norm'

norm' :: (Num prob, Ord a) => [(a,prob)] -> [(a,prob)]
norm' =
   Map.toAscList . Map.fromListWith (+)

norm'' :: (Num prob, Ord a) => [(a,prob)] -> [(a,prob)]
norm'' =
   List.map (\xs ->
      case xs of
         ((x,_):_) -> (x, sum (List.map snd xs))
         _ -> error "Probability.Distribution.norm'': every sub-list in groupBy must be non-empty") .
   ListHT.groupBy (equating fst) . sortElem


-- | pretty printing
pretty :: (Ord a, Show a, Num prob, Ord prob) =>
   (prob -> String) -> T prob a -> String
pretty _ (Cons []) = "Impossible"
pretty showProb (Cons xs) =
   let w = maximum (List.map (length.show.fst) xs)
   in  concatMap
          (\(x,p) -> showR w x++' ': showProb p++"\n")
          (sortPDesc (norm' xs))

infix 0 //%

(//%) :: (Ord a, Show a) => T Rational a -> () -> IO ()
(//%) x () = putStr (pretty show x)

instance (Num prob, Ord prob, Show prob, Ord a, Show a) =>
      Show (T prob a) where
   showsPrec p (Cons xs) =
      showParen (p>10)
         (showString "fromFreqs " . showsPrec 11 (sortPDesc (norm' xs)))


{- |
We would like to have an equality test of type

> (==) :: T prob a -> T prob a -> T prob Bool

that is consistent with the 'Num' instance.
We would certainly define

> x==y = norm (liftM2 (==) x y)   .

However the 'Eq' class enforces the type

> T prob a -> T prob a -> Bool    .

We could implement this as check for equal distributions.
This would be inconsistent with the 'Num' instance
because it compares entire distributions,
not only individual outcomes.
Thus we provide this function as 'equal'.

I would prefer to omit the 'Eq' instance completely,
but unfortunately the 'Num' instance requires 'Eq' as superclass.
-}
instance Eq (T prob a) where
   (==) = error "Probability.Distribution.== cannot be implemented sensibly."

{-
instance (Num prob, Ord a) => Eq (T prob a) where
   (==) = equal
-}

equal :: (Num prob, Eq prob, Ord a) => T prob a -> T prob a -> Bool
equal = equating (decons . norm)

{-
The Num operations consider their operands as independent distributions
(like all operations on distributions do).
All functions normalize their results if normalization is lost by the plain operation.
This is essential for performance.

Thus @sum $ replicate 10 d@ is significantly faster
than @fmap sum $ replicateM 10 d@
-}
instance (Num prob, Ord prob, Ord a, Num a) => Num (T prob a) where
   fromInteger = return . fromInteger
   x + y = norm (liftM2 (+) x y)
   x - y = norm (liftM2 (-) x y)
   x * y = norm (liftM2 (*) x y)
   abs x = norm (liftM abs x)
   signum x = norm (liftM signum x)
   negate x = liftM negate x

instance (Num prob, Ord prob, Ord a, Fractional a) =>
      Fractional (T prob a) where
   fromRational = return . fromRational
   recip x = liftM recip x
   x / y = norm (liftM2 (/) x y)



-- * Spread: functions to convert a list of values into a distribution

-- | distribution generators
type Spread prob a = [a] -> T prob a

{- not a valid distribution
impossible :: T prob a
impossible = mzero
-}

choose :: Num prob => prob -> a -> a -> T prob a
choose p x y = Cons $ zip [x,y] [p,1-p]

enum :: Fractional prob => [Int] -> Spread prob a
enum  =  relative . List.map fromIntegral

{- |
Give a list of frequencies, they do not need to sum up to 1.
-}
relative :: Fractional prob => [prob] -> Spread prob a
relative ns = fromFreqs . flip zip ns

shape :: Fractional prob =>
   (prob -> prob) -> Spread prob a
shape _ [] = error "Probability.shape: empty list"
shape f xs =
   let incr = 1 / fromIntegral (length xs - 1)
       ps = List.map f (iterate (+incr) 0)
   in  fromFreqs (zip xs ps)

linear :: Fractional prob => Spread prob a
linear = shape Shape.linear

uniform :: Fractional prob => Spread prob a
uniform = shape Shape.uniform

negExp :: Floating prob => Spread prob a
negExp = shape Shape.negExp

normal :: Floating prob => Spread prob a
normal = shape Shape.normal



-- | extracting and mapping the domain of a distribution
extract :: T prob a -> [a]
extract = List.map fst . decons

-- | 'fmap' with normalization
map :: (Num prob, Ord b) =>
   (a -> b) -> T prob a -> T prob b
map f = norm . fmap f


{- |
unfold a distribution of distributions into one distribution,
this is 'Control.Monad.join' with normalization.
-}
unfold :: (Num prob, Ord a) =>
   T prob (T prob a) -> T prob a
unfold = norm . join


-- | conditional distribution
cond :: (Num prob) =>
   T prob Bool ->
   T prob a {-^ True -} ->
   T prob a {-^ False -} ->
   T prob a
cond b d d'  =  b >>= \c -> if c then d else d'

truth :: (Num prob) => T prob Bool -> prob
truth dist =
   case decons (norm dist) of
      (b,p):_ -> if b then p else 1-p
      [] -> error "Probability.truth: corrupt boolean random variable"


infixl 1 >>=?
infixr 1 ?=<<

-- | conditional probability, identical to 'Dist.filter'
(?=<<) :: (Fractional prob) =>
   (a -> Bool) -> T prob a -> T prob a
(?=<<) = filter

{- |
'Dist.filter' in infix form.
Can be considered an additional monadic combinator,
which can be used where you would want 'Control.Monad.guard' otherwise.
-}
(>>=?) :: (Fractional prob) =>
   T prob a -> (a -> Bool) -> T prob a
(>>=?) = flip filter


-- | filtering distributions
data Select a = Case a | Other
                deriving (Eq,Ord,Show)

above, below :: (Num prob, Ord prob, Ord a) =>
   prob -> T prob a -> T prob (Select a)
above p = select (>=p)
below p = select (<=p)

select :: (Num prob, Ord prob, Ord a) =>
   (prob -> Bool) -> T prob a -> T prob (Select a)
select condp = lift $ \d ->
   let (d1,d2) = Map.partition condp $ Map.fromListWith (+) d
   in  (Other, Fold.sum d2) :
          List.map (mapFst Case) (Map.toAscList d1)

fromFreqs :: (Fractional prob) => [(a,prob)] -> T prob a
fromFreqs xs = Cons (List.map (\(x,p)->(x,p/q)) xs)
           where q = sumP xs

filter :: (Fractional prob) =>
   (a -> Bool) -> T prob a -> T prob a
filter p = fromFreqs . List.filter (p . fst) . decons

mapMaybe :: (Fractional prob) =>
   (a -> Maybe b) -> T prob a -> T prob b
mapMaybe f =
   fromFreqs . Maybe.mapMaybe (FuncHT.mapFst f) . decons


-- | selecting from distributions
selectP :: (Num prob, Ord prob) => T prob a -> prob -> a
selectP (Cons d) p = scanP p d

scanP :: (Num prob, Ord prob) => prob -> [(a,prob)] -> a
scanP p ((x,q):ps) =
   if p<=q || null ps
     then x
     else scanP (p-q) ps
scanP _ [] = error "Probability.scanP: distribution must be non-empty"

infixr 1 ??

(??) :: Num prob => Event a -> T prob a -> prob
(??) p = sumP . List.filter (p . fst) . decons


-- | expectation value
expected :: (Num a) => T a a -> a
expected = sum . List.map (\(x,p) -> x * p) . decons

-- | statistical analyses
variance :: (Num a) => T a a -> a
variance x =
   expected (fmap ((^(2::Int)) . subtract (expected x)) x)

stdDev :: (Floating a) => T a a -> a
stdDev = sqrt . variance
