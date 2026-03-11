module Synthesizer.Zip where

import qualified Synthesizer.Generic.Cut as CutG

import qualified Control.Arrow as Arrow
import Control.Arrow (Arrow, (<<<), (^<<), (<<^), )

import Data.Monoid (Monoid, mempty, mappend, )
import Data.Semigroup (Semigroup, (<>), )


{- |
Parallel combination of two signals of equal length.
-}
data T a b = Cons {first :: a, second :: b}

{- |
Zip together two signals.
It is a checked error if their lengths differ.
-}
consChecked ::
   (CutG.Read a, CutG.Read b) =>
   String -> a -> b -> T a b
consChecked name a b =
   let lenA = CutG.length a
       lenB = CutG.length b
   in  if lenA == lenB
         then Cons a b
         else error $ "different lengths " ++
              show lenA ++ " vs. " ++ show lenB ++ " in " ++ name

{- |
Zip together two signals
and shorten them to the length of the shorter one.
-}
consShorten ::
   (CutG.Transform a, CutG.Transform b) =>
   a -> b -> T a b
consShorten a b =
   let len = min (CutG.length a) (CutG.length b)
   in  Cons (CutG.take len a) (CutG.take len b)



arrowFirst ::
   Arrow arrow =>
   arrow a b -> arrow (T a c) (T b c)
arrowFirst arrow =
   uncurry Cons
   ^<<
   Arrow.first arrow
   <<^
   (\(Cons a b) -> (a,b))

arrowSecond ::
   Arrow arrow =>
   arrow a b -> arrow (T c a) (T c b)
arrowSecond arrow =
   uncurry Cons
   ^<<
   Arrow.second arrow
   <<^
   (\(Cons a b) -> (a,b))

arrowFirstShorten ::
   (Arrow arrow, CutG.Transform b, CutG.Transform c) =>
   arrow a b -> arrow (T a c) (T b c)
arrowFirstShorten arrow =
   uncurry consShorten
   ^<<
   Arrow.first arrow
   <<^
   (\(Cons a b) -> (a,b))

arrowSecondShorten ::
   (Arrow arrow, CutG.Transform b, CutG.Transform c) =>
   arrow a b -> arrow (T c a) (T c b)
arrowSecondShorten arrow =
   uncurry consShorten
   ^<<
   Arrow.second arrow
   <<^
   (\(Cons a b) -> (a,b))


arrowFanout ::
   Arrow arrow =>
   arrow a b -> arrow a c -> arrow a (T b c)
arrowFanout b c =
   uncurry Cons  Arrow.^<<  b Arrow.&&& c

arrowSplit ::
   Arrow arrow =>
   arrow a c -> arrow b d -> arrow (T a b) (T c d)
arrowSplit x y =
   uncurry Cons  Arrow.^<<  x Arrow.*** y  Arrow.<<^  (\(Cons a b) -> (a,b))


arrowFanoutShorten ::
   (Arrow arrow, CutG.Transform a, CutG.Transform b, CutG.Transform c) =>
   arrow a b -> arrow a c -> arrow a (T b c)
arrowFanoutShorten a b =
   arrowSplitShorten a b <<^ (\x -> Cons x x)

arrowSplitShorten ::
   (Arrow arrow,
    CutG.Transform a, CutG.Transform b, CutG.Transform c, CutG.Transform d) =>
   arrow a c -> arrow b d -> arrow (T a b) (T c d)
arrowSplitShorten a b =
   arrowFirstShorten a <<< arrowSecondShorten b


instance (Semigroup a, Semigroup b) => Semigroup (T a b) where
   Cons a0 b0 <> Cons a1 b1 = Cons (a0 <> a1) (b0 <> b1)

instance (Monoid a, Monoid b) => Monoid (T a b) where
   mempty = Cons mempty mempty
   mappend (Cons a0 b0) (Cons a1 b1) =
      Cons (mappend a0 a1) (mappend b0 b1)

instance (CutG.Read a, CutG.Read b) => CutG.Read (T a b) where
   {-# INLINE null #-}
   null (Cons a b) =
      case (CutG.null a, CutG.null b) of
         (False, False) -> False
         (True, True) -> True
         _ -> error "Zipped signals: one is empty and the other one is not"
   {-# INLINE length #-}
   length (Cons a b) =
      let lenA = CutG.length a
          lenB = CutG.length b
      in  if lenA == lenB
            then lenA
            else error "Zipped signals: the lengths differ"

{-
Parallel combination of two signals
where the combined signal has the length of the shorter member.
This is like in zipWith.

instance (CutG.Read a, CutG.Read b) => CutG.Read (Parallel a b) where
   null (Parallel a b) = CutG.null a || CutG.null b
   length (Parallel a b) = min (CutG.length a) (CutG.length b)
-}

instance (CutG.NormalForm a, CutG.NormalForm b) => CutG.NormalForm (T a b) where
   {-# INLINE evaluateHead #-}
   evaluateHead (Cons a b) =
      case (CutG.evaluateHead a, CutG.evaluateHead b) of
         ((), ()) -> ()

instance (CutG.Transform a, CutG.Transform b) => CutG.Transform (T a b) where
   {-# INLINE take #-}
   take n (Cons a b) =
      Cons (CutG.take n a) (CutG.take n b)
   {-# INLINE drop #-}
   drop n (Cons a b) =
      Cons (CutG.drop n a) (CutG.drop n b)
   {-# INLINE splitAt #-}
   splitAt n (Cons a b) =
      let (a0,a1) = CutG.splitAt n a
          (b0,b1) = CutG.splitAt n b
      in  (Cons a0 b0, Cons a1 b1)
   {-# INLINE dropMarginRem #-}
   dropMarginRem n m (Cons a0 b0) =
      let (ka,a1) = CutG.dropMarginRem n m a0
          (kb,b1) = CutG.dropMarginRem n m b0
      in  if ka==kb
            then (ka, Cons a1 b1)
            else error "Zip.dropMarginRem: margins differ"
   {-# INLINE reverse #-}
   reverse (Cons a b) =
      Cons (CutG.reverse a) (CutG.reverse b)
