{- |
Functions that may be useful, but I'm uncertain.
-}
module Data.StorableVector.Private where


import Data.StorableVector (empty, unfoldrN, viewL, length, )
import Data.StorableVector.Base

import qualified Data.Strictness.HT as Strict

import Foreign.Storable         (Storable(..))

import qualified System.Unsafe as Unsafe

import Control.DeepSeq (NFData, rnf, deepseq, )

import Prelude hiding (length, )



{- |
This implementation is based on viewL
and thus not as fast as possible.
-}
zipWithViewL :: (Storable a, Storable b, Storable c) =>
   (a -> b -> c) -> Vector a -> Vector b -> Vector c
zipWithViewL f ps0 qs0 =
   fst $ unfoldrN
      (min (length ps0) (length qs0))
      (\(ps,qs) ->
         do (ph,pt) <- viewL ps
            (qh,qt) <- viewL qs
            return (f ph qh, (pt,qt)))
      (ps0,qs0)


zipWithIndex :: (Storable a, Storable b, Storable c) =>
   (a -> b -> c) -> Vector a -> Vector b -> Vector c
zipWithIndex f ps qs =
   fst $ unfoldrN
      (min (length ps) (length qs))
      (\i -> Just (f (unsafeIndex ps i) (unsafeIndex qs i), succ i))
      0


unfoldrStrictN :: (Storable b, NFData a) => Int -> (a -> Maybe (b, a)) -> a -> (Vector b, Maybe a)
-- unfoldrStrictN :: (Storable b) => Int -> (a -> Maybe (b, a)) -> a -> (Vector b, Maybe a)
unfoldrStrictN i f x0 =
   if i <= 0
     then (empty, Just x0)
     else Unsafe.performIO $ createAndTrim' i $ \p -> go p 0 x0
       {-
       go must not be strict in the accumulator
       since otherwise packN would be too strict.
       -}
       where
          go = Strict.arguments3 $ \p n -> \x ->
             if n == i
               then return (0, n, Just x)
               else
                 case f x of
                   Nothing     -> return (0, n, Nothing)
                   Just (w,x') -> do poke p w
--                                     go (incPtr p) (n+1) $! x'
                                     go (incPtr p) (n+1) (x' `deepseq` x')
--                                     seq (rnf x') (((go $! incPtr p) $! n+1) $! x')
{-# INLINE unfoldrStrictN #-}

unfoldrTransitionN :: (Storable b) => Int -> (a -> a) -> (a -> Maybe b) -> a -> (Vector b, a)
unfoldrTransitionN n trans emit x =
   if n <= 0
     then (empty, x)
     else Unsafe.performIO $ createAndTrim' n $ \p ->
       case emit x of
         Nothing -> return (0, n, x)
         Just y0 -> poke p y0 >>
           {-
           go must not be strict in the accumulator
           since otherwise packN would be too strict.
           -}
           let go = Strict.arguments2 $ \p0 i0 -> \x0 ->
                  {-
                  We run 'emit' in order to evaluate the new state.
                  We need to return this new state
                  also in case the array is full.
                  The drawback is, that the whole vector becomes undefined
                  if only the state after the last element is undefined.
                  This is the same situation as in an unfoldr with strict state.
                  -}
                  let i1 = i0-1
                      x1 = trans x0
                  in  case emit x1 of
                         Nothing -> return (0, n-i1, x1)
                         Just y1 ->
                            if i1 == 0
                              then return (0, n, x1)
                              else
                                let p1 = incPtr p0
                                in  do poke p1 y1
                                       go p1 i1 x1
{-
                  let i1 = i0-1
                  in  if i1 == 0
                        then return (0, n, x0)
                        else
                          let x1 = trans x0
                              p1 = incPtr p0
                          in  case emit x1 of
                                Nothing -> return (0, n-i1, x1)
                                Just y1 -> do poke p1 y1
                                              go p1 i1 x1
-}
           in  go p n x
{-# INLINE unfoldrTransitionN #-}

-- | /O(n)/ Like 'unfoldrN' this function builds a 'Vector' from a seed
-- value.  However, it does always return a state value.
-- The vector construction can be aborted either by reaching
-- the given maximum size or by returning 'Nothing' as element.
--
-- The following equation relates 'unfoldrN' and 'unfoldrStateN':
--
-- > unfoldrN n f s ==
-- >    unfoldrStateN n
-- >       (maybe (error "state will be always Just")
-- >           ((\a -> (fmap fst a, fmap snd a)) . f))
-- >       (Just s)
--
-- It is not possible to express 'unfoldrNState' in terms of 'unfoldrN'.
--
unfoldrStateN :: (Storable b) => Int -> (a -> (Maybe b, a)) -> a -> (Vector b, a)
unfoldrStateN i f x0 =
   if i <= 0
     then (empty, x0)
     else Unsafe.performIO $ createAndTrim' i $ \p -> go p 0 x0
       {-
       go must not be strict in the accumulator
       since otherwise packN would be too strict.
       -}
       where
          go = Strict.arguments2 $ \p n -> \x ->
             if n == i
               then return (0, n, x)
               else
                 let (my,x') = f x
                 in  case my of
                       Nothing -> return (0, n, x)
                       Just w  -> do poke p w
                                     go (incPtr p) (n+1) x'
{-# INLINE unfoldrStateN #-}
