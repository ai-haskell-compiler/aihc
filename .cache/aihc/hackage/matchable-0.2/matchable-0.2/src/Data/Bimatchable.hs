module Data.Bimatchable(
  Bimatchable(..),
  bimapRecovered,
  eq2Default,
  liftEq2Default
) where

import           Control.Applicative

import           Data.Bifunctor
import           Data.Functor.Classes

import           Data.Tagged

-- | Containers that allows exact structural matching of two containers.
--   
--   @Bimatchable@ is 'Bifunctor'-version of 'Matchable'.
--   It can compare and zip containers with two parameters.
class (Eq2 t, Bifunctor t) => Bimatchable t where
  {- |
  
  'bizipMatch' is to 'Data.Matchable.zipMatch' what 'bimap' is to 'fmap'.
  
  Decides if two structures match exactly. If they match, return zipped version of them.

  ==== Law

  Forall @x :: t a b@, @y :: t a' b'@, @z :: t (a,a') (b,b')@,
  
  > bizipMatch x y = Just z
  
  holds if and only if both of
  
  > x = bimap fst fst z
  > y = bimap snd snd z
  
  holds. Otherwise, @bizipMatch x y = Nothing@.
  
  ==== Example
  >>> bizipMatch (Left 1) (Left 'a')
  Just (Left (1,'a'))
  >>> bizipMatch (Right 1) (Right False)
  Just (Right (1,False))
  >>> bizipMatch (Left 1) (Right False)
  Nothing
  -}
  bizipMatch :: t a b -> t a' b' -> Maybe (t (a,a') (b,b'))
  bizipMatch = bizipMatchWith (curry Just) (curry Just)


  {-|
  
  'bizipMatchWith' is to 'Data.Matchable.zipMatchWith' what 'bimap' is to 'fmap'.
  
  Match two structures. If they match, zip them with given functions
  @(a -> a' -> Maybe a'')@ and @(b -> b -> Maybe b'')@.
  Passed functions can make whole match failby returning @Nothing@.

  ==== Law

  For any

  > x :: t a b
  > y :: t a' b'
  > f :: a -> a' -> Maybe a''
  > g :: b -> b' -> Maybe b''
  
  'bizipMatchWith' must satisfy the following.

      - If there is a pair @(z :: t (a,a') (b,b'), w :: t a'' b'')@ such that
        fulfills all of the following three conditions, then
        @bizipMatchWith f g x y = Just w@.

            1. @x = bimap fst fst z@
            2. @y = bimap snd snd z@
            3. @bimap (uncurry f) (uncurry g) z = bimap Just Just w@

      - If there are no such pair, @bizipMatchWith f g x y = Nothing@.
  
  -}
  bizipMatchWith :: (a -> a' -> Maybe a'')
                 -> (b -> b' -> Maybe b'')
                 -> t a b -> t a' b' -> Maybe (t a'' b'')

  {-# MINIMAL bizipMatchWith #-}

instance Bimatchable Either where
  bizipMatchWith u _ (Left a)  (Left a')  = Left <$> u a a'
  bizipMatchWith _ v (Right b) (Right b') = Right <$> v b b'
  bizipMatchWith _ _ _         _          = Nothing

instance Bimatchable (,) where
  bizipMatch (a, b) (a', b') = Just ((a, a'), (b, b'))
  bizipMatchWith u v (a, b) (a', b') = (,) <$> u a a' <*> v b b'

instance Bimatchable Const where
  bizipMatch (Const a) (Const a') = Just (Const (a, a'))
  bizipMatchWith u _ (Const a) (Const a') = Const <$> u a a'

instance Bimatchable Tagged where
  bizipMatch (Tagged b) (Tagged b') = Just (Tagged (b, b'))
  bizipMatchWith _ v (Tagged b) (Tagged b') = Tagged <$> v b b'

bimapRecovered :: (Bimatchable t)
               => (a -> a') -> (b -> b') -> t a b -> t a' b'
bimapRecovered f g tab =
  case bizipMatchWith (const (Just . f)) (const (Just . g)) tab tab of
    Nothing -> error "bimapRecovered: Unlawful instance of Bimatchable"
    Just r  -> r

eq2Default :: (Bimatchable t, Eq a, Eq b)
           => t a b -> t a b -> Bool
eq2Default = liftEq2Default (==) (==)

liftEq2Default :: (Bimatchable t)
               => (a -> a' -> Bool)
               -> (b -> b' -> Bool)
               -> t a b -> t a' b' -> Bool
liftEq2Default pa pb tab tab' =
  case bizipMatchWith u v tab tab' of
    Nothing -> False
    Just _ -> True
  where u a a' = if pa a a' then Just () else Nothing
        v b b' = if pb b b' then Just () else Nothing
