{- | Generalized functors, where the type parameter(s) may be nested in
arbitrary compositions of functors.

Note that these functions are __unsafe__ because they rely on incoherent
instances.

See the <#gsolomapusage Usage> section of 'gsolomap' for details.

=== Example

@
{-\# LANGUAGE DeriveGeneric \#-}
module Main where

import "Generic.Functor"
import "GHC.Generics" ('GHC.Generics.Generic')

data T a b = C Int a b
  deriving (Show, 'GHC.Generics.Generic')

fmapT :: (b -> b') -> T a b -> T a b'
fmapT = 'gsolomap'

firstT :: (a -> a') -> T a b -> T a' b
firstT = 'gsolomap'

bothT :: (a -> a') -> T a a -> T a' a'
bothT = 'gsolomap'

watT ::
  (a -> a') ->
  T (a , a ) ((a  -> a') -> Maybe a ) ->
  T (a', a') ((a' -> a ) -> Maybe a')
watT = 'gsolomap'

-- Incoherence test
main :: IO ()
main = do
  print (fmapT    ((+1) :: Int -> Int) (C 0 0 0 :: T Int Int))
  print ('gsolomap' ((+1) :: Int -> Int) (C 0 0 0 :: T Int Int) :: T Int Int)
  -- NB: Type annotations are needed on both the input and output T Int Int.
  putStrLn "We are not the same."

  -- Output:
  --     C 0 0 1
  --     C 1 1 1
  --     We are not the same.
@
-}

module Generic.Functor.Multimap
  (
    -- ** Unary functors
    gsolomap
  , solomap

    -- ** N-ary functors
  , gmultimap
  , multimap
  , (:+)(..)

    -- ** Generalized functors
  , GSolomap()
  , Solomap()
  , GMultimap()
  , Multimap()
  ) where

import Generic.Functor.Internal
import Generic.Functor.Internal.Implicit
