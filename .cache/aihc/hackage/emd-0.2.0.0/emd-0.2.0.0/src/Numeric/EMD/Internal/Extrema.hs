{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE LambdaCase                               #-}
{-# LANGUAGE RankNTypes                               #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TypeOperators                            #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}

module Numeric.EMD.Internal.Extrema (
    extrema
  ) where

import           Control.Applicative.Backwards
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Writer
import           Data.Coerce
import           Data.Either
import           Data.Finite
import           Data.Foldable
import           Data.Functor.Identity
import           Data.Monoid
import           GHC.TypeNats
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import qualified Data.Vector.Generic           as VG
import qualified Data.Vector.Generic.Sized     as SVG

data ExState n = ESStart [Finite n]
               | ESDown  [Finite n]
               | ESUp    [Finite n]

esList
    :: Functor f
    => ([Finite n] -> f [Finite m])
    -> ExState n
    -> f (ExState m)
esList f = \case
    ESStart is -> ESStart <$> f is
    ESDown  is -> ESDown  <$> f is
    ESUp    is -> ESUp    <$> f is

data ExOut n = ExMin (Finite n)
             | ExMax (Finite n)

exEither :: ExOut n -> Either (Finite n) (Finite n)
exEither = \case
    ExMin i -> Left  i
    ExMax i -> Right i

-- | Treats every item in a "plateu" as a local minimum or maximum.
extrema
    :: forall v n a. (VG.Vector v a, KnownNat n, Fractional a, Ord a)
    => SVG.Vector v (n + 1) a
    -> (M.Map (Finite (n + 1)) a, M.Map (Finite (n + 1)) a)
extrema xs = (makeMap mins, makeMap maxs)
  where
    (mins,maxs) = partitionEithers . map exEither $ optima
    dxs :: SVG.Vector v n a
    dxs = SVG.tail xs - SVG.init xs
    optima :: [ExOut n]
    optima = flip appEndo []
           . execWriter
           . flip execStateT (ESStart [])
           $ SVG.imapM_ go dxs
      where
        go  :: Finite n
            -> a
            -> StateT (ExState n) (Writer (Endo [ExOut n])) ()
        go i d = case compare d 0 of
          LT -> do
            get >>= \case
              ESStart _  -> pure ()
              ESDown  _  -> pure ()
              ESUp    is -> lift $ do
                forwards . traverse_ (Backwards . tell . Endo . (:) . ExMax) $ is
                tell $ Endo (ExMax i:)
            put $ ESDown []
          EQ -> modify $ over esList (i:)
          GT -> do
            get >>= \case
              ESStart _  -> pure ()
              ESDown  is -> lift $ do
                forwards . traverse_ (Backwards . tell . Endo . (:) . ExMin) $ is
                tell $ Endo (ExMin i:)
              ESUp    _  -> pure ()
            put $ ESUp   []
    makeMap :: [Finite n] -> M.Map (Finite (n + 1)) a
    makeMap = M.fromSet (xs `SVG.index`)
            . S.fromAscList
            . map weaken

over :: ((a -> Identity b) -> s -> Identity t)
     -> (a -> b) -> (s -> t)
over = coerce
