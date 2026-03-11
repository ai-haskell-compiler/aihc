{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

{-|

  Optic counterparts of MonadReader combinators from the lens library.

|-}
module Control.Lens.HReader
  ( hreader
  , hasks
  , hview
  , hviews
  , hiview
  , hperform
  , hperforml
  ) where

import Control.Comonad
import Control.Lens
import Control.Lens.Action.Internal
import Control.Lens.Action.Type
import Control.Monad.HReader
import Data.HSet
import Data.Profunctor.Rep
import Data.Profunctor.Sieve


hreader :: (MonadHReader m, HGettable (MHRElements m) s) => (s -> a) -> m a
hreader f = do
  e <- hask
  return $ f e

hasks :: (MonadHReader m, HGettable (MHRElements m) s) => (s -> a) -> m a
hasks = hreader

hview :: (MonadHReader m, HGettable (MHRElements m) s) => Getting a s a -> m a
hview l = hasks (getConst . l Const)
{-# INLINE hview #-}

hviews
  :: (MonadHReader m, HGettable (MHRElements m) s)
  => LensLike' (Const r) s a -> (a -> r) -> m r
hviews l f = hasks (getConst . l (Const . f))
{-# INLINE hviews #-}

hiview
  :: (MonadHReader m, HGettable (MHRElements m) s)
  => IndexedGetting i (i,a) s a -> m (i,a)
hiview l = hasks (getConst . l (Indexed $ \i -> Const . (,) i))
{-# INLINE hiview #-}

-- | It actually semantically similar to a mix of hask and @act@, performing
-- the monadic action on @s@ taken from the optic composition on the left and @r@
-- from HReader on the right.
-- @
--
-- type A = Int
-- type B = Int
--
-- data R = R { _baz :: B }
--
-- makeLenses ''R
--
-- foo :: IO Int
-- foo = runHReaderT (HSCons (3::A) HSNil) f
--   where
--     f :: HReaderT '[Int] IO B
--     f = R 3 ^! baz . hperform g . baz
--     g :: B -> A -> HReaderT '[Int] IO R
--     g = \x y -> pure (R (x * y))
-- @
--
hperform
  :: (MonadHReader m, HGettable (MHRElements m) r)
  => (s -> r -> m a)
  -> IndexPreservingAction m s a
hperform srma pafb = cotabulate $ \ws -> effective $ do
  a <- srma (extract ws) =<< hask
  ineffective (cosieve pafb (a <$ ws))
{-# INLINE hperform #-}

-- | Flipped version of 'hperform'
hperforml
  :: (MonadHReader m, HGettable (MHRElements m) r)
  => (r -> s -> m a)
  -> IndexPreservingAction m s a
hperforml rsma pafb = cotabulate $ \ws -> effective $ do
  a <- flip rsma (extract ws) =<< hask
  ineffective (cosieve pafb (a <$ ws))
{-# INLINE hperforml #-}
