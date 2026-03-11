{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : Data.Type.Functor.XProduct
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Generalize "Data.Vinyl.XRec": provides a version of products in
-- "Data.Type.Functor.Product" that "erases" newtype wrappers and other
-- syntactical noise.
--
-- "Data.Type.Functor.Product" is the "main functionality", but this module
-- provides an alternative interface that may be more convenient in some
-- situations, in the same way that 'XRec' can be more convenient than
-- 'Rec' in some situations.
module Data.Type.Functor.XProduct (
  XProd,
  fromXProd,
  toXProd,

  -- * Functions
  mapProdX,
  mapProdXEndo,
  imapProdX,
  zipWithProdX,
  ixProdX,
  traverseProdX,
  traverseProdXEndo,
  itraverseProdX,
  foldMapProdX,
  ifoldMapProdX,

  -- * Instances
  XRec,
  pattern (::&),
  pattern XRNil,
  XMaybe,
  pattern XNothing,
  pattern XJust,
  XEither,
  pattern XLeft,
  pattern XRight,
  XNERec,
  pattern (::&|),
  XTup,
  pattern XTup,
  XIdentity,
  pattern XIdentity,
) where

import Data.Functor.Identity
import Data.Kind
import Data.List.NonEmpty (NonEmpty (..))
import Data.Singletons
import Data.Type.Functor.Product
import Data.Vinyl
import qualified Data.Vinyl.Functor as V
import Data.Vinyl.XRec
import Lens.Micro

-- | Generalize 'XRec' to work over any foldable @f@ that implements
-- 'FProd'.  See 'Prod' and 'FProd' for more information.
type XProd f g = (Prod f (XData g) :: f k -> Type)

-- | Convert an 'XProd' back into a regular ol' 'Prod'.
fromXProd :: forall f g as. (FProd f, PureProdC f (IsoHKD g) as) => XProd f g as -> Prod f g as
fromXProd =
  zipWithProd
    (\(V.Lift u) x -> u x)
    (pureProdC @_ @(IsoHKD g) (V.Lift (unHKD . unX)))

-- | Convert a 'Prod' into a fancy 'XProd'.
toXProd :: forall f g as. (FProd f, PureProdC f (IsoHKD g) as) => Prod f g as -> XProd f g as
toXProd =
  zipWithProd
    (\(V.Lift u) x -> u x)
    (pureProdC @_ @(IsoHKD g) (V.Lift (XData . toHKD)))

-- | Convenient wrapper over 'mapProd' that lets you deal with the
-- "simplified" inner types.  Generalizes 'rmapX'.
mapProdX ::
  forall f g h as.
  FProd f =>
  (forall a. HKD g a -> HKD h a) ->
  XProd f g as ->
  XProd f h as
mapProdX f = mapProd $ \(XData x :: XData g a) -> XData (f @a x)

-- | A version of 'mapProdX' that doesn't change the context @g@; this can
-- be easier for type inference in some situations.  Generalizes
-- 'rmapXEndo'.
mapProdXEndo ::
  forall f g as.
  FProd f =>
  (forall a. HKD g a -> HKD g a) ->
  XProd f g as ->
  XProd f g as
mapProdXEndo f = mapProd $ \(XData x :: XData g a) -> XData (f @a x)

-- | A version of 'mapProdX' that passes along the index 'Elem' with each
-- value.  This can help with type inference in some situations.
imapProdX ::
  forall f g h as.
  FProd f =>
  (forall a. Elem f as a -> HKD g a -> HKD h a) ->
  XProd f g as ->
  XProd f h as
imapProdX f = imapProd $ \i -> XData . f i . unX

-- | Zip two 'XProd's together by supplying a function that works on their
-- simplified 'HKD' values.
zipWithProdX ::
  forall f g h j as.
  FProd f =>
  (forall a. HKD g a -> HKD h a -> HKD j a) ->
  XProd f g as ->
  XProd f h as ->
  XProd f j as
zipWithProdX f = zipWithProd $ \(XData x :: XData g a) (XData y) -> XData (f @a x y)

-- | Given an index into an 'XProd', provides a lens into the simplified
-- item that that index points to.
ixProdX ::
  FProd f =>
  Elem f as a ->
  Lens' (XProd f g as) (HKD g a)
ixProdX i = ixProd i . (\f (XData x) -> XData <$> f x)

-- | Convenient wrapper over 'traverseProd' that lets you deal with the
-- "simplified" inner types.
traverseProdX ::
  forall f g h m as.
  (FProd f, Applicative m) =>
  (forall a. HKD g a -> m (HKD h a)) ->
  XProd f g as ->
  m (XProd f h as)
traverseProdX f = traverseProd $ \(XData x :: XData g a) -> XData <$> f @a x

-- | A version of 'traverseProdX' that doesn't change the context @g@; this can
-- be easier for type inference in some situations.
traverseProdXEndo ::
  forall f g m as.
  (FProd f, Applicative m) =>
  (forall a. HKD g a -> m (HKD g a)) ->
  XProd f g as ->
  m (XProd f g as)
traverseProdXEndo f = traverseProd $ \(XData x :: XData g a) -> XData <$> f @a x

-- | A version of 'traverseProdX' that passes along the index 'Elem' with
-- each value.  This can help with type inference in some situations.
itraverseProdX ::
  forall f g h m as.
  (FProd f, Applicative m) =>
  (forall a. Elem f as a -> HKD g a -> m (HKD h a)) ->
  XProd f g as ->
  m (XProd f h as)
itraverseProdX f = itraverseProd $ \i -> fmap XData . f i . unX

-- | Convenient wrapper over 'foldMapProd' that lets you deal with the
-- "simplified" inner types.
foldMapProdX ::
  forall f g m as.
  (FProd f, Monoid m) =>
  (forall a. HKD g a -> m) ->
  XProd f g as ->
  m
foldMapProdX f = foldMapProd $ \(XData x :: XData g a) -> f @a x

-- | A version of 'foldMapProdX' that passes along the index 'Elem' with
-- each value.  This can help with type inference in some situations.
ifoldMapProdX ::
  forall f g m as.
  (FProd f, Monoid m) =>
  (forall a. Elem f as a -> HKD g a -> m) ->
  XProd f g as ->
  m
ifoldMapProdX f = ifoldMapProd $ \i -> f i . unX

-- | 'PMaybe' over 'HKD'-d types.
type XMaybe f = PMaybe (XData f)

-- | 'PEither' over 'HKD'-d types.
type XEither f = PEither (XData f)

-- | 'NERec' over 'HKD'-d types.
type XNERec f = NERec (XData f)

-- | 'PTup' over 'HKD'-d types.
type XTup f = PTup (XData f)

-- | 'PIdentity' over 'HKD'-d types.
type XIdentity f = PIdentity (XData f)

-- | 'PNothing' for 'XMaybe'.
pattern XNothing :: XMaybe f 'Nothing
pattern XNothing = PNothing

-- | 'PJust' for 'XMaybe': allows you to provide the simplified type.
pattern XJust :: HKD f a -> XMaybe f ('Just a)
pattern XJust x = PJust (XData x)

-- | 'PLeft' for 'XEither'.
pattern XLeft :: Sing e -> XEither f ('Left e)
pattern XLeft e = PLeft e

-- | 'PRight' for 'XEither': allows you to provide the simplified type.
pattern XRight :: HKD f a -> XEither f ('Right a)
pattern XRight x = PRight (XData x)

-- | A version of ':&|' that allows you to provide the simplified type, for
-- 'XNERec'.
pattern (::&|) :: HKD f a -> XRec f as -> XNERec f (a ':| as)
pattern x ::&| xs = XData x :&| xs

-- | A version of 'PTup' that allows you to provide the simplified type,
-- for 'XTup'.
pattern XTup :: Sing w -> HKD f a -> XTup f '(w, a)
pattern XTup w x = PTup w (XData x)

-- | A version of 'PIdentity' that allows you to provide the simplified
-- type, for 'XIdentity'.
pattern XIdentity :: HKD f a -> XIdentity f ('Identity a)
pattern XIdentity x = PIdentity (XData x)

{-# COMPLETE (::&|) #-}
{-# COMPLETE XIdentity #-}
{-# COMPLETE XJust #-}
{-# COMPLETE XLeft #-}
{-# COMPLETE XNothing #-}
{-# COMPLETE XRight #-}
{-# COMPLETE XTup #-}
