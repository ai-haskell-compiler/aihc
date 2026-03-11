{-# LANGUAGE UndecidableInstances #-}

module Bowtie.Attr
  ( HasAttr (..)
  , attrLens
  , WithAttr (..)
  , attrSetter
  , memoWithAttr
  , memoWithAttrM
  , memoWithoutAttr
  )
where

import Bowtie.Anno (Anno (..))
import Bowtie.Memo (Memo (..), MemoF, memoFKey, reMkMemo, reMkMemoM, pattern MemoFP, pattern MemoP)
import Bowtie.SMap (Deleted, Inserted, Member, SMap, Val, deleteSMap, indexSMap, insertSMap, updateSMap)
import Data.Kind (Type)
import Data.Proxy (Proxy)
import GHC.TypeLits (KnownSymbol, Symbol)
import Optics (Lens', Setter, lens, sets)

class HasAttr (d :: Type) (s :: Symbol) (k :: Type) where
  viewAttr :: Proxy d -> Proxy s -> k -> Val d s
  setAttr :: Proxy d -> Proxy s -> Val d s -> k -> k

instance (KnownSymbol s, Member s xs) => HasAttr d s (SMap d xs) where
  viewAttr _ = indexSMap
  setAttr _ = updateSMap

instance (HasAttr d s k) => HasAttr d s (Anno k x) where
  viewAttr pd ps (Anno k _) = viewAttr pd ps k
  setAttr pd ps v (Anno k x) = Anno (setAttr pd ps v k) x

instance (HasAttr d s k) => HasAttr d s (MemoF f k x) where
  viewAttr pd ps (MemoFP k _) = viewAttr pd ps k
  setAttr pd ps v (MemoFP k x) = MemoFP (setAttr pd ps v k) x

instance (HasAttr d s k) => HasAttr d s (Memo f k) where
  viewAttr pd ps (MemoP k _) = viewAttr pd ps k
  setAttr pd ps v (MemoP k x) = MemoP (setAttr pd ps v k) x

attrLens :: (HasAttr d s k) => Proxy d -> Proxy s -> Lens' k (Val d s)
attrLens pd ps = lens (viewAttr pd ps) (flip (setAttr pd ps))

class (HasAttr d s k) => WithAttr (d :: Type) (s :: Symbol) (k :: Type) (j :: Type) | d s k -> j, d s j -> k where
  withAttr :: Proxy d -> Proxy s -> Val d s -> j -> k
  withoutAttr :: Proxy d -> Proxy s -> k -> j

instance (KnownSymbol s, Inserted s xs zs, Deleted s zs xs) => WithAttr d s (SMap d zs) (SMap d xs) where
  withAttr _ = insertSMap
  withoutAttr _ = deleteSMap

instance (WithAttr d s k j) => WithAttr d s (Anno k x) (Anno j x) where
  withAttr pd ps v (Anno j x) = Anno (withAttr pd ps v j) x
  withoutAttr pd ps (Anno j x) = Anno (withoutAttr pd ps j) x

instance (WithAttr d s k j) => WithAttr d s (MemoF f k x) (MemoF f j x) where
  withAttr pd ps v (MemoFP j x) = MemoFP (withAttr pd ps v j) x
  withoutAttr pd ps (MemoFP j x) = MemoFP (withoutAttr pd ps j) x

attrSetter :: (WithAttr d s k j) => Proxy d -> Proxy s -> Setter j k () (Val d s)
attrSetter pd ps = sets (\f -> withAttr pd ps (f ()))

memoWithAttr
  :: (WithAttr d s k j, Functor f) => Proxy d -> Proxy s -> (MemoF f j (Memo f k) -> Val d s) -> Memo f j -> Memo f k
memoWithAttr pd ps f = reMkMemo (\mfmk -> let v = f mfmk in withAttr pd ps v (memoFKey mfmk))

memoWithAttrM
  :: (WithAttr d s k j, Traversable f, Monad m)
  => Proxy d
  -> Proxy s
  -> (MemoF f j (Memo f k) -> m (Val d s))
  -> Memo f j
  -> m (Memo f k)
memoWithAttrM pd ps f = reMkMemoM (\mfmk -> fmap (\v -> withAttr pd ps v (memoFKey mfmk)) (f mfmk))

memoWithoutAttr :: (WithAttr d s k j, Functor f) => Proxy d -> Proxy s -> Memo f k -> Memo f j
memoWithoutAttr pd ps = fmap (withoutAttr pd ps)
