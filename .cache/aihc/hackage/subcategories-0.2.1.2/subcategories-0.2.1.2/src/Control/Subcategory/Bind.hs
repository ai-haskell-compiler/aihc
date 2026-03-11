{-# LANGUAGE CPP #-}
module Control.Subcategory.Bind (CBind(..), CMonad, creturn, (-<<)) where
import Control.Subcategory.Functor
import Control.Subcategory.Pointed

import           Control.Monad                   (join)
import qualified Control.Monad.ST.Lazy           as LST
import qualified Control.Monad.ST.Strict         as SST
import           Data.Coerce                     (coerce)
import           Data.Functor.Identity           (Identity)
import qualified Data.Functor.Product            as SOP
import           Data.Hashable                   (Hashable)
import qualified Data.HashMap.Strict             as HM
import qualified Data.HashSet                    as HS
import qualified Data.IntMap                     as IM
import qualified Data.IntSet                     as IS
import           Data.List.NonEmpty              (NonEmpty)
import qualified Data.Map                        as Map
import           Data.MonoTraversable
#if !MIN_VERSION_base(4,16,0)
import qualified Data.Semigroup                  as Sem
#endif
import qualified Data.Sequence                   as Seq
import qualified Data.Set                        as Set
import qualified Data.Tree                       as Tree
import           GHC.Conc                        (STM)
import           Text.ParserCombinators.ReadP    (ReadP)
import           Text.ParserCombinators.ReadPrec (ReadPrec)

class CFunctor m => CBind m where
  (>>-) :: (Dom m a, Dom m b) => m a -> (a -> m b) -> m b
  default (>>-) :: (Dom m a, Dom m b, Dom m (m b)) => m a -> (a -> m b) -> m b
  m >>- f = cjoin (cmap f m)
  cjoin :: (Dom m (m a), Dom m a) => m (m a) -> m a
  cjoin = (>>- id)

instance (Monad m) => CBind (WrapFunctor m) where
  (>>-) :: forall a b.
           WrapFunctor m a
        -> (a -> WrapFunctor m b) -> WrapFunctor m b
  (>>-) = coerce @(m a -> (a -> m b) -> m b) (>>=)
  cjoin :: forall a. WrapFunctor m (WrapFunctor m a) -> WrapFunctor m a
  cjoin (WrapFunctor m) = WrapFunctor $ join (fmap coerce m)

instance CBind [] where
  (>>-) = (>>=)
  cjoin  = concat

instance CBind IO where
  (>>-) = (>>=)

instance CBind STM where
  (>>-) = (>>=)

instance CBind (SST.ST s) where
  (>>-) = (>>=)

instance CBind (LST.ST s) where
  (>>-) = (>>=)

instance CBind Identity where
  (>>-) = (>>=)

instance CBind (Either a) where
  (>>-) = (>>=)

instance CBind Tree.Tree where
  (>>-) = (>>=)

instance CBind Maybe where
  (>>-) = (>>=)

instance CBind IM.IntMap where
  m >>- f = IM.mapMaybeWithKey (\k -> IM.lookup k . f) m

instance Ord k => CBind (Map.Map k) where
  m >>- f = Map.mapMaybeWithKey (\k -> Map.lookup k . f) m

instance (Hashable k, Eq k) => CBind (HM.HashMap k) where
  m >>- f = HM.mapMaybeWithKey (\k -> HM.lookup k . f) m

instance CBind Set.Set where
  (>>-) = flip foldMap
  {-# INLINE (>>-) #-}
  cjoin = foldMap id
  {-# INLINE cjoin #-}

instance CBind (WrapMono IS.IntSet) where
  (>>-) = withMonoCoercible $ flip ofoldMap
  {-# INLINE (>>-) #-}

instance CBind NonEmpty where
  (>>-) = (>>=)
  {-# INLINE (>>-) #-}

instance CBind Seq.Seq where
  (>>-) = (>>=)
  {-# INLINE (>>-) #-}
#if !MIN_VERSION_base(4,16,0)
instance CBind Sem.Option where
  (>>-) = (>>=)
  {-# INLINE (>>-) #-}
#endif

instance CBind ((->) a) where
  (>>-) = (>>=)
  {-# INLINE (>>-) #-}

instance CBind HS.HashSet where
  (>>-) = flip foldMap
  {-# INLINE (>>-) #-}
  cjoin = foldMap id
  {-# INLINE cjoin #-}

instance CBind ReadP where
  (>>-) = (>>=)
  {-# INLINE (>>-) #-}

instance CBind ReadPrec where
  (>>-) = (>>=)
  {-# INLINE (>>-) #-}

instance Semigroup w => CBind ((,) w) where
  (m, a) >>- f =
    let (w, b) = f a
    in (m <> w, b)
  {-# INLINE (>>-) #-}
  cjoin (w, (m, a)) = (w <> m, a)
  {-# INLINE cjoin #-}

infixl 1 >>-
infixr 1 -<<

(-<<) :: (Dom m b, Dom m a, CBind m) => (a -> m b) -> m a -> m b
(-<<) = flip (>>-)
{-# INLINE (-<<) #-}

instance (CBind m, CBind n) => CBind (SOP.Product m n) where
  (SOP.Pair a b) >>- f = SOP.Pair (a >>- fstP . f) (b >>- sndP . f)
    where
      fstP (SOP.Pair x _) = x
      sndP (SOP.Pair _ y) = y
  {-# INLINE (>>-) #-}

class    (CBind f, CPointed f) => CMonad f
instance (CBind f, CPointed f) => CMonad f

creturn :: (Dom m a, CMonad m) => a -> m a
creturn = cpure
{-# INLINE creturn #-}
