{-# LANGUAGE DerivingVia, StandaloneDeriving, TypeOperators #-}
{-# LANGUAGE CPP #-}
module Control.Subcategory.Pointed where
import Control.Subcategory.Functor

import qualified Control.Applicative             as App
import qualified Control.Monad.ST.Lazy           as LST
import qualified Control.Monad.ST.Strict         as SST
import qualified Data.Functor.Compose            as SOP
import           Data.Functor.Identity           (Identity)
import qualified Data.Functor.Product            as SOP
import qualified Data.HashSet                    as HS
import qualified Data.IntSet                     as IS
import           Data.List.NonEmpty              (NonEmpty)
import qualified Data.Monoid                     as Mon
import           Data.MonoTraversable            (MonoPointed, opoint)
import           Data.Ord                        (Down)
import qualified Data.Pointed                    as Pt
import qualified Data.Primitive.Array            as A
import qualified Data.Primitive.PrimArray        as PA
import qualified Data.Primitive.SmallArray       as SA
import           Data.Proxy                      (Proxy)
import qualified Data.Semigroup                  as Sem
import qualified Data.Sequence                   as Seq
import qualified Data.Set                        as Set
import qualified Data.Tree                       as Tree
import qualified Data.Vector                     as V
import qualified Data.Vector.Primitive           as P
import qualified Data.Vector.Storable            as S
import qualified Data.Vector.Unboxed             as U
import           GHC.Conc                        (STM)
import           GHC.Generics                    ((:*:) (..), (:.:) (..))
import           GHC.Generics                    (Par1, Rec1, U1)
import           Text.ParserCombinators.ReadP    (ReadP)
import           Text.ParserCombinators.ReadPrec (ReadPrec)

class Constrained f => CPointed f where
  cpure :: Dom f a => a -> f a
  default cpure :: App.Applicative f => a -> f a
  cpure = pure

instance (Functor f, Pt.Pointed f) => CPointed (WrapFunctor f) where
  cpure = Pt.point
  {-# INLINE cpure #-}

instance CPointed []
instance CPointed Maybe
instance CPointed IO
instance CPointed (SST.ST s)
instance CPointed (LST.ST s)
instance CPointed Par1
instance CPointed Sem.Min
instance CPointed Sem.Max
instance CPointed Mon.First
instance CPointed Mon.Last
instance CPointed Sem.First
instance CPointed Sem.Last
#if !MIN_VERSION_base(4,16,0)
instance CPointed Sem.Option
#endif

instance CPointed NonEmpty
instance CPointed App.ZipList
instance CPointed Identity
instance CPointed STM
instance CPointed Sem.Dual
instance CPointed Sem.Sum
instance CPointed Sem.Product
instance CPointed Down
instance CPointed Tree.Tree
instance CPointed Seq.Seq
instance CPointed Set.Set where
  cpure = Set.singleton
  {-# INLINE cpure #-}
instance CPointed (Either a)
instance CPointed U1
instance CPointed Proxy
instance (Pt.Pointed f) => CPointed (Rec1 f) where
  cpure = Pt.point
  {-# INLINE cpure #-}
instance (Pt.Pointed p, Pt.Pointed q)
      => CPointed (p :*: q) where
  cpure a = (:*:) (Pt.point a) (Pt.point a)
  {-# INLINE cpure #-}
instance (Pt.Pointed p, Pt.Pointed q)
      => CPointed (p :.: q) where
  cpure a = Comp1 $ Pt.point $ Pt.point a
  {-# INLINE cpure #-}
instance (Constrained p, Constrained q, Pt.Pointed p, Pt.Pointed q)
      => CPointed (SOP.Compose p q) where
  cpure a = SOP.Compose $ Pt.point $ Pt.point a
  {-# INLINE cpure #-}
instance (CPointed p, CPointed q)
      => CPointed (SOP.Product p q) where
  cpure a = SOP.Pair (cpure a) (cpure a)
  {-# INLINE cpure #-}
instance CPointed ReadP
instance CPointed ReadPrec
instance CPointed (WrapMono IS.IntSet) where
  cpure = WrapMono . IS.singleton
  {-# INLINE cpure #-}
instance CPointed HS.HashSet where
  cpure = HS.singleton
  {-# INLINE cpure #-}

instance MonoPointed mono => CPointed (WrapMono mono) where
  cpure = opoint

instance CPointed V.Vector where
  cpure = V.singleton
  {-# INLINE [1] cpure #-}

instance CPointed U.Vector where
  cpure = U.singleton
  {-# INLINE [1] cpure #-}

instance CPointed S.Vector where
  cpure = S.singleton
  {-# INLINE [1] cpure #-}

instance CPointed P.Vector where
  cpure = P.singleton
  {-# INLINE [1] cpure #-}

instance CPointed PA.PrimArray where
  cpure = PA.replicatePrimArray 1
  {-# INLINE [1] cpure #-}

instance CPointed SA.SmallArray where
  cpure = SA.smallArrayFromListN 1 . pure
  {-# INLINE [1] cpure #-}

instance CPointed A.Array where
#if MIN_VERSION_primitive(0,9,0)
  cpure = A.arrayFromListN 1 . pure
#else
  cpure = A.fromListN 1 . pure
#endif
  {-# INLINE [1] cpure #-}
