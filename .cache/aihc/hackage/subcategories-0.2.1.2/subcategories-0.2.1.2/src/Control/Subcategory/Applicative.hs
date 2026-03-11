{-# LANGUAGE EmptyCase, StandaloneDeriving, TupleSections #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableSuperClasses                      #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Control.Subcategory.Applicative
  ( CApplicative(..), defaultRightApply, defaultLeftApply, CApp(..)
  ) where
import Control.Subcategory.Alternative.Class
import Control.Subcategory.Applicative.Class
import Control.Subcategory.Functor
import Control.Subcategory.Pointed

import qualified Control.Applicative             as App
import qualified Control.Monad.ST.Lazy           as LST
import qualified Control.Monad.ST.Strict         as SST
import           Data.Coerce                     (coerce)
import           Data.Functor.Const              (Const)
import           Data.Functor.Identity           (Identity)
import qualified Data.Functor.Product            as SOP
import           Data.Hashable                   (Hashable)
import qualified Data.HashMap.Strict             as HM
import qualified Data.HashSet                    as HS
import qualified Data.IntMap                     as IM
import           Data.List.NonEmpty              (NonEmpty)
import qualified Data.Map                        as Map
import qualified Data.Primitive.Array            as A
import qualified Data.Primitive.SmallArray       as SA
#if !MIN_VERSION_base(4,16,0)
import qualified Data.Semigroup                  as Sem
#endif
import qualified Data.Sequence                   as Seq
import qualified Data.Set                        as Set
import qualified Data.Tree                       as Tree
import qualified Data.Vector                     as V
import           GHC.Conc                        (STM)
import           Text.ParserCombinators.ReadP    (ReadP)
import           Text.ParserCombinators.ReadPrec (ReadPrec)

defaultLeftApply :: (Dom f (b1, b2), Dom f b1, Dom f b2, CApplicative f)
                 => f b1 -> f b2 -> f b1
defaultLeftApply a b = uncurry const <$:> pair a b
defaultRightApply :: (Dom f (b1, b2), Dom f b2, Dom f b1, CApplicative f)
                  => f b1 -> f b2 -> f b2
defaultRightApply a b = uncurry (const id) <$:> pair a b

instance Semigroup w => CApplicative (Const w) where
  pair = coerce @(w -> w -> w) (<>)
  (<.>) = coerce @(w -> w -> w) (<>)
  {-# INLINE (<.>) #-}
  (<. ) = coerce @(w -> w -> w) (<>)
  {-# INLINE (<. ) #-}
  ( .>) = coerce @(w -> w -> w) (<>)
  {-# INLINE ( .>) #-}

instance CApplicative []
instance CApplicative IO
instance CApplicative STM
instance CApplicative ReadP
instance CApplicative V.Vector
instance CApplicative SA.SmallArray
instance CApplicative A.Array
instance CApplicative ReadPrec
instance CApplicative (SST.ST s)
instance CApplicative (LST.ST s)
instance CApplicative App.ZipList
instance CApplicative Maybe
instance CApplicative Identity
instance CApplicative Tree.Tree
instance CApplicative Seq.Seq
#if !MIN_VERSION_base(4,16,0)
instance CApplicative Sem.Option
#endif
instance CApplicative NonEmpty
instance CApplicative ((->) a)
instance CApplicative (Either a)
instance (CApplicative f, CApplicative g)
      => CApplicative (SOP.Product f g) where
  pair (SOP.Pair a b) (SOP.Pair c d) = SOP.Pair (pair a c) (pair b d)
  SOP.Pair f g <.> SOP.Pair a b = SOP.Pair (f <.> a) (g <.> b)
  {-# INLINE (<.>) #-}
  SOP.Pair f g <. SOP.Pair a b = SOP.Pair (f <. a) (g <. b)
  {-# INLINE (<.) #-}
  SOP.Pair f g .> SOP.Pair a b = SOP.Pair (f .> a) (g .> b)
  {-# INLINE (.>) #-}

class Dom f (g a -> g b) => DomOver f g a b
instance Dom f (g a -> g b) => DomOver f g a b

instance Applicative f => CApplicative (WrapFunctor f)
instance Semigroup w => CApplicative ((,) w) where
  pair (w, a) (u, b) = (w <> u, (a, b))
  {-# INLINE pair #-}
  (w, f) <.> (u, a) = (w <> u, f a)
  {-# INLINE (<.>) #-}
  (w, a) <.  (u, _) = (w <> u, a)
  {-# INLINE (<.) #-}
  (w, _)  .> (u, b) = (w <> u, b)
  {-# INLINE (.>) #-}
instance CApplicative IM.IntMap where
  pair = IM.intersectionWith (,)
  {-# INLINE pair #-}
  (<.>) = IM.intersectionWith id
  {-# INLINE (<.>) #-}
  (<.)  = IM.intersectionWith const
  {-# INLINE (<.) #-}
  (.>)  = IM.intersectionWith $ const id
  {-# INLINE (.>) #-}

instance Ord k => CApplicative (Map.Map k) where
  pair = Map.intersectionWith (,)
  {-# INLINE pair #-}
  (<.>) = Map.intersectionWith id
  {-# INLINE (<.>) #-}
  (<.)  = Map.intersectionWith const
  {-# INLINE (<.) #-}
  (.>)  = Map.intersectionWith $ const id
  {-# INLINE (.>) #-}

instance (Eq k, Hashable k) => CApplicative (HM.HashMap k) where
  pair = HM.intersectionWith (,)
  {-# INLINE pair #-}
  (<.>) = HM.intersectionWith id
  {-# INLINE (<.>) #-}
  (<.)  = HM.intersectionWith const
  {-# INLINE (<.) #-}
  (.>)  = HM.intersectionWith $ const id
  {-# INLINE (.>) #-}

instance CApplicative Set.Set where
  pair as bs = foldMap (\b -> Set.map (,b) as) bs
  {-# INLINE pair #-}
  fs <.> as = foldMap (\f -> Set.map f as) fs
  {-# INLINE (<.>) #-}
  a <. b | Set.null b = Set.empty
         | otherwise  = a
  {-# INLINE (<.) #-}
  a .> b | Set.null a = Set.empty
         | otherwise  = b
  {-# INLINE (.>) #-}

instance CApplicative HS.HashSet where
  pair as bs = foldMap (\b -> HS.map (,b) as) bs
  {-# INLINE pair #-}
  fs <.> as = foldMap (\f -> HS.map f as) fs
  {-# INLINE (<.>) #-}
  a <. b | HS.null b = HS.empty
         | otherwise  = a
  {-# INLINE (<.) #-}
  a .> b | HS.null a = HS.empty
         | otherwise  = b
  {-# INLINE (.>) #-}

instance Constrained f => Constrained (CApp f) where
  type Dom (CApp f) a = Dom f a

newtype CApp f a = CApp { runCApp :: f a }
  deriving (Read, Show, Eq, Ord)
  deriving newtype (Functor, Applicative, App.Alternative)

deriving newtype instance (CFunctor f) => CFunctor (CApp f)
deriving newtype instance (CChoice f) => CChoice (CApp f)
deriving newtype instance (CAlternative f) => CAlternative (CApp f)
deriving newtype instance (CApplicative f) => CApplicative (CApp f)
deriving newtype instance (CPointed f) => CPointed (CApp f)

instance (Dom f a, CApplicative f, Semigroup a, Dom f (a, a))
       => Semigroup (CApp f a) where
  CApp a <> CApp b = CApp $ uncurry (<>) <$:> pair a b

instance (Dom f a, CPointed f, CApplicative f, Monoid a, Dom f (a, a))
       => Monoid (CApp f a) where
  mappend = (<>)
  mempty = CApp $ cpure mempty
