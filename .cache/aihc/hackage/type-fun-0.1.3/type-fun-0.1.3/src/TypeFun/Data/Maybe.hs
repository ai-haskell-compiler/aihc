module TypeFun.Data.Maybe
  ( MaybeCase
  , NothingToConstr
  , JustToConstr
  , CatMaybes
  , FromJust
  ) where

import GHC.Exts

type family MaybeCase (a :: Maybe k)
                      (nothing :: r)
                      (just :: k -> r) :: r where
  MaybeCase 'Nothing nothing just  = nothing
  MaybeCase ('Just k) nothing just = just k

type family NothingToConstr (a :: Maybe k) (c :: Constraint) :: Constraint where
  NothingToConstr 'Nothing  c = c
  NothingToConstr ('Just a) c = ()

type family JustToConstr (a :: Maybe k) (c :: Constraint) :: Constraint where
  JustToConstr 'Nothing  c = ()
  JustToConstr ('Just a) c = c

-- | Like 'catMaybes' for type lists
type family CatMaybes (l :: [Maybe k]) :: [k] where
  CatMaybes '[]               = '[]
  CatMaybes (('Just a) ': as) = a ': (CatMaybes as)
  CatMaybes ('Nothing ': as)  = CatMaybes as

type family FromJust (a :: Maybe k) :: k where
  FromJust ('Just a) = a
