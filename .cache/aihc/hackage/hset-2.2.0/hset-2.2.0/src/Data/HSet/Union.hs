module Data.HSet.Union where

import Data.HSet.Remove
import Data.HSet.Type
import Data.Typeable
import TypeFun.Data.List
import TypeFun.Data.Peano

#if !(MIN_VERSION_base(4, 8, 0))
import Control.Applicative
#endif

type family MayFstIndexSnd (ts1 :: [k]) (ts2 :: [k]) :: (Maybe N) where
  MayFstIndexSnd (e ': els) x = IndexOfMay e x
  MayFstIndexSnd '[]        x = 'Nothing

class ( fidx ~ (MayFstIndexSnd els1 els2)
      , sidx ~ (MayFstIndexSnd els2 els1) )
      => HUnion els1 els2 elsr fidx sidx
       | els1 els2 fidx sidx -> elsr where
  hunion :: HSet els1 -> HSet els2 -> HSet elsr

instance HUnion '[] '[] '[] 'Nothing 'Nothing where
  hunion _ _ = HSNil

instance HUnion '[] (e ': els) (e ': els) 'Nothing 'Nothing where
  hunion _ a = a

instance HUnion (e ': els) '[] (e ': els) 'Nothing 'Nothing where
  hunion a _ = a

instance ( HUnionable els1 els2 elsr
         , NotElem e1 (e2 ': elsr)
         , NotElem e2 elsr
         , 'Nothing ~ (MayFstIndexSnd (e1 ': els1) (e2 ': els2))
         , 'Nothing ~ (MayFstIndexSnd (e2 ': els2) (e1 ': els1)) )
         => HUnion (e1 ': els1) (e2 ': els2) (e1 ': e2 ': elsr) 'Nothing 'Nothing where
  hunion (HSCons e1 els1) (HSCons e2 els2) = HSCons e1 $ HSCons e2 $ hunion els1 els2

instance ( HUnionable els1 els2 elsr
         , NotElem e elsr )
         => HUnion (e ': els1) (e ': els2) (e ': elsr) ('Just 'Z) ('Just 'Z) where
  hunion (HSCons e els1) (HSCons _ els2) = HSCons e $ hunion els1 els2

instance ( HRemove els2 elsx fi
         , HUnionable els1 elsx elsr
         , NotElem e1 elsr
         , ('Just ('S fi)) ~ (MayFstIndexSnd (e1 ': els1) (e2 ': els2))
         , ('Just si) ~ (MayFstIndexSnd (e2 ': els2) (e1 ': els1)) )
         => HUnion (e1 ': els1) (e2 ': els2) (e1 ': elsr) ('Just ('S fi)) ('Just si) where
  hunion (HSCons e1 els1) (HSCons _ els2) = HSCons e1 $ hunion els1 $ hremove (Proxy :: Proxy fi) els2

instance ( HRemove els2 elsx fi
         , HUnionable els1 elsx elsr
         , NotElem e1 elsr
         , ('Just fi) ~ (MayFstIndexSnd (e1 ': els1) els2)
         , 'Nothing ~ (MayFstIndexSnd els2 (e1 ': els1)) )
         => HUnion (e1 ': els1) els2 (e1 ': elsr) ('Just fi) 'Nothing where
  hunion (HSCons e1 els1) els2 = HSCons e1 $ hunion els1 $ hremove (Proxy :: Proxy fi) els2

instance ( HUnionable els1 els2 elsr
         , NotElem e1 elsr
         , 'Nothing ~ (MayFstIndexSnd (e1 ': els1) (e2 ': els2))
         , ('Just si) ~ (MayFstIndexSnd (e2 ': els2) (e1 ': els1)) )
         => HUnion (e1 ': els1) (e2 ': els2) (e1 ': elsr) 'Nothing ('Just si) where
  hunion (HSCons e1 els1) (HSCons _ els2) = HSCons e1 $ hunion els1 els2

type HUnionable els1 els2 elsr =
  HUnion els1 els2 elsr (MayFstIndexSnd els1 els2) (MayFstIndexSnd els2 els1)
