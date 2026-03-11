module Data.HSet.SubHSet
       ( SubHSet(..)
       , SubHSettable
       , hnarrow
       ) where

import Data.HSet.Get
import Data.HSet.Type
import TypeFun.Data.List

#if !(MIN_VERSION_base(4, 8, 0))
import Control.Applicative
#endif


{- $setup
>>> import Data.Proxy
-}

{- | Takes subset of some hset, including subset of same elements in
different order

>>> let x = (HSCons "hello" $ HSCons 1234 $ HSCons 12.123 HSNil) :: HSet '[String, Int, Double]

>>> subHSet x :: HSet '[Double, Int]
HSCons (12.123) (HSCons (1234) (HSNil))

>>> subHSet x :: HSet '[String, Double]
HSCons ("hello") (HSCons (12.123) (HSNil))

>>> subHSet x :: HSet '[Int, String]
HSCons (1234) (HSCons ("hello") (HSNil))

-}

class SubHSet els els2 where
  subHSet :: HSet els -> HSet els2

instance SubHSet els '[] where
  subHSet _ = HSNil

instance ( HGettable els el, NotElem el els2
         , SubHSet els els2 )
         => SubHSet els (el ': els2) where
  subHSet h = HSCons (hget h :: el) (subHSet h :: HSet els2)

type SubHSettable = SubHSet

{- | Like 'subHSet' but with proxy for convenience

>>> let x = (HSCons "hello" $ HSCons 123 $ HSCons 345 HSNil) :: HSet '[String, Int, Integer]

>>> hnarrow (Proxy :: Proxy '[]) x
HSNil

>>> hnarrow (Proxy :: Proxy '[String]) x
HSCons ("hello") (HSNil)

>>> hnarrow (Proxy :: Proxy '[Int, Integer]) x
HSCons (123) (HSCons (345) (HSNil))

>>> hnarrow (Proxy :: Proxy '[Integer, Int]) x
HSCons (345) (HSCons (123) (HSNil))

-}

hnarrow :: (SubHSettable els subels)
        => proxy subels -> HSet els -> HSet subels
hnarrow _ = subHSet
