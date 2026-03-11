module Data.HSet.Remove
       ( HRemove(..)
       , HDeletable
       , hdelete
       ) where

import Data.HSet.Type
import Data.Typeable
import TypeFun.Data.List
import TypeFun.Data.Peano

#if !(MIN_VERSION_base(4, 8, 0))
import Control.Applicative
#endif


-- | Remove i's element from hset. Second argument is a resulting hset
-- type
class HRemove els1 els2 i | els1 i -> els2 where
  hremove :: forall proxy. proxy i -> HSet els1 -> HSet els2

instance HRemove (e ': els) els 'Z where
  hremove _ (HSCons _ els) = els

instance ( NotElem e els2
         , HRemove els1 els2 i )
         => HRemove (e ': els1) (e ': els2) ('S i) where
  hremove _ (HSCons e els) = HSCons e $ hremove (Proxy :: Proxy i) els

{- | Delete element from HSet of specified type

>>> let x = (HSCons "sdf" $ HSCons 123 HSNil) :: HSet '[String, Int]

>>> hdelete (Proxy :: Proxy Int) x
HSCons ("sdf") (HSNil)

>>> hdelete (Proxy :: Proxy String) x
HSCons (123) (HSNil)

-}

-- | Constraints that e can be removed from els1 and els2 will be
-- produced in result
type HDeletable e els1 els2 = HRemove els1 els2 (IndexOf e els1)

-- | Delete specific element from els1 and returns HSet with els2
hdelete :: forall proxy els1 els2 e
         . (HDeletable e els1 els2)
        => proxy e
        -> HSet els1
        -> HSet els2
hdelete _ h = hremove (Proxy :: Proxy (IndexOf e els1)) h
