module Data.HSet.Modify
       ( HModify(..)
       , HModifiable
       , HMonoModifiable
       , hMonoModify
       , hModifyTagged
       , hMonoModifyTagged
       , hUntag
       , hTag
       ) where

import Data.HSet.Type
import Data.Tagged
import TypeFun.Data.List
import TypeFun.Data.Peano

#if !(MIN_VERSION_base(4, 8, 0))
import Control.Applicative
#endif

class (i ~ (IndexOf e1 els1), i ~ (IndexOf e2 els2))
      => HModify els1 els2 e1 e2 i
       | els1 els2 e2 i -> e1  , els1 els2 e1 i -> e2
       , els1 e1 e2 i   -> els2, els2 e1 e2 i   -> els1 where
  hmodify :: (e1 -> e2) -> HSet els1 -> HSet els2

instance (NotElem e2 els)
         => HModify (e1 ': els) (e2 ': els) e1 e2 'Z where
  hmodify f (HSCons e els) = HSCons (f e) els

instance ( ('S i) ~ (IndexOf e1 (ex ': els1))
         , ('S i) ~ (IndexOf e2 (ex ': els2))
         , HModify els1 els2 e1 e2 i
         , NotElem ex els2 )
         => HModify (ex ': els1) (ex ': els2) e1 e2 ('S i) where
  hmodify f (HSCons e els) = HSCons e $ hmodify f els

-- | Check that we can turn one hset to another
type HModifiable els1 els2 e1 e2 = HModify els1 els2 e1 e2 (IndexOf e1 els1)

-- | Helper type infering that hset 'els' contains element of type 'e'
-- and can be modified
type HMonoModifiable els e = HModifiable els els e e

-- | Like 'hmodify' but do not change the hset's type
hMonoModify :: HMonoModifiable els e
            => (e -> e)
            -> HSet els
            -> HSet els
hMonoModify f h = hmodify f h

hModifyTagged :: forall proxy l els1 els2 e1 e2
               . (HModifiable els1 els2 (Tagged l e1) (Tagged l e2))
              => proxy l
              -> (e1 -> e2)
              -> HSet els1
              -> HSet els2
hModifyTagged _ f = hmodify (fmap f :: Tagged l e1 -> Tagged l e2)

hMonoModifyTagged :: forall proxy l els e
                   . (HMonoModifiable els (Tagged l e))
                   => proxy l
                   -> (e -> e)
                   -> HSet els
                   -> HSet els
hMonoModifyTagged _ f = hmodify (fmap f :: Tagged l e -> Tagged l e)

hUntag :: forall proxy els1 els2 l e
        . (HModifiable els1 els2 (Tagged l e) e)
       => proxy l
       -> proxy e
       -> HSet els1
       -> HSet els2
hUntag _ _ = hmodify (unTagged :: Tagged l e -> e)

hTag :: forall proxy els1 els2 l e
      . (HModifiable els1 els2 e (Tagged l e))
     => proxy l
     -> proxy e
     -> HSet els1
     -> HSet els2
hTag _ _ = hmodify (Tagged :: e -> Tagged l e)
