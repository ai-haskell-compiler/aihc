module Data.HSet.Get
       ( HGet(..)
       , HGettable
       , AllHGettable
       , hgetTagged
       ) where

import Data.HSet.Type
import Data.Tagged
import GHC.Exts
import TypeFun.Data.List
import TypeFun.Data.Peano

#if !(MIN_VERSION_base(4, 8, 0))
import Control.Applicative
#endif

{-$setup
>>> import Data.Proxy
-}

{- | Heterogeneous read arbitrarily element from hset

>>> let x = HSCons (10 :: Int) $ HSCons (20 :: Double) HSNil
>>> x
HSCons (10) (HSCons (20.0) (HSNil))

>>> hget x :: Int
10

>>> hget x :: Double
20.0

Note that 'hget' takes specific element from list of uniquely typed
elements depending on what type is required to be returned (return
type polymorphism)

-}

class (i ~ (IndexOf e els)) => HGet els e i | els i -> e where
  -- | Gets any data from HSet for you
  hget :: HSet els -> e

instance HGet (e ': els) e 'Z where
  hget (HSCons e _) = e

instance (('S i) ~ (IndexOf e (e1 ': els)), HGet els e i)
         => HGet (e1 ': els) e ('S i) where
  hget (HSCons _ els) = hget els

-- | Enables deriving of the fact that 'e' is contained within 'els' and it's
-- safe to say that 'hget' can be performed on this particular pair.
type HGettable els e = HGet els e (IndexOf e els)

-- | Reduces to __(HGettable els e1, HGettable els e2, ..)__
type family AllHGettable (els :: [k]) (subels :: [k]) :: Constraint where
  AllHGettable els '[] = ()
  AllHGettable els (e ': subels) = (HGettable els e, AllHGettable els subels)


{- |

>>> let y = HSCons (Tagged 10 :: Tagged "x" Int) $ HSCons (Tagged 20 :: Tagged "y" Int) HSNil
>>> y
HSCons (Tagged 10) (HSCons (Tagged 20) (HSNil))

>>> hgetTagged (Proxy :: Proxy "x") y :: Int
10

>>> hgetTagged (Proxy :: Proxy "y") y :: Int
20

-}

hgetTagged :: forall proxy label e els
            . (HGettable els (Tagged label e))
           => proxy label
           -> HSet els
           -> e
hgetTagged _ hset =
  let x = hget hset
  in unTagged (x :: Tagged label e)
