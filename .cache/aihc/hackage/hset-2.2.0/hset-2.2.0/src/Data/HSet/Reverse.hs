module Data.HSet.Reverse
       ( HReverse(..)
       , HReversible
       , hreverse
       ) where

import Data.HSet.Type
import TypeFun.Data.List

-- | This is auxiliary typeclass for inversing the order of hset
-- elements
class HReverse acc els1 els2
      | acc els1 -> els2 where
  hreverse' :: HSet acc -> HSet els1 -> HSet els2

instance HReverse acc '[] acc where
  hreverse' acc _ = acc

instance ( NotElem e (ah ': at)
         , HReverse (e ': ah ': at) els1 els2 )
         => HReverse (ah ': at) (e ': els1) els2 where
  hreverse' acc (HSCons e els1) = hreverse' (HSCons e acc) els1

instance (HReverse '[e] els1 els2)
         => HReverse '[] (e ': els1) els2 where
  hreverse' acc (HSCons e els1) = hreverse' (HSCons e acc) els1


type HReversible els1 els2 =
  ( HReverse '[] els1 els2
  )

hreverse :: (HReversible a r) => HSet a -> HSet r
hreverse a = hreverse' HSNil a
