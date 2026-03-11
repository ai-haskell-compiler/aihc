module Data.HSet.Type where

import Control.DeepSeq
import Data.Typeable
import TypeFun.Data.List

#if !(MIN_VERSION_base(4, 8, 0))
import Control.Applicative
#endif


{- | Heterogeneous set (list of elements) with unique types. Useful
with MonadReader.

-}

data HSet (elems :: [*]) where
  HSNil  :: HSet '[]
  HSCons :: (NotElem elem elems) => !elem -> HSet elems -> HSet (elem ': elems)
  deriving ( Typeable )

instance Show (HSet '[]) where
  show HSNil = "HSNil"

instance (Show e, Show (HSet els)) => Show (HSet (e ': els)) where
  show (HSCons e els) = "HSCons (" ++ show e ++ ") (" ++ show els ++ ")"

instance Eq (HSet '[]) where
  HSNil == HSNil = True

instance (Eq e, Eq (HSet els)) => Eq (HSet (e ': els)) where
  (HSCons e els) == (HSCons e' els') = (e == e') && (els == els')

instance Ord (HSet '[]) where
  HSNil `compare` HSNil = EQ

instance (Ord e, Ord (HSet els)) => Ord (HSet (e ': els)) where
  (HSCons e els) `compare` (HSCons e' els') = case e `compare` e' of
    EQ -> els `compare` els'
    x  -> x

instance NFData (HSet '[]) where
  rnf a@HSNil = a `seq` ()

instance ( NFData e, NFData (HSet els) )
         => NFData (HSet (e ': els)) where
  rnf (HSCons e els) = rnf e `seq` rnf els `seq` ()
