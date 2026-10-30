module Demo where
import GHC.Types (RuntimeRep(..), Levity(..), TYPE)
data Unit = Unit
constant :: a -> b -> a
constant value _ = value
class Apply (r :: RuntimeRep) (a :: TYPE r) where
  apply :: a -> Unit
  default apply :: r ~ 'BoxedRep 'Lifted => a -> Unit
  apply = constant Unit
