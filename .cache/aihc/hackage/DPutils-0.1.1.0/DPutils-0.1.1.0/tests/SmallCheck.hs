
module SmallCheck where

import Control.Arrow (second)
import Control.Lens
import Data.Foldable
import Debug.Trace
import Test.SmallCheck
import Test.SmallCheck.Drivers
import Test.Tasty.SmallCheck as SC
import Test.Tasty.TH

import DP.Backtraced.Core



-- prop_BackTraced_Cons
--   ∷ forall m
--   . ( Monad m )
--   ⇒ Proxy a → Property m

-- xprop_Backtraced_Cons ∷ Monad m ⇒ m (Maybe PropertyFailure)
prop_Backtraced_Cons = changeDepth (const 4) $
  {- smallCheckM 4 $ -} \(bt ∷ Backtraced Bool) →
    let lst = toList bt
        btC ∷ Maybe (Bool,[Bool])
        btC = fmap (second toList) $ bt^?_Cons
--    in  traceShow (bt) $ traceShow (bt^?_Cons,lst^?_Cons) (btC == lst^?_Cons) -- bt^._Cons == lst^._Cons
    in  btC == lst^?_Cons

testSmallCheck = $(testGroupGenerator)
