module Sized (Sized (..), S (..)) where

import GHC.Types (Bool (..), Constraint)

-- | GHC's @Assert@, whose true branch is the empty constraint tuple.
type family Assert (b :: Bool) (c :: Constraint) :: Constraint where
  Assert 'True c = ()
  Assert b c = c

-- | A superclass whose head is a type family. It is irreducible while @g@ is
-- a variable, and reduces once an instance fixes the associated family.
class Assert (Big g) () => Sized g where
  type Big g :: Bool
  size :: g -> Bool

data S = S

instance Sized S where
  type Big S = 'True
  size _ = True
