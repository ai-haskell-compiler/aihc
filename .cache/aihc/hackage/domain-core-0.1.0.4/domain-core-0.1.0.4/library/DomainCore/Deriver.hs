-- |
-- Toolkit for definition of instance derivers for Domain specs.
module DomainCore.Deriver
  ( -- * Deriver definitions
    Deriver (..),
    effectless,
  )
where

import DomainCore.Model
import DomainCore.Prelude
import qualified Language.Haskell.TH as TH (Dec, Q)

-- |
-- Specification of which instances to automatically derive for all the
-- supported types in the model and how.
--
-- You can combine derivers using Monoid and Semigroup.
newtype Deriver
  = -- |
    --  Function from the type declaration in this package\'s own AST
    --  to a list of Template Haskell declarations in its quotation monad.
    Deriver (TypeDec -> TH.Q [TH.Dec])
  deriving
    (Semigroup, Monoid)
    via ((->) TypeDec (Ap TH.Q [TH.Dec]))

-- |
-- Lift a pure function, which doesn't require the context of 'TH.Q'.
effectless :: (TypeDec -> [TH.Dec]) -> Deriver
effectless f =
  Deriver (pure . f)
