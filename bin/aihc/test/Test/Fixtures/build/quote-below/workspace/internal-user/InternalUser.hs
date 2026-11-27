-- The package depends on ghc-internal and not on template-haskell, as
-- template-haskell-lift does.
module InternalUser (Code, Exp, Quote) where

import GHC.Internal.TH.Syntax (Code, Exp, Quote)
