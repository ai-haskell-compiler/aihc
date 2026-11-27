{-# LANGUAGE TemplateHaskellQuotes #-}

-- The quote library is in aihc-internal. That package is below
-- internal-user, and it is not a dependency of this package.
module QuoteBelow (quoted, typed) where

import InternalUser (Code, Exp, Quote)

quoted :: (Quote m) => m Exp
quoted = [|(not True, "q")|]

typed :: (Quote m) => Code m Bool
typed = [||not False||]
