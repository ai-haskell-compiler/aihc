{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ExplicitForAll #-}

module UnicodeSyntaxConstraint where

showTwice ∷ ∀ a . Show a ⇒ a → String
showTwice x = show x ++ show x
