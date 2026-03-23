{-# LANGUAGE UnicodeSyntax #-}

module UnicodeSyntaxLambda where

-- Lambda with Unicode arrow
incrementLambda ∷ Int → Int
incrementLambda = \x → x + 1

-- Higher-order with Unicode arrows
applyTwice ∷ (Int → Int) → Int → Int
applyTwice f x = f (f x)
