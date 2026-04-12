{- ORACLE_TEST xfail reason="Unicode bullet operator not recognized by lexer" -}
{-# LANGUAGE UnicodeSyntax #-}

module UnicodeCompositionOperator where

infixr 9 •
(•) :: (b -> c) -> (a -> b) -> (a -> c)
f • g = \x -> f (g x)

data SpecificScreenNumber = SpecificScreenNumber Int

fromSpecificScreenNumber :: SpecificScreenNumber -> Int
fromSpecificScreenNumber = undefined

instance Show SpecificScreenNumber where
  show = fromSpecificScreenNumber • succ • show
