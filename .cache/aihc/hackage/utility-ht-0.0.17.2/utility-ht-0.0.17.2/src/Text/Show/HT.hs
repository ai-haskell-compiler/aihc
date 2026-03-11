module Text.Show.HT where

{-| Show a value using an infix operator. -}
{-# INLINE showsInfixPrec #-}
showsInfixPrec ::
   (Show a, Show b) =>
   String -> Int -> Int -> a -> b -> ShowS
showsInfixPrec opStr opPrec prec x y =
   showParen
     (prec >= opPrec)
     (showsPrec opPrec x . showString " " .
      showString opStr . showString " " .
      showsPrec opPrec y)

concatS :: [ShowS] -> ShowS
concatS = flip (foldr ($))

{-
precedences

appPrec :: Int
appPrec = 10
-}
