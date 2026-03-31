{- ORACLE_TEST pass -}
module FixityUnicodeOp where

-- Sm (MathSymbol): ring operator ∘
infixr 9 ∘

(∘) :: (b -> c) -> (a -> b) -> (a -> c)
(∘) f g x = f (g x)

compose :: (Int -> String)
compose = show ∘ fromEnum

-- Sc (CurrencySymbol): currency sign ¤
infixr 9 ¤

(¤) :: (b -> c) -> (a -> b) -> (a -> c)
(¤) f g x = f (g x)

-- So (OtherSymbol): copyright sign ©
infixr 9 ©

(©) :: (b -> c) -> (a -> b) -> (a -> c)
(©) f g x = f (g x)
