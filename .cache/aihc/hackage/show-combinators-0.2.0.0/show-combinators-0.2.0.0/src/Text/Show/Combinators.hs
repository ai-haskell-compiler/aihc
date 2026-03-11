-- | Combinators to write 'Show' instances.
--
-- The following type illustrates the common use cases.
--
-- @
-- data MyType a
--   = C a a                   -- a regular constructor
--   | a :+: a                 -- an infix constructor
--   | R { f1 :: a, f2 :: a }  -- a record
--
-- infixl 4 :+:
--
-- instance 'Show' a => 'Show' (MyType a) where
--   'showsPrec' = 'flip' precShows where
--     precShows (C a b) = 'showCon' \"C\" '@|' a '@|' b
--     precShows (c :+: d) = 'showInfix'' \":+:\" 4 c d
--     precShows (R {f1 = e, f2 = f}) =
--       'showRecord' \"R\" (\"f1\" '.=.' e '&|' \"f2\" '.=.' f)
-- @

module Text.Show.Combinators
  ( module Text.Show
  , PrecShowS

    -- * Simple constructors and applications
  , showCon
  , showApp
  , (@|)

    -- * Infix constructors
  , showInfix
  , showInfix'

    -- ** Combinators for associative operators
    -- | Use with care, see warning under 'showInfixl'.
  , showInfixl
  , showInfixl'
  , showInfixr
  , showInfixr'

    -- * Records
  , ShowFields
  , showRecord
  , showField
  , (.=.)
  , noFields
  , appendFields
  , (&|)
  ) where

import Text.Show

-- | Type of strings representing expressions, parameterized by the surrounding
-- precedence level.
--
-- This is the return type of @'flip' 'showsPrec'@.
type PrecShowS = Int -> ShowS

-- | Show a constructor.
--
-- Possible constructor names are:
--
-- - regular constructors (e.g., @\"Left\"@);
-- - parenthesized infix constructors (e.g., @\"(:)\"@);
-- - smart constructors, for abstract types (e.g., @\"Map.fromList\"@).
--
-- === __Example with smart constructor__
--
-- @
-- instance (Show k, Show v) => Show (Map k v) where
--   showsPrec = 'flip' precShows where
--     precShows m = 'showCon' \"Map.fromList\" '@|' Map.toList m
--
-- -- Example output:
-- -- > Map.fromList [(33, True), (55, False)]
-- @
showCon :: String -> PrecShowS
showCon con _ = showString con

infixl 2 `showApp`, @|

-- | Show a function application.
showApp :: PrecShowS -> PrecShowS -> PrecShowS
showApp showF showX d = showParen (d > appPrec)
  (showF appPrec . showSpace . showX appPrec1)

-- | Show a function application.
--
-- This is an infix shorthand for 'showApp' when the argument type is an
-- instance of 'Show'.
--
-- > showF @| x = showApp showF (flip showsPrec x)
(@|) :: Show a => PrecShowS -> a -> PrecShowS
(@|) showF x = showApp showF (flip showsPrec x)

-- | Show an applied infix operator with a given precedence.
showInfix :: String -> Int -> PrecShowS -> PrecShowS -> PrecShowS
showInfix op prec = showInfix_ op prec (prec + 1) (prec + 1)

-- | Show an applied infix operator with a given precedence.
--
-- This is a shorthand for 'showInfix' when the arguments types are instances
-- of 'Show'.
--
-- > showInfix' op prec x y =
-- >   showInfix op prec (flip showsPrec x) (flip showsPrec y)
showInfix' :: (Show a, Show b) => String -> Int -> a -> b -> PrecShowS
showInfix' op prec x y = showInfix op prec (flip showsPrec x) (flip showsPrec y)

-- | Show an applied infix operator which is left associative (@infixl@).
-- Use with care.
--
-- ==== Warning
--
-- This combinator assumes that, if there is another infix operator to the
-- left, it is either left associative with the same precedence, or it has a
-- different precedence.
-- An expression containing two operators at the same level with different
-- associativities is ambiguous and will not be shown correctly with
-- 'showInfixl' and 'showInfixr'.
--
-- By default, prefer 'showInfix' and 'showInfix''.
showInfixl :: String -> Int -> PrecShowS -> PrecShowS -> PrecShowS
showInfixl op prec = showInfix_ op prec prec (prec + 1)

-- | Show an applied infix operator which is left associative (@infixl@).
-- Use with care, see 'showInfixl'.
--
-- This is a shorthand for 'showInfixl' when the arguments types are instances
-- of 'Show'.
--
-- By default, prefer 'showInfix' and 'showInfix''.
showInfixl' :: (Show a, Show b) => String -> Int -> a -> b -> PrecShowS
showInfixl' op prec x y = showInfixl op prec (flip showsPrec x) (flip showsPrec y)

-- | Show an applied infix operator which is right associative (@infixr@).
-- Use with care.
--
-- ==== Warning
--
-- This combinator assumes that, if there is another infix operator to the
-- right, it is either right associative with the same precedence, or it has a
-- different precedence.
-- An expression containing two operators at the same level with different
-- associativities is ambiguous and will not be shown correctly with
-- 'showInfixl' and 'showInfixr'.
--
-- By default, prefer 'showInfix' and 'showInfix''.
--
-- === __Example usage__
--
-- @
-- showList :: Show a => [a] -> PrecShowS
-- showList [] = showCon "[]"
-- showList (x : xs) = showInfixr ":" 5 (flip showsPrec x) (showList xs)
--
-- -- Example output:
-- -- > 0 : 1 : 2 : 3 : []
-- @
showInfixr :: String -> Int -> PrecShowS -> PrecShowS -> PrecShowS
showInfixr op prec = showInfix_ op prec (prec + 1) prec

-- | Show an applied infix operator which is right associative (@infixr@).
-- Use with care, see 'showInfixr'.
--
-- This is a shorthand for 'showInfixr' when the arguments types are instances
-- of 'Show'.
--
-- By default, prefer 'showInfix' and 'showInfix''.
showInfixr' :: (Show a, Show b) => String -> Int -> a -> b -> PrecShowS
showInfixr' op prec x y = showInfixr op prec (flip showsPrec x) (flip showsPrec y)

-- | An internal combinator for infix operators, to explicitly update the
-- precedence levels on each side.
showInfix_ :: String -> Int -> Int -> Int -> PrecShowS -> PrecShowS -> PrecShowS
showInfix_ op prec precX precY showX showY d = showParen (d > prec)
  (showX precX . showSpace . showString op . showSpace . showY precY)

-- | Strings representing a set of record fields separated by commas.
-- They can be constructed using ('.=.') and ('@|'), or using 'showField' and
-- 'appendFields'.
type ShowFields = ShowS

-- | Show a record. The first argument is the constructor name.
-- The second represents the set of record fields.
showRecord :: String -> ShowFields -> PrecShowS
showRecord con showFields d = showParen (d > appPrec)
  (showString con . showSpace . showChar '{' . showFields . showChar '}')

-- | Show a single record field: a field name and a value separated by @\'=\'@.
showField :: String -> PrecShowS -> ShowFields
showField field showX =
  showString field . showString " = " . showX 0

infixr 8 .=.

-- | Show a single record field: a field name and a value separated by @\'=\'@.
--
-- This is an infix shorthand for 'showField' when the value type is an
-- instance of 'Show'.
--
-- > field .=. x   =   showField field (flip showsPrec x)
(.=.) :: Show a => String -> a -> ShowFields
field .=. x = showField field (flip showsPrec x)

-- | Empty set of record fields.
noFields :: ShowFields
noFields = id

infixr 1 `appendFields`, &|

-- | Separate two __nonempty__ sets of record fields by a comma.
appendFields :: ShowFields -> ShowFields -> ShowFields
appendFields showFds1 showFds2 = showFds1 . showString ", " . showFds2

-- | An infix synonym of 'appendFields'.
(&|) :: ShowFields -> ShowFields -> ShowFields
(&|) = appendFields


-- Helpers

showSpace :: ShowS
showSpace = (' ' :)

appPrec, appPrec1 :: Int
appPrec = 10
appPrec1 = 11
