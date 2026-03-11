module Control.Lens.Misc.TH where

import Control.Lens
import Control.Lens.Internal.FieldTH
import Data.Char
import Language.Haskell.TH

-- | type restricted version of 'over'
over' :: ASetter' s a -> (a -> a) -> s -> s
over' = over

-- | A 'LensRules' used by 'makeClassy_''.
classyRules_' :: LensRules
classyRules_' = classyRules_
    & lensClass .~ (over (mapped . both) mkName . classFun . nameBase)
  where
    classFun (x:xs) = Just ("Has" ++ x:xs, '_': toLower x:xs)
    classFun [] = Nothing

-- | Make lenses and traversals for a type, and create a class when the type
-- has no arguments.  Works the same as 'makeClassy_' except that
-- the resulting *classy* lens is also prefixed with an underscore.
makeClassy_' :: Name -> DecsQ
makeClassy_' = makeFieldOptics classyRules_'

-- | A 'LensRules' used by 'makeLenses_'.
lensRules_ :: LensRules
lensRules_ = lensRules
    & lensField .~ \_ _ n -> [TopName (mkName ('_':nameBase n))]


-- | Build lenses (and traversals) with a sensible default configuration.
-- Works the same as 'makeLenses' except that
-- the resulting lens is also prefixed with an underscore.
--
-- /e.g./
--
-- @
-- data FooBar
--   = Foo { x, y :: 'Int' }
--   | Bar { x :: 'Int' }
-- 'makeLenses' ''FooBar
-- @
--
-- will create
--
-- @
-- _x :: 'Lens'' FooBar 'Int'
-- _x f (Foo a b) = (\\a\' -> Foo a\' b) \<$\> f a
-- _x f (Bar a)   = Bar \<$\> f a
-- _y :: 'Traversal'' FooBar 'Int'
-- _y f (Foo a b) = (\\b\' -> Foo a  b\') \<$\> f b
-- _y _ c\@(Bar _) = pure c
-- @
--
-- @
-- 'makeLenses_' = 'makeLensesWith' 'lensRules_'
-- @
makeLenses_ :: Name -> DecsQ
makeLenses_ = makeFieldOptics lensRules_
