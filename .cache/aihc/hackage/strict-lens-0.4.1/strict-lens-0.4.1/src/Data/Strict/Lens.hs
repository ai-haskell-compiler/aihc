{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Data.Strict.Lens (
    -- * Tuple
    -- | See instances, in particular for 'Field1' and 'Field2' type classes.
    -- * Either
    _Left, _Right,
    -- * Maybe
    _Just, _Nothing,
    -- * These
    here, there,
    _This, _That, _These,
    ) where


import           Control.Applicative (pure, (<$>))
import           Prelude             (Int, flip, ($), (.))

-- Lazy variants
import qualified Prelude             as L

import           Control.Lens        (Index, Prism, Prism', Traversal, prism,
                                      prism')

import           Data.Strict         (Either (..), Maybe (..), Pair (..),
                                      These (..), either, maybe, these)

-------------------------------------------------------------------------------
-- Tuple
-------------------------------------------------------------------------------

-- TODO: this should be removed. Probably.
type instance Index (Pair a b) = Int

-------------------------------------------------------------------------------
-- Either
-------------------------------------------------------------------------------

-- | Analogous to 'Control.Lens.Prism._Left' in "Control.Lens.Prism".
_Left :: Prism (Either a c) (Either b c) a b
_Left = prism Left $ either L.Right (L.Left . Right)

-- | Analogous to 'Control.Lens.Prism._Right' in "Control.Lens.Prism".
_Right :: Prism (Either c a) (Either c b) a b
_Right = prism Right $ either (L.Left . Left) L.Right

-------------------------------------------------------------------------------
-- Maybe
-------------------------------------------------------------------------------

-- | Analogous to 'Control.Lens.Prism._Just' in "Control.Lens.Prism"
_Just :: Prism (Maybe a) (Maybe b) a b
_Just = prism Just $ maybe (L.Left Nothing) L.Right

-- | Analogous to 'Control.Lens.Prism._Nothing' in "Control.Lens.Prism"
_Nothing :: Prism' (Maybe a) ()
_Nothing = prism' (L.const Nothing) $ maybe (L.Just ()) (L.const L.Nothing)

-------------------------------------------------------------------------------
-- These
-------------------------------------------------------------------------------

-- | A 'Control.Lens.Traversal' of the first half of a 'These', suitable for use with "Control.Lens".
--
-- >>> over here show (That 1)
-- That 1
--
-- >>> over here show (These 'a' 2)
-- These "'a'" 2
--
here :: Traversal (These a c) (These b c) a b
here f (This x)    = This <$> f x
here f (These x y) = flip These y <$> f x
here _ (That x)    = pure (That x)

-- | A 'Control.Lens.Traversal' of the second half of a 'These', suitable for use with "Control.Lens".
--
-- @
-- 'there' :: 'Control.Lens.Traversal' ('These' t b) ('These' t b) a b
-- @
--
-- >>> over there show (That 1)
-- That "1"
--
-- >>> over there show (These 'a' 2)
-- These 'a' "2"
--
there :: Traversal (These c a) (These c b) a b
there _ (This x)    = pure (This x)
there f (These x y) = These x <$> f y
there f (That x)    = That <$> f x

-- | A 'Control.Lens.Prism'' selecting the 'This' constructor.
--
-- /Note:/ cannot change type.
_This :: Prism' (These a b) a
_This = prism This (these L.Right (L.Left . That) (\x y -> L.Left $ These x y))

-- | A 'Control.Lens.Prism'' selecting the 'That' constructor.
--
-- /Note:/ cannot change type.
_That :: Prism' (These a b) b
_That = prism That (these (L.Left . This) L.Right (\x y -> L.Left $ These x y))

-- | A 'Control.Lens.Prism'' selecting the 'These' constructor. 'These' names are ridiculous!
--
-- /Note:/ cannot change type.
_These :: Prism' (These a b) (a, b)
_These = prism (\(a,b) -> These a b) (these (L.Left . This) (L.Left . That) (\x y -> L.Right (x, y)))
