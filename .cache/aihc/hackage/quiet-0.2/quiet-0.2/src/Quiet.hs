{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Generic deriving of 'Read' / 'Show' with no record labels.
--
-- Often one wants to create a @newtype@ which has a convenient field
-- accessor like @unUserId@ below, but that unfortunately makes the
-- 'Show' instance which is derived overly verbose.
--
-- For example:
--
-- @
-- newtype UserId = UserId { unUserId :: String }
--   deriving (Read, Show)
-- @
--
-- >>> show (UserId "simon")
-- UserId {unUserId = "simon"}
-- >>> read "UserId {unUserId = \"simon\"}" :: UserId
-- UserId {unUserId = "simon"}
--
-- With @DerivingVia@ 'Quiet' you can have a 'Show' instance which doesn't
-- print the field labels. It will render as if the @unUserId@ accessor
-- wasn't present at all.
--
-- @
-- newtype UserId = UserId { unUserId :: String }
--   deriving (Generic)
--   deriving (Read, Show) via (Quiet UserId)
-- @
--
-- >>> show (UserId "simon")
-- UserId "simon"
-- >>> read "UserId \"simon\"" :: UserId
-- UserId "simon"
--
-- If you want to derive 'Read' / 'Show' without using @DerivingVia@ then
-- you can use 'qreadPrec' and 'qshowsPrec' directly.
--
-- @
-- instance Read UserId where readPrec = qreadPrec
-- instance Show UserId where showsPrec = qshowsPrec
-- @
--
module Quiet (
    Quiet(..)
  , qshowsPrec
  , qreadPrec
  ) where

import           GHC.Generics (Generic(..), Rep)
import           GHC.Read (Read(..))

import           Text.ParserCombinators.ReadPrec (ReadPrec)

import           Quiet.Internal (ConType(..), QShow(..), QRead(..))


-- | This implements a quiet version of 'Text.Show.showsPrec' which omits
--   labels for record fields when rendering constructors.
qshowsPrec :: (Generic a, QShow (Rep a)) => Int -> a -> ShowS
qshowsPrec n =
  qshowsPrec_ ConPrefix n . from

-- | This implements a quiet version of 'Text.Read.readPrec' which expects
--   labels for record fields to be omitted when parsing constructors.
qreadPrec :: (Generic a, QRead (Rep a)) => ReadPrec a
qreadPrec =
  fmap to (qreadPrec_ ConPrefix)

-- | Derive 'Read' / 'Show' using @DerivingVia@.
newtype Quiet a =
  Quiet {
      unQuiet :: a
    }

instance (Generic a, QShow (Rep a)) => Show (Quiet a) where
  showsPrec n =
    qshowsPrec n . unQuiet

instance (Generic a, QRead (Rep a)) => Read (Quiet a) where
  readPrec =
    fmap Quiet qreadPrec

