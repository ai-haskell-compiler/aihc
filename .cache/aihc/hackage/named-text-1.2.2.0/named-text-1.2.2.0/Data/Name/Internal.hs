{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}

module Data.Name.Internal where

import Control.DeepSeq ( NFData )
import Data.Text ( Text )
import GHC.Generics ( Generic )
import GHC.TypeLits


-- | The 'Named' is a wrapper around any 'Data.Text' that identifies the type of
-- 'Data.Text' via the @nameOf@ phantom symbol type, as well as a usage specified
-- by the @style@ type parameter.  Use of 'Named' should always be preferred to
-- using a raw 'Data.Text' (or 'String').

newtype Named (style :: NameStyle) (nameOf :: Symbol) = Named { named :: Text }
  deriving (Generic, NFData, Semigroup)


-- | The NameStyle specifies how the name itself is styled.
--
--  * The 'UTF8' default style is orthogonal to a normal String or Text.
--
--  * The 'CaseInsensitive' style indicates that uppercase ASCII characters are
--    equivalent to their lowercase form.
--
--  * The 'Secure' style is case sensitive, but does not reveal the full contents
--    unless the specific "secureNameBypass" accessor function is used.  This is
--    useful for storing secrets (e.g. passphrases, access tokens, etc.) that
--    should not be fully visible in log messages and other miscellaneous output.
--
-- These styles will be described in more detail below.

type NameStyle = Symbol
