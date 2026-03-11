{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}

module OpenSuse.Types.EMailAddress
  ( EMailAddress, mkEMailAddress, unEMailAddress
  )
  where

import OpenSuse.Prelude
import OpenSuse.Prelude.PrettyPrinting as Pretty
import Text.Parsec.Rfc2822 ( addr_spec )

-- |
--
-- >>> mkEMailAddress " accept . full (rfc822) . syntax @ example . org "
-- Just (EMailAddress "accept.full.syntax@example.org")
--
-- >>> mkEMailAddress "@this@is@not@good@"
-- Nothing
--
-- >>> prettyShow (fromString "joe @ example.net" :: EMailAddress)
-- "joe@example.net"
newtype EMailAddress = EMailAddress String
  deriving (Show, Eq, Ord, Generic, Hashable, Binary, NFData)

-- | Constructor function for e-mail addresses. Returns 'Nothing' if the input
-- is syntactically invalid.
mkEMailAddress :: String -> Maybe EMailAddress
mkEMailAddress = parseM "e-mail address"

-- | Accessor function for the underlying path of strings.
unEMailAddress :: EMailAddress -> String
unEMailAddress (EMailAddress str) = str

instance HasParser EMailAddress where
  parser = EMailAddress <$> addr_spec

instance IsString EMailAddress where
  fromString = parse "e-mail address"

instance Pretty EMailAddress where
  pPrint = Pretty.text . unEMailAddress
