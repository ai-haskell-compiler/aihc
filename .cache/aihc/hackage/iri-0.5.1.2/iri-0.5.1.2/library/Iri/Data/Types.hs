{-# OPTIONS_GHC -funbox-strict-fields #-}

-- |
-- References:
--
-- * <https://tools.ietf.org/rfc/rfc3986.txt URI RFC>
-- * <https://tools.ietf.org/rfc/rfc3987.txt IRI RFC>
module Iri.Data.Types where

import Iri.Prelude

-- |
-- Thorough structure of IRI or URI.
data Iri = Iri !Scheme !Hierarchy !Query !Fragment

newtype Scheme = Scheme ByteString

data Hierarchy
  = AuthorisedHierarchy !Authority !Path
  | AbsoluteHierarchy !Path
  | RelativeHierarchy !Path

data Authority = Authority !UserInfo !Host !Port

data UserInfo
  = PresentUserInfo !User !Password
  | MissingUserInfo

newtype User = User ByteString

data Password
  = PresentPassword !ByteString
  | MissingPassword

data Host
  = NamedHost !RegName
  | IpV4Host !IPv4
  | IpV6Host !IPv6

newtype RegName = RegName (Vector DomainLabel)

data DomainLabel = DomainLabel Text

data Port
  = PresentPort !Word16
  | MissingPort

newtype Path = Path (Vector PathSegment)

newtype PathSegment = PathSegment ByteString

-- |
-- Since the exact structure of the query string is not standardised and
-- methods used to parse the query string may differ between websites,
-- we simply represent it as percent-decoded bytes.
--
-- See <https://en.wikipedia.org/wiki/Query_string>.
newtype Query = Query ByteString

newtype Fragment = Fragment ByteString

-- * Special cases

-------------------------

-- ** HTTP special case

-------------------------

-- |
-- HTTP being by far the most common use-case for resource identifiers,
-- it's been isolated into a dedicated data-type,
-- which is optimised for that particular case.
--
-- Compared to the general URI definition it:
--
-- * only supports the HTTP and HTTPS schemes
-- * misses the Username and Password components
-- * requires the Host component
-- * requires the Path component to be absolute
data HttpIri = HttpIri !Security !Host !Port !Path !Query !Fragment

newtype Security = Security Bool
