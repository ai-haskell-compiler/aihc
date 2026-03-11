-- |
-- References:
-- - <https://www.ietf.org/rfc/rfc3987 RFC3987>
--
-- @
-- The following rules are different from those in [RFC3986]:
--
-- IRI            = scheme ":" ihier-part [ "?" iquery ]
--                       [ "#" ifragment ]
--
-- ihier-part     = "//" iauthority ipath-abempty
--                / ipath-absolute
--                / ipath-rootless
--                / ipath-empty
--
-- IRI-reference  = IRI / irelative-ref
--
-- absolute-IRI   = scheme ":" ihier-part [ "?" iquery ]
--
-- irelative-ref  = irelative-part [ "?" iquery ] [ "#" ifragment ]
--
-- irelative-part = "//" iauthority ipath-abempty
--                     / ipath-absolute
--
--                / ipath-noscheme
--                / ipath-empty
--
-- iauthority     = [ iuserinfo "@" ] ihost [ ":" port ]
-- iuserinfo      = *( iunreserved / pct-encoded / sub-delims / ":" )
-- ihost          = IP-literal / IPv4address / ireg-name
--
-- ireg-name      = *( iunreserved / pct-encoded / sub-delims )
--
-- ipath          = ipath-abempty   ; begins with "/" or is empty
--                / ipath-absolute  ; begins with "/" but not "//"
--                / ipath-noscheme  ; begins with a non-colon segment
--                / ipath-rootless  ; begins with a segment
--                / ipath-empty     ; zero characters
--
-- ipath-abempty  = *( "/" isegment )
-- ipath-absolute = "/" [ isegment-nz *( "/" isegment ) ]
-- ipath-noscheme = isegment-nz-nc *( "/" isegment )
-- ipath-rootless = isegment-nz *( "/" isegment )
-- ipath-empty    = 0<ipchar>
--
-- isegment       = *ipchar
-- isegment-nz    = 1*ipchar
-- isegment-nz-nc = 1*( iunreserved / pct-encoded / sub-delims
--                      / "@" )
--                ; non-zero-length segment without any colon ":"
--
-- ipchar         = iunreserved / pct-encoded / sub-delims / ":"
--                / "@"
--
-- iquery         = *( ipchar / iprivate / "/" / "?" )
--
-- ifragment      = *( ipchar / "/" / "?" )
--
-- iunreserved    = ALPHA / DIGIT / "-" / "." / "_" / "~" / ucschar
--
-- ucschar        = %xA0-D7FF / %xF900-FDCF / %xFDF0-FFEF
--                / %x10000-1FFFD / %x20000-2FFFD / %x30000-3FFFD
--                / %x40000-4FFFD / %x50000-5FFFD / %x60000-6FFFD
--                / %x70000-7FFFD / %x80000-8FFFD / %x90000-9FFFD
--                / %xA0000-AFFFD / %xB0000-BFFFD / %xC0000-CFFFD
--                / %xD0000-DFFFD / %xE1000-EFFFD
--
-- iprivate       = %xE000-F8FF / %xF0000-FFFFD / %x100000-10FFFD
--
-- Some productions are ambiguous.  The "first-match-wins" (a.k.a.
-- "greedy") algorithm applies.  For details, see [RFC3986].
--
-- The following rules are the same as those in [RFC3986]:
--
-- scheme         = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
--
-- port           = *DIGIT
--
-- IP-literal     = "[" ( IPv6address / IPvFuture  ) "]"
--
-- IPvFuture      = "v" 1*HEXDIG "." 1*( unreserved / sub-delims / ":" )
--
-- IPv6address    =                            6( h16 ":" ) ls32
--                /                       "::" 5( h16 ":" ) ls32
--                / [               h16 ] "::" 4( h16 ":" ) ls32
--                / [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32
--                / [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32
--                / [ *3( h16 ":" ) h16 ] "::"    h16 ":"   ls32
--                / [ *4( h16 ":" ) h16 ] "::"              ls32
--                / [ *5( h16 ":" ) h16 ] "::"              h16
--                / [ *6( h16 ":" ) h16 ] "::"
--
-- h16            = 1*4HEXDIG
-- ls32           = ( h16 ":" h16 ) / IPv4address
--
-- IPv4address    = dec-octet "." dec-octet "." dec-octet "." dec-octet
--
-- dec-octet      = DIGIT                 ; 0-9
--                / %x31-39 DIGIT         ; 10-99
--                / "1" 2DIGIT            ; 100-199
--                / "2" %x30-34 DIGIT     ; 200-249
--                / "25" %x30-35          ; 250-255
--
-- pct-encoded    = "%" HEXDIG HEXDIG
--
-- unreserved     = ALPHA / DIGIT / "-" / "." / "_" / "~"
-- reserved       = gen-delims / sub-delims
-- gen-delims     = ":" / "/" / "?" / "#" / "[" / "]" / "@"
-- sub-delims     = "!" / "$" / "&" / "'" / "(" / ")"
--                / "*" / "+" / "," / ";" / "="
--
-- This syntax does not support IPv6 scoped addressing zone identifiers.
-- @
module Iri.CodePointPredicates.Rfc3987 where

import Iri.CodePointPredicates.Core
import qualified Iri.CodePointPredicates.Rfc3986 as A
import Iri.Prelude hiding (Predicate, inRange, (&&&), (|||))

scheme :: Predicate
scheme =
  A.scheme

unencodedUserInfoComponent :: Predicate
unencodedUserInfoComponent =
  unreserved ||| A.subDelims

{-
ireg-name      = *( iunreserved / pct-encoded / sub-delims )
-}
unencodedRegName :: Predicate
unencodedRegName =
  (unreserved ||| A.subDelims) &&& ((/=) 46)

{-
ipchar         = iunreserved / pct-encoded / sub-delims / ":"
               / "@"
-}
unencodedPathSegment :: Predicate
unencodedPathSegment =
  unreserved ||| A.subDelims ||| oneOfChars ":@"

-- |
-- Reference:
--
-- @
-- iquery         = *( ipchar / iprivate / "/" / "?" )
-- @
--
-- Notice that we've added the "|" char, because some real life URIs seem to contain it.
-- Also we've excluded the '+' char, because it gets decoded as a space char.
unencodedQuery :: Predicate
unencodedQuery =
  (unencodedPathSegment ||| private ||| oneOfChars "/?|") &&& (/= 43)

-- |
-- Notice that we've added the "|" char, because some real life URIs seem to contain it.
-- Also we've excluded the '+' char, because it gets decoded as a space char.
unencodedFragment :: Predicate
unencodedFragment =
  (unencodedPathSegment ||| oneOfChars "/?|") &&& (/= 43)

{-
iunreserved    = ALPHA / DIGIT / "-" / "." / "_" / "~" / ucschar
-}
unreserved :: Predicate
unreserved =
  A.unreserved ||| ucs

{-
ucschar        = %xA0-D7FF / %xF900-FDCF / %xFDF0-FFEF
               / %x10000-1FFFD / %x20000-2FFFD / %x30000-3FFFD
               / %x40000-4FFFD / %x50000-5FFFD / %x60000-6FFFD
               / %x70000-7FFFD / %x80000-8FFFD / %x90000-9FFFD
               / %xA0000-AFFFD / %xB0000-BFFFD / %xC0000-CFFFD
               / %xD0000-DFFFD / %xE1000-EFFFD
-}
ucs :: Predicate
ucs x =
  x
    >= 0xA0
    && x
    <= 0xD7FF
    || x
    >= 0xF900
    && x
    <= 0xFDCF
    || x
    >= 0xFDF0
    && x
    <= 0xFFEF
    || x
    >= 0x10000
    && x
    <= 0x1FFFD
    || x
    >= 0x20000
    && x
    <= 0x2FFFD
    || x
    >= 0x30000
    && x
    <= 0x3FFFD
    || x
    >= 0x40000
    && x
    <= 0x4FFFD
    || x
    >= 0x50000
    && x
    <= 0x5FFFD
    || x
    >= 0x60000
    && x
    <= 0x6FFFD
    || x
    >= 0x70000
    && x
    <= 0x7FFFD
    || x
    >= 0x80000
    && x
    <= 0x8FFFD
    || x
    >= 0x90000
    && x
    <= 0x9FFFD
    || x
    >= 0xA0000
    && x
    <= 0xAFFFD
    || x
    >= 0xB0000
    && x
    <= 0xBFFFD
    || x
    >= 0xC0000
    && x
    <= 0xCFFFD
    || x
    >= 0xD0000
    && x
    <= 0xDFFFD
    || x
    >= 0xE1000
    && x
    <= 0xEFFFD

{-
iprivate       = %xE000-F8FF / %xF0000-FFFFD / %x100000-10FFFD
-}
private :: Predicate
private x =
  x
    >= 0xE000
    && x
    <= 0xF8FF
    || x
    >= 0xF0000
    && x
    <= 0xFFFFD
    || x
    >= 0x100000
    && x
    <= 0x10FFFD
