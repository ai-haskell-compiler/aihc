-----------------------------------------------------------------------------
-- |
-- Module      :  Web.Routes.Base
-- Copyright   :  (c) 2010 Jeremy Shaw
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  partners@seereason.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Conversions between raw pathinfos and decoded path segments.
-----------------------------------------------------------------------------
module Web.Routes.Base
       ( encodePathInfo
       , decodePathInfo
       , decodePathInfoParams
       ) where

import Blaze.ByteString.Builder (Builder, toByteString)
import Codec.Binary.UTF8.String (encodeString, decodeString)
import Data.ByteString          (ByteString)
import Data.List                (intercalate, intersperse)
import Data.Text                (Text)
import Data.Text.Encoding       as Text (encodeUtf8, decodeUtf8)
import Network.HTTP.Types       (Query, encodePath, decodePath, decodePathSegments, queryTextToQuery, queryToQueryText)

{-

From RFC1738 - 3.3

   The HTTP URL scheme is used to designate Internet resources
   accessible using HTTP (HyperText Transfer Protocol).

   The HTTP protocol is specified elsewhere. This specification only
   describes the syntax of HTTP URLs.

   An HTTP URL takes the form:

      http://<host>:<port>/<path>?<searchpart>

   where <host> and <port> are as described in Section 3.1. If :<port>
   is omitted, the port defaults to 80.  No user name or password is
   allowed.  <path> is an HTTP selector, and <searchpart> is a query
   string. The <path> is optional, as is the <searchpart> and its
   preceding "?". If neither <path> nor <searchpart> is present, the "/"
   may also be omitted.

   Within the <path> and <searchpart> components, "/", ";", "?" are
   reserved.  The "/" character may be used within HTTP to designate a
   hierarchical structure.

From FRC1808 - 2.1 URL Syntactic Components

   The URL syntax is dependent upon the scheme.  Some schemes use
   reserved characters like "?" and ";" to indicate special components,
   while others just consider them to be part of the path.  However,
   there is enough uniformity in the use of URLs to allow a parser to
   resolve relative URLs based upon a single, generic-RL syntax.  This
   generic-RL syntax consists of six components:

      <scheme>://<net_loc>/<path>;<params>?<query>#<fragment>

   URL         = ( absoluteURL | relativeURL ) [ "#" fragment ]

   absoluteURL = generic-RL | ( scheme ":" *( uchar | reserved ) )

   generic-RL  = scheme ":" relativeURL

   relativeURL = net_path | abs_path | rel_path

   net_path    = "//" net_loc [ abs_path ]
   abs_path    = "/"  rel_path
   rel_path    = [ path ] [ ";" params ] [ "?" query ]

   path        = fsegment *( "/" segment )
   fsegment    = 1*pchar
   segment     =  *pchar

   params      = param *( ";" param )
   param       = *( pchar | "/" )

   pchar       = uchar | ":" | "@" | "&" | "="
   uchar       = unreserved | escape
   unreserved  = alpha | digit | safe | extra

From RFC2396 - 3.3

      path_segments = segment *( "/" segment )
      segment       = *pchar *( ";" param )
      param         = *pchar

      pchar         = unreserved | escaped |
                      ":" | "@" | "&" | "=" | "+" | "$" | ","

   The path may consist of a sequence of path segments separated by a
   single slash "/" character.  Within a path segment, the characters
   "/", ";", "=", and "?" are reserved.  Each path segment may include a
   sequence of parameters, indicated by the semicolon ";" character.
   The parameters are not significant to the parsing of relative
   references.

From RFC3986 - 3.3

   The path component contains data, usually organized in hierarchical
   form, that, along with data in the non-hierarchical query component
   (Section 3.4), serves to identify a resource within the scope of the
   URI's scheme and naming authority (if any).  The path is terminated
   by the first question mark ("?") or number sign ("#") character, or
   by the end of the URI.

   If a URI contains an authority component, then the path component
   must either be empty or begin with a slash ("/") character.  If a URI
   does not contain an authority component, then the path cannot begin
   with two slash characters ("//").  In addition, a URI reference
   (Section 4.1) may be a relative-path reference, in which case the
   first path segment cannot contain a colon (":") character.  The ABNF
   requires five separate rules to disambiguate these cases, only one of
   which will match the path substring within a given URI reference.  We
   use the generic term "path component" to describe the URI substring
   matched by the parser to one of these rules.

      path          = path-abempty    ; begins with "/" or is empty
                    / path-absolute   ; begins with "/" but not "//"
                    / path-noscheme   ; begins with a non-colon segment
                    / path-rootless   ; begins with a segment
                    / path-empty      ; zero characters

      path-abempty  = *( "/" segment )
      path-absolute = "/" [ segment-nz *( "/" segment ) ]
      path-noscheme = segment-nz-nc *( "/" segment )
      path-rootless = segment-nz *( "/" segment )
      path-empty    = 0<pchar>

      segment       = *pchar
      segment-nz    = 1*pchar
      segment-nz-nc = 1*( unreserved / pct-encoded / sub-delims / "@" )
                    ; non-zero-length segment without any colon ":"

      pchar         = unreserved / pct-encoded / sub-delims / ":" / "@"

   A path consists of a sequence of path segments separated by a slash
   ("/") character.  A path is always defined for a URI, though the
   defined path may be empty (zero length).  Use of the slash character
   to indicate hierarchy is only required when a URI will be used as the
   context for relative references.  For example, the URI
   <mailto:fred@example.com> has a path of "fred@example.com", whereas
   the URI <foo://info.example.com?fred> has an empty path.

   The path segments "." and "..", also known as dot-segments, are
   defined for relative reference within the path name hierarchy.  They
   are intended for use at the beginning of a relative-path reference
   (Section 4.2) to indicate relative position within the hierarchical
   tree of names.  This is similar to their role within some operating
   systems' file directory structures to indicate the current directory
   and parent directory, respectively.  However, unlike in a file
   system, these dot-segments are only interpreted within the URI path
   hierarchy and are removed as part of the resolution process (Section
   5.2).

   Aside from dot-segments in hierarchical paths, a path segment is
   considered opaque by the generic syntax.  URI producing applications
   often use the reserved characters allowed in a segment to delimit
   scheme-specific or dereference-handler-specific subcomponents.  For
   example, the semicolon (";") and equals ("=") reserved characters are
   often used to delimit parameters and parameter values applicable to
   that segment.  The comma (",") reserved character is often used for
   similar purposes.  For example, one URI producer might use a segment
   such as "name;v=1.1" to indicate a reference to version 1.1 of
   "name", whereas another might use a segment such as "name,1.1" to
   indicate the same.  Parameter types may be defined by scheme-specific
   semantics, but in most cases the syntax of a parameter is specific to
   the implementation of the URI's dereferencing algorithm.
-}


{-

Reserved characters:

If a character is unreserved, then you can included it as the literal
character, or percent encode it, and it does not change its
meaning. The two urls will be equal to each other.

Some characters are explicitly reserved in different url schemes. For
example the '/' character in a path component has special meaning, and
therefore any occurance of '/' must be escaped unless it is being used
for it's reserved purposed.

The spec also provides a list of characters than can be reserved in
specific url spec. For example, a url producer can choose to use , as
a reserved character. However, it is not obligated to use , as a
reserved character.

From RFC3986 - 2.2

   Characters in the "reserved" set are not reserved in all contexts.
   The set of characters actually reserved within any given URI
   component is defined by that component. In general, a character is
   reserved if the semantics of the URI changes if the character is
   replaced with its escaped US-ASCII encoding.

Some choices we made:

The presence of ; and params in a path segment is handled differently
in the different RFCs. It does some clear, though that ; is supposed
to indicate the start of parameters. Hence we should escape ; so that
if it appears in a url it does not treated as parameters when it was
not meant to be. At present we offer no way for a user who actually
wants to add parameters. That would probably be done path extending
the encodePathInfo to be more like:

 encodePathInfo :: [(String, [Param])] -> String

The spec also forbids a path from starting with // if the scheme has
no authority. This library is currently only intended to be used with
the http scheme, so we do not have to worry about that rule, since the
http scheme does have an authority.

-}

{-|
Encodes a list of path segments into a valid URL fragment.

This function takes the following three steps:

* UTF-8 encodes the characters.

* Performs percent encoding on all unreserved characters, as well as \:\@\=\+\$,

* Intercalates with a slash.

For example:

> encodePathInfo [\"foo\", \"bar\", \"baz\"]

\"foo\/bar\/baz\"

> encodePathInfo [\"foo bar\", \"baz\/bin\"]

\"foo\%20bar\/baz\%2Fbin\"

> encodePathInfo [\"שלום\"]

\"%D7%A9%D7%9C%D7%95%D7%9D\"

-}
encodePathInfo :: [Text] -> [(Text, Maybe Text)] -> Text
encodePathInfo segments qs =
    Text.decodeUtf8 $ toByteString $ encodePathInfoUtf8 segments qs

encodePathInfoUtf8 :: [Text] -> [(Text, Maybe Text)] -> Builder
encodePathInfoUtf8 segments qs =
    encodePath segments (queryTextToQuery qs)

{-|
Performs the inverse operation of 'encodePathInfo'.

In particular, this function:

* Splits a string at each occurence of a forward slash.

* Percent-decodes the individual pieces.

* UTF-8 decodes the resulting data.

This utilizes 'decodeString' from the utf8-string library, and thus all UTF-8
decoding errors are handled as per that library.

In general, you will want to strip the leading slash from a pathinfo before
passing it to this function. For example:

> decodePathInfo \"\"

\[\]

> decodePathInfo \"\/\"

[\"\"]

Note that while function accepts a 'Text' value, it is expected that 'Text' will only contain the subset of characters which are allowed to appear in a URL.
-}
decodePathInfo :: ByteString -> [Text]
decodePathInfo = decodePathSegments

-- | Returns path segments as well as possible query string components
--
-- For example:
--
-- > decodePathInfoParams "/home?q=1"
-- (["home"],[("q",Just "1")])
--
decodePathInfoParams :: ByteString -> ([Text], [(Text, Maybe Text)])
decodePathInfoParams = fmap queryToQueryText . decodePath
