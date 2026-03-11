-- |A module for parsing, comparing, and (eventually) modifying debian version
-- numbers. <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Version>
module Debian.Version
    (DebianVersion -- |Exported abstract because the internal representation is likely to change
    , prettyDebianVersion
    , parseDebianVersion
    , parseDebianVersion'
    , epoch
    , version
    , revision
    , buildDebianVersion
    , evr
    ) where

import Debian.Version.Common
import Debian.Version.String ()
