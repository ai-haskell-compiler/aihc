{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Module      : Redis.Glob
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

Provides functions to validate and use redis @glob@ patterns.

Assumes that the non-printable ASCII characters are __not__ matched.
-}
module Redis.Glob (
  -- * validate and match using redis globs
  validate,
  matches,
) where

import Data.ByteString.Lazy (ByteString)
import Redis.Glob.Internal (fromParts, matchParts, parseParts)


{- $setup
 >>> import Data.ByteString.Lazy (ByteString)
 >>> import Redis.Glob.Internal
 >>> :set -XOverloadedStrings
-}


{- | Confirm that a glob @pattern@ is valid

the result is:
- @Nothing@ when the pattern is invalid
- Just @norm@, where norm is a normalized version of the @pattern@


==== __Examples__

>>> validate "hel?o"
Just "hel?o"

>>> validate "hel[i-m]o"
Just "hel[i-m]o"

>>> validate "hell[i-]o"
Nothing
-}
validate :: ByteString -> Maybe ByteString
validate = fmap fromParts . parseParts


{- | Confirm that a @target@ 'ByteString' matches the @pattern@ defined by another.

the result is:
- 'False' when the pattern is invalid or does not match @target@
- otherwise it's 'True'


==== __Examples__

>>> "hello" `matches` "hel?o"
True

>>> "hello world" `matches` "[^m-z]el[i-m]o w*"
True

>>> "yello world" `matches` "[^m-z]el[i-m]o w???d"
False
-}
matches :: ByteString -> ByteString -> Bool
matches target patt = maybe False (matchParts target) $ parseParts patt
