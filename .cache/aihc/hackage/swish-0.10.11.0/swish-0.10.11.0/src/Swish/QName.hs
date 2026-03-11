{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

#if (__GLASGOW_HASKELL__ >= 802)
{-# LANGUAGE DerivingStrategies #-}
#endif

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  QName
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011, 2012, 2013, 2020, 2022 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  CPP, DerivingStrategies, OverloadedStrings
--
--  This module defines an algebraic datatype for qualified names (QNames),
--  which represents a 'URI' as the combination of a namespace 'URI'
--  and a local component ('LName'), which can be empty.
--
--  Although RDF supports using IRIs, the use of 'URI' here precludes this,
--  which means that, for instance, 'LName' only accepts a subset of valid
--  characters. There is currently no attempt to convert from an IRI into a URI.
--
--------------------------------------------------------------------------------

-- At present we support using URI references rather than forcing an absolute
-- URI. This is partly to support the existing tests (too lazy to resolve whether
-- the tests really should be using relative URIs in this case).

module Swish.QName
    ( QName
    , LName
    , emptyLName
    , newLName
    , getLName
    , newQName
    , qnameFromURI
    , getNamespace
    , getLocalName
    , getQNameURI
    , qnameFromFilePath
    )
    where

import Data.Char (isAscii)
import Data.Maybe (fromMaybe)
import Data.Interned (intern, unintern)
import Data.Interned.URI (InternedURI)
import Data.Ord (comparing)
import Data.String (IsString(..))

import Network.URI (URI(..), URIAuth(..), parseURIReference)

import System.Directory (canonicalizePath)
import System.FilePath (splitFileName)

import qualified Data.Text as T

------------------------------------------------------------
--  Qualified name
------------------------------------------------------------
--
--  These are RDF QNames rather than XML ones (as much as
--  RDF can claim to have them).
--


{-| A local name, which can be empty.

At present, the local name can not contain a space character and can only
contain ascii characters (those that match 'Data.Char.isAscii').

In version @0.9.0.3@ and earlier, the following characters were not
allowed in local names: \'#\', \':\', or \'/\' characters.

This is all rather experimental.
-}
newtype LName = LName T.Text
    deriving
#if (__GLASGOW_HASKELL__ >= 802)
      stock
#endif
      (Eq, Ord)

instance Show LName where
    show (LName t) = show t

-- | This is not total since attempting to convert a string
--   containing invalid characters will cause an error.
instance IsString LName where
    fromString s = 
        fromMaybe (error ("Invalid local name: " ++ s)) $
                  newLName (T.pack s)

-- | The empty local name.
emptyLName :: LName
emptyLName = LName ""

-- | Create a local name.
newLName :: T.Text -> Maybe LName
-- newLName l = if T.any (`elem` " #:/") l then Nothing else Just (LName l) -- 0.7.0.1 and earlier
-- newLName l = if T.any (\c -> c `elem` " #:/" || not (isAscii c)) l then Nothing else Just (LName l) -- 0.9.0.3 and earlier
newLName l = if T.any (\c -> c == ' ' || not (isAscii c)) l then Nothing else Just (LName l)

-- | Extract the local name.
getLName :: LName -> T.Text
getLName (LName l) = l

{-| 

A qualified name, consisting of a namespace URI
and the local part of the identifier, which can be empty.
The serialisation of a QName is formed by concatanating the
two components.

> Prelude> :set prompt "swish> "
> swish> :set -XOverloadedStrings
> swish> :m + Swish.QName
> swish> let qn1 = "http://example.com/" :: QName
> swish> let qn2 = "http://example.com/bob" :: QName
> swish> let qn3 = "http://example.com/bob/fred" :: QName
> swish> let qn4 = "http://example.com/bob/fred#x" :: QName
> swish> let qn5 = "http://example.com/bob/fred:joe" :: QName
> swish> map getLocalName [qn1, qn2, qn3, qn4, qn5]
> ["","bob","fred","x","fred:joe"]
> swish> getNamespace qn1
> http://example.com/
> swish> getNamespace qn2
> http://example.com/
> swish> getNamespace qn3
> http://example.com/bob/
> swish> getNamespace qn4
> http://example.com/bob/fred#

-}

{-
For now I have added in storing the actual URI
as well as the namespace component. This may or
may not be a good idea (space vs time saving).
-}

data QName = QName !InternedURI URI LName

-- | This is not total since it will fail if the input string is not a valid URI.
instance IsString QName where
  fromString s = 
      fromMaybe (error ("QName conversion given an invalid URI: " ++ s))
      (parseURIReference s >>= qnameFromURI)

-- | Equality is determined by a case sensitive comparison of the               
-- URI.
instance Eq QName where
    u1 == u2 = getQNameURI u1 == getQNameURI u2

-- | In @0.8.0.0@ the ordering now uses the ordering defined in
--   "Network.URI.Ord" rather than the @Show@
--   instance. This should make no difference unless a password
--   was included in the URI when using basic access authorization.
--
instance Ord QName where
    compare = comparing getQNameURI
  
-- | The format used to display the URI is @\<uri\>@, and does not
--   include the password if using basic access authorization.
instance Show QName where
    show (QName u _ _) = "<" ++ show u ++ ">"

{-
The assumption in QName is that the validation done in creating
the local name is sufficient to ensure that the combined 
URI is syntactically valid. Is this true?
-}

-- | Create a new qualified name with an explicit local component.
--
newQName ::
    URI            -- ^ Namespace
    -> LName       -- ^ Local component
    -> QName
newQName ns l@(LName local) = 
  -- Until profiling shows that this is a time/space issue, we use
  -- the following code rather than trying to deconstruct the URI
  -- directly
  let lstr   = T.unpack local
      uristr = show ns ++ lstr
  in case parseURIReference uristr of
       Just uri -> QName (intern uri) ns l
       _ -> error $ "Unable to combine " ++ show ns ++ " with " ++ lstr
  
{-

old behavior

 splitQname "http://example.org/aaa#bbb" = ("http://example.org/aaa#","bbb")
 splitQname "http://example.org/aaa/bbb" = ("http://example.org/aaa/","bbb")
 splitQname "http://example.org/aaa/"    = ("http://example.org/aaa/","")

Should "urn:foo:bar" have a local name of "" or "foo:bar"? For now go
with the first option.

-}

-- | Create a new qualified name.
qnameFromURI :: 
    URI      -- ^ The URI will be deconstructed to find if it contains a local component.
    -> Maybe QName -- ^ The failure case may be removed.
qnameFromURI uri =
  let uf = uriFragment uri
      up = uriPath uri
      q0 = Just $ start uri emptyLName
      start = QName (intern uri)
  in case uf of
       "#"    -> q0
       '#':xs -> start (uri {uriFragment = "#"}) `fmap` newLName (T.pack xs)
       ""     -> case break (== '/') (reverse up) of
                   ("",_) -> q0 -- path ends in / or is empty
                   (_,"") -> q0 -- path contains no /
                   (rlname,rpath) -> 
                       start (uri {uriPath = reverse rpath}) `fmap` 
                       newLName (T.pack (reverse rlname))

       -- e -> error $ "Unexpected: uri=" ++ show uri ++ " has fragment='" ++ show e ++ "'" 
       _ -> Nothing

-- | Return the URI of the namespace stored in the QName.
-- This does not contain the local component.
--
getNamespace :: QName -> URI
getNamespace (QName _ ns _) = ns

-- | Return the local component of the QName.
getLocalName :: QName -> LName
getLocalName (QName _ _ l) = l

-- | Returns the full URI of the QName (ie the combination of the
-- namespace and local components).
getQNameURI :: QName -> URI
getQNameURI (QName u _ _) = unintern u

{-|
Convert a filepath to a file: URI stored in a QName. If the
input file path is relative then the current working directory is used
to convert it into an absolute path.

If the input represents a directory then it *must* end in 
the directory separator - so for Posix systems use 
@\"\/foo\/bar\/\"@ rather than 
@\"\/foo\/bar\"@.

This has not been tested on Windows.
-}

{-
NOTE: not sure why I say directories should end in the path
seperator since

ghci> System.Directory.canonicalizePath "/Users/dburke/haskell/swish-text"
"/Users/dburke/haskell/swish-text"
ghci> System.Directory.canonicalizePath "/Users/dburke/haskell/swish-text/"
"/Users/dburke/haskell/swish-text"

-}

qnameFromFilePath :: FilePath -> IO QName
qnameFromFilePath fname = do
  ipath <- canonicalizePath fname
  let (dname, lname) = splitFileName ipath
      nsuri = URI "file:" emptyAuth dname "" ""
      uri = URI "file:" emptyAuth ipath "" ""
  case lname of
    "" -> return $ QName (intern nsuri) nsuri emptyLName
    _  -> return $ QName (intern uri) nsuri (LName (T.pack lname))

emptyAuth :: Maybe URIAuth
emptyAuth = Just $ URIAuth "" "" ""

--------------------------------------------------------------------------------
--
--  Copyright (c) 2003, Graham Klyne, 2009 Vasili I Galchin,
--    2011, 2012, 2013, 2020, 2022 Douglas Burke
--  All rights reserved.
--
--  This file is part of Swish.
--
--  Swish is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  Swish is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with Swish; if not, write to:
--    The Free Software Foundation, Inc.,
--    59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
--
--------------------------------------------------------------------------------
