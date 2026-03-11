-----------------------------------------------------------------------------
-- |
--
-- Module      :  Text.Regex.Base
-- Copyright   :  (c) Chris Kuklewicz 2006
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Maintainer  :  Andreas Abel
-- Stability   :  stable
-- Portability :  non-portable (MPTC+FD)
--
-- Classes and instances for Regex matching.
--
--
-- This module merely imports and re-exports the common part of the new
-- api: "Text.Regex.Base.RegexLike" and "Text.Regex.Base.Context".
--
-- To see what result types the instances of RegexContext can produce,
-- please read the "Text.Regex.Base.Context" haddock documentation.
--
-- This does not provide any of the backends, just the common interface
-- they all use.  The modules which provide the backends and their cabal
-- packages are:
--
--  * @<https://hackage.haskell.org/package/regex-posix/docs/Text-Regex-Posix.html Text.Regex.Posix>@ from <https://hackage.haskell.org/package/regex-posix regex-posix>
--
--  * @<https://hackage.haskell.org/package/regex-compat/docs/Text-Regex.html Text.Regex>@ from <https://hackage.haskell.org/package/regex-compat regex-compat> (uses <https://hackage.haskell.org/package/regex-posix regex-posix>)
--
--  * @<https://hackage.haskell.org/package/regex-parsec/docs/Text-Regex-Parsec.html Text.Regex.Parsec>@ from <https://hackage.haskell.org/package/regex-parsec regex-parsec>
--
--  * @<https://hackage.haskell.org/package/regex-dfa/docs/Text-Regex-DFA.html Text.Regex.DFA>@ from <https://hackage.haskell.org/package/regex-dfa regex-dfa>
--
--  * @<https://hackage.haskell.org/package/regex-pcre/docs/Text-Regex-PCRE.html Text.Regex.PCRE>@ from <https://hackage.haskell.org/package/regex-pcre regex-pcre>
--
--  * @<https://hackage.haskell.org/package/regex-tre/docs/Test-Regex-TRE.html Test.Regex.TRE>@ from <https://hackage.haskell.org/package/regex-tre regex-tre>
--
-- In fact, just importing one of the backends is adequate, you do not
-- also need to import this module.
--
-- == Example
--
-- The code
--
-- @
-- import Text.Regex.Base
-- import Text.Regex.Posix ((=~),(=~~)) -- or TDFA or PCRE or ...
--
-- main = do
--     print b
--     print c
--     print d
--   where
--     b :: Bool
--     b = ("abaca" =~ "(.)a")
--     c :: [MatchArray]
--     c = ("abaca" =~ "(.)a")
--     d :: Maybe (String,String,String,[String])
--     d = ("abaca" =~~ "(.)a")
-- @
--
-- will output
--
-- > True
-- > [array (0,1) [(0,(1,2)),(1,(1,1))],array (0,1) [(0,(3,2)),(1,(3,1))]]
-- > Just ("a","ba","ca",["b"])
--

-----------------------------------------------------------------------------

module Text.Regex.Base (getVersion_Text_Regex_Base
  -- | RegexLike defines classes and type, and 'Extract' instances
  ,module Text.Regex.Base.RegexLike) where

import Data.Version(Version)
import Text.Regex.Base.RegexLike
import Text.Regex.Base.Context()

import qualified Paths_regex_base

getVersion_Text_Regex_Base :: Version
getVersion_Text_Regex_Base = Paths_regex_base.version
