-----------------------------------------------------------------------------
-- |
-- Module      :  Text.StringTemplate
-- Copyright   :  (c) Sterling Clover 2008
-- License     :  BSD 3 Clause
-- Maintainer  :  s.clover@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- A StringTemplate is a String with \"holes\" in it.
-- This is a port of the Java StringTemplate library written by Terrence Parr.
-- (<http://www.stringtemplate.org>). User-contributed documentation available at
-- <http://www.haskell.org/haskellwiki/HStringTemplate>.
--
-- This library implements the basic 3.1 grammar, lacking group files
-- (though not groups themselves), Regions, and Interfaces.
-- The goal is not to blindly copy the StringTemplate API, but rather to
-- take its central ideas and implement them in a Haskellish manner.
-- Indentation and wrapping, for example, are implemented through the
-- HughesPJ Pretty Printing library. Calling toPPDoc on a StringTemplate
-- yields a Doc with appropriate paragraph-fill wrapping that can be
-- rendered in the usual fashion.
--
-- Basic instances are provided of the StringTemplateShows and ToSElem class.
-- Any type deriving ToSElem can be passed automatically as a StringTemplate
-- attribute. This package can be installed with syb-with-class bindings
-- that provide a ToSElem instance for anything deriving
-- 'Data.Generics.SYB.WithClass.Basics.Data'. When defining an instance of
-- ToSElem that can take a format parameter, you should first define an
-- instance of StringTemplateShows, and then define an instance of ToSElem
-- where @ toSElem = stShowsToSE@.
-----------------------------------------------------------------------------

module Text.StringTemplate (
  -- * Types
  StringTemplate, STGroup,
  -- * Classes
  ToSElem(..), StringTemplateShows(..), stShowsToSE, Stringable(..), SEType(..),
  -- * Creation
  newSTMP, newAngleSTMP, getStringTemplate, getStringTemplate',
  -- * Display
  toString, toPPDoc, render, dumpAttribs, checkTemplate, checkTemplateDeep,
  -- * Modification
  setAttribute, (|=), setManyAttrib,
  setNativeAttribute, setManyNativeAttrib,
  withContext,
  optInsertTmpl, optInsertGroup,
  setEncoder, setEncoderGroup,
  -- * Groups
  groupStringTemplates, addSuperGroup, addSubGroup,
  mergeSTGroups, directoryGroup, directoryGroupExt,
  unsafeVolatileDirectoryGroup,
  directoryGroupRecursive, directoryGroupRecursiveLazy,
  directoryGroupRecursiveExt, directoryGroupRecursiveLazyExt,
  directoryGroupLazy, directoryGroupLazyExt, nullGroup
  ) where
import Text.StringTemplate.Base
import Text.StringTemplate.Group
import Text.StringTemplate.Renderf
import Text.StringTemplate.Instances()
