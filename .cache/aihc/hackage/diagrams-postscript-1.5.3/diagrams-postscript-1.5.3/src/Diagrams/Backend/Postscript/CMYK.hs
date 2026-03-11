{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Postscript.CMYK
-- Copyright   :  (c) 2014 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Support for CMYK color attributes in the Postscript backend.
--
-----------------------------------------------------------------------------

module Diagrams.Backend.Postscript.CMYK (
  -- * CMYK
  -- $color

    CMYK(..)

  -- ** Line color
  , LineColorCMYK, getLineColorCMYK, mkLineColorCMYK, styleLineColorCMYK, lineColorCMYK, lineColorCMYKA, lcCMYK

  -- ** Fill color
  , FillColorCMYK, getFillColorCMYK, mkFillColorCMYK, styleFillColorCMYK, recommendFillColorCMYK
  , fillColorCMYK, fcCMYK

  ) where

import           Control.Lens          (Setter', sets, (.~))
import           Data.Default
import           Data.Maybe            (fromMaybe)
import           Data.Monoid.Recommend
import           Data.Semigroup
import           Data.Typeable

import           Diagrams.Core
import           Graphics.Rendering.Postscript(CMYK(..))

------------------------------------------------------------
--  Color  -------------------------------------------------
------------------------------------------------------------

-- $color
-- CMYK colors are represented with four values from 0.0 to 1.0.


-- | The color with which lines (strokes) are drawn.  Note that child
--   colors always override parent colors; that is, @'lineColorCMYK' c1
--   . 'lineColorCMYK' c2 $ d@ is equivalent to @'lineColorCMYK' c2 $ d@.
--   More precisely, the semigroup structure on line color attributes
--   is that of 'Last'.
newtype LineColorCMYK = LineColorCMYK (Last CMYK)
  deriving (Typeable, Semigroup)
instance AttributeClass LineColorCMYK

instance Default LineColorCMYK where
    def = LineColorCMYK (Last (CMYK 0 0 0 1))

getLineColorCMYK :: LineColorCMYK -> CMYK
getLineColorCMYK (LineColorCMYK (Last c)) = c

mkLineColorCMYK :: CMYK -> LineColorCMYK
mkLineColorCMYK = LineColorCMYK . Last

setAttr :: AttributeClass a => a -> Style v n -> Style v n
setAttr a = atAttr .~ Just a

styleLineColorCMYK :: Setter' (Style v Double ) CMYK
styleLineColorCMYK = sets modifyLineColorCMYK
  where
    modifyLineColorCMYK f s
      = flip setAttr s
      . mkLineColorCMYK
      . f
      . getLineColorCMYK
      . fromMaybe def . getAttr
      $ s

-- | Set the line (stroke) color.
lineColorCMYK :: HasStyle a => CMYK -> a -> a
lineColorCMYK = applyAttr . mkLineColorCMYK

-- | Apply a 'lineColorCMYK' attribute.
lineColorCMYKA :: HasStyle a => LineColorCMYK -> a -> a
lineColorCMYKA = applyAttr

-- | A synonym for 'lineColorCMYK'.
lcCMYK :: HasStyle a => CMYK -> a -> a
lcCMYK = lineColorCMYK

-- | The color with which shapes are filled. Note that child
--   colors always override parent colors; that is, @'fillColorCMYK' c1
--   . 'fillColorCMYK' c2 $ d@ is equivalent to @'lineColorCMYK' c2 $ d@.
--   More precisely, the semigroup structure on fill color attributes
--   is that of 'Last'.
newtype FillColorCMYK = FillColorCMYK (Recommend (Last CMYK))
  deriving (Typeable, Semigroup)
instance AttributeClass FillColorCMYK

instance Default FillColorCMYK where
  def = FillColorCMYK (Recommend (Last (CMYK 0 0 0 0)))

mkFillColorCMYK :: CMYK -> FillColorCMYK
mkFillColorCMYK = FillColorCMYK . Commit . Last

styleFillColorCMYK :: Setter' (Style v Double) CMYK
styleFillColorCMYK = sets modifyFillColorCMYK
  where
    modifyFillColorCMYK f s
      = flip setAttr s
      . mkFillColorCMYK
      . f
      . getFillColorCMYK
      . fromMaybe def . getAttr
      $ s

-- | Set the fill color.
fillColorCMYK :: HasStyle a => CMYK -> a -> a
fillColorCMYK = applyAttr . mkFillColorCMYK

-- | Set a \"recommended\" fill color, to be used only if no explicit
--   calls to 'fillColor' (or 'fc', or 'fcA') are used.
recommendFillColorCMYK :: HasStyle a => CMYK -> a -> a
recommendFillColorCMYK = applyAttr . FillColorCMYK . Recommend . Last

getFillColorCMYK :: FillColorCMYK -> CMYK
getFillColorCMYK (FillColorCMYK c) = getLast . getRecommend $ c

-- | A synonym for 'fillColorCMYK'
fcCMYK :: HasStyle a => CMYK -> a -> a
fcCMYK = fillColorCMYK
