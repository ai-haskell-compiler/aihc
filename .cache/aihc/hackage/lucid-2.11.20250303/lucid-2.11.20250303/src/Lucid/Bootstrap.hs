{-# OPTIONS -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

-- | Bootstrap layout elements. See
-- <http://getbootstrap.com/2.3.2/scaffolding.html> for more
-- information.

module Lucid.Bootstrap
   (
    -- * Containers
    container_
   ,containerFluid_
   -- * Rows
   ,row_
   ,rowFluid_
   -- * Spans
   ,span1_
   ,span2_
   ,span3_
   ,span4_
   ,span5_
   ,span6_
   ,span7_
   ,span8_
   ,span9_
   ,span10_
   ,span11_
   ,span12_)
  where

import Lucid.Base
import Lucid.Html5

-- | A grid container.
container_ :: Term arg result => arg -> result
container_ =
  termWith "div" [class_ " container "]

-- | A fluid grid container.
containerFluid_ :: Term arg result => arg -> result
containerFluid_ =
  termWith "div" [class_ " container-fluid "]

-- | A grid row.
row_ :: Term arg result => arg -> result
row_ = termWith "div" [class_ " row "]

-- | A fluid grid row.
rowFluid_ :: Term arg result => arg -> result
rowFluid_ = termWith "div" [class_ " row-fluid "]

-- | A span of 1 column.
span1_ :: Term arg result => arg -> result
span1_ = termWith "div" [class_ " span1 "]

-- | A span of 2 columns.
span2_ :: Term arg result => arg -> result
span2_ = termWith "div" [class_ " span2 "]

-- | A span of 3 columns.
span3_ :: Term arg result => arg -> result
span3_ = termWith "div" [class_ " span3 "]

-- | A span of 4 columns.
span4_ :: Term arg result => arg -> result
span4_ = termWith "div" [class_ " span4 "]

-- | A span of 5 columns.
span5_ :: Term arg result => arg -> result
span5_ = termWith "div" [class_ " span5 "]

-- | A span of 6 columns.
span6_ :: Term arg result => arg -> result
span6_ = termWith "div" [class_ " span6 "]

-- | A span of 7 columns.
span7_ :: Term arg result => arg -> result
span7_ = termWith "div" [class_ " span7 "]

-- | A span of 8 columns.
span8_ :: Term arg result => arg -> result
span8_ = termWith "div" [class_ " span8 "]

-- | A span of 9 columns.
span9_ :: Term arg result => arg -> result
span9_ = termWith "div" [class_ " span9 "]

-- | A span of 10 columns.
span10_ :: Term arg result => arg -> result
span10_ = termWith "div" [class_ " span10 "]

-- | A span of 11 columns.
span11_ :: Term arg result => arg -> result
span11_ = termWith "div" [class_ " span11 "]

-- | A span of 12 columns.
span12_ :: Term arg result => arg -> result
span12_ = termWith "div" [class_ " span12 "]
