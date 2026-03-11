-----------------------------------------------------------------------------
-- |
-- Module      :  Lentil.Export
-- Copyright   :  © 2015 Francesco Ariis
-- License     :  GPLv3 (see the LICENSE file)
--
-- Exporting issues to various formats
-----------------------------------------------------------------------------

module Lentil.Export where

import Text.CSV
import Lentil.Types

import qualified Data.List as L
import Data.Function (on)

---------------
-- FUNCTIONS --
---------------

issues2CSV :: [Issue] -> String
issues2CSV is = printCSV (firstLine : map i2r is)
    where firstLine = ["Filepath", "Row", "Description", "Tags"]
          i2r i     = [(prettyFP . iFile) i, show (iRow i),
                       iPPDesc i, tags2String (iTags i)]


-- compiler-like (gcc/ghc) warnings, parseable by emacs
issues2Compiler :: [Issue] -> String
issues2Compiler is = concatMap i2c is
    where i2c i = (prettyFP . iFile) i ++ ":" ++ show (iRow i) ++ ": " ++
                  iPPDesc i ++ " " ++ tags2StringPretty (iTags i) ++ "\n"

-- xml output
issues2Xml :: [Issue] -> String
issues2Xml is = concat ["<issues>", concatMap is2x (groupBy is), "</issues>", "\n"]
  where i2x i = concat ["<issue>",
                          "<row>", show (iRow i), "</row>",
                          maybe "" d2x (iDesc i),
                          "<tags>", concatMap t2x (iTags i), "</tags>",
                        "</issue>"]
        d2x d = concat ["<description>", cdata d, "</description>"]
        t2x t = concat ["<tag>", tagString t, "</tag>"]
        is2x [] = ""
        is2x is'@(i:_) = let
          fp = file i
          in concat ["<file>", "<filename>", cdata fp, "</filename>", concatMap i2x is', "</file>"]
        cdata x = concat ["<![CDATA[", x, "]]>"]
        file = prettyFP . iFile
        groupBy = L.groupBy ((==) `on` file)

-- todo [feature] [u:3] markdown output
--     - che genere di header? o lista di liste?
--     - come mettere i tag? /[[cdsacad] [cdsac]/
--     - lista? minuses
--     - wrap at 80? sì
--     - cmark

issues2File :: [Issue] -> String
issues2File is = unlines . L.nub . L.sort $ map iFile is


-----------------
-- ANCILLARIES --
-----------------

-- tag1 tag2 etc
tags2String :: [Tag] -> String
tags2String ts = unwords (map tagString ts)

-- [tag1] [tag2] etc.
tags2StringPretty :: [Tag] -> String
tags2StringPretty ts = unwords .  map ((openDel:) .
                       (++[closeDel]) . tagString) $ ts

