-----------------------------------------------------------------------------
-- |
-- Module      :  Lentil.Print
-- Copyright   :  © 2015 Francesco Ariis
-- License     :  GPLv3 (see the LICENSE file)
--
-- printing types
-----------------------------------------------------------------------------

module Lentil.Print where

import Lentil.Types
import Lentil.Query

import Prettyprinter

import qualified Data.List as L
import qualified Data.Text as T
import qualified Prettyprinter.Render.Terminal as PT

-- number of spaces + digits in indentation levels
indNum :: Int
indNum = 6

----------------
-- PRIMITIVES --
----------------

-- right align a number in a space of i columns
alignNumber :: Int -> Int -> String
alignNumber i n = replicate (i - length sn) ' ' ++ sn
    where sn = show n

-- Bool: wheter or not to use colours in rendering
myRender :: Bool -> Doc PT.AnsiStyle -> String
myRender col d = T.unpack . PT.renderStrict . layoutPretty lOpts $ d'
    where
          lOpts :: LayoutOptions
          lOpts = LayoutOptions (AvailablePerLine 75 1)

          d' | col       = d
             | otherwise = unAnnotate d


------------------
-- PRETTY PRINT --
------------------

data TagCol = Red | Blue deriving (Eq)
ppTag :: TagCol -> Tag -> Doc PT.AnsiStyle
ppTag c t = col (pretty openDel) <> pretty (tagString t) <>
            col (pretty closeDel)
    where
          col :: Doc PT.AnsiStyle -> Doc PT.AnsiStyle
          col | c == Red  = annotate (PT.color PT.Red)
              | otherwise = annotate (PT.color PT.Blue)

ppIssue :: Int -> Issue -> Doc PT.AnsiStyle
ppIssue ind is = indent spInd ( fillSep [pretty iNum, pretty "",
                                         hang 0 (fillSep $ ppDescTags is)] )
    where
          ppDescTags :: Issue -> [Doc PT.AnsiStyle]
          ppDescTags i = map pretty (words (iPPDesc is)) ++
                         map (ppTag Blue) (iTags i)

          spInd = indNum - ind
          iNum  = alignNumber ind $ iRow is


ppFile :: Int -> [Issue] -> Doc PT.AnsiStyle
ppFile ind is = vsep
                  [pretty (prettyFP $ iFile (head is)),
                   vsep (map (ppIssue ind) is)]

ppIssues :: Bool -> [Issue] -> String
ppIssues col is = myRender col $ vsep (L.intersperse softline igr)
    where
          ind = maximum . map (length . show . iRow) $ is
          igr = map (ppFile ind) $ groupIssues iFile is


-------------
-- REPORTS --
-------------

-- tagpop pp report
ppPopularity :: Bool -> [Issue] -> String
ppPopularity col is = myRender col $ vsep [pretty "Tags popularity:",
                                           vsep ppPops] -- PP.<$> (string "")
    where
          listpop = tagPop is

          ind   = maximum . map (length . show . snd) $ listpop
          spInd = indNum - ind

          ppPop (t, n) = indent spInd (fillSep [iNum n, pretty "",
                                                hang 0 (ppTag Red t)])
          ppPops       = map ppPop listpop

          iNum n = pretty $ alignNumber ind n

