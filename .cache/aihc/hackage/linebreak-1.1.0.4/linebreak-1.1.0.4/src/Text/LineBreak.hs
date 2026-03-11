--------------------------------------------------------------------------------
-- |
-- Module      :  Text.LineBreak
-- Copyright   :  (C) 2014 Francesco Ariis
-- License     :  BSD3 (see LICENSE file)
--
-- Maintainer  :  Francesco Ariis <fa-ml@ariis.it>
-- Stability   :  provisional
-- Portability :  portable
--
-- Simple functions to break a String to fit a maximum text width, using
-- Knuth-Liang hyphenation algorithm.
--
-- Example:
--
-- > import Text.LineBreak
-- >
-- > hyp = Just english_US
-- > bf = BreakFormat 25 4 '-' hyp
-- > cs = "Using hyphenation with gruesomely non parsimonious wording."
-- > main = putStr $ breakString bf cs
--
-- will output:
--
-- > Using hyphenation with
-- > gruesomely non parsimo-
-- > nious wording.
--
-------------------------------------------------------------------------------

module Text.LineBreak ( -- * Line breaking
                        breakString, breakStringLn, BreakFormat(..),
                        -- * Hypenators
                        -- | Convenience reexport from
                        -- "Text.Hyphenation.Language".
                        Hyphenator,
                        afrikaans, armenian, assamese, basque, bengali,
                        bulgarian, catalan, chinese, coptic, croatian,
                        czech, danish, dutch, english_US, english_GB,
                        esperanto, estonian, ethiopic, finnish, french,
                        friulan, galician, georgian, german_1901,
                        german_1996, german_Swiss, greek_Ancient,
                        greek_Mono, greek_Poly, gujarati, hindi, hungarian,
                        icelandic, indonesian, interlingua, irish,
                        italian, kannada, kurmanji, latin, latin_Classic,
                        latvian, lithuanian, malayalam, marathi,
                        mongolian, norwegian_Bokmal, norwegian_Nynorsk,
                        occitan, oriya, panjabi, piedmontese, polish,
                        portuguese, romanian, romansh, russian, sanskrit,
                        serbian_Cyrillic, serbocroatian_Cyrillic,
                        serbocroatian_Latin, slovak, slovenian, spanish,
                        swedish, tamil, telugu, thai, turkish, turkmen,
                        ukrainian, uppersorbian, welsh
                      ) where

import Text.Hyphenation
import Data.Char (isSpace)
import Data.List (find, inits, tails)

-- TODO: tabs are broken (as it is just a plain substitution). Use a
--       smart sub method. [bug] [test]

-- TODO: [improvement] valid for Text, etc.


-----------
-- TYPES --
-----------

-- | How to break the strings: maximum width of the lines, number of spaces
-- to replace tabs with (dumb replacement), symbol to use to hyphenate
-- words, hypenator to use (language, exceptions, etc.; refer to
-- "Text.Hyphenation" for usage instructions). To break lines without
-- hyphenating, put @Nothing@ in @bfHyphenator@.
data BreakFormat = BreakFormat { bfMaxCol :: Int,
                                 bfTabRep :: Int,
                                 bfHyphenSymbol :: Char,
                                 bfHyphenator :: Maybe Hyphenator }

data BrState = BrState { _bsCurrCol :: Int,      -- current column
                         bsBroken  :: String }   -- output string

data Element = ElWord String     -- things we need to place
             | ElSpace Int Bool  -- n of spaces, presence of final breakline
             deriving (Show)


---------------
-- FUNCTIONS --
---------------

-- | Breaks some text (String) to make it fit in a certain width. The output
-- is a String, suitable for writing to screen or file.
breakString :: BreakFormat -> String -> String
breakString bf cs = hackClean out
    where els = parseEls (subTabs (bfTabRep bf) cs)
          out = bsBroken $ foldl (putElem bf) (BrState 0 "") els
          -- todo horrible hack is horrible [benchmark] [refactor]

-- | Convenience for @lines $ breakString bf cs@.
breakStringLn :: BreakFormat -> String -> [String]
breakStringLn bf cs = lines $ breakString bf cs

-----------------
-- ANCILLARIES --
-----------------

-- PARSING --

-- fino a qui
-- o word 'till ws
-- o wspa 'till (\n | word). se \n, prendilo
parseEls :: String -> [Element]
parseEls []       = []
parseEls cs@(c:_) | isSpace c = let (p, r) = span isSpace cs
                                in parseWS p ++ parseEls r
                  | otherwise = let (p, r) = break isSpace cs
                                in parseWord p : parseEls r

-- Signatures between the two |parse| are different because there can
-- be more element in a single white-space string (newline newline), while
-- that is not possible with parseWord
parseWS :: String -> [Element]
parseWS [] = []
parseWS ws = case span (/= '\n') ws of
               (a, "")      -> [elspace a False] -- no newlines
               (a, '\n':rs) ->  elspace a True : parseWS rs
               _            -> error "parseWS: non-exhaustive pattern"
    where elspace cs b = ElSpace (length cs) b

parseWord :: String -> Element
parseWord wr = ElWord wr

-- number of spaces to replace \t with, string
subTabs :: Int -> String -> String
subTabs i cs = cs >>= f i
    where f _ '\t' = replicate i ' '
          f _ c    = return c

-- COMPUTATION --

putElem :: BreakFormat -> BrState -> Element -> BrState
putElem (BreakFormat maxc _ sym hyp)
        bs@(BrState currc _) el =
            if avspace >= elLenght el
              then putString bs maxc (el2string el)
              else case el of
                     (ElSpace _ _) -> putString bs maxc "\n"
                     (ElWord cs)   -> putString bs maxc (broken cs)
    where avspace = maxc - currc -- starting col: 1
          fstcol  = currc == 0
          broken cs = breakWord hyp sym avspace cs fstcol

elLenght :: Element -> Int
elLenght (ElWord cs)   = length cs
elLenght (ElSpace i _) = i

-- convert element to string
el2string :: Element -> String
el2string (ElSpace i False) = replicate i ' '
el2string (ElSpace _ True) = "\n"
el2string (ElWord cs) = cs

-- put a string and updates the state
-- (more than macol? new line, but no hyphenation!)
putString :: BrState -> Int -> String -> BrState
putString bs                      _      [] = bs
putString (BrState currc currstr) maxcol (c:cs) =
                let currc' = if c == '\n'
                               then 0
                               else currc + 1
                    bs' = if currc' <= maxcol
                            then BrState currc' (currstr ++ [c])
                            else BrState 1      (currstr ++ "\n" ++ [c])
                in putString bs' maxcol cs

-- breaks a word given remaining space, using an hypenator
-- the last bool is a "you are on the first col, can't start
-- a new line
breakWord :: Maybe Hyphenator -> Char -> Int -> String -> Bool -> String
breakWord mhy ch avspace cs nlb = case find ((<= avspace) . hypLen) poss of
                                    Just a  -> a
                                    Nothing -> cs -- don't find? return input
    where hw = case mhy of
                 Just hy -> hyphenate hy cs
                 Nothing -> [cs]
          poss = map cf $ reverse $ zip (inits hw) (tails hw)

          -- crea hyphenated from two bits
          cf ([], ew) = (if nlb then "" else "\n") ++ concat ew
          cf (iw, []) = concat iw ++ "\n"
          cf (iw, ew) = concat iw ++ [ch] ++ "\n" ++ concat ew

          hypLen wcs = length . takeWhile (/= '\n') $ wcs

-- CLEAN --

-- removes eof/eol whitespace
hackClean :: String -> String
hackClean cs = noEoflWs cs
    where noEoflWs wcs = f "" wcs

          -- the ugliness
          f acc []        = acc
          f acc wcs@(a:as) =
                let (i, e) = span (== ' ') wcs in
                if i == ""
                  then f (acc ++ [a]) as
                  else case e of
                         ('\n':rest) -> f (acc ++ "\n") rest -- eol ws
                         []          -> f acc           []   -- eof ws
                         _           -> f (acc ++ [a]) as -- normal


