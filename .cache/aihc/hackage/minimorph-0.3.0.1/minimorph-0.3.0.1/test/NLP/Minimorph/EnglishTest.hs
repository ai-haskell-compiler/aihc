module NLP.Minimorph.EnglishTest where

import           Data.Text (Text)
import qualified Data.Text as T

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import NLP.Minimorph.English

suite :: Test.Framework.Test
suite = testGroup "NLP.Minimorph.English"
     [ t_defaultVerbStuff
     , t_defaultNounPlural
     , t_indefiniteDet
     , t_acronymWantsAn
     , t_ordinal
     , t_commas
     ]

-- ----------------------------------------------------------------------
-- tests
-- ----------------------------------------------------------------------

t_defaultVerbStuff :: Test.Framework.Test
t_defaultVerbStuff =
    testGroup "defaultVerbStuff" (map tc verbs)
  where
    tc (pl, sg3, pastP) = testCase summary $
        assertEqual summary (sg3, pastP) (defaultVerbStuff pl)
      where
        summary = T.unpack $ T.concat [ pl, " (", sg3, ", ", pastP, ")" ]

t_defaultNounPlural :: Test.Framework.Test
t_defaultNounPlural =
    testGroup "defaultNounPlural" (map tc nouns)
  where
    tc (sg, pl) = testCase summary $
        assertEqual summary pl (defaultNounPlural sg)
      where
        summary = T.unpack $ T.concat [sg, " (", pl, ")"]

t_indefiniteDet :: Test.Framework.Test
t_indefiniteDet = testGroup "indefiniteDet"
    [ tc "eu"          "a"    "eukaryote"
    , tc "eu"          "a"    "Eukaryote"
    , tc "ewe"         "a"    "ewe" -- google 33k 'a ewe' vs 9k 'an ewe'
    , tc "ewok"        "an"   "ewok"
    , tc "ewok"        "an"   "Ewok"
    , tc "7th"         "a"    "7th"
    , tc "8th"         "an"   "8th"
    , tc "18th"        "an"   "18th"
    , tc "xylophone"   "a"    "xylophone"
    , tc "universe"    "a"    "universe"
    , tc "urge"        "an"   "urge"
    , tc "user"        "a"    "user"
    , tc "usher"       "an"   "usher"
    , tc "xylophone"   "a"    "xylophone"
    , tc "u-turn"      "a"    "u-turn"
    , tc "u"           "a"    "u"
    , tc "x-ray"       "an"   "x-ray"
    , tc "g-ray"       "a"    "g-ray"
    , tc "y-chromo"    "a"    "y-chromosome"
    , tc "x-chromo"    "an"   "x-chromosome"
    , tc "x chromo"    "an"   "x chromosome"
    , tc "18-fold"     "an"   "18-fold"
    , tc "-fold ending"         "a"     "-fold ending"
    , tc "'-fold' ending"       "a"     "'-fold' ending"
    , tc "\"-fold\" ending"     "a"     "\"-fold\" ending"
    , tc "mvp award"   "an"   "MVP award"
    , tc "UUCP user"   "a"    "UUCP user"
    ]
  where
    tc msg res inp = testCase summary $
       assertEqual summary res (indefiniteDet inp)
      where
       summary = msg ++ " (" ++ T.unpack (T.unwords [res, inp]) ++ ")"

t_acronymWantsAn :: Test.Framework.Test
t_acronymWantsAn = testGroup "acronymWantsAn"
    [ tc "rgb"       True    "rgb"
    , tc "kml"       False   "kml"
    , tc "ac"        True    "ac"
    , tc "dc"        False   "dc"
    ]
  where
    tc msg res inp = testCase summary $
       assertEqual summary res (acronymWantsAn inp)
      where
       summary = msg ++ " (" ++ T.unpack inp ++ ")"

t_ordinal :: Test.Framework.Test
t_ordinal = testGroup "ordinal"
    [ tc "12th"                  12
    , tc "42nd"                  42
    , tc "44th"                  44
    , tc "41st"                  41
    , tc "-3rd"                  (-3)
    ]
  where
    tc res inp = testCase (show inp ++ " => " ++ T.unpack res) $
        assertEqual "" res (ordinal inp)

t_commas :: Test.Framework.Test
t_commas = testGroup "commas"
    [ tc "foo"                    ["foo"]
    , tc "foo and bar"            ["foo","bar"]
    , tc "foo, bar and baz"       ["foo","bar","baz"]
    , tc "foo, bar, baz and quux" ["foo","bar","baz","quux"]
    ]
  where
    tc res xs = testCase (show (length xs) ++ ": " ++ T.unpack res) $
        assertEqual "" res (commas "and" xs)

-- ----------------------------------------------------------------------
-- lexicon
-- ----------------------------------------------------------------------

nouns :: [(Text,Text)]
nouns =
    [ noun "star"   "stars"
    , noun "egg"    "eggs"
    , noun "patch"  "patches"
    , noun "boy"    "boys"
    , noun "spy"    "spies"
    , noun "thesis" "theses"
    , noun "elf"    "elves"
    , noun "ace"    "aces"
    , noun "5y"     "5ys"
    ]
  where
    noun s p = (s,p)

detNouns :: [(Text,Text)]
detNouns =
    [ noun "box" "boxes"
    , noun "cat" "cats"
    , noun "dog" "dogs"
    , noun "ant" "ants"
    , noun "egg" "eggs"
    ]
  where
    noun s p = (s,p)

verbs :: [(Text,Text,Text)]
verbs =
    [ verb "walk"  "walks"   "walked"
    , verb "push"  "pushes"  "pushed"
    , verb "pass"  "passes"  "passed"
    , verb "abuse" "abuses"  "abused"
    , verb "banjo" "banjoes" "banjoed"
    , verb "play"  "plays"   "played"
    , verb "cry"   "cries"   "cried"
    , verb "goto"  "gotoes"  "gotoed"
    , verb "boo"   "boos"    "booed"
    ]
  where
    verb x y z = (x, y, z)
