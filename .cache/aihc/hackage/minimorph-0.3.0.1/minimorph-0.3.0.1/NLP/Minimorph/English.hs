{-# LANGUAGE ViewPatterns #-}
-- TODO : learn how to use Functional Morphology instead
-- |Simple default rules for English morphology
module NLP.Minimorph.English where

#if !(MIN_VERSION_base(4,11,0))
  -- this is redundant starting with base-4.11 / GHC 8.4
import Data.Semigroup
#endif

import           Data.Char (isSpace, isUpper, toLower)
import           Data.Text (Text)
import qualified Data.Text as T

import NLP.Minimorph.Util

-- ---------------------------------------------------------------------
-- ** Punctuation
-- ---------------------------------------------------------------------

-- | No Oxford commas, alas.
--
-- > commas "and" "foo bar"       == "foo and bar"
-- > commas "and" "foo, bar, baz" == "foo, bar and baz"
commas :: Text -> [Text] -> Text
commas _ []  = ""
commas _ [x] = x
commas et xs = T.intercalate ", " (init xs) <+> et <+> last xs

-- ---------------------------------------------------------------------
-- ** Numbers
-- ---------------------------------------------------------------------

-- | > cardinal 0 == "zero"
--   > cardinal 1 == "one"
--   > cardinal 2 == "two"
--   > cardinal 10 == "ten"
--   > cardinal 11 == "11"
cardinal :: Int -> Text
cardinal n = case n of
    0  -> "zero"
    1  -> "one"
    2  -> "two"
    3  -> "three"
    4  -> "four"
    5  -> "five"
    6  -> "six"
    7  -> "seven"
    8  -> "eight"
    9  -> "nine"
    10 -> "ten"
    _ -> tshow n

-- | > ordinalNotSpelled 1 == "1st"
--   > ordinalNotSpelled 2 == "2nd"
--   > ordinalNotSpelled 11 == "11th"
ordinalNotSpelled :: Int -> Text
ordinalNotSpelled k = case abs k `rem` 100 of
  n | n > 3 && n < 21 -> k `suf` "th"
    | n `rem` 10 == 1 -> k `suf` "st"
    | n `rem` 10 == 2 -> k `suf` "nd"
    | n `rem` 10 == 3 -> k `suf` "rd"
    | otherwise       -> k `suf` "th"
 where
  num `suf` s = tshow num <> s

-- | > ordinal 1 == "first"
--   > ordinal 2 == "second"
--   > ordinal 3 == "third"
--   > ordinal 11 == "11th"
--   > ordinal 42 == "42nd"
ordinal :: Int -> Text
ordinal n = case n of
    0  -> "zeroth"
    1  -> "first"
    2  -> "second"
    3  -> "third"
    4  -> "fourth"
    5  -> "fifth"
    6  -> "sixth"
    7  -> "seventh"
    8  -> "eighth"
    9  -> "ninth"
    10 -> "tenth"
    k  -> ordinalNotSpelled k

-- ---------------------------------------------------------------------
-- ** Nouns and verbs
-- ---------------------------------------------------------------------

-- | Heuristics for English plural for an unknown noun.
--
-- > defaultNounPlural "egg"    == "eggs"
-- > defaultNounPlural "patch"  == "patches"
-- > defaultNounPlural "boy"    == "boys"
-- > defaultNounPlural "spy"    == "spies"
-- > defaultNounPlural "thesis" == "theses"
--
-- http://www.paulnoll.com/Books/Clear-English/English-plurals-1.html
--
-- http://en.wikipedia.org/wiki/English_plural
defaultNounPlural :: Text -> Text
defaultNounPlural x
    | "is" `T.isSuffixOf` x = thesis
    | hasSibilantSuffix x   = sibilant_o
    | hasCoSuffix x         = sibilant_o
    | hasCySuffix x         = y_final
    | "ff" `T.isSuffixOf` x = ff_final  -- quite often not the case
    | "f" `T.isSuffixOf` x  = f_final   -- but this one as well, so both needed
    | otherwise             = plain
  where
    plain      = x            <> "s"
    sibilant_o = x            <> "es"
    y_final    = T.init x     <> "ies"
    f_final    = T.init x     <> "ves"
    ff_final   = T.dropEnd 2 x <> "ves"
    thesis     = T.dropEnd 2 x <> "es"

-- | Heuristics for 3rd person singular and past participle
--   for an unknown regular verb. Doubling of final consonants
--   can be handled via a table of (partially) irregular verbs.
--
-- > defaultVerbStuff "walk"  == ("walks",  "walked")
-- > defaultVerbStuff "push"  == ("pushes", "pushed")
-- > defaultVerbStuff "play"  == ("plays",  "played")
-- > defaultVerbStuff "cry"   == ("cries",  "cried")
defaultVerbStuff :: Text -> (Text, Text)
defaultVerbStuff x
    | hasSibilantSuffix x   = sibilant_o
    | hasCoSuffix x         = sibilant_o
    | hasCySuffix x         = y_final
    | "e" `T.isSuffixOf` x  = e_final
    | otherwise             = plain
  where
    plain      = (x <> "s"         , x <> "ed")
    sibilant_o = (x <> "es"        , x <> "ed")
    e_final    = (x <> "s"         , x <> "d")
    y_final    = (T.init x <> "ies", T.init x <> "ied")

-- | Heuristics for a possesive form for an unknown noun.
--
-- > defaultPossesive "pass"        == "pass'"
-- > defaultPossesive "SOS"         == "SOS'"
-- > defaultPossesive "Mr Blinkin'" == "Mr Blinkin's"
-- > defaultPossesive "cry"         == "cry's"
defaultPossesive :: Text -> Text
defaultPossesive t =
  case T.last t of
    's'  -> t <> "'"
    'S'  -> t <> "'"
    '\'' -> t <> "s"
    _    -> t <> "'s"

-- ---------------------------------------------------------------------
-- ** Determiners
-- ---------------------------------------------------------------------

anNumerals :: [Text]
anNumerals = [ "11", "11th", "18", "18th" ]

-- | > indefiniteDet "dog"  == "a"
--   > indefiniteDet "egg"  == "an"
--   > indefiniteDet "ewe"  == "a"
--   > indefiniteDet "ewok" == "an"
--   > indefiniteDet "8th"  == "an"
indefiniteDet :: Text -> Text
indefiniteDet t = if wantsAn t then "an" else "a"

-- | True if the indefinite determiner for a word would normally be
--   \'an\' as opposed to \'a\'.
wantsAn :: Text -> Bool
wantsAn t_ =
    if startsWithAcronym t_
       then acronymWantsAn t_
       else useAn0 || useAn1
  where
    t      = fst $ T.break isSep $ T.toLower t_
    useAn0 = t `elem` anNumerals
    useAn1 = case T.uncons t of
                Just (h, "") -> isLetterWithInitialVowelSound h
                Just ('8',_) -> True
                Just ('u',_) -> hasVowel_U_Prefix t
                Just (h, _)  -> isVowel h `butNot` hasSemivowelPrefix t
                Nothing      -> False
    x `butNot` y = x && not y
    isSep c = isSpace c || c `elem` ("-" :: String)

-- | Variant of 'wantsAn' that assumes the input string is pronounced
--   one letter at a time.
--
--   > wantsAn        "x-ray" == False
--   > acronymWantsAn "x-ray" == True
--
--   Note that this won't do the right thing for words like \"SCUBA\".
--   You really have to reserve it for those separate-letter acronyms.
acronymWantsAn :: Text -> Bool
acronymWantsAn (T.toLower -> t) =
    useAn0 || useAn1
  where
    useAn0 = t `elem` anNumerals
    useAn1 = case T.uncons t of
                Just ('8',_) -> True
                Just (h,_)   -> isLetterWithInitialVowelSound h
                Nothing      -> False

-- ---------------------------------------------------------------------
-- ** Acronyms
-- ---------------------------------------------------------------------

-- | True if all upper case from second letter and up.
--
--   > looksLikeAcronym "DNA"  == True
--   > looksLikeAcronym "tRNA" == True
--   > looksLikeAcronym "x"    == False
--   > looksLikeAcronym "DnA"  == False
looksLikeAcronym :: Text -> Bool
looksLikeAcronym "" = False
looksLikeAcronym x = T.all isUpper (if T.length x > 1 then T.drop 1 x else x)

-- | True if the first word (separating on either hyphen or space)
--   looks like an acronym.
startsWithAcronym :: Text -> Bool
startsWithAcronym =
    looksLikeAcronym . firstWord
  where
    firstWord = fst . T.break isSep
    isSep c   = isSpace c || c `elem` ("-" :: String)

-- ---------------------------------------------------------------------
-- ** Sounds
-- ---------------------------------------------------------------------

-- | Ends with a \'sh\' sound.
hasSibilantSuffix :: Text -> Bool
hasSibilantSuffix x = any (`T.isSuffixOf` x) ["x","s","ch","sh","z","j"]

-- | Starts with a semivowel.
hasSemivowelPrefix :: Text -> Bool
hasSemivowelPrefix ls = any (`T.isPrefixOf` ls) ["y","w","eu","ewe"]

-- | Starts with a vowel-y \'U\' sound
hasVowel_U_Prefix :: Text -> Bool
hasVowel_U_Prefix t =
    case T.unpack t of
        ['u']       -> False
        ['u',_]     -> True
        ('u':c:v:_) -> not (isConsonant c && isVowel v)
        _           -> False

-- | Last two letters are a consonant and \'y\'.
hasCySuffix :: Text -> Bool
hasCySuffix (T.unpack . T.takeEnd 2 -> [x, 'y']) = isConsonant x
hasCySuffix _ = False

-- | Last two letters are a consonant and \'o\'.
hasCoSuffix :: Text -> Bool
hasCoSuffix (T.unpack . T.takeEnd 2 -> [x, 'o']) = isConsonant x
hasCoSuffix _ = False

-- | Is a vowel.
isVowel :: Char -> Bool
isVowel = (`elem` ("aeiou" :: String)) . toLower

-- | Letters that when pronounced independently in English sound like they
--   begin with vowels.
--
--   > isLetterWithInitialVowelSound 'r' == True
--   > isLetterWithInitialVowelSound 'k' == False
--
--   (In the above, \'r\' is pronounced \"are\", but \'k\' is pronounced
--   \"kay\".)
isLetterWithInitialVowelSound :: Char -> Bool
isLetterWithInitialVowelSound = (`elem` ("aeiofhlmnrsx" :: String)) . toLower

-- | Is a consonant.
--
--   Note that not every `Char` is either a vowel or a consonant.
--   We consider numbers, spaces and symbols to be neither vowel or consonants
isConsonant :: Char -> Bool
isConsonant = (`elem` ("bcdfghjklmnpqrstvwxyz" :: String)) . toLower
