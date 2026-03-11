{-# LANGUAGE DeriveGeneric #-}
-- | Simple English clause creation parameterized by individual words.
-- See the tests for example texts generated.
module NLP.Miniutter.English
  ( Part(..), Person(..), Polarity(..), Irregular(..)
  , makeSentence, makePhrase, defIrregular, (<+>)
  ) where

import           Data.Binary
import           Data.Char (isAlphaNum, isSpace, toUpper)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.String (IsString (..))
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics (Generic)
import           NLP.Minimorph.English
import           NLP.Minimorph.Util

#if !(MIN_VERSION_base(4,11,0))
  -- this is redundant starting with base-4.11 / GHC 8.4
import Data.Semigroup
#endif

-- | Various basic and compound parts of English simple present tense clauses.
-- Many of the possible nestings do not make sense. We don't care.
data Part =
    String !String      -- ^ handle for a @String@ parameter
  | Text !Text          -- ^ handle for a @Text@ parameter
  | Cardinal !Int       -- ^ cardinal number, spelled in full up to 10
  | Car !Int            -- ^ cardinal number, not spelled
  | Ws !Part            -- ^ plural form of a phrase
  | CardinalAWs !Int !Part
                        -- ^ plural prefixed with a cardinal, spelled,
                        --   with \"a\" for 1 and \"no\" for 0
  | CardinalWs !Int !Part
                        -- ^ plural prefixed with a cardinal, spelled
  | CarAWs !Int !Part   -- ^ plural prefixed with a cardinal, not spelled,
                        --   with \"a\" for 1 and \"no\" for 0
  | CarWs !Int !Part    -- ^ plural prefixed with a cardinal, not spelled
  | Car1Ws !Int !Part   -- ^ plural prefixed with a cardinal, not spelled,
                        --   with no prefix at all for 1
  | Ordinal !Int        -- ^ ordinal number, spelled in full up to 10
  | Ord !Int            -- ^ ordinal number, not spelled
  | AW !Part            -- ^ phrase with indefinite article
  | WWandW ![Part]      -- ^ enumeration
  | WWxW !Part ![Part]  -- ^ collection
  | Wown !Part          -- ^ non-premodifying possesive
  | WownW !Part !Part   -- ^ attributive possesive
  | Append !Part !Part  -- ^ no space in between; one can also just use @<>@
  | Phrase ![Part]      -- ^ space-separated sequence
  | Capitalize !Part    -- ^ make the first letter into a capital letter
  | SubjectVerb !Person !Polarity !Part !Part
                        -- ^ conjugation according to polarity,
                        --   with a default person (pronouns override it)
  | SubjectVerbSg !Part !Part
                        -- ^ a shorthand for @Sg3rd@ and @Yes@
  | SubjectVVxV !Part !Person !Polarity !Part ![Part]
                        -- ^ conjugation of all verbs according to polarity,
                        --   with a default person (pronouns override it)
  | SubjectVVandVSg !Part ![Part]
                        -- ^ a shorthand for \"and\", @Sg3rd@ and @Yes@
  deriving (Show, Eq, Ord, Generic)

instance Binary Part

instance Read Part where
  readsPrec p str = [(Text x, y) | (x, y) <- readsPrec p str]

instance IsString Part where
  fromString = Text . T.pack

instance Semigroup Part where
  (<>) = Append

instance Monoid Part where
  mempty = Text ""

#if !(MIN_VERSION_base(4,11,0))
  -- this is redundant starting with base-4.11 / GHC 8.4
  mappend = (<>)
#endif

-- | Persons: singular 1st, singular 3rd and the rest.
data Person = Sg1st | Sg3rd | PlEtc
  deriving (Show, Eq, Ord, Generic)

instance Binary Person

-- | Generalized polarity: affirmative, negative, interrogative.
data Polarity = Yes | No | Why
  deriving (Show, Eq, Ord, Generic)

instance Binary Polarity

-- | Nouns with irregular plural form and nouns with irregular indefinite
-- article.
data Irregular = Irregular
  { irrPlural     :: Map Text Text
  , irrIndefinite :: Map Text Text
  }

-- | Default set of words with irregular forms.
defIrregular :: Irregular
defIrregular =
  Irregular {irrPlural = defIrrPlural, irrIndefinite = defIrrIndefinite}

-- | Realise a complete sentence, capitalized, ending with a dot.
makeSentence :: Irregular -> [Part] -> Text
makeSentence irr l = capitalize $ makePhrase irr l `T.snoc` '.'

-- | Realise a phrase. The spacing between parts resembles
-- the semantics of @(\<\+\>)@, that is, it ignores empty words.
makePhrase :: Irregular -> [Part] -> Text
makePhrase irr = T.intercalate (T.singleton ' ') . makeParts irr

makeParts :: Irregular -> [Part] -> [Text]
makeParts irr = filter (not . T.null) . map (makePart irr)

-- The semantics of the operations is compositional.
makePart :: Irregular -> Part -> Text
makePart irr part = case part of
  String t -> T.pack t
  Text t -> t
  Cardinal n -> cardinal n
  Car n -> tshow n
  Ws p -> onLastWord (makePlural irr) (mkPart p)
  CardinalAWs 0 p -> "no" <+> onLastWord (makePlural irr) (mkPart p)
  CardinalAWs 1 p -> mkPart (AW p)
  CardinalAWs n p -> cardinal n <+> onLastWord (makePlural irr) (mkPart p)
  CardinalWs 1 p -> cardinal 1 <+> mkPart p  -- spelled number
  CardinalWs n p -> cardinal n <+> onLastWord (makePlural irr) (mkPart p)
  CarAWs 0 p -> "no" <+> onLastWord (makePlural irr) (mkPart p)
  CarAWs 1 p -> mkPart (AW p)
  CarAWs n p -> tshow n <+> onLastWord (makePlural irr) (mkPart p)
  CarWs 1 p -> "1" <+> mkPart p
  CarWs n p -> tshow n <+> onLastWord (makePlural irr) (mkPart p)
  Car1Ws 1 p -> mkPart p  -- no number, article, anything; useful
  Car1Ws n p -> tshow n <+> onLastWord (makePlural irr) (mkPart p)
  Ordinal n -> ordinal n
  Ord n -> ordinalNotSpelled n
  AW p -> onFirstWord (addIndefinite irr) (mkPart p)
  WWandW lp -> let i = "and" :: Text
                   lt = makeParts irr lp
               in commas i lt
  WWxW x lp -> let i = mkPart x
                   lt = makeParts irr lp
               in commas i lt
  Wown p -> onLastWord nonPremodifying (mkPart p)
  WownW p1 p2 -> onLastWord attributive (mkPart p1) <+> mkPart p2
  Phrase lp -> makePhrase irr lp
  Append p1 p2 -> mkPart p1 <> mkPart p2
  Capitalize p -> capitalize $ mkPart p
  SubjectVerb defaultPerson Yes s v ->
    subjectVerb defaultPerson (mkPart s) (mkPart v)
  SubjectVerb defaultPerson No s v ->
    notSubjectVerb defaultPerson (mkPart s) (mkPart v)
  SubjectVerb defaultPerson Why s v ->
    qSubjectVerb defaultPerson (mkPart s) (mkPart v)
  SubjectVerbSg s v ->
    subjectVerb Sg3rd (mkPart s) (mkPart v)
  SubjectVVxV x defaultPerson Yes s vs ->
    subjectVVxV (mkPart x) defaultPerson (mkPart s) (makeParts irr vs)
  SubjectVVxV x defaultPerson No s vs ->
    notSubjectVVxV (mkPart x) defaultPerson (mkPart s) (makeParts irr vs)
  SubjectVVxV x defaultPerson Why s vs ->
    qSubjectVVxV (mkPart x) defaultPerson (mkPart s) (makeParts irr vs)
  SubjectVVandVSg s vs ->
    subjectVVxV "and" Sg3rd (mkPart s) (makeParts irr vs)
 where
  mkPart = makePart irr

onFirstWord :: (Text -> Text) -> Text -> Text
onFirstWord f t =
  let (starting, restRaw) = T.span isWordLetter t
      rest = T.dropWhile (not . isWordLetter) restRaw
      fstarting = f starting
  in if T.null starting
     then t
     else if T.null fstarting
          then rest
          else f starting <> restRaw

onLastWord :: (Text -> Text) -> Text -> Text
onLastWord f t =
  -- First ignore (and append afterwards) a suffix composed not of possible
  -- word letters and also of any trailing whitespace. We don't want
  -- to pluralise whitespace nor symbols, but neither to lose them.
  let (wordPrefix, nonWordSuffix) =
        let (wordP, nonWordP) = T.span (\c -> isWordLetter c || isSpace c) t
            (wordSpaceR, wordRestR) = T.span isSpace $ T.reverse wordP
        in (T.reverse wordRestR, T.reverse wordSpaceR <> nonWordP)
      (spanPrefix, spanRest) = T.span isWordLetter $ T.reverse wordPrefix
      (ending, restRaw) = (T.reverse spanPrefix, T.reverse spanRest)
      rest = T.dropWhile (not . isWordLetter) restRaw
      fending = f ending
      onLast = if T.null ending
               then wordPrefix
               else if T.null fending
                    then rest
                    else restRaw <> f ending
  in onLast <> nonWordSuffix

onFirstWordPair :: (Text -> (Text, Text)) -> Text -> (Text, Text)
onFirstWordPair f t =
  let (starting, restRaw) = T.span isWordLetter t
      rest = T.dropWhile (not . isWordLetter) restRaw
      (t1, t2) = f starting
  in if T.null starting
     then (t, "")
     else if T.null t2
          then (t1, rest)
          else (t1, t2 <> restRaw)

isWordLetter :: Char -> Bool
isWordLetter c = isAlphaNum c || c == '\'' || c == '-'

capitalize :: Text -> Text
capitalize t = case T.uncons t of
  Nothing        -> T.empty
  Just (c, rest) -> T.cons (toUpper c) rest

makePlural :: Irregular -> Text -> Text
makePlural Irregular{irrPlural} t =
  case Map.lookup t irrPlural of
    Just u  -> u
    Nothing -> defaultNounPlural t

addIndefinite :: Irregular -> Text -> Text
addIndefinite Irregular{irrIndefinite} t =
  case Map.lookup t irrIndefinite of
    Just u  -> u <+> t
    Nothing -> indefiniteDet t <+> t

guessPerson :: Person -> Text -> Person
guessPerson defaultPerson "i" = defaultPerson  -- letter 'i', not person 'I'
guessPerson defaultPerson word =
  case T.toLower word of
    "i"    -> Sg1st
    "he"   -> Sg3rd
    "she"  -> Sg3rd
    "it"   -> Sg3rd
    "we"   -> PlEtc
    "you"  -> PlEtc
    "they" -> PlEtc
    _      -> defaultPerson -- we don't try guessing beyond pronouns

personVerb :: Person -> Text -> Text
personVerb Sg1st "be" = "am"
personVerb PlEtc "be" = "are"
personVerb Sg3rd "be" = "is"
personVerb _ "can"    = "can"
personVerb _ "could"  = "could"
personVerb _ "must"   = "must"
personVerb _ "will"   = "will"
personVerb _ "would"  = "would"
personVerb _ "shall"  = "shall"
personVerb _ "should" = "should"
personVerb _ "ought"  = "ought"
personVerb _ "may"    = "may"
personVerb _ "might"  = "might"
personVerb _ "had"    = "had"
personVerb Sg1st v = v
personVerb PlEtc v = v
personVerb Sg3rd "have" = "has"
personVerb Sg3rd v = fst (defaultVerbStuff v)

subjectVerb :: Person -> Text -> Text -> Text
subjectVerb defaultPerson s v =
  s <+> onFirstWord (personVerb $ guessPerson defaultPerson s) v

subjectVVxV :: Text -> Person -> Text -> [Text] -> Text
subjectVVxV x defaultPerson s vs =
  let conjugate = onFirstWord (personVerb $ guessPerson defaultPerson s)
  in s <+> commas x (map conjugate vs)

notPersonVerb :: Person -> Text -> Text
notPersonVerb Sg1st "be" = "am not"
notPersonVerb PlEtc "be" = "aren't"
notPersonVerb Sg3rd "be" = "isn't"
notPersonVerb _ "can"    = "can't"
notPersonVerb _ "could"  = "couldn't"
notPersonVerb _ "must"   = "mustn't"
notPersonVerb _ "will"   = "won't"
notPersonVerb _ "would"  = "wouldn't"
notPersonVerb _ "shall"  = "shan't"
notPersonVerb _ "should" = "shouldn't"
notPersonVerb _ "ought"  = "oughtn't"
notPersonVerb _ "may"    = "may not"
notPersonVerb _ "might"  = "might not"
notPersonVerb _ "had"    = "hadn't"
notPersonVerb Sg1st v = "don't" <+> v
notPersonVerb PlEtc v = "don't" <+> v
notPersonVerb Sg3rd v = "doesn't" <+> v

notSubjectVerb :: Person -> Text -> Text -> Text
notSubjectVerb defaultPerson s v =
  s <+> onFirstWord (notPersonVerb $ guessPerson defaultPerson s) v

notSubjectVVxV :: Text -> Person -> Text -> [Text] -> Text
notSubjectVVxV _ _ s [] = s
notSubjectVVxV x defaultPerson s (v : vs) =
  let vNot = onFirstWord (notPersonVerb $ guessPerson defaultPerson s) v
  in s <+> commas x (vNot : vs)

qPersonVerb :: Person -> Text -> (Text, Text)
qPersonVerb Sg1st "be" = ("am", "")
qPersonVerb PlEtc "be" = ("are", "")
qPersonVerb Sg3rd "be" = ("is", "")
qPersonVerb _ "can"    = ("can", "")
qPersonVerb _ "could"  = ("could", "")
qPersonVerb _ "must"   = ("must", "")
qPersonVerb _ "will"   = ("will", "")
qPersonVerb _ "would"  = ("would", "")
qPersonVerb _ "shall"  = ("shall", "")
qPersonVerb _ "should" = ("should", "")
qPersonVerb _ "ought"  = ("ought", "")
qPersonVerb _ "may"    = ("may", "")
qPersonVerb _ "might"  = ("might", "")
qPersonVerb _ "had"    = ("had", "")
qPersonVerb Sg1st v = ("do", v)
qPersonVerb PlEtc v = ("do", v)
qPersonVerb Sg3rd v = ("does", v)

qSubjectVerb :: Person -> Text -> Text -> Text
qSubjectVerb defaultPerson s v =
  let (v1, v2) = onFirstWordPair (qPersonVerb $ guessPerson defaultPerson s) v
  in v1 <+> s <+> v2

qSubjectVVxV :: Text -> Person -> Text -> [Text] -> Text
qSubjectVVxV _ _ s [] = s
qSubjectVVxV x defaultPerson s (v : vs) =
  let (v1, v2) = onFirstWordPair (qPersonVerb $ guessPerson defaultPerson s) v
      glue = if T.null v2 then (<>) else (<+>)
  in v1 <+> s `glue` commas x (v2 : vs)

nonPremodifying :: Text -> Text
nonPremodifying "who"  = "whose"
nonPremodifying "Who"  = "Whose"
nonPremodifying "I"    = "mine"
nonPremodifying "you"  = "yours"
nonPremodifying "You"  = "Yours"
nonPremodifying "he"   = "his"
nonPremodifying "He"   = "His"
nonPremodifying "she"  = "her"
nonPremodifying "She"  = "Her"
nonPremodifying "it"   = "its"
nonPremodifying "It"   = "Its"
nonPremodifying "we"   = "ours"
nonPremodifying "We"   = "Ours"
nonPremodifying "they" = "theirs"
nonPremodifying "They" = "Theirs"
nonPremodifying t = defaultPossesive t

attributive :: Text -> Text
attributive "who"  = "whose"
attributive "Who"  = "Whose"
attributive "I"    = "my"
attributive "you"  = "your"
attributive "You"  = "Your"
attributive "he"   = "his"
attributive "He"   = "His"
attributive "she"  = "her"
attributive "She"  = "Her"
attributive "it"   = "its"
attributive "It"   = "Its"
attributive "we"   = "our"
attributive "We"   = "Our"
attributive "they" = "their"
attributive "They" = "Their"
attributive t = defaultPossesive t

-- TODO: use a suffix tree, to catch ableman, seaman, etc.?
-- | Default set of nouns with irregular plural form.
defIrrPlural :: Map Text Text
defIrrPlural = Map.fromList $ concatMap generateCapitalized $
  [ ("bro",         "bros")
  , ("canto",       "cantos")
  , ("homo",        "homos")
  , ("photo",       "photos")
  , ("zero",        "zeros")
  , ("piano",       "pianos")
  , ("portico",     "porticos")
  , ("pro",         "pros")
  , ("quarto",      "quartos")
  , ("kimono",      "kimonos")
  , ("knife",       "knives")
  , ("life",        "lives")
  , ("dwarf",       "dwarfs")  -- not for ME dwarves though
  , ("proof",       "proofs")
  , ("roof",        "roofs")
  , ("turf",        "turfs")
  , ("child",       "children")
  , ("foot",        "feet")
  , ("goose",       "geese")
  , ("louse",       "lice")
  , ("man",         "men")
  , ("mouse",       "mice")
  , ("tooth",       "teeth")
  , ("woman",       "women")
  , ("buffalo",     "buffalo")
  , ("deer",        "deer")
  , ("moose",       "moose")
  , ("sheep",       "sheep")
  , ("bison",       "bison")
  , ("salmon",      "salmon")
  , ("pike",        "pike")
  , ("trout",       "trout")
  , ("swine",       "swine")
  , ("aircraft",    "aircraft")
  , ("watercraft",  "watercraft")
  , ("spacecraft",  "spacecraft")
  , ("hovercraft",  "hovercraft")
  , ("information", "information")
  , ("whiff",       "whiffs")
  , ("graffiti",    "graffiti")
  , ("stomach",     "stomachs")
  ]

generateCapitalized :: (Text, Text) -> [(Text, Text)]
generateCapitalized (t1, t2) =
  let t1C = capitalize t1
      t2C = capitalize t2
  in if t1 == t1C then [(t1, t2)] else [(t1, t2), (t1C, t2C)]

-- | Default set of nouns with irregular indefinite article.
defIrrIndefinite :: Map Text Text
defIrrIndefinite = Map.fromList
  [ ("SCUBA",        "a")
  , ("HEPA",         "a")
  , ("hour",         "an")
  , ("heir",         "an")
  , ("honour",       "an")
  , ("honor",        "an")
  ]
