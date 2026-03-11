-- | As the name implies, this module is meant to be used only if you want to
-- get access to the internals, say, if you're unhappy with the provided
-- 'Data.Text.Titlecase.titlecase' function.

module Data.Text.Titlecase.Internal where

import Data.Char ( toLower, toUpper )

-- * Articles

newtype Article = Article { unArticle :: String } deriving (Eq, Show)

isArticle :: String -> Bool
isArticle = isElem unArticle articles

articles :: [Article]
articles = map Article ["a", "an", "the"]

-- * Conjunctions

newtype Conjunction = Conjunction { unConjunction :: String } deriving (Eq, Show)

conjunctions :: [Conjunction]
conjunctions = map Conjunction ["for", "and", "nor", "but", "or", "yet", "so"]

isConjunction :: String -> Bool
isConjunction = isElem unConjunction conjunctions

-- * Prepositions

data Preposition = OneWordPreposition   String
                 | TwoWordPreposition   String String
                 | ThreeWordPreposition String String String
                 | FourWordPreposition  String String String String
                 deriving (Eq, Show)

-- | The words come from
-- <https://en.wikipedia.org/wiki/List_of_English_prepositions Wikipedia>
-- but without <https://en.wikipedia.org/wiki/Conjunction_%28grammar%29#Subordinating_conjunctions subordinating conjunctions>.

prepositions :: [Preposition]
prepositions
   = oneWordPrepositions
  ++ twoWordPrepositions
  ++ threeWordPrepositions
  ++ fourWordPrepositions

unPreposition :: Preposition -> String
unPreposition p = case p of
  OneWordPreposition   a       -> a
  TwoWordPreposition   a b     -> unwords [a, b]
  ThreeWordPreposition a b c   -> unwords [a, b, c]
  FourWordPreposition  a b c d -> unwords [a, b, c, d]

isOneWordPreposition   :: String -> Bool
isOneWordPreposition = isElem unPreposition oneWordPrepositions

isTwoWordPreposition   :: String -> String -> Bool
isTwoWordPreposition a b = isElem unPreposition twoWordPrepositions $ unwords [a, b]

isThreeWordPreposition :: String -> String -> String -> Bool
isThreeWordPreposition a b c = isElem unPreposition threeWordPrepositions $ unwords [a, b, c]

isFourWordPreposition  :: String -> String -> String -> String -> Bool
isFourWordPreposition a b c d = isElem unPreposition fourWordPrepositions $ unwords [a, b, c, d]

oneWordPrepositions :: [Preposition]
oneWordPrepositions = map OneWordPreposition
  [ "a", "abaft", "abeam", "aboard", "about", "above", "absent", "across"
  , "afore", "against", "along", "alongside", "amid", "amidst"
  , "among", "amongst", "an", "anenst", "apropos", "apud", "around"
  , "aside", "astride", "at", "athwart", "atop"
  , "barring", "behind", "below", "beneath", "beside", "besides"
  , "between", "beyond", "but", "by"
  , "chez", "circa", "concerning", "considering"
  , "despite", "down", "during"
  , "except", "excluding", "failing", "following", "for", "forenenst", "from"
  , "given"
  , "in", "including", "inside", "into"
  , "like"
  , "mid", "midst", "minus", "modulo"
  , "near", "next", "notwithstanding"
  , "of", "off", "on", "onto", "opposite", "out", "outside", "over"
  , "pace", "past", "per", "plus", "pro"
  , "qua"
  , "regarding", "round"
  , "sans", "save", "through"
  , "throughout", "till", "times"
  , "to", "toward", "towards", "under", "underneath"
  , "unlike", "unto", "up", "upon", "versus", "vs.", "vs", "v."
  , "via", "vice", "vis-à-vis", "w/", "within", "w/in", "w/i", "without"
  , "w/o", "worth"
  ]

twoWordPrepositions :: [Preposition]
twoWordPrepositions = map (uncurry TwoWordPreposition)
  [ ("according", "to"), ("ahead", "of"), ("apart", "from"), ("as", "for")
  , ("as", "of"), ("as", "per"), ("as", "regards"), ("aside", "from")
  , ("astern", "of")
  , ("back", "to")
  , ("close", "to")
  , ("due", "to")
  , ("except", "for")
  , ("far", "from")
  , ("in", "to"), ("inside", "of"), ("instead", "of")
  , ("left", "of")
  , ("near", "to"), ("next", "to")
  , ("on", "to"), ("opposite", "of"), ("opposite", "to"), ("out", "from")
  , ("out", "of"), ("outside", "of"), ("owing", "to")
  , ("prior", "to"), ("pursuant", "to")
  , ("rather", "than"), ("regardless", "of"), ("right", "of")
  , ("subsequent", "to"), ("such", "as")
  , ("thanks", "to"), ("that", "of")
  , ("up", "to")
  ]

threeWordPrepositions :: [Preposition]
threeWordPrepositions = map (uncurry3 ThreeWordPreposition)
  [ ("as", "opposed", "to")
  , ("as", "well", "as")
  , ("by", "means", "of"), ("by", "virtue", "of")
  , ("in", "accordance", "with"), ("in", "addition", "to")
  , ("in", "case", "of"), ("in", "front", "of"), ("in", "lieu", "of")
  , ("in", "order", "to"), ("in", "place", "of"), ("in", "point", "of")
  , ("in", "spite", "of")
  , ("on", "account", "of"), ("on", "behalf", "of"), ("on", "top", "of")
  , ("with", "regard", "to"), ("with", "respect", "to")
  ]

fourWordPrepositions :: [Preposition]
fourWordPrepositions = map (uncurry4 FourWordPreposition)
  [ ("at", "the", "behest", "of")
  , ("for", "the", "sake", "of")
  , ("with", "a", "view", "to")
  ]

-- * Helper functions

-- | Capitalize the first character of a string.

toTitle :: String -> String
toTitle "" = ""
toTitle (x:xs) = toUpper x : xs

(<#>) :: String -> String -> String
x <#> "" = x
x <#> y  = x ++ " " ++ y

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a,b,c) = f a b c

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a,b,c,d) = f a b c d

isElem :: (a -> String) -> [a] -> String -> Bool
isElem f xs = (`elem` (fmap f xs)) . map toLower
