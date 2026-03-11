module NLP.Tokenize.String
    ( EitherList(..)
    , Tokenizer
    , tokenize
    , run
    , defaultTokenizer
    , whitespace
    , uris
    , punctuation
    , finalPunctuation
    , initialPunctuation
    , allPunctuation
    , contractions
    , negatives
    )
where

import qualified Data.Char as Char
import Data.List
import Data.Maybe
import Control.Applicative
import Data.List.Split
import Control.Monad

-- | A Tokenizer is function which takes a list and returns a list of Eithers
--  (wrapped in a newtype). Right Strings will be passed on for processing
--  to tokenizers down
--  the pipeline. Left Strings will be passed through the pipeline unchanged.
--  Use a Left String in a tokenizer to protect certain tokens from further
--  processing (e.g. see the 'uris' tokenizer).
--  You can define your own custom tokenizer pipelines by chaining tokenizers together:
---
-- > myTokenizer :: Tokenizer
-- > myTokenizer = whitespace >=> allPunctuation
---

type Tokenizer =  String -> EitherList String String

-- | The EitherList is a newtype-wrapped list of Eithers.
newtype EitherList a b =  E { unE :: [Either a b] }

-- | Split string into words using the default tokenizer pipeline
tokenize :: String -> [String]
tokenize  = run defaultTokenizer

-- | Run a tokenizer
run :: Tokenizer -> (String -> [String])
run f = map unwrap . unE . f

defaultTokenizer :: Tokenizer
defaultTokenizer =     whitespace
                   >=> uris
                   >=> punctuation
                   >=> contractions
                   >=> negatives

-- | Detect common uris and freeze them
uris :: Tokenizer
uris x | isUri x = E [Left x]
       | True    = E [Right x]
    where isUri x = any (`isPrefixOf` x) ["http://","ftp://","mailto:"]

-- | Split off initial and final punctuation
punctuation :: Tokenizer
punctuation = finalPunctuation >=> initialPunctuation

-- | Split off word-final punctuation
finalPunctuation :: Tokenizer
finalPunctuation x = E . filter (not . null . unwrap) $
    case span Char.isPunctuation . reverse $ x of
      ([],w) -> [Right . reverse $ w]
      (ps,w) -> [Right . reverse $ w, Right . reverse $ ps]

-- | Split off word-initial punctuation
initialPunctuation :: Tokenizer
initialPunctuation x = E . filter (not . null . unwrap) $
    case span Char.isPunctuation$ x of
      ([],w) -> [Right w]
      (ps,w) -> [Right ps, Right w]

-- | Split tokens on transitions between punctuation and
-- non-punctuation characters. This tokenizer is not included in
-- defaultTokenizer pipeline because dealing with word-internal
-- punctuation is quite application specific.
allPunctuation :: Tokenizer
allPunctuation = E . map Right
                 . groupBy (\a b -> Char.isPunctuation a == Char.isPunctuation b)

-- | Split words ending in n't, and freeze n't
negatives :: Tokenizer
negatives x | "n't" `isSuffixOf` x = E [ Right . reverse . drop 3 . reverse $ x
                                       , Left "n't" ]
            | True                 = E [Right x]

-- | Split common contractions off and freeze them.
-- | Currently deals with: 'm, 's, 'd, 've, 'll
contractions :: Tokenizer
contractions x = case catMaybes . map (splitSuffix x) $ cts of
                   [] -> return x
                   ((w,s):_) -> E [ Right w,Left s]
    where cts = ["'m","'s","'d","'ve","'ll"]
          splitSuffix w sfx =
              let w' = reverse w
                  len = length sfx
              in if sfx `isSuffixOf` w
                 then Just (take (length w - len) w, reverse . take len $ w')
                 else Nothing


-- | Split string on whitespace. This is just a wrapper for Data.List.words
whitespace :: Tokenizer
whitespace xs = E [Right w | w <- words xs ]

instance Monad (EitherList a) where
    E xs >>= f = E $ concatMap (either (return . Left) (unE . f)) xs

instance Applicative (EitherList a) where
    pure x = E [Right x]
    f <*> x = f `ap` x

instance Functor (EitherList a) where
    fmap f (E xs) = E $ (fmap . fmap) f xs

unwrap (Left x) = x
unwrap (Right x) = x

examples =
    ["This shouldn't happen."
    ,"Some 'quoted' stuff"
    ,"This is a URL: http://example.org."
    ,"How about an email@example.com"
    ,"ReferenceError #1065 broke my debugger!"
    ,"I would've gone."
    ,"They've been there."
    ,"Hyphen-words"
    ,"Yes/No questions"
    ]
