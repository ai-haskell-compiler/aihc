{-# LANGUAGE BangPatterns #-}
module Regex.Internal.List
  (
    list
  , manyList
  , someList
  , manyListMin
  , someListMin

  , charIgnoreCase
  , oneOfChar
  , stringIgnoreCase
  , manyStringOf
  , someStringOf
  , manyStringOfMin
  , someStringOfMin

  , naturalDec
  , integerDec
  , naturalHex
  , integerHex
  , wordRangeDec
  , intRangeDec
  , wordRangeHex
  , intRangeHex
  , wordDecN
  , wordHexN

  , toMatch
  , withMatch

  , reParse
  , parse
  , parseSure

  , find
  , findAll
  , splitOn
  , replace
  , replaceAll
  ) where

import Control.Applicative ((<|>), many, some)
import qualified Control.Applicative as Ap
import Data.Char (ord)
import Data.Maybe (fromMaybe)
import Numeric.Natural (Natural)

import Data.CharSet (CharSet)
import qualified Data.CharSet as CS
import Regex.Internal.Parser (Parser)
import qualified Regex.Internal.Parser as P
import Regex.Internal.Regex (RE(..))
import qualified Regex.Internal.Regex as R
import qualified Regex.Internal.Num as RNum
import qualified Regex.Internal.Generated.CaseFold as CF
import Regex.Internal.Solo (Solo, mkSolo, matchSolo)

------------------------
-- REs and combinators
------------------------

-- | Parse the given list.
list :: Eq c => [c] -> RE c [c]
list xs = xs <$ foldr ((*>) . R.single) (pure ()) xs

-- | Parse any list. Biased towards matching more.
manyList :: RE c [c]
manyList = many R.anySingle

-- | Parse any non-empty list. Biased towards matching more.
someList :: RE c [c]
someList = some R.anySingle

-- | Parse any list. Minimal, i.e. biased towards matching less.
manyListMin :: RE c [c]
manyListMin = R.manyMin R.anySingle

-- | Parse any non-empty @String@. Minimal, i.e. biased towards matching less.
someListMin :: RE c [c]
someListMin = R.someMin R.anySingle

-----------
-- String
-----------

-- | Parse the given @Char@, ignoring case.
--
-- Comparisons are performed after applying
-- [simple case folding](https://www.unicode.org/reports/tr44/#Simple_Case_Folding)
-- as described by the Unicode standard.
charIgnoreCase :: Char -> RE Char Char
charIgnoreCase c = R.satisfy $ (c'==) . CF.caseFoldSimple
  where
    !c' = CF.caseFoldSimple c
-- See Note [Why simple case fold] in Regex.Internal.Text

-- | Parse a @Char@ if it is a member of the @CharSet@.
oneOfChar :: CharSet -> RE Char Char
oneOfChar !cs = R.satisfy (`CS.member` cs)

-- | Parse the given @String@, ignoring case.
--
-- Comparisons are performed after applying
-- [simple case folding](https://www.unicode.org/reports/tr44/#Simple_Case_Folding)
-- as described by the Unicode standard.
stringIgnoreCase :: String -> RE Char String
stringIgnoreCase = foldr (R.liftA2' (:) . charIgnoreCase) (pure [])
-- See Note [Why simple case fold] in Regex.Internal.Text

-- | Parse any @String@ containing members of the @CharSet@.
-- Biased towards matching more.
manyStringOf :: CharSet -> RE Char String
manyStringOf !cs = many (R.satisfy (`CS.member` cs))

-- | Parse any non-empty @String@ containing members of the @CharSet@.
-- Biased towards matching more.
someStringOf :: CharSet -> RE Char String
someStringOf !cs = some (R.satisfy (`CS.member` cs))

-- | Parse any @String@ containing members of the @CharSet@.
-- Minimal, i.e. biased towards matching less.
manyStringOfMin :: CharSet -> RE Char String
manyStringOfMin !cs = R.manyMin (R.satisfy (`CS.member` cs))

-- | Parse any non-empty @String@ containing members of the @CharSet@.
-- Minimal, i.e. biased towards matching less.
someStringOfMin :: CharSet -> RE Char String
someStringOfMin !cs = R.someMin (R.satisfy (`CS.member` cs))

-----------------
-- Numeric REs
-----------------

-- | Parse a decimal @Natural@.
-- Leading zeros are not accepted. Biased towards matching more.
naturalDec :: RE Char Natural
naturalDec = RNum.mkNaturalDec digitRange

-- | Parse a decimal @Integer@. Parse an optional sign, @\'-\'@ or @\'+\'@,
-- followed by the given @RE@, followed by the absolute value of the integer.
-- Leading zeros are not accepted. Biased towards matching more.
integerDec :: RE Char a -> RE Char Integer
integerDec sep = RNum.mkSignedInteger minus plus (sep *> naturalDec)

-- | Parse a hexadecimal @Natural@. Both uppercase @\'A\'..\'F\'@ and lowercase
-- @\'a\'..\'f\'@ are accepted.
-- Leading zeros are not accepted. Biased towards matching more.
naturalHex :: RE Char Natural
naturalHex = RNum.mkNaturalHex hexDigitRange

-- | Parse a hexadecimal @Integer@. Parse an optional sign, @\'-\'@ or @\'+\'@,
-- followed by the given @RE@, followed by the absolute value of the integer.
-- Both uppercase @\'A\'..\'F\'@ and lowercase @\'a\'..\'f\'@ are accepted.
-- Leading zeros are not accepted. Biased towards matching more.
integerHex :: RE Char a -> RE Char Integer
integerHex sep = RNum.mkSignedInteger minus plus (sep *> naturalHex)

-- | Parse a decimal @Word@ in the range @[low..high]@.
-- Leading zeros are not accepted. Biased towards matching more.
wordRangeDec :: (Word, Word) -> RE Char Word
wordRangeDec lh = RNum.mkWordRangeDec digitRange lh

-- | Parse a decimal @Int@ in the range @[low..high]@. Parse an optional sign,
-- @\'-\'@ or @\'+\'@, followed by the given @RE@, followed by the absolute
-- value of the integer.
-- Leading zeros are not accepted. Biased towards matching more.
intRangeDec :: RE Char a -> (Int, Int) -> RE Char Int
intRangeDec sep lh =
  RNum.mkSignedIntRange minus plus ((sep *>) . wordRangeDec) lh

-- | Parse a hexadecimal @Word@ in the range @[low..high]@. Both uppercase
-- @\'A\'..\'F\'@ and lowercase @\'a\'..\'f\'@ are accepted.
-- Leading zeros are not accepted. Biased towards matching more.
wordRangeHex :: (Word, Word) -> RE Char Word
wordRangeHex lh = RNum.mkWordRangeHex hexDigitRange lh

-- | Parse a hexadecimal @Int@ in the range @[low..high]@. Parse an optional
-- sign, @\'-\'@ or @\'+\'@, followed by the given @RE@, followed by the
-- absolute value of the integer.
-- Both uppercase @\'A\'..\'F\'@ and lowercase @\'a\'..\'f\'@ are accepted.
-- Leading zeros are not accepted. Biased towards matching more.
intRangeHex :: RE Char a -> (Int, Int) -> RE Char Int
intRangeHex sep lh =
  RNum.mkSignedIntRange minus plus ((sep *>) . wordRangeHex) lh

-- | Parse a @Word@ of exactly n decimal digits, including any leading zeros.
-- Will not parse values that do not fit in a @Word@.
-- Biased towards matching more.
wordDecN :: Int -> RE Char Word
wordDecN n = RNum.mkWordDecN digitRange n

-- | Parse a @Word@ of exactly n hexadecimal digits, including any leading
-- zeros. Both uppercase @\'A\'..\'F\'@ and lowercase @\'a\'..\'f\'@ are
-- accepted. Will not parse values that do not fit in a @Word@.
-- Biased towards matching more.
wordHexN :: Int -> RE Char Word
wordHexN n = RNum.mkWordHexN hexDigitRange n

minus, plus :: RE Char ()
minus = R.token $ \c -> if c == '-' then Just () else Nothing
plus = R.token $ \c -> if c == '+' then Just () else Nothing

-- l and h must be in [0..9]
digitRange :: Word -> Word -> RE Char Word
digitRange !l !h = R.token $ \c ->
  let d = fromIntegral (ord c - ord '0')
  in if l <= d && d <= h then Just d else Nothing

-- l and h must be in [0..15]
hexDigitRange :: Word -> Word -> RE Char Word
hexDigitRange !l !h = R.token $ \c ->
  let dec = fromIntegral (ord c - ord '0')
      hexl = fromIntegral (ord c - ord 'a')
      hexu = fromIntegral (ord c - ord 'A')
  in do
    d <- case () of
      _ | dec <= 9 -> Just dec
        | hexl <= 5 -> Just $! 10 + hexl
        | hexu <= 5 -> Just $! 10 + hexu
        | otherwise -> Nothing
    if l <= d && d <= h then Just d else Nothing

----------------
-- Match stuff
----------------

-- | Rebuild the @RE@ such that the result is the matched section of the list
-- instead.
toMatch :: RE c a -> RE c [c]
toMatch = fmap dToL . toMatch_

toMatch_ :: RE c b -> RE c (DList c)
toMatch_ re = case re of
  RToken t -> R.token (\c -> singletonD c <$ t c)
  RFmap _ re1 -> toMatch_ re1
  RFmap_ _ re1 -> toMatch_ re1
  RPure _ -> pure mempty
  RLiftA2 _ re1 re2 -> R.liftA2' (<>) (toMatch_ re1) (toMatch_ re2)
  REmpty -> Ap.empty
  RAlt re1 re2 -> toMatch_ re1 <|> toMatch_ re2
  RFoldGr _ _ re1 -> R.foldlMany' (<>) mempty (toMatch_ re1)
  RFoldMn _ _ re1 -> R.foldlManyMin' (<>) mempty (toMatch_ re1)
  RMany _ _ _ _ re1 -> R.foldlMany' (<>) mempty (toMatch_ re1)

data WithMatch c a = WM !(DList c) a

fmapWM :: (a -> Solo b) -> WithMatch c a -> WithMatch c b
fmapWM f (WM t x) = matchSolo (f x) (WM t)

pureWM :: a -> WithMatch c a
pureWM = WM mempty

liftA2WM
  :: (a1 -> a2 -> Solo b) -> WithMatch c a1 -> WithMatch c a2 -> WithMatch c b
liftA2WM f (WM t1 x) (WM t2 y) = matchSolo (f x y) (WM (t1 <> t2))

-- | Rebuild the @RE@ to include the matched section of the list alongside the
-- result.
withMatch :: RE c a -> RE c ([c], a)
withMatch = R.fmap' (\(WM cs x) -> (dToL cs, x)) . go
  where
    go :: RE c b -> RE c (WithMatch c b)
    go re = case re of
      RToken t -> R.token (\c -> WM (singletonD c) <$> t c)
      RFmap f re1 -> R.fmap' (fmapWM f) (go re1)
      RFmap_ b re1 -> R.fmap' (flip WM b) (toMatch_ re1)
      RPure b -> pure (pureWM b)
      RLiftA2 f re1 re2 -> R.liftA2' (liftA2WM f) (go re1) (go re2)
      REmpty -> Ap.empty
      RAlt re1 re2 -> go re1 <|> go re2
      RFoldGr f z re1 -> R.foldlMany' (liftA2WM f) (pureWM z) (go re1)
      RFoldMn f z re1 -> R.foldlManyMin' (liftA2WM f) (pureWM z) (go re1)
      RMany f1 f2 f z re1 ->
        RMany
          (\x -> mkSolo $! fmapWM f1 x)
          (\x -> mkSolo $! fmapWM f2 x)
          (\x y -> mkSolo $! liftA2WM f x y)
          (pureWM z)
          (go re1)

----------
-- Parse
----------

-- | \(O(mn \log m)\). Parse a list with a @RE@.
--
-- Parses the entire list, not just a prefix or a substring.
-- Returns early without demanding the entire list on parse failure.
--
-- Uses 'Regex.List.compile', see the note there.
--
-- If parsing multiple lists using the same @RE@, it is wasteful to compile
-- the @RE@ every time. So, prefer to
--
-- * Compile once with 'Regex.List.compile' or 'Regex.List.compileBounded' and
--   use the compiled 'Parser'  with 'parse' as many times as required.
-- * Alternately, partially apply this function to a @RE@ and use the function
--   as many times as required.
reParse :: RE c a -> [c] -> Maybe a
reParse re = let !p = P.compile re in parse p
{-# INLINE reParse #-}

-- | \(O(mn \log m)\). Parse a list with a @Parser@.
--
-- Parses the entire list, not just a prefix or a substring.
-- Returns early without demanding the entire list on parse failure.
parse :: Parser c a -> [c] -> Maybe a
parse = P.parseFoldr foldr
{-# INLINE parse #-}

-- | \(O(mn \log m)\). Parse a list with a @Parser@. Calls 'error' on
-- parse failure.
--
-- For use with parsers that are known to never fail.
--
-- Parses the entire list, not just a prefix or a substring.
-- Returns early without demanding the entire list on parse failure.
parseSure :: Parser c a -> [c] -> a
parseSure p = fromMaybe parseSureError . parse p
{-# INLINE parseSure #-}

parseSureError :: a
parseSureError = error
  "Regex.List.parseSure: parse failed; if parsing can fail use 'parse' instead"

reParseSure :: RE c a -> [c] -> a
reParseSure re = let !p = P.compile re in parseSure p
{-# INLINE reParseSure #-}

-- | \(O(mn \log m)\). Find the first occurence of the given @RE@ in a list.
--
-- ==== __Examples__
--
-- >>> find (list "meow") "homeowner"
-- Just "meow"
--
-- To test whether a list is present in another list, like above, prefer
-- @Data.List.'Data.List.isInfixOf'@.
--
-- >>> find (stringIgnoreCase "haskell") "Look I'm Haskelling!"
-- Just "Haskell"
-- >>> find (list "backtracking") "parser-regex"
-- Nothing
--
find :: RE c a -> [c] -> Maybe a
find = reParse . R.toFind
{-# INLINE find #-}

-- | \(O(mn \log m)\). Find all non-overlapping occurences of the given @RE@ in
-- the list.
--
-- ==== __Examples__
--
-- >>> findAll (list "ana") "banananana"
-- ["ana","ana"]
--
-- @
-- data Roll = Roll
--   Natural -- ^ Rolls
--   Natural -- ^ Faces on the die
--   deriving Show
--
-- roll :: RE Char Roll
-- roll = Roll \<$> ('naturalDec' \<|> pure 1) \<* 'R.single' \'d\' \<*> naturalDec
-- @
--
-- >>> findAll roll "3d6, d10, 2d10"
-- [Roll 3 6,Roll 1 10,Roll 2 10]
--
findAll :: RE c a -> [c] -> [a]
findAll = reParseSure . R.toFindMany
{-# INLINE findAll #-}

-- | \(O(mn \log m)\). Split a list at occurences of the given @RE@.
--
-- ==== __Examples__
--
-- >>> splitOn (single ' ') "Glasses are really versatile"
-- ["Glasses","are","really","versatile"]
--
-- In cases like above, prefer using 'words' or 'lines' instead, if
-- applicable.
--
-- >>> splitOn (single ' ' *> oneOfChar "+-=" *> single ' ') "3 - 1 + 1/2 - 2 = 0"
-- ["3","1","1/2","2","0"]
--
-- If the list starts or ends with a delimiter, the result will contain
-- empty lists at those positions.
--
-- >>> splitOn (single 'a') "ayaya"
-- ["","y","y",""]
--
splitOn :: RE c a -> [c] -> [[c]]
splitOn = reParseSure . toSplitOn
{-# INLINE splitOn #-}

toSplitOn :: RE c a -> RE c [[c]]
toSplitOn re = manyListMin `R.sepBy` re

-- | \(O(mn \log m)\). Replace the first match of the given @RE@ with its
-- result. If there is no match, the result is @Nothing@.
--
-- ==== __Examples__
--
-- >>> replace ("world" <$ list "Haskell") "Hello, Haskell!"
-- Just "Hello, world!"
--
-- >>> replace ("," <$ some (single '.')) "one...two...ten"
-- Just "one,two...ten"
--
replace :: RE c [c] -> [c] -> Maybe [c]
replace = reParse . toReplace
{-# INLINE replace #-}

toReplace :: RE c [c] -> RE c [c]
toReplace re = Ap.liftA2 f manyListMin re <*> manyList
  where
    f a b c = concat [a,b,c]

-- | \(O(mn \log m)\). Replace all non-overlapping matches of the given @RE@
-- with their results.
--
-- ==== __Examples__
--
-- >>> replaceAll (" and " <$ list ", ") "red, blue, green"
-- "red and blue and green"
--
-- >>> replaceAll ("Fruit" <$ list "Time" <|> "a banana" <$ list "an arrow") "Time flies like an arrow"
-- "Fruit flies like a banana"
--
-- @
-- sep = 'oneOfChar' "-./"
-- digits n = 'Control.Monad.replicateM' n (oneOfChar 'Data.CharSet.digit')
-- toYmd d m y = concat [y, \"-\", m, \"-\", d]
-- date = toYmd \<$> digits 2 \<* sep
--              \<*> digits 2 \<* sep
--              \<*> digits 4
-- @
-- >>> replaceAll date "01/01/1970, 01-04-1990, 03.07.2011"
-- "1970-01-01, 1990-04-01, 2011-07-03"
--
replaceAll :: RE c [c] -> [c] -> [c]
replaceAll = reParseSure . toReplaceMany
{-# INLINE replaceAll #-}

toReplaceMany :: RE c [c] -> RE c [c]
toReplaceMany re = concat <$> many (re <|> R.token (Just . (:[])))

---------------------
-- Difference lists
---------------------

newtype DList a = DList { unDList :: [a] -> [a] }

instance Semigroup (DList a) where
  xs <> ys = DList (unDList xs . unDList ys)

instance Monoid (DList a) where
  mempty = DList id

singletonD :: a -> DList a
singletonD = DList . (:)

dToL :: DList a -> [a]
dToL = ($ []) . unDList

----------
-- Notes
----------

-- Note [Token for Regex.List]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Regex.Text uses a token TextToken, but Regex.List doesn't, why?
--
-- TextToken is used for efficient slicing, but here a DList is used for that
-- purpose. This has the effect that combinators like manyText and friends
-- don't need to allocate a linear amount of memory, since slicing is free, but
-- manyList and friends do. We could use a token type for list like
--
-- data Take a = Take !Int ![a]
--
-- to refer to the input list and save memory.
--
-- This is not done because
-- * It increases complexity. Currently this module offers the simplest possible
--   application of RE, which is nice to have.
-- * If the list does not already exist in memory, Take would keep the entire
--   list alive in memory instead of the just the slice it needs.
-- * The current implementation is a good consumer, which can fuse with a good
--   producer of the input list.
--
-- In the end it is about the two distinct use cases of lists in Haskell:
-- * As a structure in memory, the Take token would be the better choice
-- * As a stream of elements, the current implementation is the better choice
