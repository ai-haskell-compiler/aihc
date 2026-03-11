{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Module      : Redis.Glob.Internal
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

Provides types that model redis @glob@ patterns and combinators that can be used
to validate and interpret them.

Assumes that @glob@ do __not__ match the non-printable ASCII characters.
-}
module Redis.Glob.Internal (
  -- * modelling @Globs@
  Part (..),
  InSquare (..),

  -- * parse / print valid @Globs@
  parseParts,
  parsePart,
  fromParts,
  fromPart,

  -- * useful combinators
  reduceMany,
  matchParts,
) where

import qualified ASCII.Char as A
import Data.ByteString.Builder (Builder, toLazyByteString, word8)
import Data.ByteString.Lazy (ByteString)
import Data.Functor (($>))
import Data.Maybe (isJust, mapMaybe)
import Data.Void (Void)
import Data.Word (Word8)
import Text.Megaparsec
import qualified Text.Megaparsec.Byte as P


-- | Parse type for parsing @'ByteString'-like@
type Parser s m = (MonadParsec Void s m, Token s ~ Word8)


parseInSquare :: (Parser s m, Token s ~ Word8) => m a -> m a
parseInSquare = between (P.char leftSquare) (P.char rightSquare)


notLeftSquare :: Parser s m => m Word8
notLeftSquare = satisfy (\x -> not $ hasRole x && x < 128)


notRightSquare :: Parser s m => m Word8
notRightSquare = satisfy (\x -> x /= rightSquare && x < 128)


escapedChar :: Parser s m => m Word8
escapedChar = P.char backslash *> P.asciiChar


matchable :: Parser s m => m Word8
matchable = try escapedChar <|> notLeftSquare


parseInRange :: Parser s m => m InSquare
parseInRange = do
  start <- notRightSquare
  _dash <- P.char dash
  InRange start <$> P.asciiChar


parseAnyInSquare :: Parser s m => m InSquare
parseAnyInSquare =
  choice
    [ try $ Single <$> escapedChar
    , try parseInRange
    , Single <$> notRightSquare
    ]


parseSquared :: Parser s m => m Part
parseSquared = parseInSquare $ do
  isNegated <- optional $ P.char hat
  x <- parseAnyInSquare
  xs <- many parseAnyInSquare
  pure $ maybe (Squared x xs) (const $ Negated x xs) isNegated


parseUnescaped :: Parser s m => m Part
parseUnescaped = do
  choice1 <- matchable
  others <- many matchable
  pure $ Unescaped choice1 others


parseAnyPart :: Parser s m => m Part
parseAnyPart =
  choice
    [ P.char star $> Many
    , P.char qmark $> Any
    , parseSquared
    , parseUnescaped
    ]


-- | Parse several @'Part'@ from a glob @pattern@
parseParts :: ByteString -> Maybe [Part]
parseParts = parseMaybe $ many parseAnyPart


-- | Parse a single @'Part'@ from a glob @pattern@
parsePart :: ByteString -> Maybe Part
parsePart = parseMaybe parseAnyPart


-- | Represents part of a valid redis glob pattern.
data Part
  = Any
  | Many
  | GenerousMany
  | Unescaped Word8 [Word8]
  | Squared InSquare [InSquare]
  | Negated InSquare [InSquare]
  deriving (Eq, Show)


-- | Represents part of a valid redis glob pattern.
data InSquare
  = Single Word8
  | InRange Word8 Word8
  deriving (Eq, Show)


-- | Confirm that a @target@ 'ByteString' matches the pattern provided as @['Part']@.
matchParts :: ByteString -> [Part] -> Bool
matchParts target = isJust . flip parseAsMatcher target


parseAsMatcher :: [Part] -> ByteString -> Maybe [Word8]
parseAsMatcher parts = parseMaybe $ matcher parts


matcher :: Parser s m => [Part] -> m [Word8]
matcher = foldr matcherStep (pure mempty) . reduceMany


{- | Normalise parsed @'Part's@

All but a terminating @Many@ are replaced with GenerousMany;
Consecutive @Many@s are replaced by a single GenerousMany
-}
reduceMany :: [Part] -> [Part]
reduceMany =
  let step Many [] = [Many]
      step Many (Many : xs) = GenerousMany : xs
      step Many (GenerousMany : xs) = GenerousMany : xs
      step Many xs = GenerousMany : xs
      step x xs = x : xs
   in foldr step []


matcherStep :: Parser s m => Part -> m [Word8] -> m [Word8]
-- assumes the Part comes from the output of reduceMany; then only the last
-- element will be Many and the accumulator can be replaced in this way
matcherStep Many _ = many P.asciiChar
matcherStep GenerousMany acc = innerStar_ acc
matcherStep (Unescaped x xs) acc = mapM P.char (x : xs) `thenParse` acc
matcherStep (Squared x xs) acc = squaredParser x xs `thenParse'` acc
matcherStep (Negated x xs) acc = negatedParser x xs `thenParse'` acc
matcherStep Any acc = P.asciiChar `thenParse'` acc


thenParse :: Parser s m => m [Word8] -> m [Word8] -> m [Word8]
thenParse x y = foldr (:) <$> x <*> y


thenParse' :: Parser s m => m Word8 -> m [Word8] -> m [Word8]
thenParse' x y = (:) <$> x <*> y


innerStar_ :: (Parser s m, Token s ~ Word8) => m a -> m a
innerStar_ = fmap snd . innerStar


innerStar :: (Parser s m, Token s ~ Word8) => m a -> m ([Word8], a)
innerStar parser = do
  (a, b) <- someTill_ P.asciiChar parser
  pure (a, b)


singleOf :: InSquare -> Maybe Word8
singleOf (Single x) = Just x
singleOf _ = Nothing


rangeOf :: InSquare -> Maybe (Word8, Word8)
rangeOf (InRange x y) | x <= y = Just (x, y)
rangeOf (InRange x y) = Just (y, x)
rangeOf _ = Nothing


boundedBy :: (Bool -> Bool) -> [Word8] -> [(Word8, Word8)] -> Word8 -> Bool
boundedBy orNor ys xs z = orNor $ z `elem` ys || any (\(x, y) -> z >= x && z <= y) xs


squaredParser :: Parser s m => InSquare -> [InSquare] -> m Word8
squaredParser x xs =
  let xs' = x : xs
   in satisfy $ boundedBy id (mapMaybe singleOf xs') (mapMaybe rangeOf xs')


negatedParser :: Parser s m => InSquare -> [InSquare] -> m Word8
negatedParser x xs =
  let xs' = x : xs
   in satisfy $ boundedBy not (mapMaybe singleOf xs') (mapMaybe rangeOf xs')


-- | Convert a @'Part'@ to the form it would be parsed from
fromPart :: Part -> ByteString
fromPart = toLazyByteString . builderOf


-- | Convert several @'Part's@ to the form they can be parsed from.
fromParts :: [Part] -> ByteString
fromParts = toLazyByteString . mconcat . map builderOf


class BuilderOf a where
  builderOf :: a -> Builder


instance BuilderOf InSquare where
  builderOf (Single x) = escaped8 x
  builderOf (InRange x y) = word8 x <> word8 dash <> word8 y


instance BuilderOf Part where
  builderOf Any = word8 qmark
  builderOf Many = word8 star
  builderOf GenerousMany = word8 star
  builderOf (Unescaped x xs) = escaped8 x <> mconcat (map escaped8 xs)
  builderOf (Squared x xs) = inSquare $ builderOf x <> mconcat (map builderOf xs)
  builderOf (Negated x xs) = inSquare $ word8 hat <> builderOf x <> mconcat (map builderOf xs)


escaped8 :: Word8 -> Builder
escaped8 x | hasRole x = escaped x
escaped8 x = word8 x


escaped :: Word8 -> Builder
escaped = (word8 backslash <>) . word8


hasRole :: Word8 -> Bool
hasRole x = x == star || x == backslash || x == leftSquare || x == qmark


inSquare :: Builder -> Builder
inSquare inside = word8 leftSquare <> inside <> word8 rightSquare


hat, qmark, star, leftSquare, rightSquare, dash, backslash :: Word8
hat = A.toWord8 A.Caret
qmark = A.toWord8 A.QuestionMark
star = A.toWord8 A.Asterisk
leftSquare = A.toWord8 A.LeftSquareBracket
rightSquare = A.toWord8 A.RightSquareBracket
dash = A.toWord8 A.HyphenMinus
backslash = A.toWord8 A.Backslash
