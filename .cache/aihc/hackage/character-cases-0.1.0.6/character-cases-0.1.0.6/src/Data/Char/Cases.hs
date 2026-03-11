{-# LANGUAGE QuasiQuotes #-}

-- |
-- Exposes subspecies types of Char.
-- e.g. `[a-z]`, `[A-Z]`, and `[0-9]`.
module Data.Char.Cases
  ( AlphaNumChar (..)
  , alphaNumToChar
  , alphaNumChar
  , charToAlphaNum
  , alphaNumCharQ
  , AlphaChar (..)
  , alphaToChar
  , charToAlpha
  , alphaChar
  , alphaCharQ
  , UpperChar (..)
  , upperToChar
  , upperChar
  , charToUpper
  , upperCharQ
  , LowerChar (..)
  , lowerToChar
  , lowerChar
  , charToLower
  , lowerCharQ
  , DigitChar (..)
  , digitToChar
  , digitChar
  , charToDigit
  , digitCharQ
  , SnakeChar (..)
  , snakeToChar
  , snakeChar
  , charToSnake
  , snakeCharQ
  , SnakeHeadChar (..)
  , snakeHeadToChar
  , snakeHeadChar
  , charToSnakeHead
  , snakeHeadCharQ
  , UpperSnakeHeadChar (..)
  , upperSnakeHeadToChar
  , upperSnakeHeadChar
  , charToUpperSnakeHead
  , upperSnakeHeadCharQ
  , UpperSnakeChar (..)
  , upperSnakeToChar
  , upperSnakeChar
  , charToUpperSnake
  , upperSnakeCharQ
  ) where

import Cases.Megaparsec
import Control.Applicative
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

-- $setup
-- >>> :set -XQuasiQuotes

dual :: Ord a => Map k a -> Map a k
dual (Map.toList -> x) =
  Map.fromList $ map swap x


-- | '[A-Za-z0-9]'
data AlphaNumChar = AlphaNumAlpha AlphaChar
                  | AlphaNumDigit DigitChar
  deriving (Show, Eq, Ord)

alphaNumToChar :: AlphaNumChar -> Char
alphaNumToChar (AlphaNumAlpha x) = alphaToChar x
alphaNumToChar (AlphaNumDigit x) = digitToChar x

alphaNumChar :: CodeParsing m => m AlphaNumChar
alphaNumChar =
  AlphaNumAlpha <$> alphaChar <|>
  AlphaNumDigit <$> digitChar

charToAlphaNum :: Char -> Maybe AlphaNumChar
charToAlphaNum x = P.parseMaybe alphaNumChar [x]

-- |
-- Simular to 'alphaCharQ' and 'digitCharQ'.
--
-- >>> [alphaNumCharQ|x|]
-- AlphaNumAlpha (AlphaLower X_)
--
-- >>> [alphaNumCharQ|X|]
-- AlphaNumAlpha (AlphaUpper X)
--
-- >>> [alphaNumCharQ|1|]
-- AlphaNumDigit D1
alphaNumCharQ :: QuasiQuoter
alphaNumCharQ = QuasiQuoter
  { quoteExp  = expQ
  , quotePat  = error "not supported"
  , quoteType = error "not supported"
  , quoteDec  = error "not supported"
  }
  where
    expQ :: String -> Q Exp
    expQ [] = fail "alphaNumCharQ required a Char, but nothign is specified."

    expQ (x : []) = case charToAlphaNum x of
      Nothing -> fail $ show x <> " is not an AlphaNumChar."
      Just (AlphaNumAlpha _) ->
        (ConE (mkName "AlphaNumAlpha") `AppE`) <$> (quoteExp alphaCharQ) [x]
      Just (AlphaNumDigit _) ->
        (ConE (mkName "AlphaNumDigit") `AppE`) <$> (quoteExp digitCharQ) [x]

    expQ xs@(_ : _) = fail $ "alphaNumCharQ required a Char, but a String is specified: " <> xs


-- | '[A-Za-z]'
data AlphaChar = AlphaLower LowerChar
               | AlphaUpper UpperChar
  deriving (Show, Eq, Ord)

alphaToChar :: AlphaChar -> Char
alphaToChar (AlphaLower x) = lowerToChar x
alphaToChar (AlphaUpper x) = upperToChar x

charToAlpha :: Char -> Maybe AlphaChar
charToAlpha x = P.parseMaybe alphaChar [x]

alphaChar :: CodeParsing m => m AlphaChar
alphaChar =
  AlphaLower <$> lowerChar <|>
  AlphaUpper <$> upperChar

-- | Simular to 'lowerCharQ' and 'upperCharQ'.
alphaCharQ :: QuasiQuoter
alphaCharQ = QuasiQuoter
  { quoteExp  = expQ
  , quotePat  = error "not supported"
  , quoteType = error "not supported"
  , quoteDec  = error "not supported"
  }
  where
    expQ :: String -> Q Exp
    expQ [] = fail "alphaCharQ required a Char, but nothign is specified."

    expQ (x : []) = case charToAlpha x of
      Nothing -> fail $ show x <> " is not an AlphaChar."
      Just (AlphaLower _) ->
        (ConE (mkName "AlphaLower") `AppE`) <$> (quoteExp lowerCharQ) [x]
      Just (AlphaUpper _) ->
        (ConE (mkName "AlphaUpper") `AppE`) <$> (quoteExp upperCharQ) [x]

    expQ xs@(_ : _) = fail $ "alphaCharQ required a Char, but a String is specified: " <> xs


-- | '[A-Z]'
data UpperChar = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
  deriving (Show, Eq, Ord)

upperToChar :: UpperChar -> Char
upperToChar = fromJust . flip Map.lookup uppers

uppers :: Map UpperChar Char
uppers = Map.fromList
  [ (A, 'A'), (B, 'B'), (C, 'C')
  , (D, 'D'), (E, 'E'), (F, 'F')
  , (G, 'G'), (H, 'H'), (I, 'I')
  , (J, 'J'), (K, 'K'), (L, 'L')
  , (M, 'M'), (N, 'N'), (O, 'O')
  , (P, 'P'), (Q, 'Q'), (R, 'R')
  , (S, 'S'), (T, 'T'), (U, 'U')
  , (V, 'V'), (W, 'W'), (X, 'X')
  , (Y, 'Y'), (Z, 'Z')
  ]

upperChar :: CodeParsing m => m UpperChar
upperChar = do
  char <- P.upperChar
  let maybeUpper = Map.lookup char $ dual uppers
  case maybeUpper of
    Nothing -> fail "non upper char"
    Just x -> pure x

charToUpper :: Char -> Maybe UpperChar
charToUpper x = P.parseMaybe upperChar [x]

-- |
-- Extracts a Char of [A-Z].
-- Also throws compile error if non [A-Z] is passed.
--
-- >>> [upperCharQ|X|]
-- X
--
-- >>> [upperCharQ|Y|]
-- Y
upperCharQ :: QuasiQuoter
upperCharQ = QuasiQuoter
  { quoteExp  = expQ
  , quotePat  = error "not supported"
  , quoteType = error "not supported"
  , quoteDec  = error "not supported"
  }
  where
    expQ :: String -> Q Exp
    expQ [] = fail "upperCharQ required a Char, but nothign is specified."

    expQ (x : []) = case charToUpper x of
      Nothing -> fail $ show x <> " is not an UpperChar."
      Just z -> conE . mkName $ show z

    expQ xs@(_ : _) = fail $ "upperCharQ required a Char, but a String is specified: " <> xs


-- | '[a-z]'
data LowerChar = A_ | B_ | C_ | D_ | E_ | F_ | G_ | H_ | I_ | J_ | K_ | L_ | M_ | N_ | O_ | P_ | Q_ | R_ | S_ | T_ | U_ | V_ | W_ | X_ | Y_ | Z_
  deriving (Show, Eq, Ord)

lowerToChar :: LowerChar -> Char
lowerToChar = fromJust . flip Map.lookup lowers

lowers :: Map LowerChar Char
lowers = Map.fromList
  [ (A_, 'a'), (B_, 'b'), (C_, 'c')
  , (D_, 'd'), (E_, 'e'), (F_, 'f')
  , (G_, 'g'), (H_, 'h'), (I_, 'i')
  , (J_, 'j'), (K_, 'k'), (L_, 'l')
  , (M_, 'm'), (N_, 'n'), (O_, 'o')
  , (P_, 'p'), (Q_, 'q'), (R_, 'r')
  , (S_, 's'), (T_, 't'), (U_, 'u')
  , (V_, 'v'), (W_, 'w'), (X_, 'x')
  , (Y_, 'y'), (Z_, 'z')
  ]

lowerChar :: CodeParsing m => m LowerChar
lowerChar = do
  char <- P.lowerChar
  let maybeLower = Map.lookup char $ dual lowers
  case maybeLower of
    Nothing -> fail "non lower char"
    Just x -> pure x

charToLower :: Char -> Maybe LowerChar
charToLower x = P.parseMaybe lowerChar [x]

-- |
-- Extracts a Char of [a-z].
-- Also throws compile error if non [a-z] is passed.
--
-- >>> [lowerCharQ|x|]
-- X_
--
-- >>> [lowerCharQ|y|]
-- Y_
lowerCharQ :: QuasiQuoter
lowerCharQ = QuasiQuoter
  { quoteExp  = expQ
  , quotePat  = error "not supported"
  , quoteType = error "not supported"
  , quoteDec  = error "not supported"
  }
  where
    expQ :: String -> Q Exp
    expQ [] = fail "lowerCharQ required a Char, but nothign is specified."

    expQ (x : []) = case charToLower x of
      Nothing -> fail $ show x <> " is not a LowerChar."
      Just z -> conE . mkName $ show z

    expQ xs@(_ : _) = fail $ "lowerCharQ required a Char, but a String is specified: " <> xs


-- | '[0-9]'
data DigitChar = D0
               | D1
               | D2
               | D3
               | D4
               | D5
               | D6
               | D7
               | D8
               | D9
  deriving (Show, Eq, Ord)

digitToChar :: DigitChar -> Char
digitToChar = fromJust . flip Map.lookup digits

digits :: Map DigitChar Char
digits = Map.fromList
  [ (D0, '0')
  , (D1, '1'), (D2, '2'), (D3, '3')
  , (D4, '4'), (D5, '5'), (D6, '6')
  , (D4, '7'), (D8, '8'), (D9, '9')
  ]

digitChar :: CodeParsing m => m DigitChar
digitChar = do
  char <- P.digitChar
  let maybeNum = Map.lookup char $ dual digits
  case maybeNum of
    Nothing -> fail "non numeric char"
    Just x -> pure x

charToDigit :: Char -> Maybe DigitChar
charToDigit x = P.parseMaybe digitChar [x]

-- |
-- Extracts a Char of [0-9].
-- Also throws compile error if non [a-z] is passed.
--
-- >>> [digitCharQ|0|]
-- D0
--
-- >>> [digitCharQ|9|]
-- D9
digitCharQ :: QuasiQuoter
digitCharQ = QuasiQuoter
  { quoteExp  = expQ
  , quotePat  = error "not supported"
  , quoteType = error "not supported"
  , quoteDec  = error "not supported"
  }
  where
    expQ :: String -> Q Exp
    expQ [] = fail "digitCharQ required a Char, but nothign is specified."

    expQ (x : []) = case charToDigit x of
      Nothing -> fail $ show x <> " is not a DigitChar."
      Just z -> conE . mkName $ show z

    expQ xs@(_ : _) = fail $ "digitCharQ required a Char, but a String is specified: " <> xs


-- |
-- '[a-zA-Z0-9_]'
--
-- Please see 'Data.String.Cases.Snake'.
data SnakeChar = SnakeUnderscore -- ^ _
               | SnakeAlphaNum AlphaNumChar -- ^ [a-zA-Z0-9]
  deriving (Show, Eq)

snakeToChar :: SnakeChar -> Char
snakeToChar SnakeUnderscore = '_'
snakeToChar (SnakeAlphaNum x) = alphaNumToChar x

snakeChar :: CodeParsing m => m SnakeChar
snakeChar =
  SnakeUnderscore <$ P.char '_' <|>
  SnakeAlphaNum <$> alphaNumChar

charToSnake :: Char -> Maybe SnakeChar
charToSnake x = P.parseMaybe snakeChar [x]

-- |
-- Extracts a Char of [a-zA-Z0-9_].
-- Also throws compile error if non [a-zA-Z0-9_] is passed.
--
-- >>> [snakeCharQ|x|]
-- SnakeAlphaNum (AlphaNumAlpha (AlphaLower X_))
--
-- >>> [snakeCharQ|X|]
-- SnakeAlphaNum (AlphaNumAlpha (AlphaUpper X))
--
-- >>> [snakeCharQ|_|]
-- SnakeUnderscore
--
-- >>> [snakeCharQ|9|]
-- SnakeAlphaNum (AlphaNumDigit D9)
snakeCharQ :: QuasiQuoter
snakeCharQ = QuasiQuoter
  { quoteExp  = expQ
  , quotePat  = error "not supported"
  , quoteType = error "not supported"
  , quoteDec  = error "not supported"
  }
  where
    expQ :: String -> Q Exp
    expQ [] = fail "snakeCharQ required a Char, but nothign is specified."

    expQ (x : []) = case charToSnake x of
      Nothing -> fail $ show x <> " is not a SnakeChar."
      Just SnakeUnderscore ->
        conE $ mkName "SnakeUnderscore"
      Just (SnakeAlphaNum _) ->
        (ConE (mkName "SnakeAlphaNum") `AppE`) <$> (quoteExp alphaNumCharQ) [x]

    expQ xs@(_ : _) = fail $ "snakeCharQ required a Char, but a String is specified: " <> xs


-- |
-- '[a-zA-Z_]'
--
-- Please see 'Data.String.Cases.Snake'.
data SnakeHeadChar = SnakeHeadUnderscore
                   | SnakeHeadAlpha AlphaChar
  deriving (Show, Eq)

snakeHeadToChar :: SnakeHeadChar -> Char
snakeHeadToChar SnakeHeadUnderscore = '_'
snakeHeadToChar (SnakeHeadAlpha x) = alphaToChar x

snakeHeadChar :: CodeParsing m => m SnakeHeadChar
snakeHeadChar =
  SnakeHeadUnderscore <$ P.char '_' <|>
  SnakeHeadAlpha <$> alphaChar

charToSnakeHead :: Char -> Maybe SnakeHeadChar
charToSnakeHead x = P.parseMaybe snakeHeadChar [x]

-- |
-- Extracts a Char of [a-zA-Z_].
-- Also throws compile error if non [a-zA-Z_] is passed.
--
-- >>> [snakeHeadCharQ|x|]
-- SnakeHeadAlpha (AlphaLower X_)
--
-- >>> [snakeHeadCharQ|X|]
-- SnakeHeadAlpha (AlphaUpper X)
--
-- >>> [snakeHeadCharQ|_|]
-- SnakeHeadUnderscore
snakeHeadCharQ :: QuasiQuoter
snakeHeadCharQ = QuasiQuoter
  { quoteExp  = expQ
  , quotePat  = error "not supported"
  , quoteType = error "not supported"
  , quoteDec  = error "not supported"
  }
  where
    expQ :: String -> Q Exp
    expQ [] = fail "snakeHeadCharQ required a Char, but nothign is specified."

    expQ (x : []) = case charToSnakeHead x of
      Nothing -> fail $ show x <> " is not a SnakeHeadChar."
      Just SnakeHeadUnderscore ->
        conE $ mkName "SnakeHeadUnderscore"
      Just (SnakeHeadAlpha _) ->
        (ConE (mkName "SnakeHeadAlpha") `AppE`) <$> (quoteExp alphaCharQ) [x]

    expQ xs@(_ : _) = fail $ "snakeHeadCharQ required a Char, but a String is specified: " <> xs


-- |
-- '[A-Z_]'
--
-- Please sese 'Data.String.Cases.UpperSnake'.
data UpperSnakeHeadChar = UpperSnakeHeadUnderscore -- ^ _
                        | UpperSnakeHeadUpper UpperChar -- ^ [A-Z]
  deriving (Show, Eq)

upperSnakeHeadToChar :: UpperSnakeHeadChar -> Char
upperSnakeHeadToChar UpperSnakeHeadUnderscore = '_'
upperSnakeHeadToChar (UpperSnakeHeadUpper x) = upperToChar x

upperSnakeHeadChar :: CodeParsing m => m UpperSnakeHeadChar
upperSnakeHeadChar =
  UpperSnakeHeadUnderscore <$ P.char '_' <|>
  UpperSnakeHeadUpper <$> upperChar

charToUpperSnakeHead :: Char -> Maybe UpperSnakeHeadChar
charToUpperSnakeHead x = P.parseMaybe upperSnakeHeadChar [x]

-- |
-- >>> [upperSnakeHeadCharQ|_|]
-- UpperSnakeHeadUnderscore
--
-- >>> [upperSnakeHeadCharQ|A|]
-- UpperSnakeHeadUpper A
upperSnakeHeadCharQ :: QuasiQuoter
upperSnakeHeadCharQ = QuasiQuoter
  { quoteExp  = expQ
  , quotePat  = error "not supported"
  , quoteType = error "not supported"
  , quoteDec  = error "not supported"
  }
  where
    expQ :: String -> Q Exp
    expQ [] = fail "upperSnakeHeadCharQ required a Char, but nothign is specified."

    expQ (x : []) = case charToUpperSnakeHead x of
      Nothing ->
        fail $ show x <> " is not a UpperSnakeHeadChar."
      Just UpperSnakeHeadUnderscore ->
        conE $ mkName "UpperSnakeHeadUnderscore"
      Just (UpperSnakeHeadUpper _) ->
        (ConE (mkName "UpperSnakeHeadUpper") `AppE`) <$> (quoteExp upperCharQ) [x]

    expQ xs@(_ : _) = fail $ "upperSnakeHeadCharQ required a Char, but a String is specified: " <> xs


-- |
-- '[A-Z0-9_]'
--
-- Please see 'Data.String.Cases.UpperSnake'.
data UpperSnakeChar = UpperSnakeUnderscore -- ^ _
                    | UpperSnakeUpper UpperChar -- ^ [A-Z]
                    | UpperSnakeDigit DigitChar  -- ^ [0-9]
  deriving (Show, Eq)

upperSnakeToChar :: UpperSnakeChar -> Char
upperSnakeToChar UpperSnakeUnderscore = '_'
upperSnakeToChar (UpperSnakeUpper x) = upperToChar x
upperSnakeToChar (UpperSnakeDigit x) = digitToChar x

upperSnakeChar :: CodeParsing m => m UpperSnakeChar
upperSnakeChar =
  UpperSnakeUnderscore <$ P.char '_' <|>
  UpperSnakeUpper <$> upperChar <|>
  UpperSnakeDigit <$> digitChar

charToUpperSnake :: Char -> Maybe UpperSnakeChar
charToUpperSnake x = P.parseMaybe upperSnakeChar [x]

-- |
-- >>> [upperSnakeCharQ|_|]
-- UpperSnakeUnderscore
--
-- >>> [upperSnakeCharQ|A|]
-- UpperSnakeUpper A
--
-- >>> [upperSnakeCharQ|0|]
-- UpperSnakeDigit D0
upperSnakeCharQ :: QuasiQuoter
upperSnakeCharQ = QuasiQuoter
  { quoteExp  = expQ
  , quotePat  = error "not supported"
  , quoteType = error "not supported"
  , quoteDec  = error "not supported"
  }
  where
    expQ :: String -> Q Exp
    expQ [] = fail "upperSnakeCharQ required a Char, but nothign is specified."

    expQ (x : []) = case charToUpperSnake x of
      Nothing ->
        fail $ show x <> " is not a UpperSnakeChar."
      Just UpperSnakeUnderscore ->
        conE $ mkName "UpperSnakeUnderscore"
      Just (UpperSnakeUpper _) ->
        (ConE (mkName "UpperSnakeUpper") `AppE`) <$> (quoteExp upperCharQ) [x]
      Just (UpperSnakeDigit _) ->
        (ConE (mkName "UpperSnakeDigit") `AppE`) <$> (quoteExp digitCharQ) [x]

    expQ xs@(_ : _) = fail $ "upperSnakeCharQ required a Char, but a String is specified: " <> xs
