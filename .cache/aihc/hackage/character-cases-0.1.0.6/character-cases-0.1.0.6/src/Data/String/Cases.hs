{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Exposes naming cases.
module Data.String.Cases where

import Cases.Megaparsec
import Data.Char.Cases
import qualified Data.String as String
import Data.Text.Prettyprint.Doc (Pretty(..))
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import qualified Text.Megaparsec as P

-- $setup
-- >>> :set -XQuasiQuotes

-- | Non empty PascalCase names "[A-Z][a-zA-Z0-9]*"
data Pascal = Pascal UpperChar [AlphaNumChar]
  deriving (Show, Eq)

instance Pretty Pascal where
  pretty = String.fromString . unPascal

unPascal :: Pascal -> String
unPascal (Pascal x xs) =
  upperToChar x : map alphaNumToChar xs

parsePascal :: CodeParsing m => m Pascal
parsePascal =
  Pascal <$> upperChar <*> P.many alphaNumChar

-- |
-- Simular to 'nonEmptyQ',
-- but naming outsides of 'Pascal' will be rejected.
--
-- >>> [pascalQ|Pascal|]
-- Pascal P [AlphaNumAlpha (AlphaLower A_),AlphaNumAlpha (AlphaLower S_),AlphaNumAlpha (AlphaLower C_),AlphaNumAlpha (AlphaLower A_),AlphaNumAlpha (AlphaLower L_)]
pascalQ :: QuasiQuoter
pascalQ = QuasiQuoter
  { quoteExp  = expQ
  , quotePat  = error "not supported"
  , quoteType = error "not supported"
  , quoteDec  = error "not supported"
  }
  where
    expQ :: String -> Q Exp
    expQ [] = fail "pascalQ required a non empty string, but the empty string is specified."
    expQ (x : xs) = do
      z <- (quoteExp upperCharQ) [x]
      zs <- mapM (quoteExp alphaNumCharQ) $ map (: []) xs
      pure $ ConE (mkName "Pascal") `AppE` z `AppE` ListE zs


-- | Non empty names ".+"
data NonEmpty = NonEmpty Char String
  deriving (Show, Eq)

instance Pretty NonEmpty where
  pretty = String.fromString . unNonEmpty

unNonEmpty :: NonEmpty -> String
unNonEmpty (NonEmpty x xs) = x : xs

parseNonEmpty :: CodeParsing m => m NonEmpty
parseNonEmpty =
  NonEmpty <$> P.anySingle <*> P.many P.anySingle

fromString :: String -> Maybe NonEmpty
fromString "" = Nothing
fromString (x : xs) = Just $ NonEmpty x xs

-- |
-- Makes a non empty string from String on the compile time.
-- Also throws compile error if the empty string is passed.
--
-- >>> [nonEmptyQ|x|]
-- NonEmpty 'x' ""
--
-- >>> [nonEmptyQ|foo|]
-- NonEmpty 'f' "oo"
--
-- >>> [nonEmptyQ|Bar|]
-- NonEmpty 'B' "ar"
nonEmptyQ :: QuasiQuoter
nonEmptyQ = QuasiQuoter
  { quoteExp  = expQ
  , quotePat  = error "not supported"
  , quoteType = error "not supported"
  , quoteDec  = error "not supported"
  }
  where
    expQ :: String -> Q Exp
    expQ [] = fail "nonEmptyQ required a non empty string, but the empty string is specified."
    expQ (x : xs) =
      pure $ ConE (mkName "NonEmpty")
        `AppE` LitE (CharL x)
        `AppE` ListE (map (LitE . CharL) xs)


-- | Non empty camelCase names "[a-zA-Z][a-zA-Z0-9]*"
data Camel = Camel AlphaChar [AlphaNumChar]
  deriving (Show, Eq)

instance Pretty Camel where
  pretty = String.fromString . unCamel

unCamel :: Camel -> String
unCamel (Camel x xs) = alphaToChar x : map alphaNumToChar xs

parseCamel :: CodeParsing m => m Camel
parseCamel =
  Camel <$> alphaChar <*> P.many alphaNumChar

-- |
-- Simular to 'nonEmptyQ',
-- but naming outsides of 'Camel' will be rejected.
--
-- >>> [camelQ|camel|]
-- "camel"
--
-- >>> [camelQ|Pascal|]
-- "Pascal"
camelQ :: QuasiQuoter
camelQ = QuasiQuoter
  { quoteExp  = expQ
  , quotePat  = error "not supported"
  , quoteType = error "not supported"
  , quoteDec  = error "not supported"
  }
  where
    expQ :: String -> Q Exp
    expQ [] = fail "camelQ required a non empty string, but the empty string is specified."
    expQ (x : xs) = do
      z <- (quoteExp alphaCharQ) [x]
      zs <- mapM (quoteExp alphaNumCharQ) $ map (: []) xs
      pure $ ConE (mkName "Camel") `AppE` z `AppE` ListE zs


-- | Non empty snake_case names "[a-zA-Z_][a-zA-Z0-9_]*"
data Snake = Snake SnakeHeadChar [SnakeChar]
  deriving (Show, Eq)

instance Pretty Snake where
  pretty = String.fromString . unSnake

unSnake :: Snake -> String
unSnake (Snake x xs) =
  snakeHeadToChar x : map snakeToChar xs

parseSnake :: CodeParsing m => m Snake
parseSnake =
  Snake <$>
  snakeHeadChar <*>
  P.many snakeChar

-- |
-- Simular to 'nonEmptyQ',
-- but naming outsides of 'Data.String.Cases.Snake' will be rejected.
--
-- >>> [snakeQ|foo_bar|]
-- Snake (SnakeHeadAlpha (AlphaLower F_)) [SnakeAlphaNum (AlphaNumAlpha (AlphaLower O_)),SnakeAlphaNum (AlphaNumAlpha (AlphaLower O_)),SnakeUnderscore,SnakeAlphaNum (AlphaNumAlpha (AlphaLower B_)),SnakeAlphaNum (AlphaNumAlpha (AlphaLower A_)),SnakeAlphaNum (AlphaNumAlpha (AlphaLower R_))]
--
-- >>> [snakeQ|__constructor|]
-- Snake SnakeHeadUnderscore [SnakeUnderscore,SnakeAlphaNum (AlphaNumAlpha (AlphaLower C_)),SnakeAlphaNum (AlphaNumAlpha (AlphaLower O_)),SnakeAlphaNum (AlphaNumAlpha (AlphaLower N_)),SnakeAlphaNum (AlphaNumAlpha (AlphaLower S_)),SnakeAlphaNum (AlphaNumAlpha (AlphaLower T_)),SnakeAlphaNum (AlphaNumAlpha (AlphaLower R_)),SnakeAlphaNum (AlphaNumAlpha (AlphaLower U_)),SnakeAlphaNum (AlphaNumAlpha (AlphaLower C_)),SnakeAlphaNum (AlphaNumAlpha (AlphaLower T_)),SnakeAlphaNum (AlphaNumAlpha (AlphaLower O_)),SnakeAlphaNum (AlphaNumAlpha (AlphaLower R_))]
--
-- >>> [snakeQ|FOO_MEE_9|]
-- Snake (SnakeHeadAlpha (AlphaUpper F)) [SnakeAlphaNum (AlphaNumAlpha (AlphaUpper O)),SnakeAlphaNum (AlphaNumAlpha (AlphaUpper O)),SnakeUnderscore,SnakeAlphaNum (AlphaNumAlpha (AlphaUpper M)),SnakeAlphaNum (AlphaNumAlpha (AlphaUpper E)),SnakeAlphaNum (AlphaNumAlpha (AlphaUpper E)),SnakeUnderscore,SnakeAlphaNum (AlphaNumDigit D9)]
snakeQ :: QuasiQuoter
snakeQ = QuasiQuoter
  { quoteExp  = expQ
  , quotePat  = error "not supported"
  , quoteType = error "not supported"
  , quoteDec  = error "not supported"
  }
  where
    expQ :: String -> Q Exp
    expQ [] = fail "snakeQ required a non empty string, but the empty string is specified."
    expQ (x : xs) = do
      z <- (quoteExp snakeHeadCharQ) [x]
      zs <- mapM (quoteExp snakeCharQ) $ map (: []) xs
      pure $ ConE (mkName "Snake") `AppE` z `AppE` ListE zs


-- | A kind of 'Data.String.Cases'. @[A-Z_][A-Z0-9_]*@
data UpperSnake = UpperSnake UpperSnakeHeadChar [UpperSnakeChar]
  deriving (Show, Eq)

instance Pretty UpperSnake where
  pretty = String.fromString . unUpperSnake

unUpperSnake :: UpperSnake -> String
unUpperSnake (UpperSnake x xs) =
  upperSnakeHeadToChar x : map upperSnakeToChar xs

parseUpperSnake :: CodeParsing m => m UpperSnake
parseUpperSnake =
  UpperSnake <$>
  upperSnakeHeadChar <*>
  P.many upperSnakeChar

-- |
-- >>> [upperSnakeQ|FOO_BAR|]
-- UpperSnake (UpperSnakeHeadUpper F) [UpperSnakeUpper O,UpperSnakeUpper O,UpperSnakeUnderscore,UpperSnakeUpper B,UpperSnakeUpper A,UpperSnakeUpper R]
--
-- >>> [upperSnakeQ|__CONSTRUCTOR|]
-- UpperSnake UpperSnakeHeadUnderscore [UpperSnakeUnderscore,UpperSnakeUpper C,UpperSnakeUpper O,UpperSnakeUpper N,UpperSnakeUpper S,UpperSnakeUpper T,UpperSnakeUpper R,UpperSnakeUpper U,UpperSnakeUpper C,UpperSnakeUpper T,UpperSnakeUpper O,UpperSnakeUpper R]
--
-- >>> [upperSnakeQ|__FOO_MEE_9|]
-- UpperSnake UpperSnakeHeadUnderscore [UpperSnakeUnderscore,UpperSnakeUpper F,UpperSnakeUpper O,UpperSnakeUpper O,UpperSnakeUnderscore,UpperSnakeUpper M,UpperSnakeUpper E,UpperSnakeUpper E,UpperSnakeUnderscore,UpperSnakeDigit D9]
--
-- >>> [upperSnakeQ|FOO_MEE_9|]
-- UpperSnake (UpperSnakeHeadUpper F) [UpperSnakeUpper O,UpperSnakeUpper O,UpperSnakeUnderscore,UpperSnakeUpper M,UpperSnakeUpper E,UpperSnakeUpper E,UpperSnakeUnderscore,UpperSnakeDigit D9]
upperSnakeQ :: QuasiQuoter
upperSnakeQ = QuasiQuoter
  { quoteExp  = expQ
  , quotePat  = error "not supported"
  , quoteType = error "not supported"
  , quoteDec  = error "not supported"
  }
  where
    expQ :: String -> Q Exp
    expQ [] = fail "upperSnakeQ required a non empty string, but the empty string is specified."
    expQ (x : xs) = do
      z <- (quoteExp upperSnakeHeadCharQ) [x]
      zs <- mapM (quoteExp upperSnakeCharQ) $ map (: []) xs
      pure $ ConE (mkName "UpperSnake") `AppE` z `AppE` ListE zs


-- | Non empty "veryflatten" names [a-z]+
data LowerString = LowerString LowerChar [LowerChar]
  deriving (Show, Eq)

instance Pretty LowerString where
  pretty (LowerString x xs) = String.fromString $ map lowerToChar (x : xs)

unLowerString :: LowerString -> String
unLowerString (LowerString x xs) = lowerToChar x : map lowerToChar xs

parseLowerString :: CodeParsing m => m LowerString
parseLowerString = LowerString <$> lowerChar <*> P.many lowerChar

-- |
-- Simular to 'nonEmptyQ',
-- but naming outsides of 'LowerString' will be rejected.
--
-- >>> [lowerStringQ|imavimmer|]
-- LowerString I_ [M_,A_,V_,I_,M_,M_,E_,R_]
lowerStringQ :: QuasiQuoter
lowerStringQ = QuasiQuoter
  { quoteExp  = expQ
  , quotePat  = error "not supported"
  , quoteType = error "not supported"
  , quoteDec  = error "not supported"
  }
  where
    expQ :: String -> Q Exp
    expQ [] = fail "lowerStringQ required a non empty string, but the empty string is specified."
    expQ (x : xs) = do
      z <- (quoteExp lowerCharQ) [x]
      zs <- mapM (quoteExp lowerCharQ) $ map (: []) xs
      pure $ ConE (mkName "LowerString") `AppE` z `AppE` ListE zs
