{-# LANGUAGE BangPatterns, FlexibleContexts, TypeFamilies, CPP, ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.PseudoBoolean.Megaparsec
-- Copyright   :  (c) Masahiro Sakai 2011-2016
-- License     :  BSD-style
-- 
-- Maintainer  :  masahiro.sakai@gmail.com
-- Portability :  non-portable (BangPatterns, FlexibleContexts, TypeFamilies, CPP, ConstraintKinds)
--
-- A parser library for OPB file and WBO files used in pseudo boolean competition.
-- 
-- References:
--
-- * Input/Output Format and Solver Requirements for the Competitions of
--   Pseudo-Boolean Solvers
--   <http://www.cril.univ-artois.fr/PB11/format.pdf>
--
-----------------------------------------------------------------------------

module Data.PseudoBoolean.Megaparsec
  (
  -- * Parsing OPB files
    opbParser
  , parseOPBString
  , parseOPBByteString
  , parseOPBFile

  -- * Parsing WBO files
  , wboParser
  , parseWBOString
  , parseWBOByteString
  , parseWBOFile

  -- * Error type
  , ParseError
  ) where

import Prelude hiding (sum)
import Control.Applicative ((<*))
import Control.Monad
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe
#if MIN_VERSION_megaparsec(6,0,0)
import Data.String
import Data.Word
import Data.Void
#endif
import Text.Megaparsec hiding (ParseError)
import qualified Text.Megaparsec as MP
#if MIN_VERSION_megaparsec(6,0,0)
import Text.Megaparsec.Byte
#else
import Text.Megaparsec.Prim (MonadParsec ())
#endif
import Data.PseudoBoolean.Types
import Data.PseudoBoolean.Internal.TextUtil

#if MIN_VERSION_megaparsec(6,0,0)

type C e s m = (MonadParsec e s m, Token s ~ Word8, IsString (Tokens s))

char8 :: C e s m => Char -> m Word8
char8 = char . fromIntegral . fromEnum

#else

#if MIN_VERSION_megaparsec(5,0,0)
type C e s m = (MonadParsec e s m, Token s ~ Char)
#elif MIN_VERSION_megaparsec(4,4,0)
type C e s m = (MonadParsec s m Char)
#else
type C e s m = (MonadParsec s m Char, MonadPlus m)
#endif

char8 :: C e s m => Char -> m Char
char8 = char

#endif

#if MIN_VERSION_megaparsec(7,0,0)
anyChar :: C e s m => m Word8
anyChar = anySingle
#endif

-- | Parser for OPB files
#if MIN_VERSION_megaparsec(6,0,0)
opbParser :: (MonadParsec e s m, Token s ~ Word8, IsString (Tokens s)) => m Formula
#elif MIN_VERSION_megaparsec(5,0,0)
opbParser :: (MonadParsec e s m, Token s ~ Char) => m Formula
#elif MIN_VERSION_megaparsec(4,4,0)
opbParser :: (MonadParsec s m Char) => m Formula
#else
opbParser :: (MonadParsec s m Char, MonadPlus m) => m Formula
#endif
opbParser = formula

-- | Parser for WBO files
#if MIN_VERSION_megaparsec(6,0,0)
wboParser :: (MonadParsec e s m, Token s ~ Word8, IsString (Tokens s)) => m SoftFormula
#elif MIN_VERSION_megaparsec(5,0,0)
wboParser :: (MonadParsec e s m, Token s ~ Char) => m SoftFormula
#elif MIN_VERSION_megaparsec(4,4,0)
wboParser :: (MonadParsec s m Char) => m SoftFormula
#else
wboParser :: (MonadParsec s m Char, MonadPlus m) => m SoftFormula
#endif
wboParser = softformula

-- <formula>::= <sequence_of_comments> [<objective>] <sequence_of_comments_or_constraints>
formula :: C e s m => m Formula
formula = do
  h <- optional hint
  sequence_of_comments
  obj <- optional objective
  cs <- sequence_of_comments_or_constraints
  return $
    Formula
    { pbObjectiveFunction = obj
    , pbConstraints = cs
    , pbNumVars = fromMaybe (pbComputeNumVars obj cs) (fmap fst h)
    , pbNumConstraints = fromMaybe (length cs) (fmap snd h)
    }

hint :: C e s m => m (Int,Int)
hint = try $ do
  _ <- char8 '*'
  zeroOrMoreSpace
  _ <- string "#variable="
  zeroOrMoreSpace
  nv <- unsigned_integer
  oneOrMoreSpace  
  _ <- string "#constraint="
  zeroOrMoreSpace
  nc <- unsigned_integer
  _ <- manyTill anyChar eol
  return (fromIntegral nv, fromIntegral nc)

-- <sequence_of_comments>::= <comment> [<sequence_of_comments>]
sequence_of_comments :: C e s m => m ()
sequence_of_comments = skipMany comment -- XXX: we allow empty sequence

-- <comment>::= "*" <any_sequence_of_characters_other_than_EOL> <EOL>
comment :: C e s m => m ()
comment = do
  _ <- char8 '*' 
  _ <- manyTill anyChar eol
  space -- We relax the grammer and allow spaces in the beggining of next component.
  return ()

-- <sequence_of_comments_or_constraints>::= <comment_or_constraint> [<sequence_of_comments_or_constraints>]
sequence_of_comments_or_constraints :: C e s m => m [Constraint]
sequence_of_comments_or_constraints = do
  xs <- many comment_or_constraint -- We relax the grammer and allow spaces in the beginning of next component.
  return $ catMaybes xs

-- <comment_or_constraint>::= <comment>|<constraint>
comment_or_constraint :: C e s m => m (Maybe Constraint)
comment_or_constraint =
  (comment >> return Nothing) <|> (liftM Just constraint)

-- <objective>::= "min:" <zeroOrMoreSpace> <sum> ";"
objective :: C e s m => m Sum
objective = do
  _ <- string "min:"
  zeroOrMoreSpace
  obj <- sum
  semi
  return obj

-- <constraint>::= <sum> <relational_operator> <zeroOrMoreSpace> <integer> <zeroOrMoreSpace> ";"
constraint :: C e s m => m Constraint
constraint = do
  lhs <- sum
  op <- relational_operator
  zeroOrMoreSpace
  rhs <- integer
  zeroOrMoreSpace
  semi
  return (lhs, op, rhs)

-- <sum>::= <weightedterm> | <weightedterm> <sum>
sum :: C e s m => m Sum
sum = many weightedterm -- we relax the grammer to allow empty sum

-- <weightedterm>::= <integer> <oneOrMoreSpace> <term> <oneOrMoreSpace>
weightedterm :: C e s m => m WeightedTerm
weightedterm = do
  w <- integer
  oneOrMoreSpace
  t <- term
  zeroOrMoreSpace -- we relax the grammar to allow omitting spaces at the end of <sum>.
  return (w,t)

-- <integer>::= <unsigned_integer> | "+" <unsigned_integer> | "-" <unsigned_integer>
integer :: C e s m => m Integer
integer = msum
  [ unsigned_integer
  , char8 '+' >> unsigned_integer
  , char8 '-' >> liftM negate unsigned_integer
  ]

-- <unsigned_integer>::= <digit> | <digit><unsigned_integer>
unsigned_integer :: C e s m => m Integer
unsigned_integer = do
  ds <- some digitChar
#if MIN_VERSION_megaparsec(6,0,0)
  return $! readUnsignedInteger (map (toEnum . fromIntegral) ds)
#else
  return $! readUnsignedInteger ds
#endif

-- <relational_operator>::= ">=" | "="
relational_operator :: C e s m => m Op
relational_operator = (string ">=" >> return Ge) <|> (string "=" >> return Eq)

-- <variablename>::= "x" <unsigned_integer>
variablename :: C e s m => m Var
variablename = do
  _ <- char8 'x'
  i <- unsigned_integer
  return $! fromIntegral i

-- <oneOrMoreSpace>::= " " [<oneOrMoreSpace>]
oneOrMoreSpace :: C e s m => m ()
oneOrMoreSpace  = skipSome (char8 ' ')

-- <zeroOrMoreSpace>::= [" " <zeroOrMoreSpace>]
zeroOrMoreSpace :: C e s m => m ()
-- zeroOrMoreSpace = skipMany (char8 ' ')
zeroOrMoreSpace = space
-- We relax the grammer and allow more type of spacing

semi :: C e s m => m ()
semi = char8 ';' >> space
-- We relax the grammer and allow spaces in the beginning of next component.

{-
For linear pseudo-Boolean instances, <term> is defined as
<term>::=<variablename>

For non-linear instances, <term> is defined as
<term>::= <oneOrMoreLiterals>
-}
term :: C e s m => m Term
term = oneOrMoreLiterals

-- <oneOrMoreLiterals>::= <literal> | <literal> <oneOrMoreSpace> <oneOrMoreLiterals>
oneOrMoreLiterals :: C e s m => m [Lit]
oneOrMoreLiterals = do
  l <- literal
  mplus (try $ oneOrMoreSpace >> liftM (l:) oneOrMoreLiterals) (return [l])
-- Note that we cannot use sepBy1.
-- In "p `sepBy1` q", p should success whenever q success.
-- But it's not the case here.

-- <literal>::= <variablename> | "~"<variablename>
literal :: C e s m => m Lit
literal = variablename <|> (char8 '~' >> liftM negate variablename)

#if MIN_VERSION_megaparsec(7,0,0)
type ParseError = MP.ParseErrorBundle BL.ByteString Void
#elif MIN_VERSION_megaparsec(6,0,0)
type ParseError = MP.ParseError Word8 Void
#elif MIN_VERSION_megaparsec(5,0,0)
type ParseError = MP.ParseError Char Dec
#else
type ParseError = MP.ParseError
#endif

-- | Parse a OPB format string containing pseudo boolean problem.
parseOPBString :: String -> String -> Either ParseError Formula
#if MIN_VERSION_megaparsec(6,0,0)
parseOPBString info s = parse (formula <* eof) info (BL.pack s)
#else
parseOPBString = parse (formula <* eof)
#endif

-- | Parse a OPB format lazy bytestring containing pseudo boolean problem.
parseOPBByteString :: String -> ByteString -> Either ParseError Formula
parseOPBByteString = parse (formula <* eof)

-- | Parse a OPB file containing pseudo boolean problem.
parseOPBFile :: FilePath -> IO (Either ParseError Formula)
parseOPBFile filepath = do
  s <- BL.readFile filepath
  return $! parse (formula <* eof) filepath s

-- <softformula>::= <sequence_of_comments> <softheader> <sequence_of_comments_or_constraints>
softformula :: C e s m => m SoftFormula
softformula = do
  h <- optional hint
  sequence_of_comments
  top <- softheader
  cs <- wbo_sequence_of_comments_or_constraints
  return $
    SoftFormula
    { wboTopCost = top
    , wboConstraints = cs
    , wboNumVars = fromMaybe (wboComputeNumVars cs) (fmap fst h)
    , wboNumConstraints = fromMaybe (length cs) (fmap snd h)
    }

-- <softheader>::= "soft:" [<unsigned_integer>] ";"
softheader :: C e s m => m (Maybe Integer)
softheader = do
  _ <- string "soft:"
  zeroOrMoreSpace -- XXX
  top <- optional unsigned_integer
  zeroOrMoreSpace -- XXX
  semi
  return top

-- <sequence_of_comments_or_constraints>::= <comment_or_constraint> [<sequence_of_comments_or_constraints>]
wbo_sequence_of_comments_or_constraints :: C e s m => m [SoftConstraint]
wbo_sequence_of_comments_or_constraints = do
  xs <- many wbo_comment_or_constraint -- XXX: we relax the grammer to allow empty sequence
  return $ catMaybes xs

-- <comment_or_constraint>::= <comment>|<constraint>|<softconstraint>
wbo_comment_or_constraint :: C e s m => m (Maybe SoftConstraint)
wbo_comment_or_constraint = (comment >> return Nothing) <|> m
  where
    m = liftM Just $ (constraint >>= \c -> return (Nothing, c)) <|> softconstraint

-- <softconstraint>::= "[" <zeroOrMoreSpace> <unsigned_integer> <zeroOrMoreSpace> "]" <constraint>
softconstraint :: C e s m => m SoftConstraint
softconstraint = do
  _ <- char8 '['
  zeroOrMoreSpace
  cost <- unsigned_integer
  zeroOrMoreSpace
  _ <- char8 ']'
  zeroOrMoreSpace -- XXX
  c <- constraint
  return (Just cost, c)

-- | Parse a WBO format string containing weighted boolean optimization problem.
parseWBOString :: String -> String -> Either ParseError SoftFormula
#if MIN_VERSION_megaparsec(6,0,0)
parseWBOString info s = parse (softformula <* eof) info (BL.pack s)
#else
parseWBOString = parse (softformula <* eof)
#endif

-- | Parse a WBO format lazy bytestring containing pseudo boolean problem.
parseWBOByteString :: String -> ByteString -> Either ParseError SoftFormula
parseWBOByteString = parse (softformula <* eof)

-- | Parse a WBO file containing weighted boolean optimization problem.
parseWBOFile :: FilePath -> IO (Either ParseError SoftFormula)
parseWBOFile filepath = do
  s <- BL.readFile filepath
  return $! parse (softformula <* eof) filepath s
