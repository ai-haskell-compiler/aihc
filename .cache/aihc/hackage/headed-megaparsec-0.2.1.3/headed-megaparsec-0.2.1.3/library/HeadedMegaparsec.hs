module HeadedMegaparsec
  ( -- * Types
    HeadedParsec,

    -- * Execution
    toParsec,

    -- * Transformation
    wrapToHead,
    label,
    hidden,
    dbg,
    filter,

    -- * Construction
    parse,
    endHead,
  )
where

import Control.Applicative.Combinators
import qualified HeadedMegaparsec.Megaparsec as Megaparsec
import HeadedMegaparsec.Prelude hiding (filter, head, tail, try)
import Text.Megaparsec (Parsec, Stream)
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Debug as Megaparsec

-- $setup
--
-- >>> :set -XApplicativeDo

-- * Types

-- |
-- Headed parser.
--
-- Abstracts over explicit composition between consecutive megaparsec `try` blocks,
-- providing for better error messages.
--
-- With headed parser you don't need to use `try` at all.
--
-- ==__Examples__
--
-- >>> import Prelude
-- >>> import Control.Applicative
-- >>> import Data.Void
-- >>> import qualified Text.Megaparsec as M
-- >>> import qualified Text.Megaparsec.Char as M
-- >>> import qualified Text.Megaparsec.Char.Lexer as ML
-- >>> :{
--   let
--     select :: HeadedParsec Void String (Maybe [Either Char Int], Maybe Int)
--     select = do
--       string' "select"
--       endHead
--       _targets <- optional (space1 *> targets)
--       _limit <- optional (space1 *> limit)
--       return (_targets, _limit)
--       where
--         -- Lifted versions of basic parsers:
--         char = parse . M.char
--         space = parse M.space
--         space1 = parse M.space1
--         decimal = parse ML.decimal
--         string' = parse . M.string'
--         -- Syntax parsers:
--         targets = M.sepBy1 target commaSeparator
--         target = Left <$> char '*' <|> Right <$> decimal
--         commaSeparator = space *> char ',' *> endHead *> space
--         limit = string' "limit" *> endHead *> space1 *> decimal
--     test :: String -> IO ()
--     test = M.parseTest (toParsec select <* M.eof)
-- :}
--
-- >>> test "select 1, "
-- 1:11:
--   |
-- 1 | select 1,
--   |           ^
-- unexpected end of input
-- expecting '*', integer, or white space
--
-- >>> test "select limit "
-- ...
-- unexpected end of input
-- expecting integer or white space
--
-- >>> test "select 1, 2 limit 2"
-- (Just [Right 1,Right 2],Just 2)
newtype HeadedParsec err strm a = HeadedParsec (Parsec err strm (Either a (Parsec err strm a)))

-- |
-- A helper required for hacking `dbg`.
data Showable a = Showable String a

-- * Instances

-- ** Showable

instance Show (Showable a) where
  show (Showable msg _) = msg

-- ** HeadedParsec

instance Functor (HeadedParsec err strm) where
  fmap fn (HeadedParsec p) = HeadedParsec (fmap (bimap fn (fmap fn)) p)

instance (Ord err, Stream strm) => Applicative (HeadedParsec err strm) where
  pure = HeadedParsec . pure . Left
  (<*>) (HeadedParsec p1) (HeadedParsec p2) = HeadedParsec $ do
    junction1 <- p1
    case junction1 of
      Left aToB -> do
        junction2 <- p2
        case junction2 of
          Left a -> return (Left (aToB a))
          Right tailP2 -> return
            $ Right
            $ do
              a <- tailP2
              return (aToB a)
      Right tailP1 -> return
        $ Right
        $ do
          aToB <- tailP1
          junction2 <- p2
          case junction2 of
            Left a -> return (aToB a)
            Right tailP2 -> do
              a <- tailP2
              return (aToB a)

instance (Ord err, Stream strm) => Selective (HeadedParsec err strm) where
  select (HeadedParsec p1) (HeadedParsec p2) = HeadedParsec $ do
    junction1 <- p1
    case junction1 of
      Left eitherAOrB -> case eitherAOrB of
        Right b -> return (Left b)
        Left a -> do
          junction2 <- p2
          case junction2 of
            Left aToB -> return (Left (aToB a))
            Right tailP2 -> return (Right (fmap ($ a) tailP2))
      Right tailP1 -> return
        $ Right
        $ do
          eitherAOrB <- tailP1
          case eitherAOrB of
            Right b -> return b
            Left a -> do
              junction2 <- p2
              case junction2 of
                Left aToB -> return (aToB a)
                Right tailP2 -> fmap ($ a) tailP2

instance (Ord err, Stream strm) => Monad (HeadedParsec err strm) where
  return = pure
  (>>=) (HeadedParsec p1) k2 = HeadedParsec $ do
    junction1 <- p1
    case junction1 of
      Left a -> case k2 a of HeadedParsec p2 -> p2
      Right tailP1 -> return
        $ Right
        $ do
          a <- tailP1
          Megaparsec.contPossibly $ case k2 a of HeadedParsec p2 -> p2

-- |
-- Alternation is performed only the basis of heads.
-- Bodies do not participate.
instance (Ord err, Stream strm) => Alternative (HeadedParsec err strm) where
  empty = HeadedParsec empty
  (<|>) (HeadedParsec p1) (HeadedParsec p2) = HeadedParsec (Megaparsec.try p1 <|> p2)

-- |
-- Alternation is performed only the basis of heads.
-- Bodies do not participate.
instance (Ord err, Stream strm) => MonadPlus (HeadedParsec err strm) where
  mzero = empty
  mplus = (<|>)

instance (Ord err, Stream strm) => MonadFail (HeadedParsec err strm) where
  fail = HeadedParsec . fail

-- * Execution

-- |
-- Convert headed parser into megaparsec parser.
toParsec :: (Ord err, Stream strm) => HeadedParsec err strm a -> Parsec err strm a
toParsec (HeadedParsec p) = Megaparsec.contPossibly p

-- * Helpers

mapParsec :: (Parsec err1 strm1 (Either res1 (Parsec err1 strm1 res1)) -> Parsec err2 strm2 (Either res2 (Parsec err2 strm2 res2))) -> HeadedParsec err1 strm1 res1 -> HeadedParsec err2 strm2 res2
mapParsec fn (HeadedParsec p) = HeadedParsec (fn p)

-- * Transformation

-- |
-- Wrap a parser to be usable as a whole in a head block,
-- allowing it in effect to be composed with the following parsers into a single `try` when executed,
-- no matter whether it contains `endHead` or not.
wrapToHead :: (Ord err, Stream strm) => HeadedParsec err strm a -> HeadedParsec err strm a
wrapToHead = mapParsec $ fmap Left . Megaparsec.contPossibly

-- |
-- Label a headed parser.
-- Works the same way as megaparsec's `Megaparsec.label`.
label :: (Ord err, Stream strm) => String -> HeadedParsec err strm a -> HeadedParsec err strm a
label label = mapParsec (Megaparsec.label label)

-- |
-- Adaptation of 'Megaparsec.hidden'.
hidden :: (Ord err, Stream strm) => HeadedParsec err strm a -> HeadedParsec err strm a
hidden = mapParsec Megaparsec.hidden

-- |
-- Make a parser print debugging information when evaluated.
-- The first parameter is a custom label.
--
-- This function is a wrapper around `Megaparsec.dbg`.
-- It generates two debugging entries: one for head and one for tail.
dbg :: (Megaparsec.ShowErrorComponent err, Megaparsec.VisualStream strm, Show a) => String -> HeadedParsec err strm a -> HeadedParsec err strm a
dbg label = mapParsec $ \p -> do
  Showable _ junction <- Megaparsec.dbg (label <> "/head") (fmap (either (\a -> Showable (show a) (Left a)) (Showable "<tail parser>" . Right)) p)
  case junction of
    Left a -> return (Left a)
    Right tailP -> return $ Right $ Megaparsec.dbg (label <> "/tail") tailP

-- |
-- Filter the results of parser based on a predicate,
-- failing with a parameterized message.
filter :: (Stream strm) => (a -> String) -> (a -> Bool) -> HeadedParsec err strm a -> HeadedParsec err strm a
filter err pred = mapParsec $ \p -> do
  junction <- p
  case junction of
    Left a ->
      if pred a
        then return (Left a)
        else fail (err a)
    Right tailP -> return
      $ Right
      $ do
        a <- tailP
        if pred a
          then return a
          else fail (err a)

-- |
-- Lift a megaparsec parser as a head parser.
head :: Parsec err strm a -> HeadedParsec err strm a
head = HeadedParsec . fmap Left

-- |
-- Lift a megaparsec parser.
parse :: Parsec err strm a -> HeadedParsec err strm a
parse = head

-- * Control

-- |
-- Make all the following parsers compose as tail.
endHead :: (Stream strm) => HeadedParsec err strm ()
endHead = HeadedParsec (return (Right (return ())))
