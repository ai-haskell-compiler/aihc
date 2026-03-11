{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

#if defined (__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 802
{-# LANGUAGE TypeSynonymInstances #-}
#endif

-- | Parsers that can consume and return a prefix of their input.

module Text.Parser.Input (InputParsing(..), InputCharParsing(..), ConsumedInputParsing(..),
                          Lazy(..), Strict(..)) where

import Control.Applicative (Alternative ((<|>), empty))
import Control.Monad (MonadPlus, void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Identity (IdentityT(..))
import Control.Monad.Trans.Reader (ReaderT(..), mapReaderT)
import qualified Control.Monad.Trans.Writer.Lazy as Lazy (WriterT)
import qualified Control.Monad.Trans.Writer.Strict as Strict (WriterT)
import qualified Control.Monad.Trans.State.Lazy as Lazy (StateT)
import qualified Control.Monad.Trans.State.Strict as Strict (StateT)
import qualified Control.Monad.Trans.RWS.Lazy as Lazy (RWST)
import qualified Control.Monad.Trans.RWS.Strict as Strict (RWST)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as Lazy
import Data.Ord (Down)
import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as ReadP

import Text.Parser.Char (CharParsing)
import Text.Parser.Combinators (count, eof, notFollowedBy, try, unexpected)
import Text.Parser.LookAhead (LookAheadParsing, lookAhead)
import qualified Text.Parser.Char as Char

import qualified Data.Monoid.Factorial as Factorial
import qualified Data.Monoid.Null as Null
import qualified Data.Monoid.Textual as Textual
import qualified Data.Semigroup.Cancellative as Cancellative
import Data.Monoid.Factorial (FactorialMonoid)
import Data.Monoid.Textual (TextualMonoid)
import Data.Semigroup.Cancellative (LeftReductive)

#ifdef MIN_VERSION_attoparsec
import Data.Text (Text)
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.Text as Text

import qualified Data.Attoparsec.ByteString as Attoparsec
import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec.Char8
import qualified Data.Attoparsec.Text as Attoparsec.Text
#endif

#ifdef MIN_VERSION_parsec
import Text.Parsec (ParsecT)
import qualified Text.Parsec as Parsec
#endif

#ifdef MIN_VERSION_binary
import qualified Data.Binary.Get as Binary
#endif

import Text.Parser.Input.Position (fromEnd, fromStart)
import Text.Parser.Internal (mapLazyWriterT, mapStrictWriterT,
                             mapLazyStateT, mapStrictStateT,
                             mapLazyRWST, mapStrictRWST)
import Text.Parser.Wrapper (Lazy(..), Strict(..))

import Prelude hiding (take, takeWhile)

-- | Methods for parsing monoidal inputs
class LookAheadParsing m => InputParsing m where
   -- | The type of the input stream that the parser @m@ expects to parse.
   type ParserInput m
   type ParserPosition m
   -- | Always sucessful parser that returns the entire remaining input without consuming it.
   getInput :: m (ParserInput m)
   -- | Retrieve the 'Position' reached by the parser in the input source.
   getSourcePos :: m (ParserPosition m)

   -- | A parser that accepts any single atomic prefix of the input stream.
   --
   -- > anyToken == satisfy (const True)
   -- > anyToken == take 1
   anyToken :: m (ParserInput m)
   -- | A parser that accepts exactly the given number of input atoms.
   --
   -- > take n == count n anyToken
   take :: Int -> m (ParserInput m)
   -- | A parser that accepts an input atom only if it satisfies the given predicate.
   satisfy :: (ParserInput m -> Bool) -> m (ParserInput m)
   -- | A parser that succeeds exactly when satisfy doesn't, equivalent to
   -- 'Text.Parser.Combinators.notFollowedBy' @.@ 'satisfy'
   notSatisfy :: (ParserInput m -> Bool) -> m ()

   -- | A stateful scanner. The predicate modifies a state argument, and each transformed state is passed to successive
   -- invocations of the predicate on each token of the input until one returns 'Nothing' or the input ends.
   --
   -- This parser does not fail.  It will return an empty string if the predicate returns 'Nothing' on the first
   -- character.
   --
   -- /Note/: Because this parser does not fail, do not use it with combinators such as 'Control.Applicative.many',
   -- because such parsers loop until a failure occurs.  Careless use will thus result in an infinite loop.
   scan :: state -> (state -> ParserInput m -> Maybe state) -> m (ParserInput m)
   -- | A parser that consumes and returns the given prefix of the input.
   string :: ParserInput m -> m (ParserInput m)

   -- | A parser accepting the longest sequence of input atoms that match the given predicate; an optimized version of
   -- 'concat' @.@ 'Control.Applicative.many' @.@ 'satisfy'.
   --
   -- /Note/: Because this parser does not fail, do not use it with combinators such as 'Control.Applicative.many',
   -- because such parsers loop until a failure occurs.  Careless use will thus result in an infinite loop.
   takeWhile :: (ParserInput m -> Bool) -> m (ParserInput m)
   -- | A parser accepting the longest non-empty sequence of input atoms that match the given predicate; an optimized
   -- version of 'concat' @.@ 'Control.Applicative.some' @.@ 'satisfy'.
   takeWhile1 :: (ParserInput m -> Bool) -> m (ParserInput m)

   type ParserPosition m = Down Int
   default getSourcePos :: (FactorialMonoid (ParserInput m), Functor m, ParserPosition m ~ Down Int)
                        => m (ParserPosition m)
   getSourcePos = fromEnd . Factorial.length <$> getInput
   anyToken = take 1
   default satisfy :: Monad m => (ParserInput m -> Bool) -> m (ParserInput m)
   satisfy predicate = anyToken >>= \x-> if predicate x then pure x else empty
   notSatisfy predicate = try (void $ satisfy $ not . predicate) <|> eof
   default string :: (Monad m, LeftReductive (ParserInput m), FactorialMonoid (ParserInput m), Show (ParserInput m))
                  => ParserInput m -> m (ParserInput m)
   string s = do i <- getInput
                 if s `Cancellative.isPrefixOf` i
                    then take (Factorial.length s)
                    else unexpected ("string " <> show s)
   default scan :: (Monad m, FactorialMonoid (ParserInput m)) =>
                   state -> (state -> ParserInput m -> Maybe state) -> m (ParserInput m)
   scan state f = do i <- getInput
                     let (prefix, _suffix, _state) = Factorial.spanMaybe' state f i
                     take (Factorial.length prefix)
   default takeWhile :: (Monad m, FactorialMonoid (ParserInput m)) => (ParserInput m -> Bool) -> m (ParserInput m)
   takeWhile predicate = do i <- getInput
                            take (Factorial.length $ Factorial.takeWhile predicate i)
   default takeWhile1 :: (Monad m, FactorialMonoid (ParserInput m)) => (ParserInput m -> Bool) -> m (ParserInput m)
   takeWhile1 predicate = do x <- takeWhile predicate
                             if Null.null x then unexpected "takeWhile1" else pure x


-- | Methods for parsing textual monoid inputs
class (CharParsing m, InputParsing m) => InputCharParsing m where
   -- | Specialization of 'satisfy' on textual inputs, accepting an input character only if it satisfies the given
   -- predicate, and returning the input atom that represents the character. Equivalent to @fmap singleton
   -- . Char.satisfy@
   satisfyCharInput :: (Char -> Bool) -> m (ParserInput m)
   -- | A parser that succeeds exactly when satisfy doesn't, equivalent to @notFollowedBy . Char.satisfy@
   notSatisfyChar :: (Char -> Bool) -> m ()

   -- | Stateful scanner like `scan`, but specialized for 'TextualMonoid' inputs.
   scanChars :: state -> (state -> Char -> Maybe state) -> m (ParserInput m)

   -- | Specialization of 'takeWhile' on 'TextualMonoid' inputs, accepting the longest sequence of input characters that
   -- match the given predicate; an optimized version of @fmap fromString  . many . Char.satisfy@.
   --
   -- /Note/: Because this parser does not fail, do not use it with combinators such as 'Control.Applicative.many',
   -- because such parsers loop until a failure occurs.  Careless use will thus result in an infinite loop.
   takeCharsWhile :: (Char -> Bool) -> m (ParserInput m)
   -- | Specialization of 'takeWhile1' on 'TextualMonoid' inputs, accepting the longest sequence of input characters
   -- that match the given predicate; an optimized version of @fmap fromString  . some . Char.satisfy@.
   takeCharsWhile1 :: (Char -> Bool) -> m (ParserInput m)

   notSatisfyChar = notFollowedBy . Char.satisfy
   default scanChars :: (Monad m, TextualMonoid (ParserInput m)) =>
                        state -> (state -> Char -> Maybe state) -> m (ParserInput m)
   scanChars state f = do i <- getInput
                          let (prefix, _suffix, _state) = Textual.spanMaybe' state (const $ const Nothing) f i
                          take (Factorial.length prefix)
   default takeCharsWhile :: (Monad m, TextualMonoid (ParserInput m)) => (Char -> Bool) -> m (ParserInput m)
   takeCharsWhile predicate = do i <- getInput
                                 take (Factorial.length $ Textual.takeWhile_ False predicate i)
   default takeCharsWhile1 :: (Monad m, TextualMonoid (ParserInput m)) => (Char -> Bool) -> m (ParserInput m)
   takeCharsWhile1 predicate = do x <- takeCharsWhile predicate
                                  if Null.null x then unexpected "takeCharsWhile1" else pure x

-- | Parsers that keep track of the consumed input.
class InputParsing m => ConsumedInputParsing m where
   -- | Return both the result of a parse and the portion of the input that the argument parser consumed.
   match :: m a -> m (ParserInput m, a)

instance InputParsing ReadP where
   type ParserInput ReadP = String
   getInput = ReadP.look
   take n = count n ReadP.get
   anyToken = pure <$> ReadP.get
   satisfy predicate = pure <$> ReadP.satisfy (predicate . pure)
   string = ReadP.string

instance InputCharParsing ReadP where
   satisfyCharInput predicate = pure <$> ReadP.satisfy predicate

instance ConsumedInputParsing ReadP where
   match = ReadP.gather

instance (Monad m, InputParsing m) => InputParsing (IdentityT m) where
   type ParserInput (IdentityT m) = ParserInput m
   type ParserPosition (IdentityT m) = ParserPosition m
   getInput = IdentityT getInput
   getSourcePos = IdentityT getSourcePos
   anyToken = IdentityT anyToken
   take = IdentityT . take
   satisfy = IdentityT . satisfy
   notSatisfy = IdentityT . notSatisfy
   scan state f = IdentityT (scan state f)
   string = IdentityT . string
   takeWhile = IdentityT . takeWhile
   takeWhile1 = IdentityT . takeWhile1

instance (MonadPlus m, InputCharParsing m) => InputCharParsing (IdentityT m) where
   satisfyCharInput = IdentityT . satisfyCharInput
   notSatisfyChar = IdentityT . notSatisfyChar
   scanChars state f = IdentityT (scanChars state f)
   takeCharsWhile = IdentityT . takeCharsWhile
   takeCharsWhile1 = IdentityT . takeCharsWhile1

instance (Monad m, ConsumedInputParsing m) => ConsumedInputParsing (IdentityT m) where
  match (IdentityT p) = IdentityT (match p)

instance (MonadPlus m, InputParsing m) => InputParsing (ReaderT e m) where
   type ParserInput (ReaderT e m) = ParserInput m
   type ParserPosition (ReaderT e m) = ParserPosition m
   getInput = lift getInput
   getSourcePos = lift getSourcePos
   anyToken = lift anyToken
   take = lift . take
   satisfy = lift . satisfy
   notSatisfy = lift . notSatisfy
   scan state f = lift (scan state f)
   string = lift . string
   takeWhile = lift . takeWhile
   takeWhile1 = lift . takeWhile1

instance (MonadPlus m, InputCharParsing m) => InputCharParsing (ReaderT e m) where
   satisfyCharInput = lift . satisfyCharInput
   notSatisfyChar = lift . notSatisfyChar
   scanChars state f = lift (scanChars state f)
   takeCharsWhile = lift . takeCharsWhile
   takeCharsWhile1 = lift . takeCharsWhile1

instance (MonadPlus m, ConsumedInputParsing m) => ConsumedInputParsing (ReaderT e m) where
  match = mapReaderT match

instance (MonadPlus m, InputParsing m, Monoid w) => InputParsing (Lazy.WriterT w m) where
   type ParserInput (Lazy.WriterT w m) = ParserInput m
   type ParserPosition (Lazy.WriterT w m) = ParserPosition m
   getInput = lift getInput
   getSourcePos = lift getSourcePos
   anyToken = lift anyToken
   take = lift . take
   satisfy = lift . satisfy
   notSatisfy = lift . notSatisfy
   scan state f = lift (scan state f)
   string = lift . string
   takeWhile = lift . takeWhile
   takeWhile1 = lift . takeWhile1

instance (MonadPlus m, InputCharParsing m, Monoid w) => InputCharParsing (Lazy.WriterT w m) where
   satisfyCharInput = lift . satisfyCharInput
   notSatisfyChar = lift . notSatisfyChar
   scanChars state f = lift (scanChars state f)
   takeCharsWhile = lift . takeCharsWhile
   takeCharsWhile1 = lift . takeCharsWhile1

instance (MonadPlus m, ConsumedInputParsing m, Monoid w) => ConsumedInputParsing (Lazy.WriterT w m) where
  match = mapLazyWriterT match

instance (MonadPlus m, InputParsing m, Monoid w) => InputParsing (Strict.WriterT w m) where
   type ParserInput (Strict.WriterT w m) = ParserInput m
   type ParserPosition (Strict.WriterT w m) = ParserPosition m
   getInput = lift getInput
   getSourcePos = lift getSourcePos
   anyToken = lift anyToken
   take = lift . take
   satisfy = lift . satisfy
   notSatisfy = lift . notSatisfy
   scan state f = lift (scan state f)
   string = lift . string
   takeWhile = lift . takeWhile
   takeWhile1 = lift . takeWhile1

instance (MonadPlus m, InputCharParsing m, Monoid w) => InputCharParsing (Strict.WriterT w m) where
   satisfyCharInput = lift . satisfyCharInput
   notSatisfyChar = lift . notSatisfyChar
   scanChars state f = lift (scanChars state f)
   takeCharsWhile = lift . takeCharsWhile
   takeCharsWhile1 = lift . takeCharsWhile1

instance (MonadPlus m, ConsumedInputParsing m, Monoid w) => ConsumedInputParsing (Strict.WriterT w m) where
  match = mapStrictWriterT match

instance (MonadPlus m, InputParsing m) => InputParsing (Lazy.StateT s m) where
   type ParserInput (Lazy.StateT s m) = ParserInput m
   type ParserPosition (Lazy.StateT s m) = ParserPosition m
   getInput = lift getInput
   getSourcePos = lift getSourcePos
   anyToken = lift anyToken
   take = lift . take
   satisfy = lift . satisfy
   notSatisfy = lift . notSatisfy
   scan state f = lift (scan state f)
   string = lift . string
   takeWhile = lift . takeWhile
   takeWhile1 = lift . takeWhile1

instance (MonadPlus m, InputCharParsing m) => InputCharParsing (Lazy.StateT s m) where
   satisfyCharInput = lift . satisfyCharInput
   notSatisfyChar = lift . notSatisfyChar
   scanChars state f = lift (scanChars state f)
   takeCharsWhile = lift . takeCharsWhile
   takeCharsWhile1 = lift . takeCharsWhile1

instance (MonadPlus m, ConsumedInputParsing m) => ConsumedInputParsing (Lazy.StateT s m) where
  match = mapLazyStateT match

instance (MonadPlus m, InputParsing m) => InputParsing (Strict.StateT s m) where
   type ParserInput (Strict.StateT s m) = ParserInput m
   type ParserPosition (Strict.StateT s m) = ParserPosition m
   getInput = lift getInput
   getSourcePos = lift getSourcePos
   anyToken = lift anyToken
   take = lift . take
   satisfy = lift . satisfy
   notSatisfy = lift . notSatisfy
   scan state f = lift (scan state f)
   string = lift . string
   takeWhile = lift . takeWhile
   takeWhile1 = lift . takeWhile1

instance (MonadPlus m, InputCharParsing m) => InputCharParsing (Strict.StateT s m) where
   satisfyCharInput = lift . satisfyCharInput
   notSatisfyChar = lift . notSatisfyChar
   scanChars state f = lift (scanChars state f)
   takeCharsWhile = lift . takeCharsWhile
   takeCharsWhile1 = lift . takeCharsWhile1

instance (MonadPlus m, ConsumedInputParsing m) => ConsumedInputParsing (Strict.StateT s m) where
  match = mapStrictStateT match

instance (MonadPlus m, InputParsing m, Monoid w) => InputParsing (Lazy.RWST r w s m) where
   type ParserInput (Lazy.RWST r w s m) = ParserInput m
   type ParserPosition (Lazy.RWST r w s m) = ParserPosition m
   getInput = lift getInput
   getSourcePos = lift getSourcePos
   anyToken = lift anyToken
   take = lift . take
   satisfy = lift . satisfy
   notSatisfy = lift . notSatisfy
   scan state f = lift (scan state f)
   string = lift . string
   takeWhile = lift . takeWhile
   takeWhile1 = lift . takeWhile1

instance (MonadPlus m, InputCharParsing m, Monoid w) => InputCharParsing (Lazy.RWST r w s m) where
   satisfyCharInput = lift . satisfyCharInput
   notSatisfyChar = lift . notSatisfyChar
   scanChars state f = lift (scanChars state f)
   takeCharsWhile = lift . takeCharsWhile
   takeCharsWhile1 = lift . takeCharsWhile1

instance (MonadPlus m, ConsumedInputParsing m, Monoid w) => ConsumedInputParsing (Lazy.RWST r w s m) where
  match = mapLazyRWST match

instance (MonadPlus m, InputParsing m, Monoid w) => InputParsing (Strict.RWST r w s m) where
   type ParserInput (Strict.RWST r w s m) = ParserInput m
   type ParserPosition (Strict.RWST r w s m) = ParserPosition m
   getInput = lift getInput
   getSourcePos = lift getSourcePos
   anyToken = lift anyToken
   take = lift . take
   satisfy = lift . satisfy
   notSatisfy = lift . notSatisfy
   scan state f = lift (scan state f)
   string = lift . string
   takeWhile = lift . takeWhile
   takeWhile1 = lift . takeWhile1

instance (MonadPlus m, InputCharParsing m, Monoid w) => InputCharParsing (Strict.RWST r w s m) where
   satisfyCharInput = lift . satisfyCharInput
   notSatisfyChar = lift . notSatisfyChar
   scanChars state f = lift (scanChars state f)
   takeCharsWhile = lift . takeCharsWhile
   takeCharsWhile1 = lift . takeCharsWhile1

instance (MonadPlus m, ConsumedInputParsing m, Monoid w) => ConsumedInputParsing (Strict.RWST r w s m) where
  match = mapStrictRWST match

#ifdef MIN_VERSION_attoparsec
instance InputParsing Attoparsec.Parser where
   type ParserInput Attoparsec.Parser = ByteString
   getInput = lookAhead Attoparsec.takeByteString
   anyToken = Attoparsec.take 1
   take = Attoparsec.take
   satisfy predicate = Attoparsec.satisfyWith ByteString.singleton predicate
   string = Attoparsec.string
   takeWhile predicate = Attoparsec.takeWhile (predicate . ByteString.singleton)
   takeWhile1 predicate = Attoparsec.takeWhile1 (predicate . ByteString.singleton)
   scan state f = Attoparsec.scan state f'
      where f' s byte = f s (ByteString.singleton byte)

instance InputCharParsing Attoparsec.Parser where
   satisfyCharInput predicate = ByteString.Char8.singleton <$> Attoparsec.Char8.satisfy predicate
   scanChars = Attoparsec.Char8.scan
   takeCharsWhile = Attoparsec.Char8.takeWhile
   takeCharsWhile1 = Attoparsec.Char8.takeWhile1

instance ConsumedInputParsing Attoparsec.Parser where
   match = Attoparsec.match

instance InputParsing Attoparsec.Text.Parser where
   type ParserInput Attoparsec.Text.Parser = Text
   getInput = lookAhead Attoparsec.Text.takeText
   anyToken = Attoparsec.Text.take 1
   take = Attoparsec.Text.take
   satisfy predicate = Attoparsec.Text.satisfyWith Text.singleton predicate
   string = Attoparsec.Text.string
   takeWhile predicate = Attoparsec.Text.takeWhile (predicate . Text.singleton)
   takeWhile1 predicate = Attoparsec.Text.takeWhile1 (predicate . Text.singleton)
   scan state f = Attoparsec.Text.scan state f'
      where f' s c = f s (Text.singleton c)

instance InputCharParsing Attoparsec.Text.Parser where
   satisfyCharInput predicate = Text.singleton <$> Attoparsec.Text.satisfy predicate
   scanChars = Attoparsec.Text.scan
   takeCharsWhile = Attoparsec.Text.takeWhile
   takeCharsWhile1 = Attoparsec.Text.takeWhile1

instance ConsumedInputParsing Attoparsec.Text.Parser where
   match = Attoparsec.Text.match
#endif

#ifdef MIN_VERSION_parsec
instance (FactorialMonoid s, LeftReductive s, Show s, Parsec.Stream s m t, Show t) => InputParsing (ParsecT s u m) where
   type ParserInput (ParsecT s u m) = s
   getInput = Parsec.getInput
   anyToken = do rest <- Parsec.getInput
                 case Factorial.splitPrimePrefix rest
                   of Just (x, rest') -> x <$ Parsec.setInput rest'
                      Nothing -> Parsec.parserFail "anyToken"
   take n = do rest <- Parsec.getInput
               case Factorial.splitAt n rest
                 of (prefix, suffix) | Factorial.length prefix == n -> prefix <$ Parsec.setInput suffix
                    _ -> Parsec.parserFail ("take " ++ show n)

instance (TextualMonoid s, Show s, Parsec.Stream s m Char) => InputCharParsing (ParsecT s u m) where
   satisfyCharInput = fmap Textual.singleton . Parsec.satisfy
#endif

#ifdef MIN_VERSION_binary
instance InputParsing (Lazy Binary.Get) where
   type ParserInput (Lazy Binary.Get) = Lazy.ByteString
   type ParserPosition (Lazy Binary.Get) = Int
   getInput = Lazy (Binary.lookAhead Binary.getRemainingLazyByteString)
   getSourcePos = Lazy (fromStart . fromIntegral <$> Binary.bytesRead)
   anyToken = Lazy (Binary.getLazyByteString 1)
   take n = Lazy (Binary.getLazyByteString $ fromIntegral n)

instance InputParsing (Strict Binary.Get) where
   type ParserInput (Strict Binary.Get) = ByteString
   type ParserPosition (Strict Binary.Get) = Int
   getInput = Strict (Lazy.toStrict <$> Binary.lookAhead Binary.getRemainingLazyByteString)
   getSourcePos = Strict (fromStart . fromIntegral <$> Binary.bytesRead)
   anyToken = Strict (Binary.getByteString 1)
   take n = Strict (Binary.getByteString n)

instance ConsumedInputParsing (Lazy Binary.Get) where
  match (Lazy p) = Lazy $ do input <- Binary.lookAhead Binary.getRemainingLazyByteString
                             pos <- Binary.bytesRead
                             result <- p
                             pos' <- Binary.bytesRead
                             pure (Lazy.take (pos' - pos) input, result)

instance ConsumedInputParsing (Strict Binary.Get) where
  match (Strict p) = Strict $ do input <- Binary.lookAhead Binary.getRemainingLazyByteString
                                 pos <- Binary.bytesRead
                                 result <- p
                                 pos' <- Binary.bytesRead
                                 pure (Lazy.toStrict (Lazy.take (pos' - pos) input), result)
#endif
