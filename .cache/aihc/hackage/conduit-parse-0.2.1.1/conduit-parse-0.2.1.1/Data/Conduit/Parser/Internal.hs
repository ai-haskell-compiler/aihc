{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
module Data.Conduit.Parser.Internal (module Data.Conduit.Parser.Internal) where

-- {{{ Imports
import qualified Conduit

import           Control.Applicative
import           Control.Exception.Safe    as Exception
import           Control.Monad
import           Control.Monad.Error.Class
import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import           Control.Monad.Trans.State

import           Data.Bifunctor
import           Data.Conduit              hiding (await, leftover)
import qualified Data.Conduit.List         as Conduit
import           Data.DList                (DList (..), append, cons)
import           Data.Maybe                (fromMaybe)
import           Data.Semigroup
import           Data.Text                 as Text (Text, pack, unpack)

import           Safe

import           Text.Parser.Combinators   as Parser
-- }}}

-- | Core type of the package. This is basically a 'Sink' with a parsing state.
newtype ConduitParser i m a = ConduitParser (ExceptT ConduitParserException (StateT ([Text], Buffer i) (ConduitT i Void m)) a)

deriving instance Functor (ConduitParser i m)
deriving instance Applicative (ConduitParser i m)
deriving instance Monad (ConduitParser i m)
deriving instance (MonadIO m) => MonadIO (ConduitParser i m)
deriving instance (MonadThrow m) => MonadThrow (ConduitParser i m)

instance MonadTrans (ConduitParser i) where
  lift = ConduitParser . lift . lift . lift


-- | Backtracking is supported by pushing back consumed elements (using 'leftover') whenever an error is catched.
--
-- As a consequence, within the scope of a `catchError`,
-- all streamed items are kept in memory,
-- which means the consumer no longer uses constant memory.
instance MonadError ConduitParserException (ConduitParser i m) where
  throwError e = do
    names <- getParserNames
    ConduitParser . throwError $ foldr NamedParserException e $ reverse names

  catchError (ConduitParser f) handler = do
    buffer <- withBuffer resetBuffer
    withBuffer $ setEnabled True

    result <- ConduitParser $ (Right <$> f) `catchError` (return . Left)

    case result of
      Left e  -> backtrack >> setBuffer buffer >> handler e
      Right a -> withBuffer (prependBuffer buffer) >> return a

-- | Parsers can be combined with ('<|>'), 'some', 'many', 'optional', 'choice'.
--
-- The use of 'guard' is not recommended as it generates unhelpful error messages.
-- Please consider using 'throwError' or 'unexpected' instead.
instance Alternative (ConduitParser i m) where
  empty = ConduitParser $ throwError $ Unexpected "ConduitParser.empty"

  parserA <|> parserB = catchError parserA $ \ea ->
    catchError parserB $ \eb ->
      throwError $ BothFailed ea eb

-- | Parsing combinators can be used with 'ConduitParser's.
instance (Monad m) => Parsing (ConduitParser i m) where
  try parser = parser

  parser <?> name = do
    pushParserName $ pack name
    a <- parser
    popParserName
    return a

  unexpected = throwError . Unexpected . pack

  eof = do
    result <- peek
    maybe (return ()) (const $ throwError ExpectedEndOfInput) result

  notFollowedBy parser = do
    result <- optional parser
    name <- getParserName
    forM_ result $ \_ -> throwError $ UnexpectedFollowedBy name

-- | Flipped version of ('<?>').
named :: (Monad m) => Text -> ConduitParser i m a -> ConduitParser i m a
named name = flip (<?>) (unpack name)


-- | Run a 'ConduitParser'.
-- Any parsing failure will be thrown as an exception.
runConduitParser :: (MonadThrow m) => ConduitParser i m a -> ConduitT i Void m a
runConduitParser (ConduitParser p) = either throwM return . fst =<< runStateT (runExceptT p) (mempty, mempty)

-- | Return the ordered list of names (assigned through ('<?>')) for the current parser stack. First element is the most nested parser.
getParserNames :: ConduitParser i m [Text]
getParserNames = ConduitParser $ lift $ gets fst

-- | Return the name (assigned through ('<?>')) of the current parser (most nested), or 'mempty' if it has none.
getParserName :: ConduitParser i m Text
getParserName = ConduitParser $ lift $ gets (headDef "" . fst)

pushParserName :: Text -> ConduitParser i m ()
pushParserName name = ConduitParser $ lift $ modify $ first (name :)

popParserName ::  ConduitParser i m ()
popParserName = ConduitParser $ lift $ modify $ first tailSafe

getBuffer :: ConduitParser i m (Buffer i)
getBuffer = ConduitParser $ lift $ gets snd

setBuffer :: Buffer i -> ConduitParser i m (Buffer i)
setBuffer buffer = withBuffer (const buffer)

withBuffer :: (Buffer i -> Buffer i) -> ConduitParser i m (Buffer i)
withBuffer f = do
  buffer <- ConduitParser $ lift $ gets snd
  ConduitParser $ lift $ modify (second f)
  return buffer

backtrack :: ConduitParser i m ()
backtrack = mapM_ leftover =<< withBuffer resetBuffer


newtype Buffer i = Buffer (Maybe (DList i))

deriving instance Semigroup (Buffer i)
deriving instance Monoid (Buffer i)
deriving instance (Show i) => Show (Buffer i)

instance Functor Buffer where
  fmap _ (Buffer Nothing)  = Buffer mempty
  fmap f (Buffer (Just a)) = Buffer $ Just $ fmap f a

instance Foldable Buffer where
  foldMap _ (Buffer Nothing)  = mempty
  foldMap f (Buffer (Just a)) = foldMap f a


setEnabled :: Bool -> Buffer i -> Buffer i
setEnabled True (Buffer a) = Buffer (a <|> Just mempty)
setEnabled _ (Buffer _)    = Buffer mempty

prependItem :: i -> Buffer i -> Buffer i
prependItem new (Buffer a) = Buffer $ fmap (cons new) a

-- Warning: this function is asymetric
prependBuffer :: Buffer i -> Buffer i -> Buffer i
prependBuffer (Buffer a) (Buffer b) = case a of
  Just a' -> Buffer $ Just (fromMaybe mempty b `append` a')
  _       -> Buffer a

resetBuffer :: Buffer i -> Buffer i
resetBuffer (Buffer a) = Buffer $ fmap (const mempty) a

-- | 'Conduit.await' wrapped as a 'ConduitParser'.
--
-- If no data is available, 'UnexpectedEndOfInput' is thrown.
await :: (Monad m) => ConduitParser i m i
await = do
  event <- ConduitParser $ lift $ lift Conduit.await
  e     <- maybe (throwError UnexpectedEndOfInput) return event
  withBuffer $ prependItem e
  return e

-- | 'Conduit.leftover' wrapped as a 'ConduitParser'.
leftover :: i -> ConduitParser i m ()
leftover = ConduitParser . lift . lift . Conduit.leftover

-- | 'Conduit.peek' wrapped as a 'ConduitParser'.
peek :: (Monad m) => ConduitParser i m (Maybe i)
peek = ConduitParser $ lift $ lift Conduit.peek


data ConduitParserException = BothFailed ConduitParserException ConduitParserException
                            | ExpectedEndOfInput
                            | NamedParserException Text ConduitParserException
                            | UnexpectedEndOfInput
                            | UnexpectedFollowedBy Text
                            | Unexpected Text

deriving instance Eq ConduitParserException
deriving instance Show ConduitParserException

instance Exception ConduitParserException where
  displayException (BothFailed ea eb) = displayException ea ++ "\n" ++ displayException eb
  displayException ExpectedEndOfInput = "Unexpected input, expected end of input."
  displayException (NamedParserException t e) = "While parsing " ++ unpack t ++ ":\n" ++ displayException e
  displayException UnexpectedEndOfInput = "Unexpected end of input."
  displayException (UnexpectedFollowedBy t) = "Should not be followed by " ++ unpack t
  displayException (Unexpected t) = unpack t
