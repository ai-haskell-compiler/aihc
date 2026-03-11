{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving        #-}

-- | This module provides a type representing pretty exceptions. It can be used
-- as in the example below:
--
-- > {-# LANGUAGE NoImplicitPrelude #-}
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > module Main (main) where
-- >
-- > import RIO
-- >          ( Exception, Handler (..), IO, RIO, Show, SomeException (..), Typeable
-- >          , ($), catches, displayException, exitFailure, fromString, logError
-- >          , mempty, throwIO
-- >          )
-- > import RIO.PrettyPrint
-- >          ( Pretty (..), Style (..), (<+>), prettyError, prettyInfo, style )
-- > import RIO.PrettyPrint.PrettyException ( PrettyException (..) )
-- > import RIO.PrettyPrint.Simple ( SimplePrettyApp, runSimplePrettyApp )
-- >
-- > main :: IO ()
-- > main = runSimplePrettyApp 80 mempty (action `catches` handleExceptions)
-- >  where
-- >   action :: RIO SimplePrettyApp ()
-- >   action = do
-- >       prettyInfo "Running action!"
-- >       throwIO (PrettyException MyPrettyException)
-- >
-- >  handleExceptions :: [Handler (RIO SimplePrettyApp) ()]
-- >  handleExceptions =
-- >    [ Handler handlePrettyException
-- >    , Handler handleSomeException
-- >    ]
-- >
-- >  handlePrettyException :: PrettyException -> RIO SimplePrettyApp ()
-- >  handlePrettyException e = do
-- >    prettyError $ pretty e
-- >    exitFailure
-- >
-- >  handleSomeException :: SomeException -> RIO SimplePrettyApp ()
-- >  handleSomeException (SomeException e) = do
-- >    logError $ fromString $ displayException e
-- >    exitFailure
-- >
-- > data MyPrettyException
-- >   = MyPrettyException
-- >   deriving (Show, Typeable)
-- >
-- > instance Pretty MyPrettyException where
-- >   pretty MyPrettyException =
-- >     "My" <+> style Highlight "pretty" <+> "exception!"
-- >
-- > instance Exception MyPrettyException
--
module RIO.PrettyPrint.PrettyException
  ( PrettyException (..)
  , ppException
  , prettyThrowIO
  , prettyThrowM
  ) where

import RIO
         ( Exception (..), Maybe (..), MonadIO, MonadThrow, Show, SomeException
         , Typeable, (.), throwIO, throwM
         )
import Text.PrettyPrint.Leijen.Extended ( Pretty (..), StyleDoc, string )

-- | Type representing pretty exceptions.
--
-- @since 0.1.4.0
data PrettyException
  = forall e. (Exception e, Pretty e) => PrettyException e
  deriving Typeable

deriving instance Show PrettyException

instance Pretty PrettyException where
  pretty (PrettyException e) = pretty e

instance Exception PrettyException where
  displayException (PrettyException e) = displayException e

-- | Provide the prettiest available information about an exception.
ppException :: SomeException -> StyleDoc
ppException e = case fromException e of
  Just (PrettyException e') -> pretty e'
  Nothing -> (string . displayException) e

-- | Synchronously throw the given exception as a 'PrettyException'.
prettyThrowIO :: (Exception e, MonadIO m, Pretty e) => e -> m a
prettyThrowIO = throwIO . PrettyException

-- | Throw the given exception as a 'PrettyException', when the action is run in
-- the monad @m@.
prettyThrowM :: (Exception e, MonadThrow m, Pretty e) => e -> m a
prettyThrowM = throwM . PrettyException
