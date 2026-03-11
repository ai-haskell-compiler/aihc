{-# LANGUAGE NoImplicitPrelude #-}

{-|
This module exports a 'SimplePrettyApp' type, for providing a basic environment
including pretty printing functionality.
-}
module RIO.PrettyPrint.Simple
  ( SimplePrettyApp
  , mkSimplePrettyApp
  , runSimplePrettyApp
  ) where

import           RIO
                   ( Bool (..), HasLogFunc (..), Int, LogFunc, Maybe (..)
                   , MonadIO, RIO, ($), (<$>), isJust, lens, liftIO
                   , logOptionsHandle, maybe, pure, runRIO, setLogUseColor
                   , stderr, withLogFunc
                   )
import           RIO.PrettyPrint ( HasTerm (..) )
import           RIO.PrettyPrint.StylesUpdate
                   ( HasStylesUpdate (..), StylesUpdate (..) )
import           RIO.Process
                   ( HasProcessContext (..), ProcessContext
                   , mkDefaultProcessContext
                   )
import           System.Environment ( lookupEnv )

-- | A simple, non-customizable environment type, which provides
-- pretty printing functionality.
--
-- @since 0.1.3.0
data SimplePrettyApp = SimplePrettyApp
  { spaLogFunc :: !LogFunc
  , spaProcessContext :: !ProcessContext
  , spaUseColor :: !Bool
  , spaTermWidth :: !Int
  , spaStylesUpdate :: !StylesUpdate
  }

instance HasLogFunc SimplePrettyApp where
  logFuncL = lens spaLogFunc (\x y -> x { spaLogFunc = y })

instance HasProcessContext SimplePrettyApp where
  processContextL = lens spaProcessContext (\x y -> x { spaProcessContext = y })

instance HasStylesUpdate SimplePrettyApp where
  stylesUpdateL = lens spaStylesUpdate (\x y -> x { spaStylesUpdate = y })

instance HasTerm SimplePrettyApp where
  useColorL = lens spaUseColor (\x y -> x { spaUseColor = y })
  termWidthL = lens spaTermWidth (\x y -> x { spaTermWidth = y })

-- | Constructor for 'SimplePrettyApp'. If 'ProcessContext' is not supplied
-- 'mkDefaultProcessContext' will be used to create it.
--
-- @since 0.1.3.0
mkSimplePrettyApp ::
     MonadIO m
  => LogFunc
  -> Maybe ProcessContext
  -> Bool
     -- ^ Use color?
  -> Int
     -- ^ Terminal width
  -> StylesUpdate
  -> m SimplePrettyApp
mkSimplePrettyApp logFunc mProcessContext useColor termWidth stylesUpdate = do
  processContext <- maybe mkDefaultProcessContext pure mProcessContext
  pure $ SimplePrettyApp
    { spaLogFunc = logFunc
    , spaProcessContext = processContext
    , spaUseColor = useColor
    , spaTermWidth = termWidth
    , spaStylesUpdate = stylesUpdate
    }

-- | Run with a default configured @SimplePrettyApp@, consisting of:
--
-- * Logging to 'stderr'
--
-- * If the @RIO_VERBOSE@ environment variable is set, turns on verbose logging
--
-- * Default process context
--
-- * Logging using color
--
-- @since 0.1.3.0
runSimplePrettyApp ::
     MonadIO m
  => Int
     -- ^ Terminal width
  -> StylesUpdate
  -> RIO SimplePrettyApp a
  -> m a
runSimplePrettyApp termWidth stylesUpdate m = liftIO $ do
  verbose <- isJust <$> lookupEnv "RIO_VERBOSE"
  lo <- setLogUseColor True <$> logOptionsHandle stderr verbose
  withLogFunc lo $ \lf -> do
    simplePrettyApp <- mkSimplePrettyApp lf Nothing True termWidth stylesUpdate
    runRIO simplePrettyApp m
