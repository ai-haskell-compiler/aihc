{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}

module System.Nagios.Plugin.Check
(
    CheckStatus(..),
    CheckResult,
    NagiosPlugin,
    runNagiosPlugin,
    runNagiosPlugin',
    addPerfDatum,
    addPerfData,
    addBarePerfDatum,
    addResult,
    checkStatus,
    checkInfo,
    worstResult,
    finishState
) where

import           Control.Applicative
import qualified Control.Monad.Catch           as E
import           Control.Monad.State.Lazy
import           Data.Bifunctor
import           Data.Monoid
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           System.Exit

import           System.Nagios.Plugin.PerfData

-- | Nagios plugin exit statuses. Ordered by priority -
--   'OK' < 'Warning' < 'Critical' < 'Unknown', which correspond to plugin exit
--   statuses of 0, 1, 2, and 3 respectively.
data CheckStatus = OK       -- ^ Check executed successfully and
                            --   detected no service problems.
                 | Warning  -- ^ Nothing's actually broken but this
                            --   should be followed up.
                 | Critical -- ^ Check executed successfully and detected
                            --   a service failure.
                 | Unknown  -- ^ Check unable to determine service
                            --   status.
  deriving (Enum, Eq, Ord)

instance Show CheckStatus where
    show OK = "OK"
    show Warning = "WARNING"
    show Critical = "CRITICAL"
    show Unknown = "UNKNOWN"

-- | A CheckResult is the exit status of the plugin combined with the
--   plugin's info text. A `NagiosPlugin` which exits with
--
--   > CheckResult (Critical "entropy decreasing in closed system")
--
--   as its peak-badness CheckResult (and no 'PerfDatum's) will a) exit with
--   status 2 and b) output the text "CRITICAL: entropy decreasing in closed
--   system".
newtype CheckResult = CheckResult
  { unCheckResult :: (CheckStatus, Text) }
    deriving (Eq, Ord, Show)

-- | Extract the return status from a 'CheckResult'.
checkStatus :: CheckResult -> CheckStatus
checkStatus = fst . unCheckResult

-- | Extract the infotext from a 'CheckResult'.
checkInfo :: CheckResult -> Text
checkInfo = snd . unCheckResult

-- | Current check results/perfdata. If the check suddenly dies, the
--   'worst' of the CheckResults (and all the PerfDatums) will be used
--   to determine the exit state.
type CheckState = ([CheckResult], [PerfDatum])

newtype NagiosPlugin a = NagiosPlugin
  {
    unNagiosPlugin :: StateT CheckState IO a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadState CheckState, E.MonadCatch, E.MonadThrow)

-- | Execute a Nagios check. The program will terminate at the check's
--   completion. A default status will provided if none is given.
runNagiosPlugin :: NagiosPlugin a -> IO ()
runNagiosPlugin check = do
    (_, st) <- runNagiosPlugin' $ E.catch check panic
    finishWith st
  where
    panic :: E.SomeException -> NagiosPlugin a
    panic = liftIO . finishWith . panicState

-- | Execute a Nagios check as with 'runNagiosPlugin', but return its
--   final state rather than terminating.
runNagiosPlugin' :: NagiosPlugin a -> IO (a, CheckState)
runNagiosPlugin' a = runStateT (unNagiosPlugin a) mempty

-- | Insert a result. Only the 'CheckStatus' with the most 'badness'
--   will determine the check's exit status.
addResult :: CheckStatus -> Text -> NagiosPlugin ()
addResult s t =
    modify (first (CheckResult (s, t) :))

-- | Insert a performance metric into the list the check will output.
addPerfDatum ::
       Text            -- ^ Name of the quantity being measured.
    -> PerfValue       -- ^ Measured value.
    -> UOM             -- ^ Unit of the measured value.
    -> Maybe PerfValue -- ^ Minimum threshold.
    -> Maybe PerfValue -- ^ Maximum threshold.
    -> Maybe PerfValue -- ^ Warning threshold.
    -> Maybe PerfValue -- ^ Critical threshold.
    -> NagiosPlugin ()
addPerfDatum info val uom min' max' warn crit =
    modify (second (PerfDatum info val uom min' max' warn crit :))

-- | Convenience function to insert a perfdatum without thresholds for
--   min, max, warn or crit. Note that unless the range of the metric is
--   actually unbounded, specifying explicit thresholds is considered
--   good practice (it makes life easier for authors of graphing
--   packages).
--
--   FIXME: implement thresholds properly and default to negative and
--          positive infinity for min and max here.
addBarePerfDatum ::
       Text            -- ^ Name of the quantity being measured.
    -> PerfValue       -- ^ Measured value.
    -> UOM             -- ^ Unit of the measured value.
    -> NagiosPlugin ()
addBarePerfDatum info val uom =
    addPerfDatum info val uom Nothing Nothing Nothing Nothing

-- | Alternative mechanism for adding perfdata generated from complex
--   types; just implement the 'toPerfData' typeclass.
addPerfData ::
       ToPerfData a
    => a
    -> NagiosPlugin ()
addPerfData pd = modify (second (++ toPerfData pd))

-- | The result which will be used if no other results have been
--   provided.
defaultResult :: CheckResult
defaultResult = CheckResult (Unknown, T.pack "no check result specified")

-- | The state the plugin will exit with if an uncaught exception occurs.
--   within the plugin.
panicState :: E.SomeException -> CheckState
panicState = (,[]) . return . CheckResult . panicResult
  where
    panicResult e = (Critical,
                    T.pack ("unhandled exception: " ++ show e))

-- | Returns result with greatest badness, or a default UNKNOWN result
--   if no results have been specified.
worstResult :: [CheckResult] -> CheckResult
worstResult rs
    | null rs = defaultResult
    | otherwise = maximum rs

-- | Render a plugin's performance data according to the
--   <https://nagios-plugins.org/doc/guidelines.html Nagios plugin development guidelines>.
fmtPerfData :: [PerfDatum] -> Text
fmtPerfData = T.intercalate " " . map fmtPerfDatum
  where
    fmtPerfDatum PerfDatum{..} = T.concat
        [ _label
        , "="
        , T.pack (show _value)
        , T.pack (show _uom)
        , fmtThreshold _warn
        , fmtThreshold _crit
        , fmtThreshold _min
        , fmtThreshold _max
        ]

    fmtThreshold Nothing = ";"
    fmtThreshold (Just t) = T.pack $ ";" <> show t

-- | Render a plugin's result according to the
--   <https://nagios-plugins.org/doc/guidelines.html Nagios plugin development guidelines>.
--
--   FIXME: not actually true yet, need to implement verbose output.
fmtResults :: [CheckResult] -> Text
fmtResults = fmtResult . worstResult
  where
    fmtResult (CheckResult (s,t)) =
        T.pack (show s) <> ": " <> t

-- | Given a check's final state, return the status and output it would
--   exit with.
finishState :: CheckState -> (CheckStatus, Text)
finishState (rs, pds) =
    let worst  = worstResult rs
        output = fmtResults rs <> perfdataPart pds
    in (checkStatus worst, output)
  where
    perfdataPart [] = ""
    perfdataPart xs = " | " <> fmtPerfData xs

-- | Calculate our final result, print output and then exit with the
--   appropriate status.
finishWith :: MonadIO m => CheckState -> m a
finishWith = liftIO . exitWithStatus . finishState

-- | Output the final check result to stdout and then terminate the
--   check program with the appropriate exit status.
exitWithStatus :: (CheckStatus, Text) -> IO a
exitWithStatus (OK, t) = T.putStrLn t >> exitSuccess
exitWithStatus (r, t) = T.putStrLn t >> exitWith (ExitFailure $ fromEnum r)
