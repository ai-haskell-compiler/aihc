{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
#if defined(mingw32_HOST_OS)
{-# LANGUAGE RecordWildCards #-}
#endif

module Test.Main.Internal where


import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as B
import           GHC.Generics (Generic)
import           System.Exit (ExitCode)



-- | Used for the result of 'Test.Main.captureProcessResult'.
data ProcessResult =
  ProcessResult
    { prStdout :: !B.ByteString
    , prStderr :: !B.ByteString
    , prExitCode :: !ExitCode
    , prException :: !(Maybe E.SomeException)
    } deriving (Show, Generic)

-- NOTE: SomeException is not Eq! So can't derive!
instance Eq ProcessResult where
  pr1 == pr2 =
    prStdout pr1 == prStdout pr2
      && prStderr pr1 == prStderr pr2
      && prStderr pr1 == prStderr pr2
      && prExitCode pr1 == prExitCode pr2
      && fmap show (prException pr1) == fmap show (prException pr2)


-- | Use to avoid errors in related to new line code in tests.
--   Currently I use this function only for this module's test.
normalizeNewLines :: ProcessResult -> ProcessResult
#if defined(mingw32_HOST_OS)
normalizeNewLines ProcessResult {..} =
  ProcessResult (nl prStdout) (nl prStderr) prExitCode prException
  where
    nl = B.concat . B.split '\r'
#else
normalizeNewLines = id
#endif
