{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Aihc.Cpp.Types
  ( Config (..),
    MacroDef (..),
    defaultConfig,
    IncludeKind (..),
    IncludeRequest (..),
    Severity (..),
    Diagnostic (..),
    Result (..),
    Step (..),
    EngineState (..),
    emptyState,
    CondFrame (..),
    currentActive,
    mkFrame,
    Continuation,
    LineContext (..),
  )
where

import Aihc.Cpp.Cursor (Cursor)
import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text.Lazy.Builder as TB
import GHC.Generics (Generic)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import qualified Data.Map.Strict as M

-- | Configuration for the C preprocessor.
data Config = Config
  { -- | The name of the input file, used in @#line@ directives and
    -- @__FILE__@ expansion.
    configInputFile :: FilePath,
    -- | User-defined macros. These are expanded as object-like macros.
    -- Note that the values should include any necessary quoting. For
    -- example, to define a string macro, use @"\"value\""@.
    configMacros :: !(Map Text Text)
  }

data MacroDef
  = ObjectMacro !Text
  | FunctionMacro ![Text] !Text
  deriving (Eq, Show)

-- | Default configuration with sensible defaults.
--
-- * 'configInputFile' is set to @\"\<input\>\"@
-- * 'configMacros' includes @__DATE__@ and @__TIME__@ set to the Unix epoch
--
-- To customize the date and time macros:
--
-- >>> import qualified Data.Map.Strict as M
-- >>> let cfg = defaultConfig { configMacros = M.fromList [("__DATE__", "\"Mar 15 2026\""), ("__TIME__", "\"14:30:00\"")] }
-- >>> configMacros cfg
-- fromList [("__DATE__","\"Mar 15 2026\""),("__TIME__","\"14:30:00\"")]
--
-- To add additional macros while keeping the defaults:
--
-- >>> import qualified Data.Map.Strict as M
-- >>> let cfg = defaultConfig { configMacros = M.insert "VERSION" "42" (configMacros defaultConfig) }
-- >>> M.lookup "VERSION" (configMacros cfg)
-- Just "42"
defaultConfig :: Config
defaultConfig =
  Config
    { configInputFile = "<input>",
      configMacros =
        M.fromList
          [ ("__DATE__", "\"Jan  1 1970\""),
            ("__TIME__", "\"00:00:00\"")
          ]
    }

-- | The kind of @#include@ directive.
data IncludeKind = IncludeLocal | IncludeSystem deriving (Eq, Show, Generic, NFData)

-- | Information about a pending @#include@ that needs to be resolved.
data IncludeRequest = IncludeRequest
  { -- | The path specified in the include directive.
    includePath :: !FilePath,
    -- | Whether this is a local (@\"...\"@) or system (@\<...\>@) include.
    includeKind :: !IncludeKind,
    -- | The file that contains the @#include@ directive.
    includeFrom :: !FilePath,
    -- | The line number of the @#include@ directive.
    includeLine :: !Int
  }
  deriving (Eq, Show, Generic, NFData)

-- | Severity level for diagnostics.
data Severity = Warning | Error deriving (Eq, Show, Generic, NFData)

-- | A diagnostic message emitted during preprocessing.
data Diagnostic = Diagnostic
  { -- | The severity of the diagnostic.
    diagSeverity :: !Severity,
    -- | The diagnostic message text.
    diagMessage :: !Text,
    -- | The file where the diagnostic occurred.
    diagFile :: !FilePath,
    -- | The line number where the diagnostic occurred.
    diagLine :: !Int
  }
  deriving (Eq, Show, Generic, NFData)

-- | The result of preprocessing.
data Result = Result
  { -- | The preprocessed output text.
    resultOutput :: !Text,
    -- | Any diagnostics (warnings or errors) emitted during preprocessing.
    resultDiagnostics :: ![Diagnostic]
  }
  deriving (Eq, Show, Generic, NFData)

-- | A step in the preprocessing process. Either preprocessing is complete
-- ('Done') or an @#include@ directive needs to be resolved ('NeedInclude').
data Step
  = -- | Preprocessing is complete.
    Done !Result
  | -- | An @#include@ directive was encountered. The caller must provide
    -- the contents of the included file (or 'Nothing' if not found),
    -- and preprocessing will continue.
    NeedInclude !IncludeRequest !(Maybe ByteString -> Step)

data EngineState = EngineState
  { stMacros :: !(Map Text MacroDef),
    stOutput :: !TB.Builder,
    stOutputLineCount :: {-# UNPACK #-} !Int,
    stDiagnosticsRev :: ![Diagnostic],
    stPragmaOnceFiles :: !(Set FilePath),
    stSkippingDanglingElse :: !Bool,
    stHsBlockCommentDepth :: !Int,
    stCBlockCommentDepth :: !Int,
    stCurrentFile :: !FilePath,
    stCurrentLine :: !Int
  }

emptyState :: FilePath -> EngineState
emptyState filePath =
  EngineState
    { stMacros = M.empty,
      stOutput = mempty,
      stOutputLineCount = 0,
      stDiagnosticsRev = [],
      stPragmaOnceFiles = S.empty,
      stSkippingDanglingElse = False,
      stHsBlockCommentDepth = 0,
      stCBlockCommentDepth = 0,
      stCurrentFile = filePath,
      stCurrentLine = 1
    }

data CondFrame = CondFrame
  { frameOuterActive :: !Bool,
    frameConditionTrue :: !Bool,
    frameInElse :: !Bool,
    frameCurrentActive :: !Bool
  }

currentActive :: [CondFrame] -> Bool
currentActive [] = True
currentActive (f : _) = frameCurrentActive f

mkFrame :: Bool -> Bool -> CondFrame
mkFrame outer cond =
  CondFrame
    { frameOuterActive = outer,
      frameConditionTrue = cond,
      frameInElse = False,
      frameCurrentActive = outer && cond
    }

type Continuation = EngineState -> Step

data LineContext = LineContext
  { lcFilePath :: !FilePath,
    lcLineNo :: !Int,
    lcLineSpan :: !Int,
    lcNextLineNo :: !Int,
    -- | Cursor positioned after the current line (past its newline).
    lcRestCursor :: !Cursor,
    lcStack :: ![CondFrame],
    lcContinue :: EngineState -> Step,
    lcContinueWith :: [CondFrame] -> EngineState -> Step,
    lcDone :: Continuation
  }
