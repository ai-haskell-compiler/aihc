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

import Control.DeepSeq (NFData)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import GHC.Generics (Generic)

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
  { includePath :: !FilePath,
    includeKind :: !IncludeKind,
    includeFrom :: !FilePath,
    includeLine :: !Int
  }
  deriving (Eq, Show, Generic, NFData)

-- | Severity level for diagnostics.
data Severity = Warning | Error deriving (Eq, Show, Generic, NFData)

-- | A diagnostic message emitted during preprocessing.
data Diagnostic = Diagnostic
  { diagSeverity :: !Severity,
    diagMessage :: !Text,
    diagFile :: !FilePath,
    diagLine :: !Int
  }
  deriving (Eq, Show, Generic, NFData)

-- | The result of preprocessing.
data Result = Result
  { resultOutput :: !Text,
    resultDiagnostics :: ![Diagnostic]
  }
  deriving (Eq, Show, Generic, NFData)

-- | A step in the preprocessing process.
data Step
  = Done !Result
  | NeedInclude !IncludeRequest !(Maybe Text -> Step)

data EngineState = EngineState
  { stMacros :: !(Map Text MacroDef),
    stOutputRev :: ![Text],
    stDiagnosticsRev :: ![Diagnostic],
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
      stOutputRev = [],
      stDiagnosticsRev = [],
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
    lcRestLines :: ![(Int, Int, Text)],
    lcStack :: ![CondFrame],
    lcContinue :: EngineState -> Step,
    lcContinueWith :: [CondFrame] -> EngineState -> Step,
    lcDone :: Continuation
  }
