{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-missing-fields -Wno-name-shadowing -Wno-unused-imports #-}

-- | Settings needed for running the GHC Parser.
module Exon.Haskell.Settings (baseDynFlags) where

import Data.Data hiding (Fixity)
import Data.Maybe
import GHC.Data.FastString
import GHC.Data.StringBuffer
import GHC.Driver.Backpack.Syntax
import GHC.Driver.Config
import GHC.Driver.Session
import GHC.Hs
import qualified GHC.Parser as Parser
import GHC.Parser.Lexer
import qualified GHC.Parser.Lexer as Lexer
import GHC.Parser.PostProcess
import GHC.Platform
import GHC.Settings
import GHC.Settings.Config
import GHC.Types.Fixity
import GHC.Types.Name
import GHC.Types.Name.Reader
import GHC.Types.SourceText
import GHC.Types.SrcLoc
import GHC.Unit.Info
import GHC.Utils.Fingerprint
import qualified Language.Haskell.TH.Syntax as GhcTH

fakeSettings :: Settings
fakeSettings = Settings
  { sGhcNameVersion=ghcNameVersion
  , sFileSettings=fileSettings
  , sTargetPlatform=platform
  , sPlatformMisc=platformMisc
  , sToolSettings=toolSettings
  }
  where
    toolSettings = ToolSettings {
      toolSettings_opt_P_fingerprint=fingerprint0
      }
    fileSettings = FileSettings {}
    platformMisc = PlatformMisc {}
    ghcNameVersion =
      GhcNameVersion{ghcNameVersion_programName="ghc"
                    ,ghcNameVersion_projectVersion=cProjectVersion
                    }
    platform =
      Platform{
    -- It doesn't matter what values we write here as these fields are
    -- not referenced for our purposes. However the fields are strict
    -- so we must say something.
        platformByteOrder=LittleEndian
      , platformHasGnuNonexecStack=True
      , platformHasIdentDirective=False
      , platformHasSubsectionsViaSymbols=False
      , platformIsCrossCompiling=False
      , platformLeadingUnderscore=False
      , platformTablesNextToCode=False
      , platform_constants=platformConstants
      ,

        platformWordSize=PW8
      , platformArchOS=ArchOS {archOS_arch=ArchUnknown, archOS_OS=OSUnknown}

#if MIN_VERSION_ghc(9, 4, 0)
      , platformHasLibm = False
#endif
      , platformUnregisterised=True
      }
    platformConstants = Nothing

#if MIN_VERSION_ghc(9, 6, 0)
applyFakeLlvmConfig :: a -> a
applyFakeLlvmConfig = id
#else
applyFakeLlvmConfig :: (LlvmConfig -> a) -> a
applyFakeLlvmConfig f = f $ LlvmConfig [] []
#endif

baseDynFlags :: [GhcTH.Extension] -> DynFlags
baseDynFlags exts =
  let enable = GhcTH.TemplateHaskellQuotes : exts
   in foldl xopt_set (applyFakeLlvmConfig (defaultDynFlags fakeSettings)) enable
