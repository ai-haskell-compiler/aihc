-- |
-- Module      : Aihc.Testing.CoreLibrarySource
-- Description : Read a core library's own sources the way the compiler does
--
-- The compiler never reads @core-libs@ off disk: it reaches a module through
-- the package plan, which runs the CPP pass over it and answers an
-- @#include@ from the headers written for the target.  Two readers outside
-- the compiler have no package plan and parse the sources directly -- the
-- evaluation fixtures and the interface comparison behind the API divergence
-- ratchet -- and to them a module that uses CPP is a parse error.
--
-- This is the piece those two share.  It belongs here rather than in the
-- compiler because the compiler has no use for it.
module Aihc.Testing.CoreLibrarySource
  ( preprocessCoreLibraryModule,
  )
where

import Aihc.Cpp qualified as Cpp
import Aihc.Hackage.Cpp (builtinCppMacros)
import Aihc.Hackage.Headers (HeaderTarget, compilerHeaderTexts)
import Aihc.Parser.Syntax (Extension (CPP), LanguageEdition (Haskell2010Edition), effectiveExtensions, headerExtensionSettings)
import Aihc.Parser.Token (readModuleHeaderPragmas)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error (lenientDecode)
import System.FilePath (normalise)

-- | Run the CPP pass over a core library module that asks for it, answering
-- its includes from the headers of one target.
--
-- The headers are the texts themselves rather than a directory: nothing here
-- needs them on disk, and taking the target as an argument keeps the reader
-- honest about which platform's numbers it is looking at.
--
-- @Foreign.C.Error@ is the module this exists for: it takes its error
-- numbers from the @CONST_E@/xxx/@ macros of @HsBaseConfig.h@.
preprocessCoreLibraryModule :: HeaderTarget -> FilePath -> Text -> Either String Text
preprocessCoreLibraryModule target path source
  | not (cppRequested source) = Right source
  | otherwise = drive (Cpp.preprocess config (TE.encodeUtf8 source))
  where
    headers = Map.fromList [(normalise name, TE.encodeUtf8 text) | (name, text) <- compilerHeaderTexts target]
    config =
      Cpp.defaultConfig
        { Cpp.configInputFile = path,
          Cpp.configMacros = Map.mapKeys TE.encodeUtf8 (Map.map TE.encodeUtf8 builtinCppMacros)
        }
    -- Only the compiler's own headers answer an include: a core library
    -- ships none of its own, and one that started to would have to be
    -- resolved against its @include-dirs@ as well, which is the package
    -- plan's job and not this one's.
    drive step =
      case step of
        Cpp.Done result -> Right (TE.decodeUtf8With lenientDecode (Cpp.resultOutput result))
        Cpp.NeedInclude request k ->
          case Map.lookup (normalise (Cpp.includePath request)) headers of
            Just content -> drive (k (Just content))
            Nothing -> Left (path <> ": the compiler writes no header " <> Cpp.includePath request)

-- | Whether a module turns CPP on.
--
-- No language edition carries CPP, so a pragma of the module is the only
-- thing that can, and the edition a reader would fold the pragmas into
-- cannot change the answer.
cppRequested :: Text -> Bool
cppRequested source =
  CPP `elem` effectiveExtensions Haskell2010Edition (headerExtensionSettings (readModuleHeaderPragmas source))
