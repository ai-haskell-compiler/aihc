{-# LANGUAGE OverloadedStrings #-}

-- | The headers of the emulated GHC installation.
--
-- A GHC installation ships @HsFFI.h@ and @MachDeps.h@, and writes
-- @ghcplatform.h@ and @ghcautoconf.h@ for its own host.  Package code reads
-- them from two sides: the C compiler reads them for a @c-sources@ file, for
-- the wrapper of a @capi@ import and for @hsc2hs@, and the CPP pass over the
-- Haskell sources reads them for a @.hs@ file.
--
-- Each header has one definition here.  'writeCompilerHeaders' puts them in a
-- directory, which the C compiles take as an include directory and the CPP
-- pass searches for a header that the package itself does not ship.  Both
-- sides thus read one file.
--
-- Nothing here reads the host that aihc runs on.  Every answer comes from the
-- 'HeaderTarget' of the target that the code is compiled for.
module Aihc.Hackage.Headers
  ( HeaderTarget (..),
    compilerHeaderTexts,
    writeCompilerHeaders,
    haskellWordCppMacros,
  )
where

import Control.Monad (unless)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (createDirectoryIfMissing, doesFileExist, renameFile)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.IO (hClose, openBinaryTempFile)

-- | What the headers of one target say.
--
-- Each width is its own field, because a platform is free to give them
-- different values.  A pointer is not a word: the x32 and AArch64 ILP32 ABIs
-- put 32-bit pointers on a 64-bit machine, and a CHERI pointer is wider than
-- the word.  A @long@ is not a pointer either: Windows keeps @long@ at four
-- bytes with eight-byte pointers.  The Haskell word is none of the three and
-- lives in 'haskellWordBytes'.
data HeaderTarget = HeaderTarget
  { -- | @sizeof(void*)@ of the target.
    headerPointerBytes :: !Int,
    -- | @sizeof(unsigned long)@ of the target.
    headerLongBytes :: !Int,
    -- | Whether the target stores the high byte first.
    headerBigEndian :: !Bool,
    -- | The operating system, spelled as the @<name>_HOST_OS@ macro spells
    -- it: @darwin@, @linux@, @wasi@.
    headerOs :: !Text,
    -- | The architecture, spelled as the @<name>_HOST_ARCH@ macro spells it:
    -- @aarch64@, @x86_64@, @wasm32@.
    headerArch :: !Text
  }
  deriving (Eq, Show)

-- | The width of @Int#@ and of a heap slot.  The code generator gives every
-- target the same one: @repType@ lowers @IntRep@ and @WordRep@ to @i64@, and
-- a heap slot is eight bytes even where a pointer is four.
haskellWordBytes :: Int
haskellWordBytes = 8

-- | The macros that describe the Haskell word, which the CPP pass defines for
-- every module as GHC defines them from its own @MachDeps.h@.
haskellWordCppMacros :: Map Text Text
haskellWordCppMacros =
  M.fromList
    ( [("WORD_SIZE_IN_BITS", bits), ("WORD_SIZE_IN_BITS_FLOAT", bits <> ".0")]
        <> concatMap sizeAndAlignment [("HSINT", haskellWordBytes), ("HSWORD", haskellWordBytes), ("HSCHAR", 4), ("HSFLOAT", 4), ("HSDOUBLE", 8)]
    )
  where
    bits = tshow (haskellWordBytes * 8)

-- | Every header of the target, keyed by the name that includes it.
compilerHeaderTexts :: HeaderTarget -> [(FilePath, Text)]
compilerHeaderTexts target =
  [ ("ghcautoconf.h", ghcautoconfHeader target),
    -- Modern GHC's base package reduces this legacy header to a redirect.
    ("HsBaseConfig.h", header "HSBASECONFIG_H" ["#include \"ghcautoconf.h\""] []),
    ("ghcplatform.h", ghcplatformHeader target),
    ("MachDeps.h", machDepsHeader target),
    ("HsFFI.h", hsFfiHeader),
    ("Stg.h", stgHeader),
    ("Rts.h", rtsHeader),
    ("rts" </> "Signals.h", rtsSignalsHeader)
  ]

-- | Write the headers of the target under a root directory and give back the
-- directory that holds them.
--
-- A header that is already there with the same text is left alone.  A compile
-- records the headers it read and rebuilds when one of them changes, so a
-- rewrite of the same bytes would rebuild every package that reads them.
--
-- A header that differs is written through a temporary file in the same
-- directory, so that a reader sees either the old text or the new one.  Two
-- compilers that write at the same time write the same bytes.
writeCompilerHeaders :: HeaderTarget -> FilePath -> IO FilePath
writeCompilerHeaders target root = do
  let directory = root </> "include"
  createDirectoryIfMissing True directory
  mapM_ (writeHeader directory) (compilerHeaderTexts target)
  pure directory
  where
    writeHeader directory (path, text) = do
      let destination = directory </> path
      -- A header may sit in a subdirectory, because the name that includes
      -- it does: @rts/Signals.h@ is included under that name.
      createDirectoryIfMissing True (takeDirectory destination)
      current <- readHeader destination
      unless (current == Just text) $ do
        (temporary, handle) <- openBinaryTempFile (takeDirectory destination) (takeFileName path <> ".tmp")
        TIO.hPutStr handle text
        hClose handle
        renameFile temporary destination
    readHeader path = do
      exists <- doesFileExist path
      if exists then Just <$> TIO.readFile path else pure Nothing

-- | Supply the header features that the selected target supports.
ghcautoconfHeader :: HeaderTarget -> Text
ghcautoconfHeader target =
  header
    "GHCAUTOCONF_H"
    ["#include \"ghcplatform.h\""]
    [("HAVE_DLFCN_H", "1") | headerOs target `elem` ["darwin", "linux"]]

-- | The platform macros, C sizes, and byte order of the target.
ghcplatformHeader :: HeaderTarget -> Text
ghcplatformHeader target =
  header
    "GHCPLATFORM_H"
    []
    ( [ (headerOs target <> "_HOST_OS", "1"),
        (headerArch target <> "_HOST_ARCH", "1"),
        ("SIZEOF_VOID_P", tshow (headerPointerBytes target)),
        ("SIZEOF_UNSIGNED_LONG", tshow (headerLongBytes target)),
        ("SIZEOF_UNSIGNED_LONG_LONG", "8")
      ]
        <> [("WORDS_BIGENDIAN", "1") | headerBigEndian target]
    )

-- | The sizes and alignments of the Haskell types.  A pointer follows the
-- target and everything else follows the Haskell word.
machDepsHeader :: HeaderTarget -> Text
machDepsHeader target =
  header "MACHDEPS_H" ["#include \"ghcplatform.h\""] (M.toList (M.union haskellWordCppMacros pointerMacros))
  where
    pointerMacros =
      M.fromList
        ( concatMap
            sizeAndAlignment
            ( [(name, headerPointerBytes target) | name <- ["HSPTR", "HSFUNPTR", "HSSTABLEPTR"]]
                <> [("INT8", 1), ("WORD8", 1), ("INT16", 2), ("WORD16", 2), ("INT32", 4), ("WORD32", 4), ("INT64", 8), ("WORD64", 8)]
            )
        )

-- | The C types that a foreign declaration names.
--
-- @HsInt@ follows the Haskell word and not the pointer, because that is the
-- width a foreign import of @Int#@ passes.
hsFfiHeader :: Text
hsFfiHeader =
  T.unlines
    [ "#ifndef HSFFI_H",
      "#define HSFFI_H",
      "",
      "#include \"ghcautoconf.h\"",
      "#include <stdint.h>",
      "",
      "#include \"MachDeps.h\"",
      "",
      "typedef int" <> wordBits <> "_t HsInt;",
      "typedef uint" <> wordBits <> "_t HsWord;",
      "typedef int8_t HsInt8;",
      "typedef int16_t HsInt16;",
      "typedef int32_t HsInt32;",
      "typedef int64_t HsInt64;",
      "typedef uint8_t HsWord8;",
      "typedef uint16_t HsWord16;",
      "typedef uint32_t HsWord32;",
      "typedef uint64_t HsWord64;",
      "typedef float HsFloat;",
      "typedef double HsDouble;",
      "typedef int HsBool;",
      "typedef uint32_t HsChar;",
      "typedef void *HsPtr;",
      "typedef void (*HsFunPtr)(void);",
      "typedef void *HsStablePtr;",
      "",
      "#endif"
    ]
  where
    wordBits = tshow (haskellWordBytes * 8)

-- | The action codes @stg_sig_install@ takes, which is all GHC's own
-- @rts/Signals.h@ holds: the header carries no prototypes, because Haskell
-- code includes it through the CPP pass rather than through a C compile.
--
-- @unix@ reads it that way. @System.Posix.Signals.hsc@ emits an
-- @#include "rts/Signals.h"@ into the module it generates and then names
-- @STG_SIG_DFL@ and its siblings in Haskell expressions; the codes it passes
-- to @stg_sig_install@ and reads back from it are this header's whole
-- contribution.
rtsSignalsHeader :: Text
rtsSignalsHeader =
  header
    "RTS_SIGNALS_H"
    []
    [ ("STG_SIG_DFL", "(-1)"),
      ("STG_SIG_IGN", "(-2)"),
      ("STG_SIG_ERR", "(-3)"),
      ("STG_SIG_HAN", "(-4)"),
      ("STG_SIG_RST", "(-5)")
    ]

-- | The C names of the Haskell types as the RTS spells them.
--
-- GHC's @Stg.h@ is the first header of the code generator's world: it also
-- holds the STG register model, the closure macros and the tables the
-- generated code reads.  None of that describes aihc's heap, so the header
-- carries only the type names, each an alias of the @HsFFI.h@ type with the
-- same width.  Package C code that names @StgInt@ or @StgWord@ to match a
-- foreign import compiles against these; code that dereferences a closure
-- does not compile, which is the right answer, since it could not run.
stgHeader :: Text
stgHeader =
  headerLines
    "STG_H"
    ["#include \"HsFFI.h\""]
    [ "typedef HsInt StgInt;",
      "typedef HsWord StgWord;",
      "typedef HsInt8 StgInt8;",
      "typedef HsInt16 StgInt16;",
      "typedef HsInt32 StgInt32;",
      "typedef HsInt64 StgInt64;",
      "typedef HsWord8 StgWord8;",
      "typedef HsWord16 StgWord16;",
      "typedef HsWord32 StgWord32;",
      "typedef HsWord64 StgWord64;",
      "typedef HsChar StgChar;",
      "typedef HsBool StgBool;",
      "typedef HsFloat StgFloat;",
      "typedef HsDouble StgDouble;",
      "typedef HsPtr StgAddr;",
      "typedef StgWord *StgPtr;",
      "typedef HsFunPtr StgFunPtr;",
      "typedef HsStablePtr StgStablePtr;"
    ]

-- | The API of the runtime, as GHC's @Rts.h@ presents it to package C code.
--
-- GHC's header is the umbrella over the whole runtime: the storage manager,
-- the scheduler, the capabilities and the closure layouts.  Package code
-- includes it for a few entry points and nothing else.  @unix@, @process@,
-- @posix-pty@ and @rawfilepath@ stop the interval timer and block the user
-- signals around @fork@, so that the child sees neither; @basement@ and
-- @byteslice@ want the @Stg*@ types.  The header declares those entry points
-- and stops there.  A package that reaches for a closure layout fails to
-- compile rather than link against a runtime whose heap looks different.
--
-- aihc's runtime defines every function declared here, in
-- @aihc_runtime.c@.  It runs no interval timer and installs no signal
-- handlers of its own, so the timer and signal calls do nothing, and there is
-- no threaded runtime to support bound threads.
rtsHeader :: Text
rtsHeader =
  headerLines
    "RTS_H"
    ["#include \"Stg.h\"", "#include \"rts/Signals.h\""]
    [ "void startTimer(void);",
      "void stopTimer(void);",
      "void blockUserSignals(void);",
      "void unblockUserSignals(void);",
      "HsBool rtsSupportsBoundThreads(void);"
    ]

sizeAndAlignment :: (Text, Int) -> [(Text, Text)]
sizeAndAlignment (name, bytes) =
  [("SIZEOF_" <> name, tshow bytes), ("ALIGNMENT_" <> name, tshow bytes)]

header :: Text -> [Text] -> [(Text, Text)] -> Text
header guard includes definitions = headerLines guard includes (map define definitions)
  where
    define (name, value) = "#define " <> name <> " " <> value

-- | A guarded header whose body is arbitrary C lines.
headerLines :: Text -> [Text] -> [Text] -> Text
headerLines guard includes body =
  T.unlines
    (["#ifndef " <> guard, "#define " <> guard] <> includes <> body <> ["#endif"])

tshow :: Int -> Text
tshow = T.pack . show
