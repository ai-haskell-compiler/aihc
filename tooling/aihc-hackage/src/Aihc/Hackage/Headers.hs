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
    defaultHeaderTarget,
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
import System.FilePath ((</>))
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

-- | The 64-bit POSIX target, for a reader that documents sources rather than
-- compiling them and so has no target of its own.
defaultHeaderTarget :: HeaderTarget
defaultHeaderTarget =
  HeaderTarget
    { headerPointerBytes = 8,
      headerLongBytes = 8,
      headerBigEndian = False,
      headerOs = "linux",
      headerArch = "x86_64"
    }

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
  [ ("ghcautoconf.h", header "GHCAUTOCONF_H" ["#include \"ghcplatform.h\""] []),
    -- Modern GHC's base package reduces this legacy header to a redirect.
    ("HsBaseConfig.h", header "HSBASECONFIG_H" ["#include \"ghcautoconf.h\""] []),
    ("ghcplatform.h", ghcplatformHeader target),
    ("MachDeps.h", machDepsHeader target),
    ("HsFFI.h", hsFfiHeader)
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
      current <- readHeader destination
      unless (current == Just text) $ do
        (temporary, handle) <- openBinaryTempFile directory (path <> ".tmp")
        TIO.hPutStr handle text
        hClose handle
        renameFile temporary destination
    readHeader path = do
      exists <- doesFileExist path
      if exists then Just <$> TIO.readFile path else pure Nothing

-- | The platform of the target: the host macros that Cabal also defines, the
-- C sizes and the byte order.
--
-- @ghcautoconf.h@ holds the results of GHC's configure script and includes
-- this header.  aihc runs no configure script, so feature macros stay
-- undefined, which is what GHC's header does for a feature that the host does
-- not have.
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

sizeAndAlignment :: (Text, Int) -> [(Text, Text)]
sizeAndAlignment (name, bytes) =
  [("SIZEOF_" <> name, tshow bytes), ("ALIGNMENT_" <> name, tshow bytes)]

header :: Text -> [Text] -> [(Text, Text)] -> Text
header guard includes definitions =
  T.unlines
    (["#ifndef " <> guard, "#define " <> guard] <> includes <> map define definitions <> ["#endif"])
  where
    define (name, value) = "#define " <> name <> " " <> value

tshow :: Int -> Text
tshow = T.pack . show
