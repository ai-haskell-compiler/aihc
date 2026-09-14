{-# LANGUAGE OverloadedStrings #-}

-- | The headers of the emulated GHC installation.
--
-- A GHC installation ships @HsFFI.h@ and @MachDeps.h@, and writes
-- @ghcplatform.h@ and @ghcautoconf.h@ for its own host.  Package code reads
-- them from two sides: the C compiler reads them for a @c-sources@ file, for
-- the wrapper of a @capi@ import and for @hsc2hs@, and the CPP pass over the
-- Haskell sources reads them for a @.hs@ file.
--
-- Each header has one definition here, and both sides receive the same text.
-- The C compiler needs a file, so 'writeCompilerHeaders' puts the text in a
-- directory that the compile then gets as an include directory.  The CPP pass
-- takes the text through 'compilerHeader' and needs no file.
--
-- Nothing here reads the host that aihc runs on.  Every answer comes from the
-- 'HeaderTarget' of the target that the code is compiled for.
module Aihc.Hackage.Headers
  ( HeaderTarget (..),
    posix64HeaderTarget,
    compilerHeader,
    compilerHeaderFiles,
    writeCompilerHeaders,
    machineCppMacros,
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
-- The pointer and the Haskell word are two different sizes.  A heap slot and
-- an @Int#@ are eight bytes on every target, and a @wasm32@ pointer is four,
-- so a header must not derive one from the other.
data HeaderTarget = HeaderTarget
  { -- | @sizeof(void*)@ and @sizeof(unsigned long)@ of the target.
    headerPointerBytes :: !Int,
    -- | The Haskell word: the width of @Int#@ and of a heap slot.
    headerWordBytes :: !Int,
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

-- | The 64-bit POSIX target, which every native target of aihc matches.
--
-- A reader that has no target of its own, such as a documentation tool, takes
-- this one.
posix64HeaderTarget :: HeaderTarget
posix64HeaderTarget =
  HeaderTarget
    { headerPointerBytes = 8,
      headerWordBytes = 8,
      headerBigEndian = False,
      headerOs = "linux",
      headerArch = "x86_64"
    }

-- | The text of one header, for the CPP pass over a Haskell source.
--
-- @HsFFI.h@ is absent, although 'compilerHeaderFiles' writes it.  That header
-- is C type declarations only, and a Haskell source that includes it is
-- broken.  A missing include says so; the declarations would reach the parser
-- instead and report something else.
compilerHeader :: HeaderTarget -> FilePath -> Maybe Text
compilerHeader target path = case path of
  "ghcautoconf.h" -> Just (ghcautoconfHeader target)
  "ghcplatform.h" -> Just (ghcplatformHeader target)
  "MachDeps.h" -> Just (machDepsHeader target)
  -- Modern GHC's base package reduces this legacy header to a redirect.
  "HsBaseConfig.h" -> Just (header "HSBASECONFIG_H" ["#include \"ghcautoconf.h\""] [])
  _ -> Nothing

-- | Every header of the target, keyed by the name that includes it.
compilerHeaderFiles :: HeaderTarget -> [(FilePath, Text)]
compilerHeaderFiles target =
  [(path, text) | path <- ["ghcautoconf.h", "ghcplatform.h", "MachDeps.h", "HsBaseConfig.h"], Just text <- [compilerHeader target path]]
    <> [("HsFFI.h", hsFfiHeader target)]

-- | Write the headers of the target into a directory, for a C compile.
--
-- A header that is already there with the same text is left alone.  A compile
-- records the headers it read and rebuilds when one of them changes, so a
-- rewrite of the same bytes would rebuild every package that reads them.
--
-- A header that differs is written through a temporary file in the same
-- directory, so that a reader of the directory sees either the old text or
-- the new one.  Two compilers that write at the same time write the same
-- bytes.
writeCompilerHeaders :: HeaderTarget -> FilePath -> IO ()
writeCompilerHeaders target directory = do
  createDirectoryIfMissing True directory
  mapM_ writeHeader (compilerHeaderFiles target)
  where
    writeHeader (path, text) = do
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

-- | GHC writes this header from the results of its configure script.  aihc
-- runs no configure script, so feature macros stay undefined, which is what
-- GHC's header does for a feature that the host does not have.  What package
-- code reads from it, the word size and the byte order, comes from
-- @ghcplatform.h@.
ghcautoconfHeader :: HeaderTarget -> Text
ghcautoconfHeader _ = header "GHCAUTOCONF_H" ["#include \"ghcplatform.h\""] []

-- | The platform of the target: the host macros that Cabal also defines, the
-- C sizes and the byte order.
ghcplatformHeader :: HeaderTarget -> Text
ghcplatformHeader target =
  header
    "GHCPLATFORM_H"
    []
    ( [ (headerOs target <> "_HOST_OS", "1"),
        (headerArch target <> "_HOST_ARCH", "1"),
        ("SIZEOF_VOID_P", pointerBytes),
        ("SIZEOF_UNSIGNED_LONG", pointerBytes),
        ("SIZEOF_UNSIGNED_LONG_LONG", "8")
      ]
        <> [("WORDS_BIGENDIAN", "1") | headerBigEndian target]
    )
  where
    pointerBytes = tshow (headerPointerBytes target)

-- | The representation of the Haskell types, which is the same on a 32-bit
-- target as on a 64-bit one.
machDepsHeader :: HeaderTarget -> Text
machDepsHeader target =
  header "MACHDEPS_H" ["#include \"ghcplatform.h\""] (M.toList (machineCppMacros target))

-- | The C types that a foreign declaration names.
--
-- @HsInt@ follows the Haskell word and not the pointer, because that is the
-- width that a foreign import of @Int#@ passes.
hsFfiHeader :: HeaderTarget -> Text
hsFfiHeader target =
  T.unlines
    ( [ "#ifndef HSFFI_H",
        "#define HSFFI_H",
        "",
        "#include <stdint.h>",
        "",
        "#include \"MachDeps.h\"",
        "",
        "typedef int" <> wordBits <> "_t HsInt;",
        "typedef uint" <> wordBits <> "_t HsWord;"
      ]
        <> [ "typedef int8_t HsInt8;",
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
    )
  where
    wordBits = tshow (headerWordBytes target * 8)

-- | The sizes and alignments that @MachDeps.h@ defines, which the CPP pass
-- also passes as @-D@ macros.
machineCppMacros :: HeaderTarget -> Map Text Text
machineCppMacros target =
  M.fromList
    ( [("WORD_SIZE_IN_BITS", bits), ("WORD_SIZE_IN_BITS_FLOAT", bits <> ".0")]
        <> concatMap
          sizeAndAlignment
          [ ("HSCHAR", 4),
            ("HSINT", wordBytes),
            ("HSWORD", wordBytes),
            ("HSFLOAT", 4),
            ("HSDOUBLE", 8),
            ("HSPTR", pointerBytes),
            ("HSFUNPTR", pointerBytes),
            ("HSSTABLEPTR", pointerBytes),
            ("INT8", 1),
            ("WORD8", 1),
            ("INT16", 2),
            ("WORD16", 2),
            ("INT32", 4),
            ("WORD32", 4),
            ("INT64", 8),
            ("WORD64", 8)
          ]
    )
  where
    wordBytes = headerWordBytes target
    pointerBytes = headerPointerBytes target
    bits = tshow (wordBytes * 8)
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
