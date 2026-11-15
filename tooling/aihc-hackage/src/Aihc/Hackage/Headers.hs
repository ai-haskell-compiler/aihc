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
    -- GHC uses this header to include both target configuration headers.
    ("ghcconfig.h", header "GHCCONFIG_H" ["#include \"ghcautoconf.h\"", "#include \"ghcplatform.h\""] []),
    ("HsBaseConfig.h", header "HSBASECONFIG_H" ["#include \"ghcautoconf.h\""] (baseTypeMacros target)),
    ("ghcplatform.h", ghcplatformHeader target),
    ("MachDeps.h", machDepsHeader target),
    ("HsFFI.h", hsFfiHeader),
    ("Stg.h", stgHeader),
    ("Rts.h", rtsHeader),
    ("rts" </> "Signals.h", rtsSignalsHeader)
  ]
    <> (if headerOs target == "wasi" then wasiCompatibilityHeaders else [])

-- | Headers that replace or extend internal headers of the WASI libc.
--
-- WASI has no UNIX-domain sockets and no terminals, so the libc leaves out
-- some declarations that packages read at compile time. @network@ reads
-- @sun_path@ of @struct sockaddr_un@, and @terminal-size@ reads
-- @struct winsize@ and @TIOCGWINSZ@. The Template Haskell engine compiles
-- these packages for WebAssembly when a splice imports them. The engine
-- gives a splice no sockets and no terminal, so a call fails at run time,
-- but the code compiles. The compiler header directory comes before the
-- sysroot in the include path, so these headers replace the headers of the
-- libc with the same names.
wasiCompatibilityHeaders :: [(FilePath, Text)]
wasiCompatibilityHeaders =
  [ -- The libc includes this header from @sys/un.h@. Its own copy has only
    -- @sun_family@. This copy has the same guard and the field of musl.
    ( "__struct_sockaddr_un.h",
      headerLines
        "__wasilibc___struct_sockaddr_un_h"
        ["#include <__typedef_sa_family_t.h>"]
        [ "struct sockaddr_un {",
          "  __attribute__((aligned(__BIGGEST_ALIGNMENT__))) sa_family_t sun_family;",
          "  char sun_path[108];",
          "};"
        ]
    ),
    -- The libc includes this header from @sys/socket.h@. This copy includes
    -- the header of the libc, and adds the control messages and the
    -- functions of musl that WASI does not have.
    ( "__header_sys_socket.h",
      T.unlines
        [ "#include_next <__header_sys_socket.h>",
          "#ifndef AIHC_WASI_SOCKET_H",
          "#define AIHC_WASI_SOCKET_H",
          "#include <stddef.h>",
          "#include <__typedef_socklen_t.h>",
          "#include <__typedef_ssize_t.h>",
          "struct cmsghdr {",
          "  socklen_t cmsg_len;",
          "  int cmsg_level;",
          "  int cmsg_type;",
          "};",
          "#define __MHDR_END(mhdr) ((unsigned char *)(mhdr)->msg_control + (mhdr)->msg_controllen)",
          "#define __CMSG_LEN(cmsg) (((cmsg)->cmsg_len + sizeof(long) - 1) & ~(long)(sizeof(long) - 1))",
          "#define __CMSG_NEXT(cmsg) ((unsigned char *)(cmsg) + __CMSG_LEN(cmsg))",
          "#define CMSG_DATA(cmsg) ((unsigned char *)(((struct cmsghdr *)(cmsg)) + 1))",
          "#define CMSG_NXTHDR(mhdr, cmsg) ((cmsg)->cmsg_len < sizeof(struct cmsghdr) || __CMSG_LEN(cmsg) + sizeof(struct cmsghdr) >= __MHDR_END(mhdr) - (unsigned char *)(cmsg) ? 0 : (struct cmsghdr *)__CMSG_NEXT(cmsg))",
          "#define CMSG_FIRSTHDR(mhdr) ((size_t)(mhdr)->msg_controllen >= sizeof(struct cmsghdr) ? (struct cmsghdr *)(mhdr)->msg_control : (struct cmsghdr *)0)",
          "#define CMSG_ALIGN(len) (((len) + sizeof(size_t) - 1) & (size_t) ~(sizeof(size_t) - 1))",
          "#define CMSG_SPACE(len) (CMSG_ALIGN(len) + CMSG_ALIGN(sizeof(struct cmsghdr)))",
          "#define CMSG_LEN(len) (CMSG_ALIGN(sizeof(struct cmsghdr)) + (len))",
          "#define SCM_RIGHTS 0x01",
          "#define SCM_CREDENTIALS 0x02",
          "ssize_t sendmsg(int, const struct msghdr *, int);",
          "ssize_t recvmsg(int, struct msghdr *, int);",
          "int socketpair(int, int, int, int[2]);",
          "#endif"
        ]
    ),
    -- WASI has no threads that can stop, so the libc does not declare
    -- @pthread_exit@. The argon2 code of @crypton@ calls it.
    ( "pthread.h",
      T.unlines
        [ "#include_next <pthread.h>",
          "#ifndef AIHC_WASI_PTHREAD_H",
          "#define AIHC_WASI_PTHREAD_H",
          "_Noreturn void pthread_exit(void *);",
          "#endif"
        ]
    ),
    -- The libc includes this header from @sys/ioctl.h@. This copy includes
    -- the header of the libc, and adds the terminal size request of musl.
    -- The @ioctl@ of the libc gives an error for that request.
    ( "__header_sys_ioctl.h",
      T.unlines
        [ "#include_next <__header_sys_ioctl.h>",
          "#ifndef AIHC_WASI_IOCTL_H",
          "#define AIHC_WASI_IOCTL_H",
          "#ifndef __DEFINED_struct_winsize",
          "#define __DEFINED_struct_winsize",
          "struct winsize { unsigned short ws_row, ws_col, ws_xpixel, ws_ypixel; };",
          "#endif",
          "#define TIOCGWINSZ 0x5413",
          "#endif"
        ]
    )
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
--
-- The header also enables the system extensions, as the
-- @AC_USE_SYSTEM_EXTENSIONS@ block of GHC's @ghcautoconf.h@ does.  A C
-- library reads these macros at its first header only, and @HsFFI.h@
-- includes this header before any system header.  Without them, glibc does
-- not declare X/Open functions such as @ptsname@, and the capi wrappers of
-- @unix@ do not compile.  A package that sets a macro itself keeps its value.
ghcautoconfHeader :: HeaderTarget -> Text
ghcautoconfHeader target =
  headerLines
    "GHCAUTOCONF_H"
    ["#include \"ghcplatform.h\""]
    ( concatMap defineUnlessSet systemExtensions
        <> ["#define HAVE_" <> headerMacroName name <> " 1" | name <- systemHeaders]
    )
  where
    defineUnlessSet name = ["#ifndef " <> name, "#define " <> name <> " 1", "#endif"]
    -- GHC's @ghcautoconf.h@ gives a @HAVE_<NAME>_H@ macro for each system
    -- header that its configure script found. Some packages stop with an
    -- error when such a macro is not set: ngx-export requires
    -- @HAVE_FCNTL_H@. The list has the headers of GHC's file that each
    -- target has. The wasm sysroot also has @dlfcn.h@, but the compiler
    -- gives @HAVE_DLFCN_H@ for it on the command line.
    systemHeaders
      | headerOs target `elem` ["darwin", "linux"] = commonHeaders <> ["dlfcn.h", "grp.h", "pwd.h", "signal.h", "sys/mman.h", "sys/resource.h", "sys/times.h", "sys/wait.h", "termios.h"]
      | otherwise = commonHeaders
    -- The headers that all targets have. The wasm sysroot has more
    -- headers, but some of them only give an error.
    commonHeaders =
      [ "alloca.h",
        "ctype.h",
        "dirent.h",
        "errno.h",
        "fcntl.h",
        "inttypes.h",
        "limits.h",
        "locale.h",
        "pthread.h",
        "sched.h",
        "stdint.h",
        "stdio.h",
        "stdlib.h",
        "string.h",
        "strings.h",
        "sys/param.h",
        "sys/select.h",
        "sys/stat.h",
        "sys/time.h",
        "sys/timeb.h",
        "sys/types.h",
        "sys/utsname.h",
        "unistd.h",
        "utime.h",
        "wchar.h"
      ]
    headerMacroName = T.toUpper . T.map (\character -> if character == '/' || character == '.' then '_' else character)
    systemExtensions =
      [ "_ALL_SOURCE",
        "_DARWIN_C_SOURCE",
        "__EXTENSIONS__",
        "_GNU_SOURCE",
        "_HPUX_ALT_XOPEN_SOCKET_API",
        "_NETBSD_SOURCE",
        "_OPENBSD_SOURCE",
        "_POSIX_PTHREAD_SEMANTICS",
        "__STDC_WANT_IEC_60559_ATTRIBS_EXT__",
        "__STDC_WANT_IEC_60559_BFP_EXT__",
        "__STDC_WANT_IEC_60559_DFP_EXT__",
        "__STDC_WANT_IEC_60559_FUNCS_EXT__",
        "__STDC_WANT_IEC_60559_TYPES_EXT__",
        "__STDC_WANT_LIB_EXT2__",
        "__STDC_WANT_MATH_SPEC_FUNCS__",
        "_TANDEM_SOURCE"
      ]

-- | The platform macros, C sizes, and byte order of the target.
ghcplatformHeader :: HeaderTarget -> Text
ghcplatformHeader target =
  header
    "GHCPLATFORM_H"
    []
    ( [ (headerOs target <> "_HOST_OS", "1"),
        (headerArch target <> "_HOST_ARCH", "1")
      ]
        <> concatMap sizeAndAlignment cTypeSizes
        <> [("WORDS_BIGENDIAN", "1") | headerBigEndian target]
    )
  where
    -- GHC's @ghcautoconf.h@ gives the size and the alignment of each C type
    -- from @AC_CHECK_SIZEOF@ and @AC_CHECK_ALIGNOF@.  On each target of aihc,
    -- the alignment of a C type is the same as its size.
    cTypeSizes =
      [ ("VOID_P", headerPointerBytes target),
        ("LONG", headerLongBytes target),
        ("UNSIGNED_LONG", headerLongBytes target),
        ("LONG_LONG", 8),
        ("UNSIGNED_LONG_LONG", 8),
        ("CHAR", 1),
        ("UNSIGNED_CHAR", 1),
        ("SHORT", 2),
        ("UNSIGNED_SHORT", 2),
        ("INT", 4),
        ("UNSIGNED_INT", 4),
        ("FLOAT", 4),
        ("DOUBLE", 8),
        ("INT8_T", 1),
        ("UINT8_T", 1),
        ("INT16_T", 2),
        ("UINT16_T", 2),
        ("INT32_T", 4),
        ("UINT32_T", 4),
        ("INT64_T", 8),
        ("UINT64_T", 8)
      ]

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
  T.unlines $
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
      ""
    ]
      <> limitMacros
      <> ["", "#endif"]
  where
    wordBits = tshow (haskellWordBytes * 8)
    -- The Haskell 2010 report gives these symbols for the bounds of the
    -- types. GHC's header gives them too, and C code such as the cbits of
    -- vty uses them.
    limitMacros =
      [ "#include <float.h>",
        "",
        "#define HS_CHAR_MIN 0",
        "#define HS_CHAR_MAX 0x10FFFF",
        "#define HS_BOOL_FALSE 0",
        "#define HS_BOOL_TRUE 1",
        "#define HS_BOOL_MIN HS_BOOL_FALSE",
        "#define HS_BOOL_MAX HS_BOOL_TRUE",
        "#define HS_INT_MIN INT" <> wordBits <> "_MIN",
        "#define HS_INT_MAX INT" <> wordBits <> "_MAX",
        "#define HS_WORD_MAX UINT" <> wordBits <> "_MAX"
      ]
        <> concat
          [ [ "#define HS_INT" <> bits <> "_MIN INT" <> bits <> "_MIN",
              "#define HS_INT" <> bits <> "_MAX INT" <> bits <> "_MAX",
              "#define HS_WORD" <> bits <> "_MAX UINT" <> bits <> "_MAX"
            ]
          | bits <- ["8", "16", "32", "64"]
          ]
        <> [ "#define HS_FLOAT_RADIX FLT_RADIX",
             "#define HS_FLOAT_ROUNDS FLT_ROUNDS",
             "#define HS_FLOAT_ROUND FLT_ROUNDS"
           ]
        <> concat
          [ [ "#define HS_" <> haskell <> "_" <> suffix <> " " <> c <> "_" <> suffix
            | suffix <- ["EPSILON", "DIG", "MANT_DIG", "MIN", "MIN_EXP", "MIN_10_EXP", "MAX", "MAX_EXP", "MAX_10_EXP"]
            ]
          | (haskell, c) <- [("FLOAT", "FLT"), ("DOUBLE", "DBL")]
          ]

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
    -- GHC's @Stg.h@ includes @math.h@, so package C code that includes it
    -- can call the math functions with no include of its own.
    ["#include \"HsFFI.h\"", "#include <math.h>"]
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
    -- GHC's @Rts.h@ includes @assert.h@ and @stdlib.h@, and its
    -- @rts/Flags.h@ includes @stdio.h@. Package C code such as the
    -- @cbits@ of @bytebuild@ calls @sprintf@ with only @Rts.h@ included.
    ["#include <assert.h>", "#include <stdlib.h>", "#include <stdio.h>", "#include \"Stg.h\"", "#include \"rts/Signals.h\""]
    [ "void startTimer(void);",
      "void stopTimer(void);",
      "void blockUserSignals(void);",
      "void unblockUserSignals(void);",
      "HsBool rtsSupportsBoundThreads(void);"
    ]

-- | The @HTYPE_@ macros of GHC's @HsBaseConfig.h@.
--
-- Each macro names the Haskell type that a newtype of @Foreign.C.Types@
-- wraps.  A package such as @lens@ uses the macro as the type that the
-- newtype unwraps to.  Thus each value must agree with the representation
-- that @aihc-base@ gives the newtype for the target.
baseTypeMacros :: HeaderTarget -> [(Text, Text)]
baseTypeMacros target =
  [ ("HTYPE_CHAR", "Int8"),
    ("HTYPE_SIGNED_CHAR", "Int8"),
    ("HTYPE_UNSIGNED_CHAR", "Word8"),
    ("HTYPE_SHORT", "Int16"),
    ("HTYPE_UNSIGNED_SHORT", "Word16"),
    ("HTYPE_INT", "Int32"),
    ("HTYPE_UNSIGNED_INT", "Word32"),
    ("HTYPE_LONG", signed long),
    ("HTYPE_UNSIGNED_LONG", unsigned long),
    ("HTYPE_LONG_LONG", "Int64"),
    ("HTYPE_UNSIGNED_LONG_LONG", "Word64"),
    ("HTYPE_FLOAT", "Float"),
    ("HTYPE_DOUBLE", "Double"),
    ("HTYPE_PTRDIFF_T", signed pointer),
    ("HTYPE_SIZE_T", unsigned pointer),
    ("HTYPE_WCHAR_T", "Int32"),
    ("HTYPE_SIG_ATOMIC_T", "Int32"),
    ("HTYPE_CLOCK_T", "Word64"),
    ("HTYPE_TIME_T", "Int64"),
    ("HTYPE_USECONDS_T", "Word32"),
    ("HTYPE_SUSECONDS_T", "Int32"),
    ("HTYPE_INTPTR_T", signed pointer),
    ("HTYPE_UINTPTR_T", unsigned pointer),
    ("HTYPE_INTMAX_T", "Int64"),
    ("HTYPE_UINTMAX_T", "Word64"),
    ("HTYPE_BOOL", "Word8")
  ]
  where
    long = headerLongBytes target
    pointer = headerPointerBytes target
    signed bytes = "Int" <> tshow (bytes * 8)
    unsigned bytes = "Word" <> tshow (bytes * 8)

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
