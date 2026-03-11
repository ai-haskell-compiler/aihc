{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module: Regex.RE2
-- License: MIT
--
-- re2 is a regular expression library offering predictable run-time and
-- memory consumption. This package is a binding to re2.
--
-- Supported expression syntax is documented at
-- <http://code.google.com/p/re2/wiki/Syntax>.
--
-- >$ ghci -XOverloadedStrings
-- >ghci> import Regex.RE2
-- >
-- >ghci> find "\\w+" "hello world"
-- >Just (Match [Just "hello"])
-- >
-- >ghci> find "\\w+$" "hello world"
-- >Just (Match [Just "world"])
-- >
-- >ghci> find "^\\w+$" "hello world"
-- >Nothing
module Regex.RE2
    (
    -- * Compiling patterns
      Pattern
    , compile
    , compileWith

    -- ** Pattern properties
    , patternInput
    , patternOptions
    , patternGroups

    -- ** Options
    , Options
    , defaultOptions
    , Encoding(..)
    , optionEncoding
    , optionPosixSyntax
    , optionLongestMatch
    , optionMaxMemory
    , optionLiteral
    , optionNeverNewline
    , optionDotNewline
    , optionNeverCapture
    , optionCaseSensitive
    , optionPerlClasses
    , optionWordBoundary
    , optionOneLine

    -- ** Compilation errors
    , Error
    , ErrorCode
        ( ErrorInternal
        , ErrorBadEscape
        , ErrorBadCharClass
        , ErrorBadCharRange
        , ErrorMissingBracket
        , ErrorMissingParen
        , ErrorTrailingBackslash
        , ErrorRepeatArgument
        , ErrorRepeatSize
        , ErrorRepeatOp
        , ErrorBadPerlOp
        , ErrorBadUTF8
        , ErrorBadNamedCapture
        , ErrorPatternTooLarge
        )
    , errorMessage
    , errorCode

    -- * Matching
    , Match
    , matchGroup
    , matchGroups
    , Anchor(..)
    , match

    -- * Searching
    , find

    -- * Replacing
    , replace
    , replaceAll
    , extract

    -- * Utility functions
    , quoteMeta
    ) where

import Control.Exception (bracket, mask_)
import Control.Monad (join)
import Data.Int
import Data.String (IsString, fromString)
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Unsafe as B
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as V

-- | A pattern is a compiled regular expression plus its compilation options.
--
-- Patterns can be created by calling 'compile' explicitly:
--
-- @
--import Data.ByteString.Char8 (pack)
--
--p :: Pattern
--p = case 'compile' (pack "^hello world$") of
--        Right ok -> ok
--        Left err -> error ("compilation error: " ++ 'errorMessage' err)
-- @
--
-- Or by using the 'IsString' instance:
--
-- >import Data.String (fromString)
-- >
-- >p :: Pattern
-- >p = fromString "^hello world$"
--
-- Or by using the `OverloadedStrings` language extension:
--
-- >{-# LANGUAGE OverloadedStrings #-}
-- >
-- >p :: Pattern
-- >p = "^hello world$"
--
data Pattern = Pattern (ForeignPtr Pattern) Options

instance Show Pattern where
    showsPrec d pat = showParen (d > 10) (showString "Pattern " . shows (patternInput pat))

instance Eq Pattern where
    x == y = tup x == tup y where
        tup p = (patternInput p, patternOptions p)

instance IsString Pattern where
    fromString s = case compile (B8.pack s) of
        Left err -> error ("re2: failed to compile pattern " ++ show s ++ ": " ++ errorMessage err)
        Right pat -> pat

data Error = Error ErrorCode String
    deriving (Eq, Show)

data ErrorCode
    = ErrorInternal
    | ErrorBadEscape
    | ErrorBadCharClass
    | ErrorBadCharRange
    | ErrorMissingBracket
    | ErrorMissingParen
    | ErrorTrailingBackslash
    | ErrorRepeatArgument
    | ErrorRepeatSize
    | ErrorRepeatOp
    | ErrorBadPerlOp
    | ErrorBadUTF8
    | ErrorBadNamedCapture
    | ErrorPatternTooLarge
    | ErrorUnknown CInt
    deriving (Eq, Show)

errorCode :: Error -> ErrorCode
errorCode (Error x _) = x

errorMessage :: Error -> String
errorMessage (Error _ x) = x

data Encoding
    = EncodingUtf8
    | EncodingLatin1
    deriving (Eq, Show)

-- | Options controlling how to compile a regular expression. The fields in
-- this value may be set using record syntax:
--
-- @
--compileNoCase :: B.ByteString -> Either Error 'Pattern'
--compileNoCase = 'compileWith' ('defaultOptions' { 'optionCaseSensitive' = False })
-- @
data Options = Options
    { optionEncoding :: Encoding
    , optionPosixSyntax :: Bool
    , optionLongestMatch :: Bool
    , optionMaxMemory :: Int64
    , optionLiteral :: Bool
    , optionNeverNewline :: Bool
    , optionDotNewline :: Bool
    , optionNeverCapture :: Bool
    , optionCaseSensitive :: Bool

    -- | Only checked in posix mode
    , optionPerlClasses :: Bool

    -- | Only checked in posix mode
    , optionWordBoundary :: Bool

    -- | Only checked in posix mode
    , optionOneLine :: Bool
    }
    deriving (Eq, Show)

-- |
-- @
--defaultOptions = Options
--        { optionEncoding = EncodingUtf8
--        , optionPosixSyntax = False
--        , optionLongestMatch = False
--        , optionMaxMemory = 8388608  -- 8 << 20
--        , optionLiteral = False
--        , optionNeverNewline = False
--        , optionDotNewline = False
--        , optionNeverCapture = False
--        , optionCaseSensitive = True
--        , optionPerlClasses = False
--        , optionWordBoundary = False
--        , optionOneLine = False
--        }
-- @
defaultOptions :: Options
defaultOptions = Options
    { optionEncoding = EncodingUtf8
    , optionPosixSyntax = False
    , optionLongestMatch = False
    , optionMaxMemory = 8388608  -- 8 << 20
    , optionLiteral = False
    , optionNeverNewline = False
    , optionDotNewline = False
    , optionNeverCapture = False
    , optionCaseSensitive = True
    , optionPerlClasses = False
    , optionWordBoundary = False
    , optionOneLine = False
    }

withOptions :: Options -> (Ptr Options -> IO a) -> IO a
withOptions opts io = bracket c_alloc_options c_free_options $ \ptr -> do
    c_setopt_encoding ptr (case optionEncoding opts of
        EncodingUtf8 -> 1
        EncodingLatin1 -> 2)
    c_setopt_posix_syntax ptr (optionPosixSyntax opts)
    c_setopt_longest_match ptr (optionLongestMatch opts)
    c_setopt_max_mem ptr (optionMaxMemory opts)
    c_setopt_literal ptr (optionLiteral opts)
    c_setopt_never_nl ptr (optionNeverNewline opts)
    c_setopt_dot_nl ptr (optionDotNewline opts)
    c_setopt_never_capture ptr (optionNeverCapture opts)
    c_setopt_case_sensitive ptr (optionCaseSensitive opts)
    c_setopt_perl_classes ptr (optionPerlClasses opts)
    c_setopt_word_boundary ptr (optionWordBoundary opts)
    c_setopt_one_line ptr (optionOneLine opts)
    io ptr

foreign import ccall unsafe "haskell-re2.h haskell_re2_alloc_options"
    c_alloc_options :: IO (Ptr Options)

foreign import ccall unsafe "haskell-re2.h haskell_re2_free_options"
    c_free_options :: Ptr Options -> IO ()

foreign import ccall unsafe "haskell-re2.h haskell_re2_setopt_encoding"
    c_setopt_encoding :: Ptr Options -> CInt -> IO ()

foreign import ccall unsafe "haskell-re2.h haskell_re2_setopt_posix_syntax"
    c_setopt_posix_syntax :: Ptr Options -> Bool -> IO ()

foreign import ccall unsafe "haskell-re2.h haskell_re2_setopt_longest_match"
    c_setopt_longest_match :: Ptr Options -> Bool -> IO ()

foreign import ccall unsafe "haskell-re2.h haskell_re2_setopt_max_mem"
    c_setopt_max_mem :: Ptr Options -> Int64 -> IO ()

foreign import ccall unsafe "haskell-re2.h haskell_re2_setopt_literal"
    c_setopt_literal :: Ptr Options -> Bool -> IO ()

foreign import ccall unsafe "haskell-re2.h haskell_re2_setopt_never_nl"
    c_setopt_never_nl :: Ptr Options -> Bool -> IO ()

foreign import ccall unsafe "haskell-re2.h haskell_re2_setopt_dot_nl"
    c_setopt_dot_nl :: Ptr Options -> Bool -> IO ()

foreign import ccall unsafe "haskell-re2.h haskell_re2_setopt_never_capture"
    c_setopt_never_capture :: Ptr Options -> Bool -> IO ()

foreign import ccall unsafe "haskell-re2.h haskell_re2_setopt_case_sensitive"
    c_setopt_case_sensitive :: Ptr Options -> Bool -> IO ()

foreign import ccall unsafe "haskell-re2.h haskell_re2_setopt_perl_classes"
    c_setopt_perl_classes :: Ptr Options -> Bool -> IO ()

foreign import ccall unsafe "haskell-re2.h haskell_re2_setopt_word_boundary"
    c_setopt_word_boundary :: Ptr Options -> Bool -> IO ()

foreign import ccall unsafe "haskell-re2.h haskell_re2_setopt_one_line"
    c_setopt_one_line :: Ptr Options -> Bool -> IO ()

-- | @compile = 'compileWith' 'defaultOptions'@
compile :: B.ByteString -> Either Error Pattern
compile = compileWith defaultOptions

-- | Compile a regular expression with the given options. If compilation fails,
-- the error can be inspected with 'errorMessage' and 'errorCode'.
--
-- Use 'optionEncoding' to select whether the input bytes should be interpreted
-- as UTF-8 or Latin1. The default is UTF8.
compileWith :: Options -> B.ByteString -> Either Error Pattern
compileWith opts input = unsafePerformIO $ withOptions opts $ \optsPtr -> do
    fptr <- mask_ $ do
        p <- unsafeUseAsCStringIntLen input $ \(inPtr, inLen) ->
            c_compile_pattern optsPtr inPtr inLen
        newForeignPtr c_delete_pattern p
    withForeignPtr fptr $ \ptr -> do
        errPtr <- c_error ptr
        if errPtr == nullPtr
            then return (Right (Pattern fptr opts))
            else do
                err <- peekCString errPtr
                errCodeInt <- c_error_code ptr
                let errCode = case errCodeInt of
                        1 -> ErrorInternal
                        2 -> ErrorBadEscape
                        3 -> ErrorBadCharClass
                        4 -> ErrorBadCharRange
                        5 -> ErrorMissingBracket
                        6 -> ErrorMissingParen
                        7 -> ErrorTrailingBackslash
                        8 -> ErrorRepeatArgument
                        9 -> ErrorRepeatSize
                        10 -> ErrorRepeatOp
                        11 -> ErrorBadPerlOp
                        12 -> ErrorBadUTF8
                        13 -> ErrorBadNamedCapture
                        14 -> ErrorPatternTooLarge
                        _ -> ErrorUnknown errCodeInt
                return (Left (Error errCode err))

foreign import ccall unsafe "haskell-re2.h haskell_re2_compile_pattern"
    c_compile_pattern :: Ptr Options -> CString -> CInt -> IO (Ptr Pattern)

foreign import ccall unsafe "haskell-re2.h &haskell_re2_delete_pattern"
    c_delete_pattern :: FunPtr (Ptr Pattern -> IO ())

foreign import ccall unsafe "haskell-re2.h haskell_re2_error"
    c_error :: Ptr Pattern -> IO CString

foreign import ccall unsafe "haskell-re2.h haskell_re2_error_code"
    c_error_code :: Ptr Pattern -> IO CInt

-- | The regular expression originally provided to 'compileWith'.
patternInput :: Pattern -> B.ByteString
patternInput (Pattern fptr _) = unsafePerformIO $
    withForeignPtr fptr $ \ptr -> do
        cstr <- c_pattern_input ptr
        B.packCString cstr

foreign import ccall unsafe "haskell-re2.h haskell_re2_pattern_input"
    c_pattern_input :: Ptr Pattern -> IO CString

-- | The options originally provided to 'compileWith'.
patternOptions :: Pattern -> Options
patternOptions (Pattern _ opts) = opts

-- | The capturing groups defined within the pattern. Groups are listed
-- from left to right, and are @Nothing@ if the group is unnamed.
--
-- >ghci> patternGroups "(\\d+)|(?P<word>\\w+)"
-- >fromList [Nothing,Just "word"]
patternGroups :: Pattern -> V.Vector (Maybe B.ByteString)
patternGroups (Pattern fptr _) = unsafePerformIO $
    alloca $ \groupNamesPtr ->
    alloca $ \groupNameLensPtr ->
    withForeignPtr fptr $ \patternPtr -> do
        count <- c_pattern_groups patternPtr groupNamesPtr groupNameLensPtr
        if count == 0
            then return V.empty
            else do
                groupNames <- peek groupNamesPtr
                groupNameLens <- peek groupNameLensPtr
                peekPatternGroups (fromIntegral count) groupNames groupNameLens

peekPatternGroups :: Int -> Ptr CString -> Ptr CSize -> IO (V.Vector (Maybe B.ByteString))
peekPatternGroups 0 _ _ = return V.empty
peekPatternGroups groupCount groupNames groupNameLens = io where
    io = do
        vec <- V.new groupCount
        loop vec 0
        c_free groupNames
        c_free groupNameLens
        V.freeze vec
    loop _ idx | idx == groupCount = return ()
    loop vec idx = do
        cstr <- peekElemOff groupNames idx
        if cstr == nullPtr
            then V.write vec idx Nothing
            else do
                len <- peekElemOff groupNameLens idx
                bytes <- unsafePackMallocCStringSizeLen cstr len
                V.write vec idx (Just bytes)
        loop vec (idx+1)

foreign import ccall unsafe "stdlib.h free"
    c_free :: Ptr a -> IO ()

foreign import ccall unsafe "haskell-re2.h haskell_re2_pattern_groups"
    c_pattern_groups :: Ptr Pattern -> Ptr (Ptr CString) -> Ptr (Ptr CSize) -> IO CInt

-- | A successful match of the pattern against some input. Capturing groups
-- may be retrieved with 'matchGroup' or 'matchGroups'.
newtype Match = Match (V.Vector (Maybe B.ByteString))
    deriving (Eq)

instance Show Match where
    showsPrec d (Match vec) = showParen (d > 10) (showString "Match " . shows (V.toList vec))

-- | The capturing group with the given index, or @Nothing@ if the group was
-- not set in this match.
--
-- The entire match is group 0.
matchGroup :: Match -> Int -> Maybe B.ByteString
matchGroup (Match vals) idx = join (vals V.!? idx)

-- | All of the groups in the pattern, with each group being @Nothing@ if it
-- was not set in this match. Groups are returned in the same order as
-- 'patternGroups'.
--
-- The entire match is group 0.
matchGroups :: Match -> V.Vector (Maybe B.ByteString)
matchGroups (Match vals) = vals

data Anchor
    = AnchorStart
    | AnchorBoth
    deriving (Eq, Show)

-- | The most general matching function. Attempt to match the pattern to the
-- input within the given constraints.
--
-- If the number of match groups to populate is 0, matching can be performed
-- more efficiently.
match :: Pattern
      -> B.ByteString
      -> Int -- ^ Start position
      -> Int -- ^ End position
      -> Maybe Anchor
      -> Int -- ^ How many match groups to populate
      -> Maybe Match
match (Pattern fptr _) input startPos endPos anchor maxCaptures = unsafePerformIO $
    alloca $ \capturesPtr ->
    alloca $ \captureLensPtr ->
    alloca $ \captureCountPtr ->
    unsafeUseAsCStringIntLen input $ \(inPtr, inLen) ->
    withForeignPtr fptr $ \patternPtr -> do
        let cStartPos = fromIntegral (min startPos c_INT_MAX)
        let cEndPos = fromIntegral (min endPos c_INT_MAX)
        let cAnchor = case anchor of
                Nothing -> 0
                Just AnchorStart -> 1
                Just AnchorBoth -> 2
        let cMaxCaptures = fromIntegral (max maxCaptures 0)
        matched <- c_match patternPtr inPtr inLen cStartPos cEndPos cAnchor cMaxCaptures capturesPtr captureLensPtr captureCountPtr
        if not matched
            then return Nothing
            else do
                captures <- peek capturesPtr
                captureLens <- peek captureLensPtr
                captureCount <- peek captureCountPtr
                vec <- peekCaptures (fromIntegral captureCount) input inPtr captures captureLens
                return (Just (Match vec))

-- | Attempt to find the pattern somewhere within the input.
find :: Pattern -> B.ByteString -> Maybe Match
find (Pattern fptr _) input = unsafePerformIO $
    alloca $ \capturesPtr ->
    alloca $ \captureLensPtr ->
    alloca $ \captureCountPtr ->
    unsafeUseAsCStringIntLen input $ \(inPtr, inLen) ->
    withForeignPtr fptr $ \patternPtr -> do
        matched <- c_match patternPtr inPtr inLen 0 inLen 0 (-1) capturesPtr captureLensPtr captureCountPtr
        if not matched
            then return Nothing
            else do
                captures <- peek capturesPtr
                captureLens <- peek captureLensPtr
                captureCount <- peek captureCountPtr
                vec <- peekCaptures (fromIntegral captureCount) input inPtr captures captureLens
                return (Just (Match vec))

peekCaptures :: Int -> B.ByteString -> CString -> Ptr CString -> Ptr CSize -> IO (V.Vector (Maybe B.ByteString))
peekCaptures 0 _ _ _ _ = return V.empty
peekCaptures groupCount input inPtr groupNames groupNameLens = io where
    io = do
        vec <- V.new groupCount
        loop vec 0
        c_free groupNames
        c_free groupNameLens
        V.freeze vec
    loop _ idx | idx == groupCount = return ()
    loop vec idx = do
        cstr <- peekElemOff groupNames idx
        if cstr == nullPtr
            then V.write vec idx Nothing
            else do
                len <- peekElemOff groupNameLens idx
                let bytes = unsafeTakeCSize len (B.unsafeDrop (minusPtr cstr inPtr) input)
                V.write vec idx (Just bytes)
        loop vec (idx+1)

foreign import ccall "haskell-re2.h haskell_re2_match"
    c_match :: Ptr Pattern
            -> CString -> CInt -- ^ Input
            -> CInt -> CInt -- ^ startpos, endpos
            -> CInt -- ^ anchor
            -> CInt -- ^ num captures, -1 to capture all groups
            -> Ptr (Ptr CString) -> Ptr (Ptr CSize) -- ^ Captures
            -> Ptr CInt -- ^ How many groups were captured
            -> IO Bool

-- | Replace the first occurance of the pattern with the given replacement
-- template. If the template contains backslash escapes such as @\\1@, the
-- capture group with the given index will be inserted in their place.
--
-- Returns the new bytes, and @True@ if a replacement occured.
replace :: Pattern
        -> B.ByteString -- ^ Input
        -> B.ByteString -- ^ Replacement template
        -> (B.ByteString, Bool)
replace (Pattern fptr _) input rewrite = unsafePerformIO $
    unsafeUseAsCStringSizeLen input $ \(inPtr, inLen) ->
    unsafeUseAsCStringIntLen rewrite $ \(rewritePtr, rewriteLen) ->
    alloca $ \outPtr ->
    alloca $ \outLenPtr ->
    withForeignPtr fptr $ \patternPtr -> do
        replaced <- c_replace patternPtr inPtr inLen rewritePtr rewriteLen outPtr outLenPtr
        if replaced
            then do
                out <- peek outPtr
                outLen <- peek outLenPtr
                outBytes <- unsafePackMallocCStringSizeLen out outLen
                return (outBytes, True)
            else return (input, False)

foreign import ccall unsafe "haskell-re2.h haskell_re2_replace"
    c_replace :: Ptr Pattern
              -> CString -> CSize -- in, in_len
              -> CString -> CInt  -- rewrite, rewrite_len
              -> Ptr CString -> Ptr CSize -- out, out_len
              -> IO Bool

-- | Replace every occurance of the pattern with the given replacement
-- template. If the template contains backslash escapes such as @\\1@, the
-- capture group with the given index will be inserted in their place.
--
-- Returns the new bytes, and how many replacements occured.
replaceAll :: Pattern
           -> B.ByteString -- ^ Input
           -> B.ByteString -- ^ Replacement template
           -> (B.ByteString, Int)
replaceAll (Pattern fptr _) input rewrite = unsafePerformIO $
    unsafeUseAsCStringSizeLen input $ \(inPtr, inLen) ->
    unsafeUseAsCStringIntLen rewrite $ \(rewritePtr, rewriteLen) ->
    alloca $ \outPtr ->
    alloca $ \outLenPtr ->
    alloca $ \countPtr ->
    withForeignPtr fptr $ \patternPtr -> do
        c_global_replace patternPtr inPtr inLen rewritePtr rewriteLen outPtr outLenPtr countPtr
        count <- peek countPtr
        if count > 0
            then do
                out <- peek outPtr
                outLen <- peek outLenPtr
                outBytes <- unsafePackMallocCStringSizeLen out outLen
                return (outBytes, fromIntegral count)
            else return (input, 0)

foreign import ccall "haskell-re2.h haskell_re2_global_replace"
    c_global_replace :: Ptr Pattern
                     -> CString -> CSize -- in, in_len
                     -> CString -> CInt -- rewrite, rewrite_len
                     -> Ptr CString -> Ptr CSize  -- out, out_len
                     -> Ptr CInt  -- count
                     -> IO ()

-- | Attempt to find the pattern somewhere within the input, and extract
-- it using the given template. If the template contains backslash escapes
-- such as @\\1@, the capture group with the given index will be inserted
-- in their place.
--
-- Returns @Nothing@ if the pattern was not found in the input.
extract :: Pattern
        -> B.ByteString -- ^ Input
        -> B.ByteString -- ^ Extraction template
        -> Maybe B.ByteString
extract (Pattern fptr _) input rewrite = unsafePerformIO $
    unsafeUseAsCStringIntLen input $ \(inPtr, inLen) ->
    unsafeUseAsCStringIntLen rewrite $ \(rewritePtr, rewriteLen) ->
    alloca $ \outPtr ->
    alloca $ \outLenPtr ->
    withForeignPtr fptr $ \patternPtr -> do
        replaced <- c_extract patternPtr inPtr inLen rewritePtr rewriteLen outPtr outLenPtr
        if replaced
            then do
                out <- peek outPtr
                outLen <- peek outLenPtr
                outBytes <- unsafePackMallocCStringSizeLen out outLen
                return (Just outBytes)
            else return Nothing

foreign import ccall unsafe "haskell-re2.h haskell_re2_extract"
    c_extract :: Ptr Pattern
              -> CString -> CInt -- in, in_len
              -> CString -> CInt  -- rewrite, rewrite_len
              -> Ptr CString -> Ptr CSize -- out, out_len
              -> IO Bool

-- | Escapes bytes such that the output is a regular expression which will
-- exactly match the input.
quoteMeta :: B.ByteString -> B.ByteString
quoteMeta input = unsafePerformIO $
    unsafeUseAsCStringIntLen input $ \(inPtr, inLen) ->
    alloca $ \outPtr ->
    alloca $ \outLenPtr -> do
        c_quote_meta inPtr inLen outPtr outLenPtr
        out <- peek outPtr
        outLen <- peek outLenPtr
        unsafePackMallocCStringSizeLen out outLen

foreign import ccall "haskell-re2.h haskell_re2_quote_meta"
    c_quote_meta :: CString -> CInt -> Ptr CString -> Ptr CSize -> IO ()

-- note: we assume that:
--   (maxBound::Int) >= (maxBound::CInt)
--   (maxBound::Int) <= (maxBound::CSize)
--
-- This is not technically correct, because the Haskell spec permits
-- (maxBound::Int) to be as small as 2^29-1. However, it is correct in
-- compilers such as GHC that use machine ints for Int.
unsafeUseAsCStringIntLen :: B.ByteString -> ((CString, CInt) -> IO a) -> IO a
unsafeUseAsCStringSizeLen :: B.ByteString -> ((CString, CSize) -> IO a) -> IO a
unsafePackMallocCStringSizeLen :: CString -> CSize -> IO B.ByteString

c_INT_MAX :: Int
c_INT_MAX = fromIntegral (maxBound :: CInt)

hs_INT_MAX :: CSize
hs_INT_MAX = fromIntegral (maxBound :: Int)

unsafeUseAsCStringIntLen bytes fn = B.unsafeUseAsCStringLen bytes (\(ptr, rawLen) -> if rawLen > c_INT_MAX
    then error ("re2: bytestring length " ++ show rawLen ++ " exceeds INT_MAX")
    else fn (ptr, fromIntegral rawLen))

unsafeUseAsCStringSizeLen bytes fn = B.unsafeUseAsCStringLen bytes (\(ptr, rawLen) -> fn (ptr, fromIntegral rawLen))

unsafePackMallocCStringSizeLen ptr len = if len > hs_INT_MAX
    then error ("re2: std::string length " ++ show len ++ " exceeds (maxBound::Int)")
    else B.unsafePackMallocCStringLen (ptr, fromIntegral len)

unsafeTakeCSize :: CSize -> B.ByteString -> B.ByteString
unsafeTakeCSize len bytes = if len > hs_INT_MAX
    then error ("re2: std::string length " ++ show len ++ " exceeds (maxBound::Int)")
    else B.unsafeTake (fromIntegral len) bytes
