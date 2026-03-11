{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
-- The exported symbols are the same whether HAVE_PCRE_H is defined,
-- but when if it is not defined then 'getVersion == Nothing' and all
-- other exported values will call error or fail.

-- | This will fail or error only if allocation fails or a nullPtr is passed in.

-- TODO :: Consider wrapMatchAll using list of start/end offsets and not MatchArray
--

{- Copyright   :  (c) Chris Kuklewicz 2007 -}
module Text.Regex.PCRE.Wrap(
  -- ** High-level interface
  Regex,
  CompOption(CompOption),
  ExecOption(ExecOption),
  (=~),
  (=~~),

  -- ** Low-level interface
  StartOffset,
  EndOffset,
  ReturnCode(ReturnCode),
  WrapError,
  wrapCompile,
  wrapTest,
  wrapMatch,
  wrapMatchAll,
  wrapCount,

  -- ** Miscellaneous
  getVersion,
  configUTF8,
  getNumSubs,
  unusedOffset,

  -- ** CompOption values
  compBlank,
  compAnchored,
  compAutoCallout,
  compCaseless,
  compDollarEndOnly,
  compDotAll,
  compExtended,
  compExtra,
  compFirstLine,
  compMultiline,
  compNoAutoCapture,
  compUngreedy,
  compUTF8,
  compNoUTF8Check,

  -- ** ExecOption values
  execBlank,
  execAnchored,
  execNotBOL,
  execNotEOL,
  execNotEmpty,
  execNoUTF8Check,
  execPartial,

  -- ** ReturnCode values
  retOk,
  retNoMatch,
  retNull,
  retBadOption,
  retBadMagic,
  retUnknownNode,
  retNoMemory,
  retNoSubstring
  ) where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(fail))

import Control.Monad(when)
import Data.Array(Array,accumArray)
import Data.Bits(Bits((.|.))) -- ((.&.),(.|.),complement))
import System.IO.Unsafe(unsafePerformIO)
import Foreign(Ptr,ForeignPtr,FinalizerPtr -- ,FunPtr
              ,alloca,allocaBytes,nullPtr
              ,peek,peekElemOff
              ,newForeignPtr,withForeignPtr)
import Foreign.C(CInt(..),CChar)
import Foreign.C.String(CString,CStringLen,peekCString)
import Text.Regex.Base.RegexLike(RegexOptions(..),RegexMaker(..),RegexContext(..),MatchArray,MatchOffset)

-- | Version string of PCRE library
--
-- __NOTE__: The 'Maybe' type is used for historic reasons; practically, 'getVersion' is never 'Nothing'.
{-# NOINLINE getVersion #-}
getVersion :: Maybe String

type PCRE = ()
type StartOffset = MatchOffset
type EndOffset = MatchOffset
type WrapError = (ReturnCode,String)

newtype CompOption = CompOption CInt deriving (Eq,Show,Num,Bits)
newtype ExecOption = ExecOption CInt deriving (Eq,Show,Num,Bits)
newtype ReturnCode = ReturnCode CInt deriving (Eq,Show)

-- | A compiled regular expression
data Regex = Regex (ForeignPtr PCRE) CompOption ExecOption

compBlank :: CompOption
execBlank :: ExecOption
unusedOffset :: MatchOffset
retOk :: ReturnCode

wrapCompile :: CompOption -- ^ Flags (summed together)
            -> ExecOption -- ^ Flags (summed together)
            -> CString  -- ^ The regular expression to compile
            -> IO (Either (MatchOffset,String) Regex) -- ^ Returns: an error offset and string or the compiled regular expression
wrapTest :: StartOffset -- ^ Starting index in CStringLen
         -> Regex       -- ^ Compiled regular expression
         -> CStringLen  -- ^ String to match against and length in bytes
         -> IO (Either WrapError Bool)
wrapMatch :: StartOffset -- ^ Starting index in CStringLen
          -> Regex       -- ^ Compiled regular expression
          -> CStringLen  -- ^ String to match against and length in bytes
          -> IO (Either WrapError (Maybe [(StartOffset,EndOffset)]))
                -- ^ Returns: 'Right Nothing' if the regex did not match the
                -- string, or:
                --   'Right Just' an array of (offset,length) pairs where index 0 is whole match, and the rest are the captured subexpressions, or:
                --   'Left ReturnCode' if there is some strange error
wrapMatchAll :: Regex -> CStringLen -> IO (Either WrapError [ MatchArray ])
wrapCount :: Regex -> CStringLen -> IO (Either WrapError Int)

getNumSubs :: Regex -> Int

{-# NOINLINE configUTF8 #-}
configUTF8 :: Bool

(=~)  :: (RegexMaker Regex CompOption ExecOption source,RegexContext Regex source1 target)
      => source1 -> source -> target
(=~~) :: (RegexMaker Regex CompOption ExecOption source,RegexContext Regex source1 target,MonadFail m)
      => source1 -> source -> m target

#include <sys/types.h>
#include <pcre.h>

instance RegexOptions Regex CompOption ExecOption where
  blankCompOpt = compBlank
  blankExecOpt = execBlank
  defaultCompOpt = compMultiline
  defaultExecOpt = execBlank
  setExecOpts e' (Regex r c _) = Regex r c e'
  getExecOpts (Regex _ _ e) = e

-- (=~) :: (RegexMaker Regex CompOption ExecOption source,RegexContext Regex source1 target) => source1 -> source -> target
(=~) x r = let q :: Regex
               q = makeRegex r
           in match q x

-- (=~~) ::(RegexMaker Regex CompOption ExecOption source,RegexContext Regex source1 target,MonadFail m) => source1 -> source -> m target
(=~~) x r = do (q :: Regex) <-  makeRegexM r
               matchM q x

type PCRE_Extra = ()

fi :: (Integral i,Num n ) => i -> n
fi x = fromIntegral x

compBlank = CompOption 0
execBlank = ExecOption 0
unusedOffset = (-1)
retOk = ReturnCode 0

retNeededMoreSpace :: ReturnCode
retNeededMoreSpace = ReturnCode 0

newtype InfoWhat = InfoWhat CInt deriving (Eq,Show)
newtype ConfigWhat = ConfigWhat CInt deriving (Eq,Show)

nullTest' :: Ptr a -> String -> IO (Either (MatchOffset,String) b) -> IO (Either (MatchOffset,String) b)
{-# INLINE nullTest' #-}
nullTest' ptr msg io = do
  if nullPtr == ptr
    then return (Left (0,"Ptr parameter was nullPtr in Text.Regex.PCRE.Wrap."++msg))
    else io

nullTest :: Ptr a -> String -> IO (Either WrapError b) -> IO (Either WrapError b)
{-# INLINE nullTest #-}
nullTest ptr msg io = do
  if nullPtr == ptr
    then return (Left (retOk,"Ptr parameter was nullPtr in Text.Regex.PCRE.Wrap."++msg))
    else io

wrapRC :: ReturnCode -> IO (Either WrapError b)
{-# INLINE wrapRC #-}
wrapRC r = return (Left (r,"Error in Text.Regex.PCRE.Wrap: "++show r))

-- | Compiles a regular expression
wrapCompile flags e pattern = do
 nullTest' pattern "wrapCompile pattern" $ do
  alloca $ \errOffset -> alloca $ \errPtr -> do
   nullTest' errPtr "wrapCompile errPtr" $ do
    pcre_ptr <- c_pcre_compile pattern flags errPtr errOffset nullPtr
    if pcre_ptr == nullPtr
      then do
        -- No need to use c_ptr_free in the error case (e.g. pcredemo.c)
        offset <- peek errOffset
        string <- peekCString =<< peek errPtr
        return (Left (fi offset,string))
      else do regex <- newForeignPtr c_ptr_free pcre_ptr
              return . Right $ Regex regex flags e

getNumSubs (Regex pcre_fptr _ _) = fi . unsafePerformIO $ withForeignPtr pcre_fptr getNumSubs'

getNumSubs' :: Ptr PCRE -> IO CInt
{-# INLINE getNumSubs' #-}
getNumSubs' pcre_ptr =
  alloca $ \st -> do -- (st :: Ptr CInt)
    when (st == nullPtr) (fail "Text.Regex.PCRE.Wrap.getNumSubs' could not allocate a CInt!!!")
    c_pcre_fullinfo pcre_ptr nullPtr pcreInfoCapturecount st
    peek st

wrapTest startOffset (Regex pcre_fptr _ flags) (cstr,len) = do
 nullTest cstr "wrapTest cstr" $ do
  withForeignPtr pcre_fptr $ \pcre_ptr -> do
    r@(ReturnCode r') <- c_pcre_exec pcre_ptr nullPtr cstr (fi len) (fi startOffset) flags nullPtr 0
    if r == retNoMatch
      then return (Right False)
      else if r' < 0
             then wrapRC r
             else return (Right True)

-- | Matches a regular expression against a string
--
-- Should never return (Right (Just []))
wrapMatch startOffset (Regex pcre_fptr _ flags) (cstr,len) = do
 nullTest cstr "wrapMatch cstr" $ do
  withForeignPtr pcre_fptr $ \pcre_ptr -> do
    nsub <- getNumSubs' pcre_ptr
    let nsub_int :: Int
        nsub_int = fi nsub
        ovec_size :: CInt
        ovec_size = ((nsub + 1) * 3) -- "man pcreapi" for explanation
        ovec_bytes :: Int
        ovec_bytes = (fi ovec_size) * (#const sizeof(int))
    allocaBytes ovec_bytes $ \ovec -> do
     nullTest ovec "wrapMatch ovec" $ do
      r@(ReturnCode r') <- c_pcre_exec pcre_ptr nullPtr cstr (fi len) (fi startOffset) flags ovec ovec_size
      if r == retNoMatch
        then return (Right Nothing)
        else if r' < 0
          then wrapRC r
          else do
            let pairsSet :: Int
                pairsSet = if r == retNeededMoreSpace -- if r == ReturnCode 0
                             then nsub_int + 1 -- should not happen
                             else fi r' -- implies pairsSet > 0
                extraPairs :: [(Int,Int)]
                extraPairs = replicate (nsub_int + 1 - pairsSet)
                                       (unusedOffset,unusedOffset)
            pairs <- return . toPairs =<< mapM (peekElemOff ovec) [0 .. ((pairsSet*2)-1)]
            return . Right . Just $ (pairs ++ extraPairs)

-- | wrapMatchAll is an improvement over wrapMatch since it only
-- allocates memory with allocaBytes once at the start.
--
--
wrapMatchAll (Regex pcre_fptr _ flags) (cstr,len) = do
 nullTest cstr "wrapMatchAll cstr" $ do
  withForeignPtr pcre_fptr $ \regex -> do
    nsub <- getNumSubs' regex
    let nsub_int :: Int
        nsub_int = fi nsub
        ovec_size :: CInt
        ovec_size = ((nsub + 1) * 3) -- "man pcreapi" for explanation
        ovec_bytes :: Int
        ovec_bytes = (fi ovec_size) * (#const sizeof(int))
        clen = fi len
        flags' = (execNotEmpty .|. execAnchored .|. flags)
    allocaBytes ovec_bytes $ \ovec ->
     nullTest ovec "wrapMatchAll ovec" $
      let loop acc flags_in_use pos = do
            r@(ReturnCode r') <- c_pcre_exec regex nullPtr cstr clen (fi pos) flags_in_use ovec ovec_size
            if r == retNoMatch
              then return (Right (acc []))
              else if r' < 0
                     then wrapRC r
                     else do
                       let pairsSet = if r == retNeededMoreSpace then nsub_int+1 else fi r'
                       pairs <- return . toPairs =<< mapM (peekElemOff ovec) [0 .. ((pairsSet*2)-1)]
                       let acc' = acc . (toMatchArray nsub_int pairs:)
                       case pairs of
                         [] -> return (Right (acc' []))
                         ((s,e):_) | s==e -> if s == len
                                               then return (Right (acc' []))
                                               else loop acc' flags' e
                                   | otherwise -> loop acc' flags e
      in loop id flags 0
toMatchArray :: Int -> [(Int,Int)] -> Array Int (Int,Int)
toMatchArray n pairs = accumArray (\_ (s,e) -> (s,(e-s))) (-1,0) (0,n) (zip [0..] pairs)

toPairs :: [CInt] -> [(Int,Int)]
toPairs [] = []
toPairs (a:b:rest) = (fi a,fi b):toPairs rest
toPairs [_] = error "Should not have just one element in WrapPCRE.wrapMatchAll.toPairs"

wrapCount (Regex pcre_fptr _ flags) (cstr,len) = do
 nullTest cstr "wrapCount cstr" $ do
  withForeignPtr pcre_fptr $ \pcre_ptr -> do
    nsub <- getNumSubs' pcre_ptr
    let ovec_size :: CInt
        ovec_size = ((nsub + 1) * 3) -- "man pcreapi" for explanation
        ovec_bytes :: Int
        ovec_bytes = (fi ovec_size) * (#const sizeof(int))
        clen = fi len
    allocaBytes ovec_bytes $ \ovec ->
     nullTest ovec "wrapCount ovec" $
      let act pos = c_pcre_exec pcre_ptr nullPtr cstr clen (fi pos) flags ovec ovec_size
          loop acc pos | acc `seq` pos `seq` False = undefined
                       | otherwise  = do
            r@(ReturnCode r') <- act pos
            if r == retNoMatch
              then return (Right acc)
              else if r' < 0
                then wrapRC r
                else do
                  pairs <- return . toPairs =<< mapM (peekElemOff ovec) [0,1]
                  case pairs of
                    [] -> return (Right (succ acc))
                    ((s,e):_) | s==e -> return (Right (succ acc))
                              | otherwise -> loop (succ acc) e
      in loop 0 0

getVersion = unsafePerformIO $ do
  version <- c_pcre_version
  if version == nullPtr
    then return (Just "pcre_version was null")
    else return . Just =<< peekCString version

configUTF8 = unsafePerformIO $
  alloca $ \ptrVal -> do -- (ptrVal :: Ptr CInt)
    when (ptrVal == nullPtr) (fail "Text.Regex.PCRE.Wrap.configUTF8 could not alloca CInt!!!")
    c_pcre_config pcreConfigUtf8 ptrVal
    val <- peek ptrVal
    case val of
      (1 :: CInt) -> return True
      0 -> return False
      _ -> return False -- should not happen

foreign import ccall unsafe "pcre.h pcre_compile"
  c_pcre_compile :: CString -> CompOption -> Ptr CString -> Ptr CInt -> CString -> IO (Ptr PCRE)
foreign import ccall unsafe "&free"
  c_ptr_free :: FinalizerPtr a -- FunPtr (Ptr a -> IO ())
foreign import ccall unsafe "pcre.h pcre_exec"
  c_pcre_exec :: Ptr PCRE -> Ptr PCRE_Extra -> CString -> CInt -> CInt -> ExecOption -> Ptr CInt -> CInt -> IO ReturnCode
foreign import ccall unsafe "pcre.h pcre_fullinfo"
  c_pcre_fullinfo :: Ptr PCRE -> Ptr PCRE_Extra -> InfoWhat -> Ptr a -> IO CInt
foreign import ccall unsafe "pcre.h pcre_version"
  c_pcre_version :: IO (Ptr CChar)
foreign import ccall unsafe "pcre.h pcre_config"
  c_pcre_config :: ConfigWhat -> Ptr a -> IO CInt


#enum CompOption,CompOption, \
  compAnchored = PCRE_ANCHORED, \
  compAutoCallout = PCRE_AUTO_CALLOUT, \
  compCaseless = PCRE_CASELESS, \
  compDollarEndOnly = PCRE_DOLLAR_ENDONLY, \
  compDotAll = PCRE_DOTALL, \
  compExtended = PCRE_EXTENDED, \
  compExtra = PCRE_EXTRA, \
  compFirstLine = PCRE_FIRSTLINE, \
  compMultiline = PCRE_MULTILINE, \
  compNoAutoCapture = PCRE_NO_AUTO_CAPTURE, \
  compUngreedy = PCRE_UNGREEDY, \
  compUTF8 = PCRE_UTF8, \
  compNoUTF8Check = PCRE_NO_UTF8_CHECK

#enum ExecOption,ExecOption, \
  execAnchored = PCRE_ANCHORED, \
  execNotBOL = PCRE_NOTBOL, \
  execNotEOL = PCRE_NOTEOL, \
  execNotEmpty = PCRE_NOTEMPTY, \
  execNoUTF8Check = PCRE_NO_UTF8_CHECK, \
  execPartial = PCRE_PARTIAL

#enum ReturnCode,ReturnCode, \
  retNoMatch = PCRE_ERROR_NOMATCH, \
  retNull = PCRE_ERROR_NULL, \
  retBadOption = PCRE_ERROR_BADOPTION, \
  retBadMagic = PCRE_ERROR_BADMAGIC, \
  retUnknownNode = PCRE_ERROR_UNKNOWN_NODE, \
  retNoMemory = PCRE_ERROR_NOMEMORY, \
  retNoSubstring = PCRE_ERROR_NOSUBSTRING

-- Comment out most of these to avoid unused binding warnings

-- PCRE_INFO_FIRSTCHAR is deprecated, use PCRE_INFO_FIRSTBYTE instead.
#enum InfoWhat,InfoWhat, \
  PCRE_INFO_CAPTURECOUNT
{-
  PCRE_INFO_BACKREFMAX, \
  PCRE_INFO_DEFAULT_TABLES, \
  PCRE_INFO_FIRSTBYTE, \
  PCRE_INFO_FIRSTCHAR, \
  PCRE_INFO_FIRSTTABLE, \
  PCRE_INFO_LASTLITERAL, \
  PCRE_INFO_NAMECOUNT, \
  PCRE_INFO_NAMEENTRYSIZE, \
  PCRE_INFO_NAMETABLE, \
  PCRE_INFO_OPTIONS, \
  PCRE_INFO_SIZE, \
  PCRE_INFO_STUDYSIZE
-}
#enum ConfigWhat,ConfigWhat, \
  PCRE_CONFIG_UTF8
{-
  PCRE_CONFIG_UNICODE_PROPERTIES, \
  PCRE_CONFIG_NEWLINE, \
  PCRE_CONFIG_LINK_SIZE, \
  PCRE_CONFIG_POSIX_MALLOC_THRESHOLD, \
  PCRE_CONFIG_MATCH_LIMIT, \
  PCRE_CONFIG_MATCH_LIMIT_RECURSION, \
  PCRE_CONFIG_STACKRECURSE
-}
