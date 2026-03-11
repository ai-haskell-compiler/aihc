{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}


{-|
Module      : LibOath.Internal
Description : Exports the C code functions. Use at your own risk!
Copyright   : (c) Matt Parsons, 2017
License     : GPL-3
Maintainer  : parsonsmatt@gmail.com
Stability   : experimental
Portability : POSIX

This module contains the C functions called via "inline-c". Import these to use at your own risk.

The functions are undocumented here. They are mirrored from <http://www.nongnu.org/oath-toolkit/liboath-api/liboath-oath.html the library documentation>.

-}
module LibOath.Internal where

import Data.Monoid ((<>))
import Data.ByteString (ByteString)
import Control.Monad.IO.Class (MonadIO)
import           Data.Maybe        (fromMaybe)
import qualified Foreign.C.String  as C
import           Foreign.Ptr       (Ptr)
import           GHC.Stack         (HasCallStack)
import qualified Language.C.Inline as C

C.context (C.baseCtx <> C.bsCtx)

C.include "<liboath/oath.h>"

-- Haskell utilities

-- | The type of @liboath@ actions. This is a wrapper over 'IO' that we use
-- to help use the library safely. The library needs to be initialized and
-- finalized. The 'run' method provided in 'LibOath' handles unwrapping
-- this type safely.
newtype Action a = Action { unAction :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

-- | The type of errors returned by the @liboath@ library. Technically
-- speaking, this is the Haskell representation of the @oath_rc@ type
-- defined in @liboath/oath.h.in@, which is why it includes the 'OK'
-- constructor. The 'OK' constructor will never be returned in an error
-- case.
data Error
  = OK -- ^ This will never be returned as an error.
  | CryptoError
  | InvalidDigits
  | PrintfError
  | InvalidHex
  | TooSmallBuffer
  | InvalidOtp
  | ReplayedOtp
  | BadPassword
  | InvalidCounter
  | InvalidTimestamp
  | NoSuchFile
  | UnknownUser
  | FileSeekError
  | FileCreateError
  | FileLockError
  | FileRenameError
  | FileUnlinkError
  | TimeError
  | StrcmpError
  | InvalidBase32
  | Base32Overflow
  | MallocError
  | FileFlushError
  | FileSyncError
  | FileCloseError
  | UnknownError
  deriving (Eq, Show)

-- | A type representing 'Base32' encoded 'ByteString's.
newtype Base32 = UnsafeBase32 { unBase32 :: ByteString }
  deriving (Eq, Ord, Show)

pattern Base32 a = UnsafeBase32 a

-- | This function should only be called with the return value of @liboath@
-- library calls.
fromCInt :: C.CInt -> Error
fromCInt i = case i of
  0   -> OK
  -1  -> CryptoError
  -2  -> InvalidDigits
  -3  -> PrintfError
  -4  -> InvalidHex
  -5  -> TooSmallBuffer
  -6  -> InvalidOtp
  -7  -> ReplayedOtp
  -8  -> BadPassword
  -9  -> InvalidCounter
  -10 -> InvalidTimestamp
  -11 -> NoSuchFile
  -12 -> UnknownUser
  -13 -> FileSeekError
  -14 -> FileCreateError
  -15 -> FileLockError
  -16 -> FileRenameError
  -17 -> FileUnlinkError
  -18 -> TimeError
  -19 -> StrcmpError
  -20 -> InvalidBase32
  -21 -> Base32Overflow
  -22 -> MallocError
  -23 -> FileFlushError
  -24 -> FileSyncError
  -25 -> FileCloseError
  _   -> UnknownError



-- C Bindings

_OATH_OK :: C.CInt
_OATH_OK = 0

oath_init :: IO C.CInt
oath_init = [C.exp|int { oath_init() }|]

oath_done :: IO C.CInt
oath_done = [C.exp|int { oath_done() }|]

oath_check_version :: ByteString -> IO C.CString
oath_check_version minVersion =
    [C.exp|const char * { oath_check_version($bs-ptr:minVersion) }|]

oath_strerror :: C.CInt -> IO C.CString
oath_strerror err = [C.exp|const char * { oath_strerror($(int err)) }|]

oath_base32_decode :: C.CString -> C.CSize -> Ptr C.CString -> Ptr C.CSize -> IO C.CInt
oath_base32_decode in_ inlen out outlen =
    [C.exp|int {
        oath_base32_decode($(const char * in_), $(size_t inlen), $(char ** out), $(size_t * outlen))
    }|]

oath_base32_encode :: C.CString -> C.CSize -> Ptr C.CString -> Ptr C.CSize -> IO C.CInt
oath_base32_encode in_ inlen out outlen =
    [C.exp|int {
        oath_base32_encode($(const char * in_), $(size_t inlen), $(char ** out), $(size_t * outlen))
    }|]

oath_totp_generate
    :: C.CString -> C.CSize -> C.CTime -> C.CUInt -> C.CTime -> C.CUInt -> C.CString -> IO C.CInt
oath_totp_generate secret secretLen now timeStepSize startOffset digits outputOtp =
    [C.exp|int {
        oath_totp_generate(
            $(const char * secret),
            $(size_t secretLen),
            $(time_t now),
            $(unsigned int timeStepSize),
            $(time_t startOffset),
            $(unsigned int digits),
            $(char * outputOtp)
        )
    }|]
