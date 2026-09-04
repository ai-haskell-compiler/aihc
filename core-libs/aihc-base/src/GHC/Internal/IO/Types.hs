{-# LANGUAGE ExistentialQuantification #-}

{- HLINT ignore "Use camelCase" -}

-- | The device classes, the handle types, and the IO exception type.
--
-- GHC defines these in "GHC.IO.Device", "GHC.IO.BufferedIO",
-- "GHC.IO.Handle.Types", and "GHC.IO.Exception". Those modules import
-- each other through @hs-boot@ files. This compiler has no @hs-boot@
-- files, so the public modules re-export this one.
module GHC.Internal.IO.Types
  ( -- * Devices
    RawIO (..),
    IODevice (..),
    IODeviceType (..),
    SeekMode (..),
    ioe_unsupportedOperation,

    -- * Buffered devices
    BufferedIO (..),

    -- * Handles
    Handle (..),
    Handle__ (..),
    showHandle,
    checkHandleInvariants,
    BufferList (..),
    HandleType (..),
    isReadableHandleType,
    isWritableHandleType,
    isReadWriteHandleType,
    BufferMode (..),
    Newline (..),
    NewlineMode (..),
    nativeNewline,
    universalNewlineMode,
    noNewlineTranslation,
    nativeNewlineMode,

    -- * IO exceptions
    IOException (..),
    IOError,
    IOErrorType (..),
    ioException,
    ioError,
    userError,
    unsupportedOperation,
  )
where

import Data.Bool (Bool (..), not, (&&))
import Data.Maybe (Maybe (..))
import Data.Typeable (Typeable)
import Foreign.C.Types (CInt)
import GHC.Base (Monad (..), String, id, (.))
import GHC.Exception.Type (Exception (..))
import GHC.IO (FilePath, IO, throwIO)
import GHC.IO.Buffer (Buffer (..), BufferState (..), CharBufElem, RawBuffer)
import GHC.IO.Encoding.Types (TextEncoding)
import GHC.IORef (IORef)
import GHC.Int (Int)
import GHC.Integer (Integer)
import GHC.Internal.Classes (Eq (..), Ord (..), Ordering (..))
import GHC.MVar (MVar)
import GHC.Ptr (Ptr)
import GHC.Show (Show (..), ShowS, showParen, showString, shows)
import GHC.Word (Word64, Word8)

-- ---------------------------------------------------------------------
-- Devices

-- | A device that reads and writes raw bytes.
class RawIO a where
  read :: a -> Ptr Word8 -> Word64 -> Int -> IO Int
  readNonBlocking :: a -> Ptr Word8 -> Word64 -> Int -> IO (Maybe Int)
  write :: a -> Ptr Word8 -> Word64 -> Int -> IO ()
  writeNonBlocking :: a -> Ptr Word8 -> Word64 -> Int -> IO Int

-- | The operations on a device that the handle layer needs. A method
-- without a definition raises an unsupported-operation error.
class IODevice a where
  ready :: a -> Bool -> Int -> IO Bool
  close :: a -> IO ()
  isTerminal :: a -> IO Bool
  isTerminal _ = return False
  isSeekable :: a -> IO Bool
  isSeekable _ = return False
  seek :: a -> SeekMode -> Integer -> IO Integer
  seek _ _ _ = ioe_unsupportedOperation
  tell :: a -> IO Integer
  tell _ = ioe_unsupportedOperation
  getSize :: a -> IO Integer
  getSize _ = ioe_unsupportedOperation
  setSize :: a -> Integer -> IO ()
  setSize _ _ = ioe_unsupportedOperation
  setEcho :: a -> Bool -> IO ()
  setEcho _ _ = ioe_unsupportedOperation
  getEcho :: a -> IO Bool
  getEcho _ = ioe_unsupportedOperation
  setRaw :: a -> Bool -> IO ()
  setRaw _ _ = ioe_unsupportedOperation
  devType :: a -> IO IODeviceType
  dup :: a -> IO a
  dup _ = ioe_unsupportedOperation
  dup2 :: a -> a -> IO a
  dup2 _ _ = ioe_unsupportedOperation

ioe_unsupportedOperation :: IO a
ioe_unsupportedOperation = throwIO unsupportedOperation

data IODeviceType
  = Directory
  | Stream
  | RegularFile
  | RawDevice

instance Eq IODeviceType where
  left == right = deviceTypeTag left == deviceTypeTag right

deviceTypeTag :: IODeviceType -> Int
deviceTypeTag Directory = 0
deviceTypeTag Stream = 1
deviceTypeTag RegularFile = 2
deviceTypeTag RawDevice = 3

data SeekMode
  = AbsoluteSeek
  | RelativeSeek
  | SeekFromEnd

seekModeTag :: SeekMode -> Int
seekModeTag AbsoluteSeek = 0
seekModeTag RelativeSeek = 1
seekModeTag SeekFromEnd = 2

instance Eq SeekMode where
  left == right = seekModeTag left == seekModeTag right

instance Ord SeekMode where
  compare left right = compare (seekModeTag left) (seekModeTag right)

instance Show SeekMode where
  showsPrec _ AbsoluteSeek = showString "AbsoluteSeek"
  showsPrec _ RelativeSeek = showString "RelativeSeek"
  showsPrec _ SeekFromEnd = showString "SeekFromEnd"

-- ---------------------------------------------------------------------
-- Buffered devices

-- | A device that moves bytes through a 'Buffer'.
class BufferedIO dev where
  newBuffer :: dev -> BufferState -> IO (Buffer Word8)
  fillReadBuffer :: dev -> Buffer Word8 -> IO (Int, Buffer Word8)
  fillReadBuffer0 :: dev -> Buffer Word8 -> IO (Maybe Int, Buffer Word8)
  emptyWriteBuffer :: dev -> Buffer Word8 -> IO (Buffer Word8)
  emptyWriteBuffer _ buffer = return buffer {bufL = 0, bufR = 0, bufState = WriteBuffer}
  flushWriteBuffer :: dev -> Buffer Word8 -> IO (Buffer Word8)
  flushWriteBuffer0 :: dev -> Buffer Word8 -> IO (Int, Buffer Word8)

-- ---------------------------------------------------------------------
-- Handles

-- | A file handle. A duplex handle has one read side and one write side.
data Handle
  = FileHandle FilePath !(MVar Handle__)
  | DuplexHandle FilePath !(MVar Handle__) !(MVar Handle__)

-- | The state of one handle side. The device is existential, so the
-- handle layer only uses it through the device classes.
data Handle__
  = forall dev.
  (RawIO dev, IODevice dev, BufferedIO dev, Typeable dev) =>
  Handle__
  { haDevice :: !dev,
    haType :: HandleType,
    haByteBuffer :: !(IORef (Buffer Word8)),
    haBufferMode :: BufferMode,
    haCharBuffer :: !(IORef (Buffer CharBufElem)),
    haBuffers :: !(IORef (BufferList CharBufElem)),
    haCodec :: Maybe TextEncoding,
    haInputNL :: Newline,
    haOutputNL :: Newline,
    haOtherSide :: Maybe (MVar Handle__)
  }

-- | Spare character buffers of a handle.
data BufferList e
  = BufferListNil
  | BufferListCons (RawBuffer e) (BufferList e)

data HandleType
  = ClosedHandle
  | SemiClosedHandle
  | ReadHandle
  | WriteHandle
  | AppendHandle
  | ReadWriteHandle

instance Show HandleType where
  showsPrec _ handleType =
    case handleType of
      ClosedHandle -> showString "closed"
      SemiClosedHandle -> showString "semi-closed"
      ReadHandle -> showString "readable"
      WriteHandle -> showString "writable"
      AppendHandle -> showString "writable (append)"
      ReadWriteHandle -> showString "read-writable"

isReadableHandleType :: HandleType -> Bool
isReadableHandleType ReadHandle = True
isReadableHandleType ReadWriteHandle = True
isReadableHandleType _ = False

isWritableHandleType :: HandleType -> Bool
isWritableHandleType AppendHandle = True
isWritableHandleType WriteHandle = True
isWritableHandleType ReadWriteHandle = True
isWritableHandleType _ = False

isReadWriteHandleType :: HandleType -> Bool
isReadWriteHandleType ReadWriteHandle = True
isReadWriteHandleType _ = False

-- | The buffer invariants hold by construction.
checkHandleInvariants :: Handle__ -> IO ()
checkHandleInvariants _ = return ()

data BufferMode
  = NoBuffering
  | LineBuffering
  | BlockBuffering (Maybe Int)

instance Eq BufferMode where
  NoBuffering == NoBuffering = True
  LineBuffering == LineBuffering = True
  BlockBuffering left == BlockBuffering right = left == right
  _ == _ = False

instance Ord BufferMode where
  compare = compareBufferMode

compareBufferMode :: BufferMode -> BufferMode -> Ordering
compareBufferMode left right =
  case compare (bufferModeTag left) (bufferModeTag right) of
    EQ ->
      case (left, right) of
        (BlockBuffering leftSize, BlockBuffering rightSize) -> compareSize leftSize rightSize
        _ -> EQ
    ordering -> ordering

compareSize :: Maybe Int -> Maybe Int -> Ordering
compareSize Nothing Nothing = EQ
compareSize Nothing (Just _) = LT
compareSize (Just _) Nothing = GT
compareSize (Just left) (Just right) = compare left right

bufferModeTag :: BufferMode -> Int
bufferModeTag NoBuffering = 0
bufferModeTag LineBuffering = 1
bufferModeTag (BlockBuffering _) = 2

instance Show BufferMode where
  showsPrec _ NoBuffering = showString "NoBuffering"
  showsPrec _ LineBuffering = showString "LineBuffering"
  showsPrec precedence (BlockBuffering size) =
    showParen (precedence > 10) (showString "BlockBuffering " . showsPrec 11 size)

data Newline = LF | CRLF

instance Eq Newline where
  LF == LF = True
  CRLF == CRLF = True
  _ == _ = False

instance Ord Newline where
  compare left right = compare (newlineTag left) (newlineTag right)

newlineTag :: Newline -> Int
newlineTag LF = 0
newlineTag CRLF = 1

instance Show Newline where
  showsPrec _ LF = showString "LF"
  showsPrec _ CRLF = showString "CRLF"

data NewlineMode = NewlineMode
  { inputNL :: Newline,
    outputNL :: Newline
  }

instance Eq NewlineMode where
  NewlineMode leftIn leftOut == NewlineMode rightIn rightOut = leftIn == rightIn && leftOut == rightOut

instance Ord NewlineMode where
  compare (NewlineMode leftIn leftOut) (NewlineMode rightIn rightOut) =
    case compare leftIn rightIn of
      EQ -> compare leftOut rightOut
      ordering -> ordering

instance Show NewlineMode where
  showsPrec precedence (NewlineMode input output) =
    showParen
      (precedence > 10)
      (showString "NewlineMode {inputNL = " . shows input . showString ", outputNL = " . shows output . showString "}")

nativeNewline :: Newline
nativeNewline = LF

universalNewlineMode :: NewlineMode
universalNewlineMode = NewlineMode {inputNL = CRLF, outputNL = nativeNewline}

nativeNewlineMode :: NewlineMode
nativeNewlineMode = NewlineMode {inputNL = nativeNewline, outputNL = nativeNewline}

noNewlineTranslation :: NewlineMode
noNewlineTranslation = NewlineMode {inputNL = LF, outputNL = LF}

instance Show Handle where
  showsPrec _ (FileHandle file _) = showHandle' file
  showsPrec _ (DuplexHandle file _ _) = showHandle' file

showHandle' :: FilePath -> ShowS
showHandle' file = showString "{handle: " . showString file . showString "}"

showHandle :: FilePath -> String -> String
showHandle = showHandle'

-- ---------------------------------------------------------------------
-- IO exceptions

-- | The exception that IO operations raise.
data IOException = IOError
  { ioe_handle :: Maybe Handle,
    ioe_type :: IOErrorType,
    ioe_location :: String,
    ioe_description :: String,
    ioe_errno :: Maybe CInt,
    ioe_filename :: Maybe FilePath
  }

type IOError = IOException

instance Exception IOException

-- | Two errors are equal when every field but the handle is equal. The
-- runtime has no handle identity test.
instance Eq IOException where
  IOError _ leftType leftLocation leftDescription leftErrno leftName == IOError _ rightType rightLocation rightDescription rightErrno rightName =
    leftType
      == rightType
      && leftDescription
      == rightDescription
      && leftLocation
      == rightLocation
      && leftErrno
      == rightErrno
      && leftName
      == rightName

instance Show IOException where
  showsPrec _ (IOError handle errorType location description _ name) =
    showSource . showLocation . showsPrec 11 errorType . showDescription
    where
      showSource =
        case name of
          Nothing ->
            case handle of
              Nothing -> id
              Just h -> showsPrec 11 h . showString ": "
          Just fileName -> showString fileName . showString ": "
      showLocation =
        case location of
          [] -> id
          _ -> showString location . showString ": "
      showDescription =
        case description of
          [] -> id
          _ -> showString " (" . showString description . showString ")"

data IOErrorType
  = AlreadyExists
  | NoSuchThing
  | ResourceBusy
  | ResourceExhausted
  | EOF
  | IllegalOperation
  | PermissionDenied
  | UserError
  | UnsatisfiedConstraints
  | SystemError
  | ProtocolError
  | OtherError
  | InvalidArgument
  | InappropriateType
  | HardwareFault
  | UnsupportedOperation
  | TimeExpired
  | ResourceVanished
  | Interrupted

errorTypeTag :: IOErrorType -> Int
errorTypeTag errorType =
  case errorType of
    AlreadyExists -> 0
    NoSuchThing -> 1
    ResourceBusy -> 2
    ResourceExhausted -> 3
    EOF -> 4
    IllegalOperation -> 5
    PermissionDenied -> 6
    UserError -> 7
    UnsatisfiedConstraints -> 8
    SystemError -> 9
    ProtocolError -> 10
    OtherError -> 11
    InvalidArgument -> 12
    InappropriateType -> 13
    HardwareFault -> 14
    UnsupportedOperation -> 15
    TimeExpired -> 16
    ResourceVanished -> 17
    Interrupted -> 18

instance Eq IOErrorType where
  left == right = errorTypeTag left == errorTypeTag right

instance Show IOErrorType where
  showsPrec _ errorType =
    showString
      ( case errorType of
          AlreadyExists -> "already exists"
          NoSuchThing -> "does not exist"
          ResourceBusy -> "resource busy"
          ResourceExhausted -> "resource exhausted"
          EOF -> "end of file"
          IllegalOperation -> "illegal operation"
          PermissionDenied -> "permission denied"
          UserError -> "user error"
          HardwareFault -> "hardware fault"
          InappropriateType -> "inappropriate type"
          Interrupted -> "interrupted"
          InvalidArgument -> "invalid argument"
          OtherError -> "failed"
          ProtocolError -> "protocol error"
          ResourceVanished -> "resource vanished"
          SystemError -> "system error"
          TimeExpired -> "timeout"
          UnsatisfiedConstraints -> "unsatisfied constraints"
          UnsupportedOperation -> "unsupported operation"
      )

ioException :: IOException -> IO a
ioException = throwIO

ioError :: IOError -> IO a
ioError = ioException

userError :: String -> IOError
userError description = IOError Nothing UserError "" description Nothing Nothing

unsupportedOperation :: IOError
unsupportedOperation = IOError Nothing UnsupportedOperation "" "Operation is not supported" Nothing Nothing
