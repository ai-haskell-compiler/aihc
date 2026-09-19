{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Discarding a result would read better as void, but Data.Functor imports
-- Prelude, which imports this module.
{-# HLINT ignore "Use void" #-}

-- | C error numbers and their IO error forms.
--
-- Each constant is the value the C library of the target gives the error.
-- The numbers differ between the platforms -- @EAGAIN@ is 11 on Linux, 35 on
-- macOS and 6 under WASI -- so none of them is written out here: they come
-- from "Foreign.C.Error.Repr", which has one copy per platform and which the
-- cabal file picks between, as it does for the POSIX type widths. An error
-- the platform does not have is @-1@ there, which 'isValidErrno' rejects, as
-- it does in GHC.
--
-- Each constant is therefore an integer literal, so a comparison against one
-- is a comparison against a number rather than a call into C. GHC arrives at
-- the same place by a different road: its configure script writes the
-- numbers into a header its @Foreign.C.Error@ reads through CPP.
--
-- @errno@ itself is a macro over a thread-local location, so 'getErrno'
-- and 'resetErrno' reach it through a runtime shim rather than naming it
-- in a foreign import.
module Foreign.C.Error
  ( Errno (..),
    eOK,
    ePERM,
    eNOENT,
    eSRCH,
    eINTR,
    eIO,
    eNXIO,
    e2BIG,
    eNOEXEC,
    eBADF,
    eCHILD,
    eDEADLK,
    eNOMEM,
    eACCES,
    eFAULT,
    eNOTBLK,
    eBUSY,
    eEXIST,
    eXDEV,
    eNODEV,
    eNOTDIR,
    eISDIR,
    eINVAL,
    eNFILE,
    eMFILE,
    eNOTTY,
    eTXTBSY,
    eFBIG,
    eNOSPC,
    eSPIPE,
    eROFS,
    eMLINK,
    ePIPE,
    eDOM,
    eRANGE,
    eAGAIN,
    eINPROGRESS,
    eALREADY,
    eNOTSOCK,
    eDESTADDRREQ,
    eMSGSIZE,
    ePROTOTYPE,
    eNOPROTOOPT,
    ePROTONOSUPPORT,
    eSOCKTNOSUPPORT,
    eNOTSUP,
    ePFNOSUPPORT,
    eAFNOSUPPORT,
    eADDRINUSE,
    eADDRNOTAVAIL,
    eNETDOWN,
    eNETUNREACH,
    eNETRESET,
    eCONNABORTED,
    eCONNRESET,
    eNOBUFS,
    eISCONN,
    eNOTCONN,
    eSHUTDOWN,
    eTOOMANYREFS,
    eTIMEDOUT,
    eCONNREFUSED,
    eLOOP,
    eNAMETOOLONG,
    eHOSTDOWN,
    eHOSTUNREACH,
    eNOTEMPTY,
    ePROCLIM,
    eUSERS,
    eDQUOT,
    eSTALE,
    eREMOTE,
    eBADRPC,
    eRPCMISMATCH,
    ePROGUNAVAIL,
    ePROGMISMATCH,
    ePROCUNAVAIL,
    eNOLCK,
    eNOSYS,
    eFTYPE,
    eIDRM,
    eNOMSG,
    eOPNOTSUPP,
    eILSEQ,
    eBADMSG,
    eMULTIHOP,
    eNODATA,
    eNOLINK,
    eNOSR,
    eNOSTR,
    ePROTO,
    eTIME,
    eADV,
    eCOMM,
    eDIRTY,
    eNONET,
    eREMCHG,
    eRREMOTE,
    eSRMNT,
    eWOULDBLOCK,
    isValidErrno,
    getErrno,
    resetErrno,
    errnoToIOError,
    throwErrno,
    throwErrnoIf,
    throwErrnoIf_,
    throwErrnoIfRetry,
    throwErrnoIfRetry_,
    throwErrnoIfMinus1,
    throwErrnoIfMinus1_,
    throwErrnoIfMinus1Retry,
    throwErrnoIfMinus1Retry_,
    throwErrnoIfNull,
    throwErrnoIfNullRetry,
    throwErrnoPath,
    throwErrnoPathIf,
    throwErrnoPathIf_,
    throwErrnoPathIfNull,
    throwErrnoPathIfMinus1,
    throwErrnoPathIfMinus1_,
  )
where

import Data.Bool (Bool (..), not)
import Data.Maybe (Maybe (..))
import Foreign.C.Error.Repr
import Foreign.C.Types (CInt)
import GHC.Base (Applicative (..), Monad (..), String, (++))
import GHC.IO (FilePath, IO)
import GHC.Int (Int)
import GHC.Internal.Classes (Eq (..), Ord (..))
import GHC.Internal.IO.Types (Handle, IOError, IOErrorType (..), IOException (..), ioError)
import GHC.Num (Num (..))
import GHC.Ptr (Ptr, nullPtr)
import GHC.Real (fromIntegral)
import GHC.Show (Show (..))

newtype Errno = Errno CInt
  deriving newtype (Eq)

-- | Success. No C library names a macro for it, so it is the one constant
-- written out here.
eOK :: Errno
eOK = Errno 0

ePERM :: Errno
ePERM = Errno cErrnoPERM

eNOENT :: Errno
eNOENT = Errno cErrnoNOENT

eSRCH :: Errno
eSRCH = Errno cErrnoSRCH

eINTR :: Errno
eINTR = Errno cErrnoINTR

eIO :: Errno
eIO = Errno cErrnoIO

eNXIO :: Errno
eNXIO = Errno cErrnoNXIO

e2BIG :: Errno
e2BIG = Errno cErrno2BIG

eNOEXEC :: Errno
eNOEXEC = Errno cErrnoNOEXEC

eBADF :: Errno
eBADF = Errno cErrnoBADF

eCHILD :: Errno
eCHILD = Errno cErrnoCHILD

eDEADLK :: Errno
eDEADLK = Errno cErrnoDEADLK

eNOMEM :: Errno
eNOMEM = Errno cErrnoNOMEM

eACCES :: Errno
eACCES = Errno cErrnoACCES

eFAULT :: Errno
eFAULT = Errno cErrnoFAULT

eNOTBLK :: Errno
eNOTBLK = Errno cErrnoNOTBLK

eBUSY :: Errno
eBUSY = Errno cErrnoBUSY

eEXIST :: Errno
eEXIST = Errno cErrnoEXIST

eXDEV :: Errno
eXDEV = Errno cErrnoXDEV

eNODEV :: Errno
eNODEV = Errno cErrnoNODEV

eNOTDIR :: Errno
eNOTDIR = Errno cErrnoNOTDIR

eISDIR :: Errno
eISDIR = Errno cErrnoISDIR

eINVAL :: Errno
eINVAL = Errno cErrnoINVAL

eNFILE :: Errno
eNFILE = Errno cErrnoNFILE

eMFILE :: Errno
eMFILE = Errno cErrnoMFILE

eNOTTY :: Errno
eNOTTY = Errno cErrnoNOTTY

eTXTBSY :: Errno
eTXTBSY = Errno cErrnoTXTBSY

eFBIG :: Errno
eFBIG = Errno cErrnoFBIG

eNOSPC :: Errno
eNOSPC = Errno cErrnoNOSPC

eSPIPE :: Errno
eSPIPE = Errno cErrnoSPIPE

eROFS :: Errno
eROFS = Errno cErrnoROFS

eMLINK :: Errno
eMLINK = Errno cErrnoMLINK

ePIPE :: Errno
ePIPE = Errno cErrnoPIPE

eDOM :: Errno
eDOM = Errno cErrnoDOM

eRANGE :: Errno
eRANGE = Errno cErrnoRANGE

eAGAIN :: Errno
eAGAIN = Errno cErrnoAGAIN

eINPROGRESS :: Errno
eINPROGRESS = Errno cErrnoINPROGRESS

eALREADY :: Errno
eALREADY = Errno cErrnoALREADY

eNOTSOCK :: Errno
eNOTSOCK = Errno cErrnoNOTSOCK

eDESTADDRREQ :: Errno
eDESTADDRREQ = Errno cErrnoDESTADDRREQ

eMSGSIZE :: Errno
eMSGSIZE = Errno cErrnoMSGSIZE

ePROTOTYPE :: Errno
ePROTOTYPE = Errno cErrnoPROTOTYPE

eNOPROTOOPT :: Errno
eNOPROTOOPT = Errno cErrnoNOPROTOOPT

ePROTONOSUPPORT :: Errno
ePROTONOSUPPORT = Errno cErrnoPROTONOSUPPORT

eSOCKTNOSUPPORT :: Errno
eSOCKTNOSUPPORT = Errno cErrnoSOCKTNOSUPPORT

eNOTSUP :: Errno
eNOTSUP = Errno cErrnoNOTSUP

ePFNOSUPPORT :: Errno
ePFNOSUPPORT = Errno cErrnoPFNOSUPPORT

eAFNOSUPPORT :: Errno
eAFNOSUPPORT = Errno cErrnoAFNOSUPPORT

eADDRINUSE :: Errno
eADDRINUSE = Errno cErrnoADDRINUSE

eADDRNOTAVAIL :: Errno
eADDRNOTAVAIL = Errno cErrnoADDRNOTAVAIL

eNETDOWN :: Errno
eNETDOWN = Errno cErrnoNETDOWN

eNETUNREACH :: Errno
eNETUNREACH = Errno cErrnoNETUNREACH

eNETRESET :: Errno
eNETRESET = Errno cErrnoNETRESET

eCONNABORTED :: Errno
eCONNABORTED = Errno cErrnoCONNABORTED

eCONNRESET :: Errno
eCONNRESET = Errno cErrnoCONNRESET

eNOBUFS :: Errno
eNOBUFS = Errno cErrnoNOBUFS

eISCONN :: Errno
eISCONN = Errno cErrnoISCONN

eNOTCONN :: Errno
eNOTCONN = Errno cErrnoNOTCONN

eSHUTDOWN :: Errno
eSHUTDOWN = Errno cErrnoSHUTDOWN

eTOOMANYREFS :: Errno
eTOOMANYREFS = Errno cErrnoTOOMANYREFS

eTIMEDOUT :: Errno
eTIMEDOUT = Errno cErrnoTIMEDOUT

eCONNREFUSED :: Errno
eCONNREFUSED = Errno cErrnoCONNREFUSED

eLOOP :: Errno
eLOOP = Errno cErrnoLOOP

eNAMETOOLONG :: Errno
eNAMETOOLONG = Errno cErrnoNAMETOOLONG

eHOSTDOWN :: Errno
eHOSTDOWN = Errno cErrnoHOSTDOWN

eHOSTUNREACH :: Errno
eHOSTUNREACH = Errno cErrnoHOSTUNREACH

eNOTEMPTY :: Errno
eNOTEMPTY = Errno cErrnoNOTEMPTY

ePROCLIM :: Errno
ePROCLIM = Errno cErrnoPROCLIM

eUSERS :: Errno
eUSERS = Errno cErrnoUSERS

eDQUOT :: Errno
eDQUOT = Errno cErrnoDQUOT

eSTALE :: Errno
eSTALE = Errno cErrnoSTALE

eREMOTE :: Errno
eREMOTE = Errno cErrnoREMOTE

eBADRPC :: Errno
eBADRPC = Errno cErrnoBADRPC

eRPCMISMATCH :: Errno
eRPCMISMATCH = Errno cErrnoRPCMISMATCH

ePROGUNAVAIL :: Errno
ePROGUNAVAIL = Errno cErrnoPROGUNAVAIL

ePROGMISMATCH :: Errno
ePROGMISMATCH = Errno cErrnoPROGMISMATCH

ePROCUNAVAIL :: Errno
ePROCUNAVAIL = Errno cErrnoPROCUNAVAIL

eNOLCK :: Errno
eNOLCK = Errno cErrnoNOLCK

eNOSYS :: Errno
eNOSYS = Errno cErrnoNOSYS

eFTYPE :: Errno
eFTYPE = Errno cErrnoFTYPE

eIDRM :: Errno
eIDRM = Errno cErrnoIDRM

eNOMSG :: Errno
eNOMSG = Errno cErrnoNOMSG

eOPNOTSUPP :: Errno
eOPNOTSUPP = Errno cErrnoOPNOTSUPP

eILSEQ :: Errno
eILSEQ = Errno cErrnoILSEQ

eBADMSG :: Errno
eBADMSG = Errno cErrnoBADMSG

eMULTIHOP :: Errno
eMULTIHOP = Errno cErrnoMULTIHOP

eNODATA :: Errno
eNODATA = Errno cErrnoNODATA

eNOLINK :: Errno
eNOLINK = Errno cErrnoNOLINK

eNOSR :: Errno
eNOSR = Errno cErrnoNOSR

eNOSTR :: Errno
eNOSTR = Errno cErrnoNOSTR

ePROTO :: Errno
ePROTO = Errno cErrnoPROTO

eTIME :: Errno
eTIME = Errno cErrnoTIME

eADV :: Errno
eADV = Errno cErrnoADV

eCOMM :: Errno
eCOMM = Errno cErrnoCOMM

eDIRTY :: Errno
eDIRTY = Errno cErrnoDIRTY

eNONET :: Errno
eNONET = Errno cErrnoNONET

eREMCHG :: Errno
eREMCHG = Errno cErrnoREMCHG

eRREMOTE :: Errno
eRREMOTE = Errno cErrnoRREMOTE

eSRMNT :: Errno
eSRMNT = Errno cErrnoSRMNT

eWOULDBLOCK :: Errno
eWOULDBLOCK = Errno cErrnoWOULDBLOCK

isValidErrno :: Errno -> Bool
isValidErrno (Errno value) = value /= negate 1

-- | The IO error for an error number. The description is the C library
-- text for the error number.
errnoToIOError :: String -> Errno -> Maybe Handle -> Maybe String -> IOError
errnoToIOError location errno@(Errno code) handle name =
  case errnoDetails errno of
    (errorType, description) -> IOError handle errorType location description (Just code) name

-- | An error the C library of the target does not have is @-1@, which no
-- operation reports, so the unsupported constants are ruled out before the
-- comparisons below could confuse one of them with another.
errnoDetails :: Errno -> (IOErrorType, String)
errnoDetails errno@(Errno code)
  | not (isValidErrno errno) = unknown
  | errno == ePERM = (PermissionDenied, "Operation not permitted")
  | errno == eNOENT = (NoSuchThing, "No such file or directory")
  | errno == eSRCH = (NoSuchThing, "No such process")
  | errno == eINTR = (Interrupted, "Interrupted system call")
  | errno == eIO = (HardwareFault, "Input/output error")
  | errno == eNXIO = (NoSuchThing, "Device not configured")
  | errno == e2BIG = (ResourceExhausted, "Argument list too long")
  | errno == eNOEXEC = (InvalidArgument, "Exec format error")
  | errno == eBADF = (InvalidArgument, "Bad file descriptor")
  | errno == eCHILD = (NoSuchThing, "No child processes")
  | errno == eDEADLK = (ResourceBusy, "Resource deadlock avoided")
  | errno == eNOMEM = (ResourceExhausted, "Cannot allocate memory")
  | errno == eACCES = (PermissionDenied, "Permission denied")
  | errno == eFAULT = (OtherError, "Bad address")
  | errno == eNOTBLK = (InvalidArgument, "Block device required")
  | errno == eBUSY = (ResourceBusy, "Resource busy")
  | errno == eEXIST = (AlreadyExists, "File exists")
  | errno == eXDEV = (UnsupportedOperation, "Cross-device link")
  | errno == eNODEV = (UnsupportedOperation, "Operation not supported by device")
  | errno == eNOTDIR = (InappropriateType, "Not a directory")
  | errno == eISDIR = (InappropriateType, "Is a directory")
  | errno == eINVAL = (InvalidArgument, "Invalid argument")
  | errno == eNFILE = (ResourceExhausted, "Too many open files in system")
  | errno == eMFILE = (ResourceExhausted, "Too many open files")
  | errno == eNOTTY = (IllegalOperation, "Inappropriate ioctl for device")
  | errno == eTXTBSY = (ResourceBusy, "Text file busy")
  | errno == eFBIG = (PermissionDenied, "File too large")
  | errno == eNOSPC = (ResourceExhausted, "No space left on device")
  | errno == eSPIPE = (UnsupportedOperation, "Illegal seek")
  | errno == eROFS = (PermissionDenied, "Read-only file system")
  | errno == eMLINK = (ResourceExhausted, "Too many links")
  | errno == ePIPE = (ResourceVanished, "Broken pipe")
  | errno == eDOM = (InvalidArgument, "Numerical argument out of domain")
  | errno == eRANGE = (UnsupportedOperation, "Result too large")
  | errno == eAGAIN = (ResourceExhausted, "Resource temporarily unavailable")
  | errno == eINPROGRESS = (AlreadyExists, "Operation now in progress")
  | errno == eALREADY = (AlreadyExists, "Operation already in progress")
  | errno == eNOTSOCK = (InvalidArgument, "Socket operation on non-socket")
  | errno == eDESTADDRREQ = (InvalidArgument, "Destination address required")
  | errno == eMSGSIZE = (ResourceExhausted, "Message too long")
  | errno == ePROTOTYPE = (ProtocolError, "Protocol wrong type for socket")
  | errno == eNOPROTOOPT = (UnsupportedOperation, "Protocol not available")
  | errno == ePROTONOSUPPORT = (ProtocolError, "Protocol not supported")
  | errno == eSOCKTNOSUPPORT = (UnsupportedOperation, "Socket type not supported")
  | errno == eNOTSUP = (UnsupportedOperation, "Operation not supported")
  | errno == ePFNOSUPPORT = (UnsupportedOperation, "Protocol family not supported")
  | errno == eAFNOSUPPORT = (UnsupportedOperation, "Address family not supported by protocol family")
  | errno == eADDRINUSE = (ResourceBusy, "Address already in use")
  | errno == eADDRNOTAVAIL = (UnsupportedOperation, "Can't assign requested address")
  | errno == eNETDOWN = (ResourceVanished, "Network is down")
  | errno == eNETUNREACH = (NoSuchThing, "Network is unreachable")
  | errno == eNETRESET = (ResourceVanished, "Network dropped connection on reset")
  | errno == eCONNABORTED = (OtherError, "Software caused connection abort")
  | errno == eCONNRESET = (ResourceVanished, "Connection reset by peer")
  | errno == eNOBUFS = (ResourceExhausted, "No buffer space available")
  | errno == eISCONN = (AlreadyExists, "Socket is already connected")
  | errno == eNOTCONN = (InvalidArgument, "Socket is not connected")
  | errno == eSHUTDOWN = (IllegalOperation, "Can't send after socket shutdown")
  | errno == eTOOMANYREFS = (ResourceExhausted, "Too many references: can't splice")
  | errno == eTIMEDOUT = (TimeExpired, "Operation timed out")
  | errno == eCONNREFUSED = (NoSuchThing, "Connection refused")
  | errno == eLOOP = (InvalidArgument, "Too many levels of symbolic links")
  | errno == eNAMETOOLONG = (InvalidArgument, "File name too long")
  | errno == eHOSTDOWN = (NoSuchThing, "Host is down")
  | errno == eHOSTUNREACH = (NoSuchThing, "No route to host")
  | errno == eNOTEMPTY = (UnsatisfiedConstraints, "Directory not empty")
  | errno == ePROCLIM = (PermissionDenied, "Too many processes")
  | errno == eUSERS = (ResourceExhausted, "Too many users")
  | errno == eDQUOT = (PermissionDenied, "Disc quota exceeded")
  | errno == eSTALE = (ResourceVanished, "Stale NFS file handle")
  | errno == eREMOTE = (IllegalOperation, "Too many levels of remote in path")
  | errno == eBADRPC = (OtherError, "RPC struct is bad")
  | errno == eRPCMISMATCH = (ProtocolError, "RPC version wrong")
  | errno == ePROGUNAVAIL = (UnsupportedOperation, "RPC program not available")
  | errno == ePROGMISMATCH = (ProtocolError, "Program version wrong")
  | errno == ePROCUNAVAIL = (UnsupportedOperation, "Bad procedure for program")
  | errno == eNOLCK = (ResourceExhausted, "No locks available")
  | errno == eNOSYS = (UnsupportedOperation, "Function not implemented")
  | errno == eFTYPE = (InappropriateType, "Inappropriate file type or format")
  | errno == eIDRM = (ResourceVanished, "Identifier removed")
  | errno == eNOMSG = (NoSuchThing, "No message of desired type")
  | errno == eOPNOTSUPP = (UnsupportedOperation, "Operation not supported")
  | errno == eILSEQ = (InvalidArgument, "Illegal byte sequence")
  | errno == eBADMSG = (InappropriateType, "Bad message")
  | errno == eMULTIHOP = (UnsupportedOperation, "Multihop attempted")
  | errno == eNODATA = (NoSuchThing, "No message available")
  | errno == eNOLINK = (ResourceVanished, "Link has been severed")
  | errno == eNOSR = (ResourceExhausted, "No STREAM resources")
  | errno == eNOSTR = (InvalidArgument, "Not a STREAM")
  | errno == ePROTO = (ProtocolError, "Protocol error")
  | errno == eTIME = (TimeExpired, "STREAM ioctl timeout")
  | errno == eADV = (OtherError, "Advertise error")
  | errno == eCOMM = (ResourceVanished, "Communication error on send")
  | errno == eDIRTY = (UnsatisfiedConstraints, "Mounting a dirty file system")
  | errno == eNONET = (NoSuchThing, "Machine is not on the network")
  | errno == eREMCHG = (ResourceVanished, "Remote address changed")
  | errno == eRREMOTE = (IllegalOperation, "Object is remote")
  | errno == eSRMNT = (UnsatisfiedConstraints, "Srmount error")
  | errno == eWOULDBLOCK = (OtherError, "Operation would block")
  | otherwise = unknown
  where
    otherwise = True
    unknown = (OtherError, "errno " ++ show (fromIntegral code :: Int))

foreign import ccall unsafe "aihc_errno_get"
  errnoGet :: IO Int

foreign import ccall unsafe "aihc_errno_set"
  errnoSet :: Int -> IO Int

-- | The current value of @errno@.
getErrno :: IO Errno
getErrno = errnoGet >>= \value -> pure (Errno (fromIntegral value))

-- | Set @errno@ back to zero.
resetErrno :: IO ()
resetErrno = errnoSet 0 >> pure ()

-- | Throw the IO error that the current @errno@ denotes.
throwErrno :: String -> IO a
throwErrno location =
  getErrno >>= \errno -> ioError (errnoToIOError location errno Nothing Nothing)

-- | Run an action and throw when its result denotes an error.
throwErrnoIf :: (a -> Bool) -> String -> IO a -> IO a
throwErrnoIf failed location action =
  action >>= \result ->
    if failed result then throwErrno location else pure result

-- | 'throwErrnoIf' discarding the result.
throwErrnoIf_ :: (a -> Bool) -> String -> IO a -> IO ()
throwErrnoIf_ failed location action =
  throwErrnoIf failed location action >> pure ()

-- | 'throwErrnoIf' that runs the action again when it was interrupted.
throwErrnoIfRetry :: (a -> Bool) -> String -> IO a -> IO a
throwErrnoIfRetry failed location action = retry
  where
    retry =
      action >>= \result ->
        if failed result
          then
            getErrno >>= \errno ->
              if errno == eINTR then retry else throwErrno location
          else pure result

-- | 'throwErrnoIfRetry' discarding the result.
throwErrnoIfRetry_ :: (a -> Bool) -> String -> IO a -> IO ()
throwErrnoIfRetry_ failed location action =
  throwErrnoIfRetry failed location action >> pure ()

-- | Throw when the action returns @-1@, the usual C failure result.
throwErrnoIfMinus1 :: (Eq a, Num a) => String -> IO a -> IO a
throwErrnoIfMinus1 = throwErrnoIf (\result -> result == negate 1)

-- | 'throwErrnoIfMinus1' discarding the result.
throwErrnoIfMinus1_ :: (Eq a, Num a) => String -> IO a -> IO ()
throwErrnoIfMinus1_ = throwErrnoIf_ (\result -> result == negate 1)

-- | 'throwErrnoIfMinus1' that runs the action again when it was interrupted.
throwErrnoIfMinus1Retry :: (Eq a, Num a) => String -> IO a -> IO a
throwErrnoIfMinus1Retry = throwErrnoIfRetry (\result -> result == negate 1)

-- | 'throwErrnoIfMinus1Retry' discarding the result.
throwErrnoIfMinus1Retry_ :: (Eq a, Num a) => String -> IO a -> IO ()
throwErrnoIfMinus1Retry_ = throwErrnoIfRetry_ (\result -> result == negate 1)

-- | Throw when the action returns a null pointer.
throwErrnoIfNull :: String -> IO (Ptr a) -> IO (Ptr a)
throwErrnoIfNull = throwErrnoIf (== nullPtr)

-- | 'throwErrnoIfNull' that runs the action again when it was interrupted.
throwErrnoIfNullRetry :: String -> IO (Ptr a) -> IO (Ptr a)
throwErrnoIfNullRetry = throwErrnoIfRetry (== nullPtr)

-- | 'throwErrno' naming the file the operation was working on.
throwErrnoPath :: String -> FilePath -> IO a
throwErrnoPath location path =
  getErrno >>= \errno -> ioError (errnoToIOError location errno Nothing (Just path))

-- | 'throwErrnoIf' naming the file the operation was working on.
throwErrnoPathIf :: (a -> Bool) -> String -> FilePath -> IO a -> IO a
throwErrnoPathIf failed location path action =
  action >>= \result ->
    if failed result then throwErrnoPath location path else pure result

-- | 'throwErrnoPathIf' discarding the result.
throwErrnoPathIf_ :: (a -> Bool) -> String -> FilePath -> IO a -> IO ()
throwErrnoPathIf_ failed location path action =
  throwErrnoPathIf failed location path action >> pure ()

-- | 'throwErrnoPathIf' for an action that returns a null pointer.
throwErrnoPathIfNull :: String -> FilePath -> IO (Ptr a) -> IO (Ptr a)
throwErrnoPathIfNull = throwErrnoPathIf (== nullPtr)

-- | 'throwErrnoPathIf' for an action that returns @-1@.
throwErrnoPathIfMinus1 :: (Eq a, Num a) => String -> FilePath -> IO a -> IO a
throwErrnoPathIfMinus1 = throwErrnoPathIf (\result -> result == negate 1)

-- | 'throwErrnoPathIfMinus1' discarding the result.
throwErrnoPathIfMinus1_ :: (Eq a, Num a) => String -> FilePath -> IO a -> IO ()
throwErrnoPathIfMinus1_ = throwErrnoPathIf_ (\result -> result == negate 1)
