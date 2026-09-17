{-# LANGUAGE CApiFFI #-}
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
-- macOS and 6 under WASI -- so none of them is written out here: each is read
-- out of the target's own @errno.h@ through @aihc_errno.h@, which names every
-- error the module exports and gives @-1@ to an error the platform does not
-- have. 'isValidErrno' rejects that, as it does in GHC.
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

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_EPERM"
  cErrnoPERM :: CInt

ePERM :: Errno
ePERM = Errno cErrnoPERM

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_ENOENT"
  cErrnoNOENT :: CInt

eNOENT :: Errno
eNOENT = Errno cErrnoNOENT

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_ESRCH"
  cErrnoSRCH :: CInt

eSRCH :: Errno
eSRCH = Errno cErrnoSRCH

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_EINTR"
  cErrnoINTR :: CInt

eINTR :: Errno
eINTR = Errno cErrnoINTR

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_EIO"
  cErrnoIO :: CInt

eIO :: Errno
eIO = Errno cErrnoIO

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_ENXIO"
  cErrnoNXIO :: CInt

eNXIO :: Errno
eNXIO = Errno cErrnoNXIO

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_E2BIG"
  cErrno2BIG :: CInt

e2BIG :: Errno
e2BIG = Errno cErrno2BIG

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_ENOEXEC"
  cErrnoNOEXEC :: CInt

eNOEXEC :: Errno
eNOEXEC = Errno cErrnoNOEXEC

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_EBADF"
  cErrnoBADF :: CInt

eBADF :: Errno
eBADF = Errno cErrnoBADF

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_ECHILD"
  cErrnoCHILD :: CInt

eCHILD :: Errno
eCHILD = Errno cErrnoCHILD

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_EDEADLK"
  cErrnoDEADLK :: CInt

eDEADLK :: Errno
eDEADLK = Errno cErrnoDEADLK

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_ENOMEM"
  cErrnoNOMEM :: CInt

eNOMEM :: Errno
eNOMEM = Errno cErrnoNOMEM

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_EACCES"
  cErrnoACCES :: CInt

eACCES :: Errno
eACCES = Errno cErrnoACCES

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_EFAULT"
  cErrnoFAULT :: CInt

eFAULT :: Errno
eFAULT = Errno cErrnoFAULT

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_ENOTBLK"
  cErrnoNOTBLK :: CInt

eNOTBLK :: Errno
eNOTBLK = Errno cErrnoNOTBLK

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_EBUSY"
  cErrnoBUSY :: CInt

eBUSY :: Errno
eBUSY = Errno cErrnoBUSY

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_EEXIST"
  cErrnoEXIST :: CInt

eEXIST :: Errno
eEXIST = Errno cErrnoEXIST

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_EXDEV"
  cErrnoXDEV :: CInt

eXDEV :: Errno
eXDEV = Errno cErrnoXDEV

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_ENODEV"
  cErrnoNODEV :: CInt

eNODEV :: Errno
eNODEV = Errno cErrnoNODEV

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_ENOTDIR"
  cErrnoNOTDIR :: CInt

eNOTDIR :: Errno
eNOTDIR = Errno cErrnoNOTDIR

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_EISDIR"
  cErrnoISDIR :: CInt

eISDIR :: Errno
eISDIR = Errno cErrnoISDIR

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_EINVAL"
  cErrnoINVAL :: CInt

eINVAL :: Errno
eINVAL = Errno cErrnoINVAL

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_ENFILE"
  cErrnoNFILE :: CInt

eNFILE :: Errno
eNFILE = Errno cErrnoNFILE

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_EMFILE"
  cErrnoMFILE :: CInt

eMFILE :: Errno
eMFILE = Errno cErrnoMFILE

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_ENOTTY"
  cErrnoNOTTY :: CInt

eNOTTY :: Errno
eNOTTY = Errno cErrnoNOTTY

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_ETXTBSY"
  cErrnoTXTBSY :: CInt

eTXTBSY :: Errno
eTXTBSY = Errno cErrnoTXTBSY

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_EFBIG"
  cErrnoFBIG :: CInt

eFBIG :: Errno
eFBIG = Errno cErrnoFBIG

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_ENOSPC"
  cErrnoNOSPC :: CInt

eNOSPC :: Errno
eNOSPC = Errno cErrnoNOSPC

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_ESPIPE"
  cErrnoSPIPE :: CInt

eSPIPE :: Errno
eSPIPE = Errno cErrnoSPIPE

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_EROFS"
  cErrnoROFS :: CInt

eROFS :: Errno
eROFS = Errno cErrnoROFS

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_EMLINK"
  cErrnoMLINK :: CInt

eMLINK :: Errno
eMLINK = Errno cErrnoMLINK

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_EPIPE"
  cErrnoPIPE :: CInt

ePIPE :: Errno
ePIPE = Errno cErrnoPIPE

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_EDOM"
  cErrnoDOM :: CInt

eDOM :: Errno
eDOM = Errno cErrnoDOM

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_ERANGE"
  cErrnoRANGE :: CInt

eRANGE :: Errno
eRANGE = Errno cErrnoRANGE

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_EAGAIN"
  cErrnoAGAIN :: CInt

eAGAIN :: Errno
eAGAIN = Errno cErrnoAGAIN

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_EINPROGRESS"
  cErrnoINPROGRESS :: CInt

eINPROGRESS :: Errno
eINPROGRESS = Errno cErrnoINPROGRESS

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_EALREADY"
  cErrnoALREADY :: CInt

eALREADY :: Errno
eALREADY = Errno cErrnoALREADY

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_ENOTSOCK"
  cErrnoNOTSOCK :: CInt

eNOTSOCK :: Errno
eNOTSOCK = Errno cErrnoNOTSOCK

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_EDESTADDRREQ"
  cErrnoDESTADDRREQ :: CInt

eDESTADDRREQ :: Errno
eDESTADDRREQ = Errno cErrnoDESTADDRREQ

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_EMSGSIZE"
  cErrnoMSGSIZE :: CInt

eMSGSIZE :: Errno
eMSGSIZE = Errno cErrnoMSGSIZE

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_EPROTOTYPE"
  cErrnoPROTOTYPE :: CInt

ePROTOTYPE :: Errno
ePROTOTYPE = Errno cErrnoPROTOTYPE

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_ENOPROTOOPT"
  cErrnoNOPROTOOPT :: CInt

eNOPROTOOPT :: Errno
eNOPROTOOPT = Errno cErrnoNOPROTOOPT

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_EPROTONOSUPPORT"
  cErrnoPROTONOSUPPORT :: CInt

ePROTONOSUPPORT :: Errno
ePROTONOSUPPORT = Errno cErrnoPROTONOSUPPORT

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_ESOCKTNOSUPPORT"
  cErrnoSOCKTNOSUPPORT :: CInt

eSOCKTNOSUPPORT :: Errno
eSOCKTNOSUPPORT = Errno cErrnoSOCKTNOSUPPORT

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_ENOTSUP"
  cErrnoNOTSUP :: CInt

eNOTSUP :: Errno
eNOTSUP = Errno cErrnoNOTSUP

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_EPFNOSUPPORT"
  cErrnoPFNOSUPPORT :: CInt

ePFNOSUPPORT :: Errno
ePFNOSUPPORT = Errno cErrnoPFNOSUPPORT

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_EAFNOSUPPORT"
  cErrnoAFNOSUPPORT :: CInt

eAFNOSUPPORT :: Errno
eAFNOSUPPORT = Errno cErrnoAFNOSUPPORT

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_EADDRINUSE"
  cErrnoADDRINUSE :: CInt

eADDRINUSE :: Errno
eADDRINUSE = Errno cErrnoADDRINUSE

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_EADDRNOTAVAIL"
  cErrnoADDRNOTAVAIL :: CInt

eADDRNOTAVAIL :: Errno
eADDRNOTAVAIL = Errno cErrnoADDRNOTAVAIL

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_ENETDOWN"
  cErrnoNETDOWN :: CInt

eNETDOWN :: Errno
eNETDOWN = Errno cErrnoNETDOWN

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_ENETUNREACH"
  cErrnoNETUNREACH :: CInt

eNETUNREACH :: Errno
eNETUNREACH = Errno cErrnoNETUNREACH

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_ENETRESET"
  cErrnoNETRESET :: CInt

eNETRESET :: Errno
eNETRESET = Errno cErrnoNETRESET

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_ECONNABORTED"
  cErrnoCONNABORTED :: CInt

eCONNABORTED :: Errno
eCONNABORTED = Errno cErrnoCONNABORTED

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_ECONNRESET"
  cErrnoCONNRESET :: CInt

eCONNRESET :: Errno
eCONNRESET = Errno cErrnoCONNRESET

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_ENOBUFS"
  cErrnoNOBUFS :: CInt

eNOBUFS :: Errno
eNOBUFS = Errno cErrnoNOBUFS

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_EISCONN"
  cErrnoISCONN :: CInt

eISCONN :: Errno
eISCONN = Errno cErrnoISCONN

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_ENOTCONN"
  cErrnoNOTCONN :: CInt

eNOTCONN :: Errno
eNOTCONN = Errno cErrnoNOTCONN

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_ESHUTDOWN"
  cErrnoSHUTDOWN :: CInt

eSHUTDOWN :: Errno
eSHUTDOWN = Errno cErrnoSHUTDOWN

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_ETOOMANYREFS"
  cErrnoTOOMANYREFS :: CInt

eTOOMANYREFS :: Errno
eTOOMANYREFS = Errno cErrnoTOOMANYREFS

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_ETIMEDOUT"
  cErrnoTIMEDOUT :: CInt

eTIMEDOUT :: Errno
eTIMEDOUT = Errno cErrnoTIMEDOUT

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_ECONNREFUSED"
  cErrnoCONNREFUSED :: CInt

eCONNREFUSED :: Errno
eCONNREFUSED = Errno cErrnoCONNREFUSED

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_ELOOP"
  cErrnoLOOP :: CInt

eLOOP :: Errno
eLOOP = Errno cErrnoLOOP

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_ENAMETOOLONG"
  cErrnoNAMETOOLONG :: CInt

eNAMETOOLONG :: Errno
eNAMETOOLONG = Errno cErrnoNAMETOOLONG

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_EHOSTDOWN"
  cErrnoHOSTDOWN :: CInt

eHOSTDOWN :: Errno
eHOSTDOWN = Errno cErrnoHOSTDOWN

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_EHOSTUNREACH"
  cErrnoHOSTUNREACH :: CInt

eHOSTUNREACH :: Errno
eHOSTUNREACH = Errno cErrnoHOSTUNREACH

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_ENOTEMPTY"
  cErrnoNOTEMPTY :: CInt

eNOTEMPTY :: Errno
eNOTEMPTY = Errno cErrnoNOTEMPTY

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_EPROCLIM"
  cErrnoPROCLIM :: CInt

ePROCLIM :: Errno
ePROCLIM = Errno cErrnoPROCLIM

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_EUSERS"
  cErrnoUSERS :: CInt

eUSERS :: Errno
eUSERS = Errno cErrnoUSERS

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_EDQUOT"
  cErrnoDQUOT :: CInt

eDQUOT :: Errno
eDQUOT = Errno cErrnoDQUOT

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_ESTALE"
  cErrnoSTALE :: CInt

eSTALE :: Errno
eSTALE = Errno cErrnoSTALE

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_EREMOTE"
  cErrnoREMOTE :: CInt

eREMOTE :: Errno
eREMOTE = Errno cErrnoREMOTE

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_EBADRPC"
  cErrnoBADRPC :: CInt

eBADRPC :: Errno
eBADRPC = Errno cErrnoBADRPC

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_ERPCMISMATCH"
  cErrnoRPCMISMATCH :: CInt

eRPCMISMATCH :: Errno
eRPCMISMATCH = Errno cErrnoRPCMISMATCH

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_EPROGUNAVAIL"
  cErrnoPROGUNAVAIL :: CInt

ePROGUNAVAIL :: Errno
ePROGUNAVAIL = Errno cErrnoPROGUNAVAIL

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_EPROGMISMATCH"
  cErrnoPROGMISMATCH :: CInt

ePROGMISMATCH :: Errno
ePROGMISMATCH = Errno cErrnoPROGMISMATCH

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_EPROCUNAVAIL"
  cErrnoPROCUNAVAIL :: CInt

ePROCUNAVAIL :: Errno
ePROCUNAVAIL = Errno cErrnoPROCUNAVAIL

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_ENOLCK"
  cErrnoNOLCK :: CInt

eNOLCK :: Errno
eNOLCK = Errno cErrnoNOLCK

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_ENOSYS"
  cErrnoNOSYS :: CInt

eNOSYS :: Errno
eNOSYS = Errno cErrnoNOSYS

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_EFTYPE"
  cErrnoFTYPE :: CInt

eFTYPE :: Errno
eFTYPE = Errno cErrnoFTYPE

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_EIDRM"
  cErrnoIDRM :: CInt

eIDRM :: Errno
eIDRM = Errno cErrnoIDRM

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_ENOMSG"
  cErrnoNOMSG :: CInt

eNOMSG :: Errno
eNOMSG = Errno cErrnoNOMSG

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_EOPNOTSUPP"
  cErrnoOPNOTSUPP :: CInt

eOPNOTSUPP :: Errno
eOPNOTSUPP = Errno cErrnoOPNOTSUPP

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_EILSEQ"
  cErrnoILSEQ :: CInt

eILSEQ :: Errno
eILSEQ = Errno cErrnoILSEQ

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_EBADMSG"
  cErrnoBADMSG :: CInt

eBADMSG :: Errno
eBADMSG = Errno cErrnoBADMSG

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_EMULTIHOP"
  cErrnoMULTIHOP :: CInt

eMULTIHOP :: Errno
eMULTIHOP = Errno cErrnoMULTIHOP

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_ENODATA"
  cErrnoNODATA :: CInt

eNODATA :: Errno
eNODATA = Errno cErrnoNODATA

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_ENOLINK"
  cErrnoNOLINK :: CInt

eNOLINK :: Errno
eNOLINK = Errno cErrnoNOLINK

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_ENOSR"
  cErrnoNOSR :: CInt

eNOSR :: Errno
eNOSR = Errno cErrnoNOSR

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_ENOSTR"
  cErrnoNOSTR :: CInt

eNOSTR :: Errno
eNOSTR = Errno cErrnoNOSTR

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_EPROTO"
  cErrnoPROTO :: CInt

ePROTO :: Errno
ePROTO = Errno cErrnoPROTO

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_ETIME"
  cErrnoTIME :: CInt

eTIME :: Errno
eTIME = Errno cErrnoTIME

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_EADV"
  cErrnoADV :: CInt

eADV :: Errno
eADV = Errno cErrnoADV

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_ECOMM"
  cErrnoCOMM :: CInt

eCOMM :: Errno
eCOMM = Errno cErrnoCOMM

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_EDIRTY"
  cErrnoDIRTY :: CInt

eDIRTY :: Errno
eDIRTY = Errno cErrnoDIRTY

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_ENONET"
  cErrnoNONET :: CInt

eNONET :: Errno
eNONET = Errno cErrnoNONET

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_EREMCHG"
  cErrnoREMCHG :: CInt

eREMCHG :: Errno
eREMCHG = Errno cErrnoREMCHG

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_ERREMOTE"
  cErrnoRREMOTE :: CInt

eRREMOTE :: Errno
eRREMOTE = Errno cErrnoRREMOTE

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_ESRMNT"
  cErrnoSRMNT :: CInt

eSRMNT :: Errno
eSRMNT = Errno cErrnoSRMNT

foreign import capi unsafe "aihc_errno.h value AIHC_ERRNO_EWOULDBLOCK"
  cErrnoWOULDBLOCK :: CInt

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
