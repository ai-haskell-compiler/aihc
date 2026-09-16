-- | The error numbers of WASI.
--
-- An error number is whatever the platform's headers say it is, and the
-- platforms disagree: @EAGAIN@ is 11 on Linux, 35 on macOS and 6 under WASI.
-- Each number therefore has one definition per platform rather than a rule,
-- and 'Foreign.C.Error' names these instead of a number. An error the
-- platform does not have is @-1@, which @isValidErrno@ rejects; GHC gives
-- such a constant the same value.
--
-- The numbers come from the platform's own headers, and the spec suite
-- checks them: it turns this module into a C file of static assertions and
-- compiles it against the real headers of the platform it runs on, so a
-- wrong number fails a test rather than silently misreading an error.
module Foreign.C.Error.Repr
  ( errnoE2BIG,
    errnoEACCES,
    errnoEADDRINUSE,
    errnoEADDRNOTAVAIL,
    errnoEADV,
    errnoEAFNOSUPPORT,
    errnoEAGAIN,
    errnoEALREADY,
    errnoEBADF,
    errnoEBADMSG,
    errnoEBADRPC,
    errnoEBUSY,
    errnoECHILD,
    errnoECOMM,
    errnoECONNABORTED,
    errnoECONNREFUSED,
    errnoECONNRESET,
    errnoEDEADLK,
    errnoEDESTADDRREQ,
    errnoEDIRTY,
    errnoEDOM,
    errnoEDQUOT,
    errnoEEXIST,
    errnoEFAULT,
    errnoEFBIG,
    errnoEFTYPE,
    errnoEHOSTDOWN,
    errnoEHOSTUNREACH,
    errnoEIDRM,
    errnoEILSEQ,
    errnoEINPROGRESS,
    errnoEINTR,
    errnoEINVAL,
    errnoEIO,
    errnoEISCONN,
    errnoEISDIR,
    errnoELOOP,
    errnoEMFILE,
    errnoEMLINK,
    errnoEMSGSIZE,
    errnoEMULTIHOP,
    errnoENAMETOOLONG,
    errnoENETDOWN,
    errnoENETRESET,
    errnoENETUNREACH,
    errnoENFILE,
    errnoENOBUFS,
    errnoENODATA,
    errnoENODEV,
    errnoENOENT,
    errnoENOEXEC,
    errnoENOLCK,
    errnoENOLINK,
    errnoENOMEM,
    errnoENOMSG,
    errnoENONET,
    errnoENOPROTOOPT,
    errnoENOSPC,
    errnoENOSR,
    errnoENOSTR,
    errnoENOSYS,
    errnoENOTBLK,
    errnoENOTCONN,
    errnoENOTDIR,
    errnoENOTEMPTY,
    errnoENOTSOCK,
    errnoENOTSUP,
    errnoENOTTY,
    errnoENXIO,
    errnoEOPNOTSUPP,
    errnoEPERM,
    errnoEPFNOSUPPORT,
    errnoEPIPE,
    errnoEPROCLIM,
    errnoEPROCUNAVAIL,
    errnoEPROGMISMATCH,
    errnoEPROGUNAVAIL,
    errnoEPROTO,
    errnoEPROTONOSUPPORT,
    errnoEPROTOTYPE,
    errnoERANGE,
    errnoEREMCHG,
    errnoEREMOTE,
    errnoEROFS,
    errnoERPCMISMATCH,
    errnoERREMOTE,
    errnoESHUTDOWN,
    errnoESOCKTNOSUPPORT,
    errnoESPIPE,
    errnoESRCH,
    errnoESRMNT,
    errnoESTALE,
    errnoETIME,
    errnoETIMEDOUT,
    errnoETOOMANYREFS,
    errnoETXTBSY,
    errnoEUSERS,
    errnoEWOULDBLOCK,
    errnoEXDEV,
  )
where

import Foreign.C.Types (CInt)

errnoE2BIG :: CInt
errnoE2BIG = 1

errnoEACCES :: CInt
errnoEACCES = 2

errnoEADDRINUSE :: CInt
errnoEADDRINUSE = 3

errnoEADDRNOTAVAIL :: CInt
errnoEADDRNOTAVAIL = 4

errnoEADV :: CInt
errnoEADV = -1

errnoEAFNOSUPPORT :: CInt
errnoEAFNOSUPPORT = 5

errnoEAGAIN :: CInt
errnoEAGAIN = 6

errnoEALREADY :: CInt
errnoEALREADY = 7

errnoEBADF :: CInt
errnoEBADF = 8

errnoEBADMSG :: CInt
errnoEBADMSG = 9

errnoEBADRPC :: CInt
errnoEBADRPC = -1

errnoEBUSY :: CInt
errnoEBUSY = 10

errnoECHILD :: CInt
errnoECHILD = 12

errnoECOMM :: CInt
errnoECOMM = -1

errnoECONNABORTED :: CInt
errnoECONNABORTED = 13

errnoECONNREFUSED :: CInt
errnoECONNREFUSED = 14

errnoECONNRESET :: CInt
errnoECONNRESET = 15

errnoEDEADLK :: CInt
errnoEDEADLK = 16

errnoEDESTADDRREQ :: CInt
errnoEDESTADDRREQ = 17

errnoEDIRTY :: CInt
errnoEDIRTY = -1

errnoEDOM :: CInt
errnoEDOM = 18

errnoEDQUOT :: CInt
errnoEDQUOT = 19

errnoEEXIST :: CInt
errnoEEXIST = 20

errnoEFAULT :: CInt
errnoEFAULT = 21

errnoEFBIG :: CInt
errnoEFBIG = 22

errnoEFTYPE :: CInt
errnoEFTYPE = -1

errnoEHOSTDOWN :: CInt
errnoEHOSTDOWN = -1

errnoEHOSTUNREACH :: CInt
errnoEHOSTUNREACH = 23

errnoEIDRM :: CInt
errnoEIDRM = 24

errnoEILSEQ :: CInt
errnoEILSEQ = 25

errnoEINPROGRESS :: CInt
errnoEINPROGRESS = 26

errnoEINTR :: CInt
errnoEINTR = 27

errnoEINVAL :: CInt
errnoEINVAL = 28

errnoEIO :: CInt
errnoEIO = 29

errnoEISCONN :: CInt
errnoEISCONN = 30

errnoEISDIR :: CInt
errnoEISDIR = 31

errnoELOOP :: CInt
errnoELOOP = 32

errnoEMFILE :: CInt
errnoEMFILE = 33

errnoEMLINK :: CInt
errnoEMLINK = 34

errnoEMSGSIZE :: CInt
errnoEMSGSIZE = 35

errnoEMULTIHOP :: CInt
errnoEMULTIHOP = 36

errnoENAMETOOLONG :: CInt
errnoENAMETOOLONG = 37

errnoENETDOWN :: CInt
errnoENETDOWN = 38

errnoENETRESET :: CInt
errnoENETRESET = 39

errnoENETUNREACH :: CInt
errnoENETUNREACH = 40

errnoENFILE :: CInt
errnoENFILE = 41

errnoENOBUFS :: CInt
errnoENOBUFS = 42

errnoENODATA :: CInt
errnoENODATA = -1

errnoENODEV :: CInt
errnoENODEV = 43

errnoENOENT :: CInt
errnoENOENT = 44

errnoENOEXEC :: CInt
errnoENOEXEC = 45

errnoENOLCK :: CInt
errnoENOLCK = 46

errnoENOLINK :: CInt
errnoENOLINK = 47

errnoENOMEM :: CInt
errnoENOMEM = 48

errnoENOMSG :: CInt
errnoENOMSG = 49

errnoENONET :: CInt
errnoENONET = -1

errnoENOPROTOOPT :: CInt
errnoENOPROTOOPT = 50

errnoENOSPC :: CInt
errnoENOSPC = 51

errnoENOSR :: CInt
errnoENOSR = -1

errnoENOSTR :: CInt
errnoENOSTR = -1

errnoENOSYS :: CInt
errnoENOSYS = 52

errnoENOTBLK :: CInt
errnoENOTBLK = -1

errnoENOTCONN :: CInt
errnoENOTCONN = 53

errnoENOTDIR :: CInt
errnoENOTDIR = 54

errnoENOTEMPTY :: CInt
errnoENOTEMPTY = 55

errnoENOTSOCK :: CInt
errnoENOTSOCK = 57

errnoENOTSUP :: CInt
errnoENOTSUP = 58

errnoENOTTY :: CInt
errnoENOTTY = 59

errnoENXIO :: CInt
errnoENXIO = 60

errnoEOPNOTSUPP :: CInt
errnoEOPNOTSUPP = 58

errnoEPERM :: CInt
errnoEPERM = 63

errnoEPFNOSUPPORT :: CInt
errnoEPFNOSUPPORT = -1

errnoEPIPE :: CInt
errnoEPIPE = 64

errnoEPROCLIM :: CInt
errnoEPROCLIM = -1

errnoEPROCUNAVAIL :: CInt
errnoEPROCUNAVAIL = -1

errnoEPROGMISMATCH :: CInt
errnoEPROGMISMATCH = -1

errnoEPROGUNAVAIL :: CInt
errnoEPROGUNAVAIL = -1

errnoEPROTO :: CInt
errnoEPROTO = 65

errnoEPROTONOSUPPORT :: CInt
errnoEPROTONOSUPPORT = 66

errnoEPROTOTYPE :: CInt
errnoEPROTOTYPE = 67

errnoERANGE :: CInt
errnoERANGE = 68

errnoEREMCHG :: CInt
errnoEREMCHG = -1

errnoEREMOTE :: CInt
errnoEREMOTE = -1

errnoEROFS :: CInt
errnoEROFS = 69

errnoERPCMISMATCH :: CInt
errnoERPCMISMATCH = -1

errnoERREMOTE :: CInt
errnoERREMOTE = -1

errnoESHUTDOWN :: CInt
errnoESHUTDOWN = -1

errnoESOCKTNOSUPPORT :: CInt
errnoESOCKTNOSUPPORT = -1

errnoESPIPE :: CInt
errnoESPIPE = 70

errnoESRCH :: CInt
errnoESRCH = 71

errnoESRMNT :: CInt
errnoESRMNT = -1

errnoESTALE :: CInt
errnoESTALE = 72

errnoETIME :: CInt
errnoETIME = -1

errnoETIMEDOUT :: CInt
errnoETIMEDOUT = 73

errnoETOOMANYREFS :: CInt
errnoETOOMANYREFS = -1

errnoETXTBSY :: CInt
errnoETXTBSY = 74

errnoEUSERS :: CInt
errnoEUSERS = -1

errnoEWOULDBLOCK :: CInt
errnoEWOULDBLOCK = 6

errnoEXDEV :: CInt
errnoEXDEV = 75
