-- | The error numbers of Linux.
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
errnoE2BIG = 7

errnoEACCES :: CInt
errnoEACCES = 13

errnoEADDRINUSE :: CInt
errnoEADDRINUSE = 98

errnoEADDRNOTAVAIL :: CInt
errnoEADDRNOTAVAIL = 99

errnoEADV :: CInt
errnoEADV = 68

errnoEAFNOSUPPORT :: CInt
errnoEAFNOSUPPORT = 97

errnoEAGAIN :: CInt
errnoEAGAIN = 11

errnoEALREADY :: CInt
errnoEALREADY = 114

errnoEBADF :: CInt
errnoEBADF = 9

errnoEBADMSG :: CInt
errnoEBADMSG = 74

errnoEBADRPC :: CInt
errnoEBADRPC = -1

errnoEBUSY :: CInt
errnoEBUSY = 16

errnoECHILD :: CInt
errnoECHILD = 10

errnoECOMM :: CInt
errnoECOMM = 70

errnoECONNABORTED :: CInt
errnoECONNABORTED = 103

errnoECONNREFUSED :: CInt
errnoECONNREFUSED = 111

errnoECONNRESET :: CInt
errnoECONNRESET = 104

errnoEDEADLK :: CInt
errnoEDEADLK = 35

errnoEDESTADDRREQ :: CInt
errnoEDESTADDRREQ = 89

errnoEDIRTY :: CInt
errnoEDIRTY = -1

errnoEDOM :: CInt
errnoEDOM = 33

errnoEDQUOT :: CInt
errnoEDQUOT = 122

errnoEEXIST :: CInt
errnoEEXIST = 17

errnoEFAULT :: CInt
errnoEFAULT = 14

errnoEFBIG :: CInt
errnoEFBIG = 27

errnoEFTYPE :: CInt
errnoEFTYPE = -1

errnoEHOSTDOWN :: CInt
errnoEHOSTDOWN = 112

errnoEHOSTUNREACH :: CInt
errnoEHOSTUNREACH = 113

errnoEIDRM :: CInt
errnoEIDRM = 43

errnoEILSEQ :: CInt
errnoEILSEQ = 84

errnoEINPROGRESS :: CInt
errnoEINPROGRESS = 115

errnoEINTR :: CInt
errnoEINTR = 4

errnoEINVAL :: CInt
errnoEINVAL = 22

errnoEIO :: CInt
errnoEIO = 5

errnoEISCONN :: CInt
errnoEISCONN = 106

errnoEISDIR :: CInt
errnoEISDIR = 21

errnoELOOP :: CInt
errnoELOOP = 40

errnoEMFILE :: CInt
errnoEMFILE = 24

errnoEMLINK :: CInt
errnoEMLINK = 31

errnoEMSGSIZE :: CInt
errnoEMSGSIZE = 90

errnoEMULTIHOP :: CInt
errnoEMULTIHOP = 72

errnoENAMETOOLONG :: CInt
errnoENAMETOOLONG = 36

errnoENETDOWN :: CInt
errnoENETDOWN = 100

errnoENETRESET :: CInt
errnoENETRESET = 102

errnoENETUNREACH :: CInt
errnoENETUNREACH = 101

errnoENFILE :: CInt
errnoENFILE = 23

errnoENOBUFS :: CInt
errnoENOBUFS = 105

errnoENODATA :: CInt
errnoENODATA = 61

errnoENODEV :: CInt
errnoENODEV = 19

errnoENOENT :: CInt
errnoENOENT = 2

errnoENOEXEC :: CInt
errnoENOEXEC = 8

errnoENOLCK :: CInt
errnoENOLCK = 37

errnoENOLINK :: CInt
errnoENOLINK = 67

errnoENOMEM :: CInt
errnoENOMEM = 12

errnoENOMSG :: CInt
errnoENOMSG = 42

errnoENONET :: CInt
errnoENONET = 64

errnoENOPROTOOPT :: CInt
errnoENOPROTOOPT = 92

errnoENOSPC :: CInt
errnoENOSPC = 28

errnoENOSR :: CInt
errnoENOSR = 63

errnoENOSTR :: CInt
errnoENOSTR = 60

errnoENOSYS :: CInt
errnoENOSYS = 38

errnoENOTBLK :: CInt
errnoENOTBLK = 15

errnoENOTCONN :: CInt
errnoENOTCONN = 107

errnoENOTDIR :: CInt
errnoENOTDIR = 20

errnoENOTEMPTY :: CInt
errnoENOTEMPTY = 39

errnoENOTSOCK :: CInt
errnoENOTSOCK = 88

errnoENOTSUP :: CInt
errnoENOTSUP = 95

errnoENOTTY :: CInt
errnoENOTTY = 25

errnoENXIO :: CInt
errnoENXIO = 6

errnoEOPNOTSUPP :: CInt
errnoEOPNOTSUPP = 95

errnoEPERM :: CInt
errnoEPERM = 1

errnoEPFNOSUPPORT :: CInt
errnoEPFNOSUPPORT = 96

errnoEPIPE :: CInt
errnoEPIPE = 32

errnoEPROCLIM :: CInt
errnoEPROCLIM = -1

errnoEPROCUNAVAIL :: CInt
errnoEPROCUNAVAIL = -1

errnoEPROGMISMATCH :: CInt
errnoEPROGMISMATCH = -1

errnoEPROGUNAVAIL :: CInt
errnoEPROGUNAVAIL = -1

errnoEPROTO :: CInt
errnoEPROTO = 71

errnoEPROTONOSUPPORT :: CInt
errnoEPROTONOSUPPORT = 93

errnoEPROTOTYPE :: CInt
errnoEPROTOTYPE = 91

errnoERANGE :: CInt
errnoERANGE = 34

errnoEREMCHG :: CInt
errnoEREMCHG = 78

errnoEREMOTE :: CInt
errnoEREMOTE = 66

errnoEROFS :: CInt
errnoEROFS = 30

errnoERPCMISMATCH :: CInt
errnoERPCMISMATCH = -1

errnoERREMOTE :: CInt
errnoERREMOTE = -1

errnoESHUTDOWN :: CInt
errnoESHUTDOWN = 108

errnoESOCKTNOSUPPORT :: CInt
errnoESOCKTNOSUPPORT = 94

errnoESPIPE :: CInt
errnoESPIPE = 29

errnoESRCH :: CInt
errnoESRCH = 3

errnoESRMNT :: CInt
errnoESRMNT = 69

errnoESTALE :: CInt
errnoESTALE = 116

errnoETIME :: CInt
errnoETIME = 62

errnoETIMEDOUT :: CInt
errnoETIMEDOUT = 110

errnoETOOMANYREFS :: CInt
errnoETOOMANYREFS = 109

errnoETXTBSY :: CInt
errnoETXTBSY = 26

errnoEUSERS :: CInt
errnoEUSERS = 87

errnoEWOULDBLOCK :: CInt
errnoEWOULDBLOCK = 11

errnoEXDEV :: CInt
errnoEXDEV = 18
