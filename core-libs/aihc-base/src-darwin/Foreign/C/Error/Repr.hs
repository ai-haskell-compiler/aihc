-- | The error numbers of Apple platforms.
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
errnoEADDRINUSE = 48

errnoEADDRNOTAVAIL :: CInt
errnoEADDRNOTAVAIL = 49

errnoEADV :: CInt
errnoEADV = -1

errnoEAFNOSUPPORT :: CInt
errnoEAFNOSUPPORT = 47

errnoEAGAIN :: CInt
errnoEAGAIN = 35

errnoEALREADY :: CInt
errnoEALREADY = 37

errnoEBADF :: CInt
errnoEBADF = 9

errnoEBADMSG :: CInt
errnoEBADMSG = 94

errnoEBADRPC :: CInt
errnoEBADRPC = 72

errnoEBUSY :: CInt
errnoEBUSY = 16

errnoECHILD :: CInt
errnoECHILD = 10

errnoECOMM :: CInt
errnoECOMM = -1

errnoECONNABORTED :: CInt
errnoECONNABORTED = 53

errnoECONNREFUSED :: CInt
errnoECONNREFUSED = 61

errnoECONNRESET :: CInt
errnoECONNRESET = 54

errnoEDEADLK :: CInt
errnoEDEADLK = 11

errnoEDESTADDRREQ :: CInt
errnoEDESTADDRREQ = 39

errnoEDIRTY :: CInt
errnoEDIRTY = -1

errnoEDOM :: CInt
errnoEDOM = 33

errnoEDQUOT :: CInt
errnoEDQUOT = 69

errnoEEXIST :: CInt
errnoEEXIST = 17

errnoEFAULT :: CInt
errnoEFAULT = 14

errnoEFBIG :: CInt
errnoEFBIG = 27

errnoEFTYPE :: CInt
errnoEFTYPE = 79

errnoEHOSTDOWN :: CInt
errnoEHOSTDOWN = 64

errnoEHOSTUNREACH :: CInt
errnoEHOSTUNREACH = 65

errnoEIDRM :: CInt
errnoEIDRM = 90

errnoEILSEQ :: CInt
errnoEILSEQ = 92

errnoEINPROGRESS :: CInt
errnoEINPROGRESS = 36

errnoEINTR :: CInt
errnoEINTR = 4

errnoEINVAL :: CInt
errnoEINVAL = 22

errnoEIO :: CInt
errnoEIO = 5

errnoEISCONN :: CInt
errnoEISCONN = 56

errnoEISDIR :: CInt
errnoEISDIR = 21

errnoELOOP :: CInt
errnoELOOP = 62

errnoEMFILE :: CInt
errnoEMFILE = 24

errnoEMLINK :: CInt
errnoEMLINK = 31

errnoEMSGSIZE :: CInt
errnoEMSGSIZE = 40

errnoEMULTIHOP :: CInt
errnoEMULTIHOP = 95

errnoENAMETOOLONG :: CInt
errnoENAMETOOLONG = 63

errnoENETDOWN :: CInt
errnoENETDOWN = 50

errnoENETRESET :: CInt
errnoENETRESET = 52

errnoENETUNREACH :: CInt
errnoENETUNREACH = 51

errnoENFILE :: CInt
errnoENFILE = 23

errnoENOBUFS :: CInt
errnoENOBUFS = 55

errnoENODATA :: CInt
errnoENODATA = 96

errnoENODEV :: CInt
errnoENODEV = 19

errnoENOENT :: CInt
errnoENOENT = 2

errnoENOEXEC :: CInt
errnoENOEXEC = 8

errnoENOLCK :: CInt
errnoENOLCK = 77

errnoENOLINK :: CInt
errnoENOLINK = 97

errnoENOMEM :: CInt
errnoENOMEM = 12

errnoENOMSG :: CInt
errnoENOMSG = 91

errnoENONET :: CInt
errnoENONET = -1

errnoENOPROTOOPT :: CInt
errnoENOPROTOOPT = 42

errnoENOSPC :: CInt
errnoENOSPC = 28

errnoENOSR :: CInt
errnoENOSR = 98

errnoENOSTR :: CInt
errnoENOSTR = 99

errnoENOSYS :: CInt
errnoENOSYS = 78

errnoENOTBLK :: CInt
errnoENOTBLK = 15

errnoENOTCONN :: CInt
errnoENOTCONN = 57

errnoENOTDIR :: CInt
errnoENOTDIR = 20

errnoENOTEMPTY :: CInt
errnoENOTEMPTY = 66

errnoENOTSOCK :: CInt
errnoENOTSOCK = 38

errnoENOTSUP :: CInt
errnoENOTSUP = 45

errnoENOTTY :: CInt
errnoENOTTY = 25

errnoENXIO :: CInt
errnoENXIO = 6

errnoEOPNOTSUPP :: CInt
errnoEOPNOTSUPP = 102

errnoEPERM :: CInt
errnoEPERM = 1

errnoEPFNOSUPPORT :: CInt
errnoEPFNOSUPPORT = 46

errnoEPIPE :: CInt
errnoEPIPE = 32

errnoEPROCLIM :: CInt
errnoEPROCLIM = 67

errnoEPROCUNAVAIL :: CInt
errnoEPROCUNAVAIL = 76

errnoEPROGMISMATCH :: CInt
errnoEPROGMISMATCH = 75

errnoEPROGUNAVAIL :: CInt
errnoEPROGUNAVAIL = 74

errnoEPROTO :: CInt
errnoEPROTO = 100

errnoEPROTONOSUPPORT :: CInt
errnoEPROTONOSUPPORT = 43

errnoEPROTOTYPE :: CInt
errnoEPROTOTYPE = 41

errnoERANGE :: CInt
errnoERANGE = 34

errnoEREMCHG :: CInt
errnoEREMCHG = -1

errnoEREMOTE :: CInt
errnoEREMOTE = 71

errnoEROFS :: CInt
errnoEROFS = 30

errnoERPCMISMATCH :: CInt
errnoERPCMISMATCH = 73

errnoERREMOTE :: CInt
errnoERREMOTE = -1

errnoESHUTDOWN :: CInt
errnoESHUTDOWN = 58

errnoESOCKTNOSUPPORT :: CInt
errnoESOCKTNOSUPPORT = 44

errnoESPIPE :: CInt
errnoESPIPE = 29

errnoESRCH :: CInt
errnoESRCH = 3

errnoESRMNT :: CInt
errnoESRMNT = -1

errnoESTALE :: CInt
errnoESTALE = 70

errnoETIME :: CInt
errnoETIME = 101

errnoETIMEDOUT :: CInt
errnoETIMEDOUT = 60

errnoETOOMANYREFS :: CInt
errnoETOOMANYREFS = 59

errnoETXTBSY :: CInt
errnoETXTBSY = 26

errnoEUSERS :: CInt
errnoEUSERS = 68

errnoEWOULDBLOCK :: CInt
errnoEWOULDBLOCK = 35

errnoEXDEV :: CInt
errnoEXDEV = 18
