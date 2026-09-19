-- | The @errno@ value of each error on macOS.
--
-- An @errno@ value belongs to the ABI of its operating system: fixed for the
-- system, not for the architecture, the libc or their versions.  A Haskell
-- module cannot ask the C headers for one, so each is written by hand, once
-- per platform, and the spec suite holds the numbers against the platform's
-- own @errno.h@ rather than against another number written here.
--
-- The numbers are the ones @<sys/errno.h>@ gives. They descend from 4.4BSD,
-- so the first ten match every Unix and the rest do not: @EAGAIN@ is 35 here
-- and 11 on Linux.
--
-- An error macOS does not have is @-1@, which no operation reports and
-- which 'Foreign.C.Error.isValidErrno' rejects.  GHC's configure script
-- writes @-1@ for the same reason.
module Foreign.C.Error.Repr
  ( cErrno2BIG,
    cErrnoACCES,
    cErrnoADDRINUSE,
    cErrnoADDRNOTAVAIL,
    cErrnoADV,
    cErrnoAFNOSUPPORT,
    cErrnoAGAIN,
    cErrnoALREADY,
    cErrnoBADF,
    cErrnoBADMSG,
    cErrnoBADRPC,
    cErrnoBUSY,
    cErrnoCHILD,
    cErrnoCOMM,
    cErrnoCONNABORTED,
    cErrnoCONNREFUSED,
    cErrnoCONNRESET,
    cErrnoDEADLK,
    cErrnoDESTADDRREQ,
    cErrnoDIRTY,
    cErrnoDOM,
    cErrnoDQUOT,
    cErrnoEXIST,
    cErrnoFAULT,
    cErrnoFBIG,
    cErrnoFTYPE,
    cErrnoHOSTDOWN,
    cErrnoHOSTUNREACH,
    cErrnoIDRM,
    cErrnoILSEQ,
    cErrnoINPROGRESS,
    cErrnoINTR,
    cErrnoINVAL,
    cErrnoIO,
    cErrnoISCONN,
    cErrnoISDIR,
    cErrnoLOOP,
    cErrnoMFILE,
    cErrnoMLINK,
    cErrnoMSGSIZE,
    cErrnoMULTIHOP,
    cErrnoNAMETOOLONG,
    cErrnoNETDOWN,
    cErrnoNETRESET,
    cErrnoNETUNREACH,
    cErrnoNFILE,
    cErrnoNOBUFS,
    cErrnoNODATA,
    cErrnoNODEV,
    cErrnoNOENT,
    cErrnoNOEXEC,
    cErrnoNOLCK,
    cErrnoNOLINK,
    cErrnoNOMEM,
    cErrnoNOMSG,
    cErrnoNONET,
    cErrnoNOPROTOOPT,
    cErrnoNOSPC,
    cErrnoNOSR,
    cErrnoNOSTR,
    cErrnoNOSYS,
    cErrnoNOTBLK,
    cErrnoNOTCONN,
    cErrnoNOTDIR,
    cErrnoNOTEMPTY,
    cErrnoNOTSOCK,
    cErrnoNOTSUP,
    cErrnoNOTTY,
    cErrnoNXIO,
    cErrnoOPNOTSUPP,
    cErrnoPERM,
    cErrnoPFNOSUPPORT,
    cErrnoPIPE,
    cErrnoPROCLIM,
    cErrnoPROCUNAVAIL,
    cErrnoPROGMISMATCH,
    cErrnoPROGUNAVAIL,
    cErrnoPROTO,
    cErrnoPROTONOSUPPORT,
    cErrnoPROTOTYPE,
    cErrnoRANGE,
    cErrnoREMCHG,
    cErrnoREMOTE,
    cErrnoROFS,
    cErrnoRPCMISMATCH,
    cErrnoRREMOTE,
    cErrnoSHUTDOWN,
    cErrnoSOCKTNOSUPPORT,
    cErrnoSPIPE,
    cErrnoSRCH,
    cErrnoSRMNT,
    cErrnoSTALE,
    cErrnoTIME,
    cErrnoTIMEDOUT,
    cErrnoTOOMANYREFS,
    cErrnoTXTBSY,
    cErrnoUSERS,
    cErrnoWOULDBLOCK,
    cErrnoXDEV,
  )
where

import Foreign.C.Types (CInt)

cErrno2BIG :: CInt
cErrno2BIG = 7

cErrnoACCES :: CInt
cErrnoACCES = 13

cErrnoADDRINUSE :: CInt
cErrnoADDRINUSE = 48

cErrnoADDRNOTAVAIL :: CInt
cErrnoADDRNOTAVAIL = 49

cErrnoADV :: CInt
cErrnoADV = -1

cErrnoAFNOSUPPORT :: CInt
cErrnoAFNOSUPPORT = 47

cErrnoAGAIN :: CInt
cErrnoAGAIN = 35

cErrnoALREADY :: CInt
cErrnoALREADY = 37

cErrnoBADF :: CInt
cErrnoBADF = 9

cErrnoBADMSG :: CInt
cErrnoBADMSG = 94

cErrnoBADRPC :: CInt
cErrnoBADRPC = 72

cErrnoBUSY :: CInt
cErrnoBUSY = 16

cErrnoCHILD :: CInt
cErrnoCHILD = 10

cErrnoCOMM :: CInt
cErrnoCOMM = -1

cErrnoCONNABORTED :: CInt
cErrnoCONNABORTED = 53

cErrnoCONNREFUSED :: CInt
cErrnoCONNREFUSED = 61

cErrnoCONNRESET :: CInt
cErrnoCONNRESET = 54

cErrnoDEADLK :: CInt
cErrnoDEADLK = 11

cErrnoDESTADDRREQ :: CInt
cErrnoDESTADDRREQ = 39

cErrnoDIRTY :: CInt
cErrnoDIRTY = -1

cErrnoDOM :: CInt
cErrnoDOM = 33

cErrnoDQUOT :: CInt
cErrnoDQUOT = 69

cErrnoEXIST :: CInt
cErrnoEXIST = 17

cErrnoFAULT :: CInt
cErrnoFAULT = 14

cErrnoFBIG :: CInt
cErrnoFBIG = 27

cErrnoFTYPE :: CInt
cErrnoFTYPE = 79

cErrnoHOSTDOWN :: CInt
cErrnoHOSTDOWN = 64

cErrnoHOSTUNREACH :: CInt
cErrnoHOSTUNREACH = 65

cErrnoIDRM :: CInt
cErrnoIDRM = 90

cErrnoILSEQ :: CInt
cErrnoILSEQ = 92

cErrnoINPROGRESS :: CInt
cErrnoINPROGRESS = 36

cErrnoINTR :: CInt
cErrnoINTR = 4

cErrnoINVAL :: CInt
cErrnoINVAL = 22

cErrnoIO :: CInt
cErrnoIO = 5

cErrnoISCONN :: CInt
cErrnoISCONN = 56

cErrnoISDIR :: CInt
cErrnoISDIR = 21

cErrnoLOOP :: CInt
cErrnoLOOP = 62

cErrnoMFILE :: CInt
cErrnoMFILE = 24

cErrnoMLINK :: CInt
cErrnoMLINK = 31

cErrnoMSGSIZE :: CInt
cErrnoMSGSIZE = 40

cErrnoMULTIHOP :: CInt
cErrnoMULTIHOP = 95

cErrnoNAMETOOLONG :: CInt
cErrnoNAMETOOLONG = 63

cErrnoNETDOWN :: CInt
cErrnoNETDOWN = 50

cErrnoNETRESET :: CInt
cErrnoNETRESET = 52

cErrnoNETUNREACH :: CInt
cErrnoNETUNREACH = 51

cErrnoNFILE :: CInt
cErrnoNFILE = 23

cErrnoNOBUFS :: CInt
cErrnoNOBUFS = 55

cErrnoNODATA :: CInt
cErrnoNODATA = 96

cErrnoNODEV :: CInt
cErrnoNODEV = 19

cErrnoNOENT :: CInt
cErrnoNOENT = 2

cErrnoNOEXEC :: CInt
cErrnoNOEXEC = 8

cErrnoNOLCK :: CInt
cErrnoNOLCK = 77

cErrnoNOLINK :: CInt
cErrnoNOLINK = 97

cErrnoNOMEM :: CInt
cErrnoNOMEM = 12

cErrnoNOMSG :: CInt
cErrnoNOMSG = 91

cErrnoNONET :: CInt
cErrnoNONET = -1

cErrnoNOPROTOOPT :: CInt
cErrnoNOPROTOOPT = 42

cErrnoNOSPC :: CInt
cErrnoNOSPC = 28

cErrnoNOSR :: CInt
cErrnoNOSR = 98

cErrnoNOSTR :: CInt
cErrnoNOSTR = 99

cErrnoNOSYS :: CInt
cErrnoNOSYS = 78

cErrnoNOTBLK :: CInt
cErrnoNOTBLK = 15

cErrnoNOTCONN :: CInt
cErrnoNOTCONN = 57

cErrnoNOTDIR :: CInt
cErrnoNOTDIR = 20

cErrnoNOTEMPTY :: CInt
cErrnoNOTEMPTY = 66

cErrnoNOTSOCK :: CInt
cErrnoNOTSOCK = 38

cErrnoNOTSUP :: CInt
cErrnoNOTSUP = 45

cErrnoNOTTY :: CInt
cErrnoNOTTY = 25

cErrnoNXIO :: CInt
cErrnoNXIO = 6

cErrnoOPNOTSUPP :: CInt
cErrnoOPNOTSUPP = 102

cErrnoPERM :: CInt
cErrnoPERM = 1

cErrnoPFNOSUPPORT :: CInt
cErrnoPFNOSUPPORT = 46

cErrnoPIPE :: CInt
cErrnoPIPE = 32

cErrnoPROCLIM :: CInt
cErrnoPROCLIM = 67

cErrnoPROCUNAVAIL :: CInt
cErrnoPROCUNAVAIL = 76

cErrnoPROGMISMATCH :: CInt
cErrnoPROGMISMATCH = 75

cErrnoPROGUNAVAIL :: CInt
cErrnoPROGUNAVAIL = 74

cErrnoPROTO :: CInt
cErrnoPROTO = 100

cErrnoPROTONOSUPPORT :: CInt
cErrnoPROTONOSUPPORT = 43

cErrnoPROTOTYPE :: CInt
cErrnoPROTOTYPE = 41

cErrnoRANGE :: CInt
cErrnoRANGE = 34

cErrnoREMCHG :: CInt
cErrnoREMCHG = -1

cErrnoREMOTE :: CInt
cErrnoREMOTE = 71

cErrnoROFS :: CInt
cErrnoROFS = 30

cErrnoRPCMISMATCH :: CInt
cErrnoRPCMISMATCH = 73

cErrnoRREMOTE :: CInt
cErrnoRREMOTE = -1

cErrnoSHUTDOWN :: CInt
cErrnoSHUTDOWN = 58

cErrnoSOCKTNOSUPPORT :: CInt
cErrnoSOCKTNOSUPPORT = 44

cErrnoSPIPE :: CInt
cErrnoSPIPE = 29

cErrnoSRCH :: CInt
cErrnoSRCH = 3

cErrnoSRMNT :: CInt
cErrnoSRMNT = -1

cErrnoSTALE :: CInt
cErrnoSTALE = 70

cErrnoTIME :: CInt
cErrnoTIME = 101

cErrnoTIMEDOUT :: CInt
cErrnoTIMEDOUT = 60

cErrnoTOOMANYREFS :: CInt
cErrnoTOOMANYREFS = 59

cErrnoTXTBSY :: CInt
cErrnoTXTBSY = 26

cErrnoUSERS :: CInt
cErrnoUSERS = 68

cErrnoWOULDBLOCK :: CInt
cErrnoWOULDBLOCK = 35

cErrnoXDEV :: CInt
cErrnoXDEV = 18
