-- | The @errno@ value of each error on Linux.
--
-- An @errno@ value belongs to the ABI of its operating system: fixed for the
-- system, not for the architecture, the libc or their versions.  A Haskell
-- module cannot ask the C headers for one, so each is written by hand, once
-- per platform, and the spec suite holds the numbers against the platform's
-- own @errno.h@ rather than against another number written here.
--
-- The numbers are the ones the kernel's @asm-generic/errno.h@ gives, which
-- glibc and musl both expose unchanged. Only the first ten match the other
-- Unixes: @EAGAIN@ is 11 here and 35 on macOS.
--
-- An error Linux does not have is @-1@, which no operation reports and
-- which 'Foreign.C.Error.isValidErrno' rejects.  GHC's configure script
-- writes @-1@ for the same reason.
module Foreign.C.Error.Repr where

import Foreign.C.Types (CInt)

cErrno2BIG :: CInt
cErrno2BIG = 7

cErrnoACCES :: CInt
cErrnoACCES = 13

cErrnoADDRINUSE :: CInt
cErrnoADDRINUSE = 98

cErrnoADDRNOTAVAIL :: CInt
cErrnoADDRNOTAVAIL = 99

cErrnoADV :: CInt
cErrnoADV = 68

cErrnoAFNOSUPPORT :: CInt
cErrnoAFNOSUPPORT = 97

cErrnoAGAIN :: CInt
cErrnoAGAIN = 11

cErrnoALREADY :: CInt
cErrnoALREADY = 114

cErrnoBADF :: CInt
cErrnoBADF = 9

cErrnoBADMSG :: CInt
cErrnoBADMSG = 74

cErrnoBADRPC :: CInt
cErrnoBADRPC = -1

cErrnoBUSY :: CInt
cErrnoBUSY = 16

cErrnoCHILD :: CInt
cErrnoCHILD = 10

cErrnoCOMM :: CInt
cErrnoCOMM = 70

cErrnoCONNABORTED :: CInt
cErrnoCONNABORTED = 103

cErrnoCONNREFUSED :: CInt
cErrnoCONNREFUSED = 111

cErrnoCONNRESET :: CInt
cErrnoCONNRESET = 104

cErrnoDEADLK :: CInt
cErrnoDEADLK = 35

cErrnoDESTADDRREQ :: CInt
cErrnoDESTADDRREQ = 89

cErrnoDIRTY :: CInt
cErrnoDIRTY = -1

cErrnoDOM :: CInt
cErrnoDOM = 33

cErrnoDQUOT :: CInt
cErrnoDQUOT = 122

cErrnoEXIST :: CInt
cErrnoEXIST = 17

cErrnoFAULT :: CInt
cErrnoFAULT = 14

cErrnoFBIG :: CInt
cErrnoFBIG = 27

cErrnoFTYPE :: CInt
cErrnoFTYPE = -1

cErrnoHOSTDOWN :: CInt
cErrnoHOSTDOWN = 112

cErrnoHOSTUNREACH :: CInt
cErrnoHOSTUNREACH = 113

cErrnoIDRM :: CInt
cErrnoIDRM = 43

cErrnoILSEQ :: CInt
cErrnoILSEQ = 84

cErrnoINPROGRESS :: CInt
cErrnoINPROGRESS = 115

cErrnoINTR :: CInt
cErrnoINTR = 4

cErrnoINVAL :: CInt
cErrnoINVAL = 22

cErrnoIO :: CInt
cErrnoIO = 5

cErrnoISCONN :: CInt
cErrnoISCONN = 106

cErrnoISDIR :: CInt
cErrnoISDIR = 21

cErrnoLOOP :: CInt
cErrnoLOOP = 40

cErrnoMFILE :: CInt
cErrnoMFILE = 24

cErrnoMLINK :: CInt
cErrnoMLINK = 31

cErrnoMSGSIZE :: CInt
cErrnoMSGSIZE = 90

cErrnoMULTIHOP :: CInt
cErrnoMULTIHOP = 72

cErrnoNAMETOOLONG :: CInt
cErrnoNAMETOOLONG = 36

cErrnoNETDOWN :: CInt
cErrnoNETDOWN = 100

cErrnoNETRESET :: CInt
cErrnoNETRESET = 102

cErrnoNETUNREACH :: CInt
cErrnoNETUNREACH = 101

cErrnoNFILE :: CInt
cErrnoNFILE = 23

cErrnoNOBUFS :: CInt
cErrnoNOBUFS = 105

cErrnoNODATA :: CInt
cErrnoNODATA = 61

cErrnoNODEV :: CInt
cErrnoNODEV = 19

cErrnoNOENT :: CInt
cErrnoNOENT = 2

cErrnoNOEXEC :: CInt
cErrnoNOEXEC = 8

cErrnoNOLCK :: CInt
cErrnoNOLCK = 37

cErrnoNOLINK :: CInt
cErrnoNOLINK = 67

cErrnoNOMEM :: CInt
cErrnoNOMEM = 12

cErrnoNOMSG :: CInt
cErrnoNOMSG = 42

cErrnoNONET :: CInt
cErrnoNONET = 64

cErrnoNOPROTOOPT :: CInt
cErrnoNOPROTOOPT = 92

cErrnoNOSPC :: CInt
cErrnoNOSPC = 28

cErrnoNOSR :: CInt
cErrnoNOSR = 63

cErrnoNOSTR :: CInt
cErrnoNOSTR = 60

cErrnoNOSYS :: CInt
cErrnoNOSYS = 38

cErrnoNOTBLK :: CInt
cErrnoNOTBLK = 15

cErrnoNOTCONN :: CInt
cErrnoNOTCONN = 107

cErrnoNOTDIR :: CInt
cErrnoNOTDIR = 20

cErrnoNOTEMPTY :: CInt
cErrnoNOTEMPTY = 39

cErrnoNOTSOCK :: CInt
cErrnoNOTSOCK = 88

cErrnoNOTSUP :: CInt
cErrnoNOTSUP = 95

cErrnoNOTTY :: CInt
cErrnoNOTTY = 25

cErrnoNXIO :: CInt
cErrnoNXIO = 6

cErrnoOPNOTSUPP :: CInt
cErrnoOPNOTSUPP = 95

cErrnoPERM :: CInt
cErrnoPERM = 1

cErrnoPFNOSUPPORT :: CInt
cErrnoPFNOSUPPORT = 96

cErrnoPIPE :: CInt
cErrnoPIPE = 32

cErrnoPROCLIM :: CInt
cErrnoPROCLIM = -1

cErrnoPROCUNAVAIL :: CInt
cErrnoPROCUNAVAIL = -1

cErrnoPROGMISMATCH :: CInt
cErrnoPROGMISMATCH = -1

cErrnoPROGUNAVAIL :: CInt
cErrnoPROGUNAVAIL = -1

cErrnoPROTO :: CInt
cErrnoPROTO = 71

cErrnoPROTONOSUPPORT :: CInt
cErrnoPROTONOSUPPORT = 93

cErrnoPROTOTYPE :: CInt
cErrnoPROTOTYPE = 91

cErrnoRANGE :: CInt
cErrnoRANGE = 34

cErrnoREMCHG :: CInt
cErrnoREMCHG = 78

cErrnoREMOTE :: CInt
cErrnoREMOTE = 66

cErrnoROFS :: CInt
cErrnoROFS = 30

cErrnoRPCMISMATCH :: CInt
cErrnoRPCMISMATCH = -1

cErrnoRREMOTE :: CInt
cErrnoRREMOTE = -1

cErrnoSHUTDOWN :: CInt
cErrnoSHUTDOWN = 108

cErrnoSOCKTNOSUPPORT :: CInt
cErrnoSOCKTNOSUPPORT = 94

cErrnoSPIPE :: CInt
cErrnoSPIPE = 29

cErrnoSRCH :: CInt
cErrnoSRCH = 3

cErrnoSRMNT :: CInt
cErrnoSRMNT = 69

cErrnoSTALE :: CInt
cErrnoSTALE = 116

cErrnoTIME :: CInt
cErrnoTIME = 62

cErrnoTIMEDOUT :: CInt
cErrnoTIMEDOUT = 110

cErrnoTOOMANYREFS :: CInt
cErrnoTOOMANYREFS = 109

cErrnoTXTBSY :: CInt
cErrnoTXTBSY = 26

cErrnoUSERS :: CInt
cErrnoUSERS = 87

cErrnoWOULDBLOCK :: CInt
cErrnoWOULDBLOCK = 11

cErrnoXDEV :: CInt
cErrnoXDEV = 18
