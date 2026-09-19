-- | The @errno@ value of each error on WASI.
--
-- An @errno@ value belongs to the ABI of its operating system: fixed for the
-- system, not for the architecture, the libc or their versions.  A Haskell
-- module cannot ask the C headers for one, so each is written by hand, once
-- per platform, and the spec suite holds the numbers against the platform's
-- own @errno.h@ rather than against another number written here.
--
-- wasi-libc numbers its errors alphabetically rather than inheriting the
-- Unix order, so not even @EPERM@, which every Unix took from V7 as 1, keeps
-- its value: it is 63 here. The numbers belong to the
-- @wasi_snapshot_preview1@ ABI, so preview2 repeats them.
--
-- An error WASI does not have is @-1@, which no operation reports and
-- which 'Foreign.C.Error.isValidErrno' rejects.  GHC's configure script
-- writes @-1@ for the same reason.
module Foreign.C.Error.Repr where

import Foreign.C.Types (CInt)

cErrno2BIG :: CInt
cErrno2BIG = 1

cErrnoACCES :: CInt
cErrnoACCES = 2

cErrnoADDRINUSE :: CInt
cErrnoADDRINUSE = 3

cErrnoADDRNOTAVAIL :: CInt
cErrnoADDRNOTAVAIL = 4

cErrnoADV :: CInt
cErrnoADV = -1

cErrnoAFNOSUPPORT :: CInt
cErrnoAFNOSUPPORT = 5

cErrnoAGAIN :: CInt
cErrnoAGAIN = 6

cErrnoALREADY :: CInt
cErrnoALREADY = 7

cErrnoBADF :: CInt
cErrnoBADF = 8

cErrnoBADMSG :: CInt
cErrnoBADMSG = 9

cErrnoBADRPC :: CInt
cErrnoBADRPC = -1

cErrnoBUSY :: CInt
cErrnoBUSY = 10

cErrnoCHILD :: CInt
cErrnoCHILD = 12

cErrnoCOMM :: CInt
cErrnoCOMM = -1

cErrnoCONNABORTED :: CInt
cErrnoCONNABORTED = 13

cErrnoCONNREFUSED :: CInt
cErrnoCONNREFUSED = 14

cErrnoCONNRESET :: CInt
cErrnoCONNRESET = 15

cErrnoDEADLK :: CInt
cErrnoDEADLK = 16

cErrnoDESTADDRREQ :: CInt
cErrnoDESTADDRREQ = 17

cErrnoDIRTY :: CInt
cErrnoDIRTY = -1

cErrnoDOM :: CInt
cErrnoDOM = 18

cErrnoDQUOT :: CInt
cErrnoDQUOT = 19

cErrnoEXIST :: CInt
cErrnoEXIST = 20

cErrnoFAULT :: CInt
cErrnoFAULT = 21

cErrnoFBIG :: CInt
cErrnoFBIG = 22

cErrnoFTYPE :: CInt
cErrnoFTYPE = -1

cErrnoHOSTDOWN :: CInt
cErrnoHOSTDOWN = -1

cErrnoHOSTUNREACH :: CInt
cErrnoHOSTUNREACH = 23

cErrnoIDRM :: CInt
cErrnoIDRM = 24

cErrnoILSEQ :: CInt
cErrnoILSEQ = 25

cErrnoINPROGRESS :: CInt
cErrnoINPROGRESS = 26

cErrnoINTR :: CInt
cErrnoINTR = 27

cErrnoINVAL :: CInt
cErrnoINVAL = 28

cErrnoIO :: CInt
cErrnoIO = 29

cErrnoISCONN :: CInt
cErrnoISCONN = 30

cErrnoISDIR :: CInt
cErrnoISDIR = 31

cErrnoLOOP :: CInt
cErrnoLOOP = 32

cErrnoMFILE :: CInt
cErrnoMFILE = 33

cErrnoMLINK :: CInt
cErrnoMLINK = 34

cErrnoMSGSIZE :: CInt
cErrnoMSGSIZE = 35

cErrnoMULTIHOP :: CInt
cErrnoMULTIHOP = 36

cErrnoNAMETOOLONG :: CInt
cErrnoNAMETOOLONG = 37

cErrnoNETDOWN :: CInt
cErrnoNETDOWN = 38

cErrnoNETRESET :: CInt
cErrnoNETRESET = 39

cErrnoNETUNREACH :: CInt
cErrnoNETUNREACH = 40

cErrnoNFILE :: CInt
cErrnoNFILE = 41

cErrnoNOBUFS :: CInt
cErrnoNOBUFS = 42

cErrnoNODATA :: CInt
cErrnoNODATA = -1

cErrnoNODEV :: CInt
cErrnoNODEV = 43

cErrnoNOENT :: CInt
cErrnoNOENT = 44

cErrnoNOEXEC :: CInt
cErrnoNOEXEC = 45

cErrnoNOLCK :: CInt
cErrnoNOLCK = 46

cErrnoNOLINK :: CInt
cErrnoNOLINK = 47

cErrnoNOMEM :: CInt
cErrnoNOMEM = 48

cErrnoNOMSG :: CInt
cErrnoNOMSG = 49

cErrnoNONET :: CInt
cErrnoNONET = -1

cErrnoNOPROTOOPT :: CInt
cErrnoNOPROTOOPT = 50

cErrnoNOSPC :: CInt
cErrnoNOSPC = 51

cErrnoNOSR :: CInt
cErrnoNOSR = -1

cErrnoNOSTR :: CInt
cErrnoNOSTR = -1

cErrnoNOSYS :: CInt
cErrnoNOSYS = 52

cErrnoNOTBLK :: CInt
cErrnoNOTBLK = -1

cErrnoNOTCONN :: CInt
cErrnoNOTCONN = 53

cErrnoNOTDIR :: CInt
cErrnoNOTDIR = 54

cErrnoNOTEMPTY :: CInt
cErrnoNOTEMPTY = 55

cErrnoNOTSOCK :: CInt
cErrnoNOTSOCK = 57

cErrnoNOTSUP :: CInt
cErrnoNOTSUP = 58

cErrnoNOTTY :: CInt
cErrnoNOTTY = 59

cErrnoNXIO :: CInt
cErrnoNXIO = 60

cErrnoOPNOTSUPP :: CInt
cErrnoOPNOTSUPP = 58

cErrnoPERM :: CInt
cErrnoPERM = 63

cErrnoPFNOSUPPORT :: CInt
cErrnoPFNOSUPPORT = -1

cErrnoPIPE :: CInt
cErrnoPIPE = 64

cErrnoPROCLIM :: CInt
cErrnoPROCLIM = -1

cErrnoPROCUNAVAIL :: CInt
cErrnoPROCUNAVAIL = -1

cErrnoPROGMISMATCH :: CInt
cErrnoPROGMISMATCH = -1

cErrnoPROGUNAVAIL :: CInt
cErrnoPROGUNAVAIL = -1

cErrnoPROTO :: CInt
cErrnoPROTO = 65

cErrnoPROTONOSUPPORT :: CInt
cErrnoPROTONOSUPPORT = 66

cErrnoPROTOTYPE :: CInt
cErrnoPROTOTYPE = 67

cErrnoRANGE :: CInt
cErrnoRANGE = 68

cErrnoREMCHG :: CInt
cErrnoREMCHG = -1

cErrnoREMOTE :: CInt
cErrnoREMOTE = -1

cErrnoROFS :: CInt
cErrnoROFS = 69

cErrnoRPCMISMATCH :: CInt
cErrnoRPCMISMATCH = -1

cErrnoRREMOTE :: CInt
cErrnoRREMOTE = -1

cErrnoSHUTDOWN :: CInt
cErrnoSHUTDOWN = -1

cErrnoSOCKTNOSUPPORT :: CInt
cErrnoSOCKTNOSUPPORT = -1

cErrnoSPIPE :: CInt
cErrnoSPIPE = 70

cErrnoSRCH :: CInt
cErrnoSRCH = 71

cErrnoSRMNT :: CInt
cErrnoSRMNT = -1

cErrnoSTALE :: CInt
cErrnoSTALE = 72

cErrnoTIME :: CInt
cErrnoTIME = -1

cErrnoTIMEDOUT :: CInt
cErrnoTIMEDOUT = 73

cErrnoTOOMANYREFS :: CInt
cErrnoTOOMANYREFS = -1

cErrnoTXTBSY :: CInt
cErrnoTXTBSY = 74

cErrnoUSERS :: CInt
cErrnoUSERS = -1

cErrnoWOULDBLOCK :: CInt
cErrnoWOULDBLOCK = 6

cErrnoXDEV :: CInt
cErrnoXDEV = 75
