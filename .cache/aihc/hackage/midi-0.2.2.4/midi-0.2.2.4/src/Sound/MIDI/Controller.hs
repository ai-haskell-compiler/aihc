{- |
MIDI controller data type and common controller definitions.

See <http://www.midi.org/techspecs/midimessages.php>.
-}
module Sound.MIDI.Controller (
   T, Value, fromInt, toInt,

   bankSelect, modulation, breathControl, footControl, portamentoTime,
   dataEntry, volume, balance, panorama, expression,
   effectControl1, effectControl2,
   generalPurpose1, generalPurpose2, generalPurpose3, generalPurpose4,
   vectorX, vectorY,
   soundController1, soundController2, soundController3,
   soundController4, soundController5, soundController6,
   soundController7, soundController8, soundController9,
   soundController10, portamentoControl,
   effect1Depth, effect2Depth, effect3Depth, effect4Depth, effect5Depth,

   bankSelectMSB, modulationMSB, breathControlMSB, footControlMSB,
   portamentoTimeMSB, dataEntryMSB, volumeMSB, balanceMSB,
   panoramaMSB, expressionMSB,
   effectControl1MSB, effectControl2MSB,
   generalPurpose1MSB, generalPurpose2MSB,
   generalPurpose3MSB, generalPurpose4MSB,

   bankSelectLSB, modulationLSB, breathControlLSB, footControlLSB,
   portamentoTimeLSB, dataEntryLSB, volumeLSB, balanceLSB,
   panoramaLSB, expressionLSB,
   effectControl1LSB, effectControl2LSB,
   generalPurpose1LSB, generalPurpose2LSB,
   generalPurpose3LSB, generalPurpose4LSB,

   soundVariation, timbre, harmonicIntensity,
   releaseTime, attackTime, brightness, decayTime,
   vibratoRate, vibratoDepth, vibratoDelay,
   reverbSendLevel, chorusSendLevel, legato,

   sustain, portamento, sustenuto, softPedal, hold2,
   generalPurpose5, generalPurpose6, generalPurpose7, generalPurpose8,
   externalEffectDepth, tremoloDepth, chorusDepth, celesteDepth, phaserDepth,

   dataIncrement, dataDecrement,
   nonRegisteredParameterLSB, nonRegisteredParameterMSB,
   registeredParameterLSB, registeredParameterMSB,
   ) where

import qualified Sound.MIDI.ControllerPrivate as CtrlP

import Sound.MIDI.Utility (checkRange, )


-- * MIDI controller data type


{-
In the future this will be the main type,
and @Controller@ becomes a synonym and is eventually removed.
-}
type T = CtrlP.Controller

fromInt :: Int -> T
fromInt = checkRange "Controller" CtrlP.Controller

toInt :: T -> Int
toInt = CtrlP.fromController


type Value = Int


-- * predefined MIDI controllers

-- ** simple names for controllers, if only most-significant bytes are used

bankSelect, modulation, breathControl, footControl, portamentoTime,
   dataEntry, volume, balance, panorama, expression,
   effectControl1, effectControl2,
   generalPurpose1, generalPurpose2, generalPurpose3, generalPurpose4 :: T
bankSelect      = bankSelectMSB
modulation      = modulationMSB
breathControl   = breathControlMSB
footControl     = footControlMSB
portamentoTime  = portamentoTimeMSB
dataEntry       = dataEntryMSB
volume          = volumeMSB
balance         = balanceMSB
panorama        = panoramaMSB
expression      = expressionMSB
effectControl1  = effectControl1MSB
effectControl2  = effectControl2MSB
generalPurpose1 = generalPurpose1MSB
generalPurpose2 = generalPurpose2MSB
generalPurpose3 = generalPurpose3MSB
generalPurpose4 = generalPurpose4MSB


-- ** aliases for general purpose controllers

vectorX, vectorY,
   soundVariation, timbre, harmonicIntensity,
   releaseTime, attackTime, brightness, decayTime,
   vibratoRate, vibratoDepth, vibratoDelay,
   reverbSendLevel, chorusSendLevel,
   externalEffectDepth, tremoloDepth, chorusDepth, celesteDepth, phaserDepth :: T

vectorX = generalPurpose1
vectorY = generalPurpose2

soundVariation    = soundController1
timbre            = soundController2
harmonicIntensity = soundController2
releaseTime       = soundController3
attackTime        = soundController4
brightness        = soundController5
decayTime         = soundController6
vibratoRate       = soundController7
vibratoDepth      = soundController8
vibratoDelay      = soundController9

reverbSendLevel = effect1Depth
chorusSendLevel = effect3Depth

externalEffectDepth = effect1Depth
tremoloDepth        = effect2Depth
chorusDepth         = effect3Depth
celesteDepth        = effect4Depth
phaserDepth         = effect5Depth


-- ** controllers for most-significant bytes of control values
bankSelectMSB, modulationMSB, breathControlMSB, footControlMSB,
   portamentoTimeMSB, dataEntryMSB,
   volumeMSB, balanceMSB, panoramaMSB, expressionMSB,
   effectControl1MSB, effectControl2MSB,
   generalPurpose1MSB, generalPurpose2MSB,
   generalPurpose3MSB, generalPurpose4MSB :: T

-- ** controllers for least-significant bytes of control values
bankSelectLSB, modulationLSB, breathControlLSB, footControlLSB,
   portamentoTimeLSB, dataEntryLSB,
   volumeLSB, balanceLSB, panoramaLSB, expressionLSB,
   effectControl1LSB, effectControl2LSB,
   generalPurpose1LSB, generalPurpose2LSB,
   generalPurpose3LSB, generalPurpose4LSB :: T

-- ** controllers of sound and global effects
sustain, portamento, sustenuto, softPedal, legato, hold2,
   soundController1, soundController2, soundController3,
   soundController4, soundController5, soundController6,
   soundController7, soundController8, soundController9,
   soundController10,
   generalPurpose5, generalPurpose6, generalPurpose7, generalPurpose8,
   portamentoControl,
   effect1Depth, effect2Depth, effect3Depth, effect4Depth, effect5Depth :: T

-- ** increment/decrement and parameter numbers
dataIncrement, dataDecrement,
   nonRegisteredParameterLSB, nonRegisteredParameterMSB,
   registeredParameterLSB, registeredParameterMSB :: T

bankSelectMSB             = toEnum 0x00  {-  00 00 -}
modulationMSB             = toEnum 0x01  {-  01 01 -}
breathControlMSB          = toEnum 0x02  {-  02 02 -}
footControlMSB            = toEnum 0x04  {-  04 04 -}
portamentoTimeMSB         = toEnum 0x05  {-  05 05 -}
dataEntryMSB              = toEnum 0x06  {-  06 06 -}
volumeMSB                 = toEnum 0x07  {-  07 07 -}
balanceMSB                = toEnum 0x08  {-  08 08 -}
panoramaMSB               = toEnum 0x0A  {-  10 0A -}
expressionMSB             = toEnum 0x0B  {-  11 0B -}
effectControl1MSB         = toEnum 0x0C  {-  10 0C -}
effectControl2MSB         = toEnum 0x0D  {-  11 0D -}

generalPurpose1MSB        = toEnum 0x10  {-  16 10 -}
generalPurpose2MSB        = toEnum 0x11  {-  17 11 -}
generalPurpose3MSB        = toEnum 0x12  {-  18 12 -}
generalPurpose4MSB        = toEnum 0x13  {-  19 13 -}

bankSelectLSB             = toEnum 0x20  {-  32 20 -}
modulationLSB             = toEnum 0x21  {-  33 21 -}
breathControlLSB          = toEnum 0x22  {-  34 22 -}
footControlLSB            = toEnum 0x24  {-  36 24 -}
portamentoTimeLSB         = toEnum 0x25  {-  37 25 -}
dataEntryLSB              = toEnum 0x26  {-  38 26 -}
volumeLSB                 = toEnum 0x27  {-  39 27 -}
balanceLSB                = toEnum 0x28  {-  40 28 -}
panoramaLSB               = toEnum 0x2A  {-  42 2A -}
expressionLSB             = toEnum 0x2B  {-  43 2B -}
effectControl1LSB         = toEnum 0x2C  {-  44 2C -}
effectControl2LSB         = toEnum 0x2D  {-  45 2D -}

generalPurpose1LSB        = toEnum 0x30  {-  48 30 -}
generalPurpose2LSB        = toEnum 0x31  {-  49 31 -}
generalPurpose3LSB        = toEnum 0x32  {-  50 32 -}
generalPurpose4LSB        = toEnum 0x33  {-  51 33 -}

sustain                   = toEnum 0x40  {-  64 40 -}
portamento                = toEnum 0x41  {-  65 41 -}
sustenuto                 = toEnum 0x42  {-  66 42 -}
softPedal                 = toEnum 0x43  {-  67 43 -}
legato                    = toEnum 0x44  {-  68 44 -}
hold2                     = toEnum 0x45  {-  69 45 -}
soundController1          = toEnum 0x46  {-  70 46 -}
soundController2          = toEnum 0x47  {-  71 47 -}
soundController3          = toEnum 0x48  {-  72 48 -}
soundController4          = toEnum 0x49  {-  73 49 -}
soundController5          = toEnum 0x4A  {-  74 4A -}
soundController6          = toEnum 0x4B  {-  75 4B -}
soundController7          = toEnum 0x4C  {-  76 4C -}
soundController8          = toEnum 0x4D  {-  77 4D -}
soundController9          = toEnum 0x4E  {-  78 4E -}
soundController10         = toEnum 0x4F  {-  79 4F -}

generalPurpose5           = toEnum 0x50  {-  80 50 -}
generalPurpose6           = toEnum 0x51  {-  81 51 -}
generalPurpose7           = toEnum 0x52  {-  82 52 -}
generalPurpose8           = toEnum 0x53  {-  83 53 -}
portamentoControl         = toEnum 0x54  {-  84 54 -}
effect1Depth              = toEnum 0x5B  {-  91 5B -}
effect2Depth              = toEnum 0x5C  {-  92 5C -}
effect3Depth              = toEnum 0x5D  {-  93 5D -}
effect4Depth              = toEnum 0x5E  {-  94 5E -}
effect5Depth              = toEnum 0x5F  {-  95 5F -}

dataIncrement             = toEnum 0x60  {-  96 60 -}
dataDecrement             = toEnum 0x61  {-  97 61 -}
nonRegisteredParameterLSB = toEnum 0x62  {-  98 62 -}
nonRegisteredParameterMSB = toEnum 0x63  {-  99 63 -}
registeredParameterLSB    = toEnum 0x64  {- 100 64 -}
registeredParameterMSB    = toEnum 0x65  {- 101 65 -}
