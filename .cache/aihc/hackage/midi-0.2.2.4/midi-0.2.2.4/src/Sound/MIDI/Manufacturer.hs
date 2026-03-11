{- |
MIDI device manufacturers and their id's.
-}
module Sound.MIDI.Manufacturer
 (T, get, put,

  sequential, idp, octavePlateau, moog, passport, lexicon, kurzweil, 
  fender,  gulbransen,  akg,  voyce,  waveframe,  ada,  garfield,  ensoniq, 
  oberheim,  apple,  greyMatter,  digidesign,  palmTree,  jlCooper,  lowrey, 
  adamsSmith,  emu,  harmony,  art,  baldwin,  eventide,  inventronics, 
  keyConcepts,  clarity, 

  timeWarner,  digitalMusic,  iota,  newEngland,  artisyn,  ivl, 
  southernMusic,  lakeButler,  alesis,  dod,  studerEditech,  perfectFret, 
  kat,  opcode,  rane,  anadi,  kmx,  brenell,  peavey,  systems360, 
  spectrum,  marquis,  zeta,  axxes,  orban,  kti,  breakaway,  cae, 
  rocktron,  pianoDisc,  cannon,  rogers,  blueSkyLogic,  encore,  uptown, 
  voce,  cti,  ssResearch,  broderbund,  allenOrgan,  musicQuest,  aphex, 
  gallienKrueger,  ibm,  hotzInstruments,  etaLighting,  nsi,  adLib, 
  richmond,  microsoft,  softwareToolworks,  rjmgNiche,  intone, 
  grooveTubes,  euphonix,  interMIDI,  loneWolf,  musonix,  taHorng,  eTek, 
  electrovoice,  midisoft,  qSoundLabs,  westrex,  nVidia,  ess,  mediaTrix, 
  brooktree,  otari,  keyElectronics,  crystalake,  crystal,  rockwell, 
  siliconGraphics,  midiman,  preSonus,  topaz,  castLighting, 
  microsoftConsumer,  fastForward,  headspace,  vanKoevering,  altech, 
  vlsi,  chromaticResearch,  sapphire,  idrc,  justonic,  torComp,  newtek, 
  soundSculpture,  walker,  pavo,  inVision,  tSquareDesign,  nemesys,  dbx, 
  syndyne,  bitheadz,  cakewalk,  staccato,  nationalSemiconductor, 
  boomTheory,  virtualDSP,  antares,  angelSoftware,  stLouis,  lyrrus,

  passac,  siel,  synthaxe,  hohner,  twister,  solton,  jellinghaus, 
  southworth,  ppg,  jen,  ssl,  audioVeritrieb,  elka,  dynacord, 
  viscount,  clavia,  audioArchitect,  generalMusic,  soundcraft,  wersi, 
  avab,  digigram,  waldorf,  quasimidi, 

  dream,  strandLighting,  amek,  drBohm,  trident,  realWorldDesign, 
  yesTechnology,  audiomatica,  bontempiFarfisa,  fbtElectronica,  miditemp, 
  larkingAudio,  zero88lighting,  miconAudio,  forefront,  kenton,  adb, 
  jimMarshall,  dda,  bssAudio,  tcElectronic,  medeli,  charlieLab, 
  blueChip,  beeOH,  lgSemiconductor,  tesi,  emagic,  behringer,  access, 
  synoptic,  hanmesoft,  terratec,  proel,  ibk,

  kawai,  roland,  korg,  yamaha,  casio,  kamiya,  akai,  japanVictor, 
  mesosha,  hoshinoGakki,  fujitsuElect,  sony,  nisshinOnpa,  teac, 
  matsushitaElec,  fostex,  zoom,  midori,  matsushitaComm,  suzuki,

  nonCommercial,  nonRealTime,  realTime,
  ) where


import           Sound.MIDI.Parser.Primitive
import qualified Sound.MIDI.Parser.Class as Parser

import qualified Sound.MIDI.Writer.Basic as Writer
import Sound.MIDI.Monoid ((+#+))

import Control.Monad (liftM2, )

import Data.Word (Word8)



data T =
     Short Word8
   | Extended Word8 Word8
   deriving (Show, Eq, Ord)

-- * North American Group

sequential, idp, octavePlateau, moog, passport, lexicon, kurzweil, 
  fender,  gulbransen,  akg,  voyce,  waveframe,  ada,  garfield,  ensoniq, 
  oberheim,  apple,  greyMatter,  digidesign,  palmTree,  jlCooper,  lowrey, 
  adamsSmith,  emu,  harmony,  art,  baldwin,  eventide,  inventronics, 
  keyConcepts,  clarity, 

  timeWarner,  digitalMusic,  iota,  newEngland,  artisyn,  ivl, 
  southernMusic,  lakeButler,  alesis,  dod,  studerEditech,  perfectFret, 
  kat,  opcode,  rane,  anadi,  kmx,  brenell,  peavey,  systems360, 
  spectrum,  marquis,  zeta,  axxes,  orban,  kti,  breakaway,  cae, 
  rocktron,  pianoDisc,  cannon,  rogers,  blueSkyLogic,  encore,  uptown, 
  voce,  cti,  ssResearch,  broderbund,  allenOrgan,  musicQuest,  aphex, 
  gallienKrueger,  ibm,  hotzInstruments,  etaLighting,  nsi,  adLib, 
  richmond,  microsoft,  softwareToolworks,  rjmgNiche,  intone, 
  grooveTubes,  euphonix,  interMIDI,  loneWolf,  musonix,  taHorng,  eTek, 
  electrovoice,  midisoft,  qSoundLabs,  westrex,  nVidia,  ess,  mediaTrix, 
  brooktree,  otari,  keyElectronics,  crystalake,  crystal,  rockwell, 
  siliconGraphics,  midiman,  preSonus,  topaz,  castLighting, 
  microsoftConsumer,  fastForward,  headspace,  vanKoevering,  altech, 
  vlsi,  chromaticResearch,  sapphire,  idrc,  justonic,  torComp,  newtek, 
  soundSculpture,  walker,  pavo,  inVision,  tSquareDesign,  nemesys,  dbx, 
  syndyne,  bitheadz,  cakewalk,  staccato,  nationalSemiconductor, 
  boomTheory,  virtualDSP,  antares,  angelSoftware,  stLouis,  lyrrus :: T

sequential     = Short 0x01
idp            = Short 0x02
octavePlateau  = Short 0x03
moog           = Short 0x04
passport       = Short 0x05
lexicon        = Short 0x06
kurzweil       = Short 0x07
fender         = Short 0x08
gulbransen     = Short 0x09
akg            = Short 0x0A
voyce          = Short 0x0B
waveframe      = Short 0x0C
ada            = Short 0x0D
garfield       = Short 0x0E
ensoniq        = Short 0x0F
oberheim       = Short 0x10
apple          = Short 0x11
greyMatter     = Short 0x12
digidesign     = Short 0x13
palmTree       = Short 0x14
jlCooper       = Short 0x15
lowrey         = Short 0x16
adamsSmith     = Short 0x17
emu            = Short 0x18
harmony        = Short 0x19
art            = Short 0x1A
baldwin        = Short 0x1B
eventide       = Short 0x1C
inventronics   = Short 0x1D
keyConcepts    = Short 0x1E
clarity        = Short 0x1F


timeWarner     = Extended 0x00 0x01
digitalMusic   = Extended 0x00 0x07
iota           = Extended 0x00 0x08
newEngland     = Extended 0x00 0x09
artisyn        = Extended 0x00 0x0A
ivl            = Extended 0x00 0x0B
southernMusic  = Extended 0x00 0x0C
lakeButler     = Extended 0x00 0x0D
alesis         = Extended 0x00 0x0E
dod            = Extended 0x00 0x10
studerEditech  = Extended 0x00 0x11
perfectFret    = Extended 0x00 0x14
kat            = Extended 0x00 0x15
opcode         = Extended 0x00 0x16
rane           = Extended 0x00 0x17
anadi          = Extended 0x00 0x18  -- spatialSound ?
kmx            = Extended 0x00 0x19
brenell        = Extended 0x00 0x1A
peavey         = Extended 0x00 0x1B
systems360     = Extended 0x00 0x1C
spectrum       = Extended 0x00 0x1D
marquis        = Extended 0x00 0x1E
zeta           = Extended 0x00 0x1F
axxes          = Extended 0x00 0x20
orban          = Extended 0x00 0x21
kti            = Extended 0x00 0x24
breakaway      = Extended 0x00 0x25
cae            = Extended 0x00 0x26
rocktron       = Extended 0x00 0x29
pianoDisc      = Extended 0x00 0x2A
cannon         = Extended 0x00 0x2B
rogers         = Extended 0x00 0x2D
blueSkyLogic   = Extended 0x00 0x2E
encore         = Extended 0x00 0x2F
uptown         = Extended 0x00 0x30
voce           = Extended 0x00 0x31

cti                      = Extended 0x00 0x32
ssResearch               = Extended 0x00 0x33
broderbund               = Extended 0x00 0x34
allenOrgan               = Extended 0x00 0x35
musicQuest               = Extended 0x00 0x37
aphex                    = Extended 0x00 0x38
gallienKrueger           = Extended 0x00 0x39
ibm                      = Extended 0x00 0x3A
hotzInstruments          = Extended 0x00 0x3C
etaLighting              = Extended 0x00 0x3D
nsi                      = Extended 0x00 0x3E
adLib                    = Extended 0x00 0x3F
richmond                 = Extended 0x00 0x40
microsoft                = Extended 0x00 0x41
softwareToolworks        = Extended 0x00 0x42
rjmgNiche                = Extended 0x00 0x43
intone                   = Extended 0x00 0x44
grooveTubes              = Extended 0x00 0x47
euphonix                 = Extended 0x00 0x4E
interMIDI                = Extended 0x00 0x4F
loneWolf                 = Extended 0x00 0x55
musonix                  = Extended 0x00 0x64
taHorng                  = Extended 0x00 0x74
eTek                     = Extended 0x00 0x75   -- formerly Forte
electrovoice             = Extended 0x00 0x76
midisoft                 = Extended 0x00 0x77
qSoundLabs               = Extended 0x00 0x78
westrex                  = Extended 0x00 0x79
nVidia                   = Extended 0x00 0x7A
ess                      = Extended 0x00 0x7B
mediaTrix                = Extended 0x00 0x7C
brooktree                = Extended 0x00 0x7D
otari                    = Extended 0x00 0x7E
keyElectronics           = Extended 0x00 0x7F
crystalake               = Extended 0x01 0x01
crystal                  = Extended 0x01 0x02
rockwell                 = Extended 0x01 0x03
siliconGraphics          = Extended 0x01 0x04
midiman                  = Extended 0x01 0x05
preSonus                 = Extended 0x01 0x06
topaz                    = Extended 0x01 0x08
castLighting             = Extended 0x01 0x09
microsoftConsumer        = Extended 0x01 0x0A
fastForward              = Extended 0x01 0x0C
headspace                = Extended 0x01 0x0D   -- Igor's Labs
vanKoevering             = Extended 0x01 0x0E
altech                   = Extended 0x01 0x0F
-- ssResearch               = Extended 0x01 0x10
vlsi                     = Extended 0x01 0x11
chromaticResearch        = Extended 0x01 0x12
sapphire                 = Extended 0x01 0x13
idrc                     = Extended 0x01 0x14
justonic                 = Extended 0x01 0x15
torComp                  = Extended 0x01 0x16
newtek                   = Extended 0x01 0x17
soundSculpture           = Extended 0x01 0x18
walker                   = Extended 0x01 0x19
pavo                     = Extended 0x01 0x1A
inVision                 = Extended 0x01 0x1B
tSquareDesign            = Extended 0x01 0x1C
nemesys                  = Extended 0x01 0x1D
dbx                      = Extended 0x01 0x1E
syndyne                  = Extended 0x01 0x1F
bitheadz                 = Extended 0x01 0x20
cakewalk                 = Extended 0x01 0x21
staccato                 = Extended 0x01 0x22
nationalSemiconductor    = Extended 0x01 0x23
boomTheory               = Extended 0x01 0x24   -- Adinolfi Alternative Percussion
virtualDSP               = Extended 0x01 0x25
antares                  = Extended 0x01 0x26
angelSoftware            = Extended 0x01 0x27
stLouis                  = Extended 0x01 0x28
lyrrus                   = Extended 0x01 0x29


-- * European Group
passac,  siel,  synthaxe,  hohner,  twister,  solton,  jellinghaus, 
  southworth,  ppg,  jen,  ssl,  audioVeritrieb,  elka,  dynacord, 
  viscount,  clavia,  audioArchitect,  generalMusic,  soundcraft,  wersi, 
  avab,  digigram,  waldorf,  quasimidi, 

  dream,  strandLighting,  amek,  drBohm,  trident,  realWorldDesign, 
  yesTechnology,  audiomatica,  bontempiFarfisa,  fbtElectronica,  miditemp, 
  larkingAudio,  zero88lighting,  miconAudio,  forefront,  kenton,  adb, 
  jimMarshall,  dda,  bssAudio,  tcElectronic,  medeli,  charlieLab, 
  blueChip,  beeOH,  lgSemiconductor,  tesi,  emagic,  behringer,  access, 
  synoptic,  hanmesoft,  terratec,  proel,  ibk
    :: T

passac         = Short 0x20
siel           = Short 0x21
synthaxe       = Short 0x22
hohner         = Short 0x24
twister        = Short 0x25
solton         = Short 0x26
jellinghaus    = Short 0x27
southworth     = Short 0x28
ppg            = Short 0x29
jen            = Short 0x2A
ssl            = Short 0x2B
audioVeritrieb = Short 0x2C
elka           = Short 0x2F
dynacord       = Short 0x30
viscount       = Short 0x31
clavia         = Short 0x33
audioArchitect = Short 0x34
generalMusic   = Short 0x35
soundcraft     = Short 0x39
wersi          = Short 0x3B
avab           = Short 0x3C
digigram       = Short 0x3D
waldorf        = Short 0x3E
quasimidi      = Short 0x3F

dream            = Extended 0x20 0x00
strandLighting   = Extended 0x20 0x01
amek             = Extended 0x20 0x02
drBohm           = Extended 0x20 0x04
trident          = Extended 0x20 0x06
realWorldDesign  = Extended 0x20 0x07
yesTechnology    = Extended 0x20 0x09
audiomatica      = Extended 0x20 0x0A
bontempiFarfisa  = Extended 0x20 0x0B
fbtElectronica   = Extended 0x20 0x0C
miditemp         = Extended 0x20 0x0D
larkingAudio     = Extended 0x20 0x0E
zero88lighting   = Extended 0x20 0x0F
miconAudio       = Extended 0x20 0x10
forefront        = Extended 0x20 0x11
kenton           = Extended 0x20 0x13
adb              = Extended 0x20 0x15
jimMarshall      = Extended 0x20 0x16
dda              = Extended 0x20 0x17
bssAudio         = Extended 0x20 0x18
tcElectronic     = Extended 0x20 0x1F
medeli           = Extended 0x20 0x2B
charlieLab       = Extended 0x20 0x2C
blueChip         = Extended 0x20 0x2D
beeOH            = Extended 0x20 0x2E
lgSemiconductor  = Extended 0x20 0x2F
tesi             = Extended 0x20 0x30
emagic           = Extended 0x20 0x31
behringer        = Extended 0x20 0x32
access           = Extended 0x20 0x33
synoptic         = Extended 0x20 0x34
hanmesoft        = Extended 0x20 0x35
terratec         = Extended 0x20 0x36
proel            = Extended 0x20 0x37
ibk              = Extended 0x20 0x38


-- * Japanese Group
kawai,  roland,  korg,  yamaha,  casio,  kamiya,  akai,  japanVictor, 
  mesosha,  hoshinoGakki,  fujitsuElect,  sony,  nisshinOnpa,  teac, 
  matsushitaElec,  fostex,  zoom,  midori,  matsushitaComm,  suzuki
    :: T

kawai          = Short 0x40
roland         = Short 0x41
korg           = Short 0x42
yamaha         = Short 0x43
casio          = Short 0x44
kamiya         = Short 0x46
akai           = Short 0x47
japanVictor    = Short 0x48
mesosha        = Short 0x49
hoshinoGakki   = Short 0x4A
fujitsuElect   = Short 0x4B
sony           = Short 0x4C
nisshinOnpa    = Short 0x4D
teac           = Short 0x4E
matsushitaElec = Short 0x50
fostex         = Short 0x51
zoom           = Short 0x52
midori         = Short 0x53
matsushitaComm = Short 0x54
suzuki         = Short 0x55

-- * Universal ID Numbers
nonCommercial,  nonRealTime,  realTime :: T
nonCommercial  = Short 0x7D
nonRealTime    = Short 0x7E
realTime       = Short 0x7F




-- * serialization

get :: Parser.C parser => Parser.Fragile parser T
get =
   do subId <- getByte
      if subId == 0
        then liftM2 Extended getByte getByte
        else return $ Short subId

put :: Writer.C writer => T -> writer
put subId =
   case subId of
     Short n -> Writer.putByte n
     Extended hi lo ->
        Writer.putByte 0 +#+
        Writer.putByte hi +#+
        Writer.putByte lo
