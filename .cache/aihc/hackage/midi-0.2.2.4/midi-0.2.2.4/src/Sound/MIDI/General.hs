{- |
General-MIDI definitions.

Taken from Haskore.
-}

module Sound.MIDI.General where

import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg
import           Sound.MIDI.Message.Channel (Channel, toChannel, )
import           Data.Ix(Ix)
import qualified Data.List as List

import Sound.MIDI.Utility
          (enumRandomR, boundedEnumRandom, chooseEnum, )
import Data.Tuple.HT (mapSnd, )
import Test.QuickCheck (Arbitrary(arbitrary), )
import System.Random (Random(random,randomR), )


{- * Instrument definitions -}

instrumentNameToProgram :: String -> Maybe VoiceMsg.Program
instrumentNameToProgram =
   fmap VoiceMsg.toProgram . flip List.elemIndex instrumentNames

instrumentNames :: [String]
instrumentNames = map fst instrumentPrograms

instrumentPrograms :: [(String, VoiceMsg.Program)]
instrumentPrograms =
   map (mapSnd VoiceMsg.toProgram) [
      ("Acoustic Grand Piano",0),       ("Bright Acoustic Piano",1),
      ("Electric Grand Piano",2),       ("Honky Tonk Piano",3),
      ("Rhodes Piano",4),               ("Chorused Piano",5),
      ("Harpsichord",6),                ("Clavinet",7),
      ("Celesta",8),                    ("Glockenspiel",9),
      ("Music Box",10),                 ("Vibraphone",11),
      ("Marimba",12),                   ("Xylophone",13),
      ("Tubular Bells",14),             ("Dulcimer",15),
      ("Hammond Organ",16),             ("Percussive Organ",17),
      ("Rock Organ",18),                ("Church Organ",19),
      ("Reed Organ",20),                ("Accordion",21),
      ("Harmonica",22),                 ("Tango Accordion",23),
      ("Acoustic Guitar (nylon)",24),   ("Acoustic Guitar (steel)",25),
      ("Electric Guitar (jazz)",26),    ("Electric Guitar (clean)",27),
      ("Electric Guitar (muted)",28),   ("Overdriven Guitar",29),
      ("Distortion Guitar",30),         ("Guitar Harmonics",31),
      ("Acoustic Bass",32),             ("Electric Bass (fingered)",33),
      ("Electric Bass (picked)",34),    ("Fretless Bass",35),
      ("Slap Bass 1",36),               ("Slap Bass 2",37),
      ("Synth Bass 1",38),              ("Synth Bass 2",39),
      ("Violin",40),                    ("Viola",41),
      ("Cello",42),                     ("Contrabass",43),
      ("Tremolo Strings",44),           ("Pizzicato Strings",45),
      ("Orchestral Harp",46),           ("Timpani",47),
      ("String Ensemble 1",48),         ("String Ensemble 2",49),
      ("Synth Strings 1",50),           ("Synth Strings 2",51),
      ("Choir Aahs",52),                ("Voice Oohs",53),
      ("Synth Voice",54),               ("Orchestra Hit",55),
      ("Trumpet",56),                   ("Trombone",57),
      ("Tuba",58),                      ("Muted Trumpet",59),
      ("French Horn",60),               ("Brass Section",61),
      ("Synth Brass 1",62),             ("Synth Brass 2",63),
      ("Soprano Sax",64),               ("Alto Sax",65),
      ("Tenor Sax",66),                 ("Baritone Sax",67),
      ("Oboe",68),                      ("Bassoon",69),
      ("English Horn",70),              ("Clarinet",71),
      ("Piccolo",72),                   ("Flute",73),
      ("Recorder",74),                  ("Pan Flute",75),
      ("Blown Bottle",76),              ("Shakuhachi",77),
      ("Whistle",78),                   ("Ocarina",79),
      ("Lead 1 (square)",80),           ("Lead 2 (sawtooth)",81),
      ("Lead 3 (calliope)",82),         ("Lead 4 (chiff)",83),
      ("Lead 5 (charang)",84),          ("Lead 6 (voice)",85),
      ("Lead 7 (fifths)",86),           ("Lead 8 (bass+lead)",87),
      ("Pad 1 (new age)",88),           ("Pad 2 (warm)",89),
      ("Pad 3 (polysynth)",90),         ("Pad 4 (choir)",91),
      ("Pad 5 (bowed)",92),             ("Pad 6 (metallic)",93),
      ("Pad 7 (halo)",94),              ("Pad 8 (sweep)",95),
      ("FX1 (train)",96),               ("FX2 (soundtrack)",97),
      ("FX3 (crystal)",98),             ("FX4 (atmosphere)",99),
      ("FX5 (brightness)",100),         ("FX6 (goblins)",101),
      ("FX7 (echoes)",102),             ("FX8 (sci-fi)",103),
      ("Sitar",104),                    ("Banjo",105),
      ("Shamisen",106),                 ("Koto",107),
      ("Kalimba",108),                  ("Bagpipe",109),
      ("Fiddle",110),                   ("Shanai",111),
      ("Tinkle Bell",112),              ("Agogo",113),
      ("Steel Drums",114),              ("Woodblock",115),
      ("Taiko Drum",116),               ("Melodic Drum",117),
      ("Synth Drum",118),               ("Reverse Cymbal",119),
      ("Guitar Fret Noise",120),        ("Breath Noise",121),
      ("Seashore",122),                 ("Bird Tweet",123),
      ("Telephone Ring",124),           ("Helicopter",125),
      ("Applause",126),                 ("Gunshot",127)
   ]

instrumentFromProgram :: VoiceMsg.Program -> Instrument
instrumentFromProgram = toEnum . VoiceMsg.fromProgram

instrumentToProgram :: Instrument -> VoiceMsg.Program
instrumentToProgram = VoiceMsg.toProgram . fromEnum

instrumentChannels :: [Channel]
instrumentChannels = map toChannel $ [0..8] ++ [10..15]

instruments :: [Instrument]
instruments = enumFromTo minBound maxBound

data Instrument =
     AcousticGrandPiano              | BrightAcousticPiano
   | ElectricGrandPiano              | HonkyTonk
   | ElectricPiano1                  | ElectricPiano2
   | Harpsichord                     | Clavinet
   | Celesta                         | Glockenspiel
   | MusicBox                        | Vibraphone
   | Marimba                         | Xylophone
   | TubularBells                    | Dulcimer
   | DrawbarOrgan                    | PercussiveOrgan
   | RockOrgan                       | ChurchOrgan
   | ReedOrgan                       | Accordion
   | Harmonica                       | TangoAccordian
   | AcousticGuitarNylon             | AcousticGuitarSteel
   | ElectricGuitarJazz              | ElectricGuitarClean
   | ElectricGuitarMuted             | OverdrivenGuitar
   | DistortionGuitar                | GuitarHarmonics
   | AcousticBass                    | ElectricBassFinger
   | ElectricBassPick                | FretlessBass
   | SlapBass1                       | SlapBass2
   | SynthBass1                      | SynthBass2
   | Violin                          | Viola
   | Cello                           | Contrabass
   | TremoloStrings                  | PizzicatoStrings
   | OrchestralHarp                  | Timpani
   | StringEnsemble1                 | StringEnsemble2
   | SynthStrings1                   | SynthStrings2
   | ChoirAahs                       | VoiceOohs
   | SynthVoice                      | OrchestraHit
   | Trumpet                         | Trombone
   | Tuba                            | MutedTrumpet
   | FrenchHorn                      | BrassSection
   | SynthBrass1                     | SynthBrass2
   | SopranoSax                      | AltoSax
   | TenorSax                        | BaritoneSax
   | Oboe                            | EnglishHorn
   | Bassoon                         | Clarinet
   | Piccolo                         | Flute
   | Recorder                        | PanFlute
   | BlownBottle                     | Skakuhachi
   | Whistle                         | Ocarina
   | Lead1Square                     | Lead2Sawtooth
   | Lead3Calliope                   | Lead4Chiff
   | Lead5Charang                    | Lead6Voice
   | Lead7Fifths                     | Lead8BassLead
   | Pad1NewAge                      | Pad2Warm
   | Pad3Polysynth                   | Pad4Choir
   | Pad5Bowed                       | Pad6Metallic
   | Pad7Halo                        | Pad8Sweep
   | FX1Rain                         | FX2Soundtrack
   | FX3Crystal                      | FX4Atmosphere
   | FX5Brightness                   | FX6Goblins
   | FX7Echoes                       | FX8SciFi
   | Sitar                           | Banjo
   | Shamisen                        | Koto
   | Kalimba                         | Bagpipe
   | Fiddle                          | Shanai
   | TinkleBell                      | Agogo
   | SteelDrums                      | Woodblock
   | TaikoDrum                       | MelodicTom
   | SynthDrum                       | ReverseCymbal
   | GuitarFretNoise                 | BreathNoise
   | Seashore                        | BirdTweet
   | TelephoneRing                   | Helicopter
   | Applause                        | Gunshot
     deriving (Show, Eq, Ord, Ix, Enum, Bounded)

instance Random Instrument where
   random  = boundedEnumRandom
   randomR = enumRandomR

instance Arbitrary Instrument where
   arbitrary = chooseEnum



{- * Drum definitions -}


drumChannel :: Channel
drumChannel = toChannel 9

drumProgram :: VoiceMsg.Program
drumProgram = VoiceMsg.toProgram 0

drumMinKey :: VoiceMsg.Pitch
drumMinKey = VoiceMsg.toPitch 35

drumKeyTable :: [(Drum, VoiceMsg.Pitch)]
drumKeyTable = zip drums [drumMinKey ..]

drumFromKey :: VoiceMsg.Pitch -> Drum
drumFromKey = toEnum . VoiceMsg.subtractPitch drumMinKey

drumToKey :: Drum -> VoiceMsg.Pitch
drumToKey = flip VoiceMsg.increasePitch drumMinKey . fromEnum

drums :: [Drum]
drums = enumFromTo minBound maxBound


data Drum =
        AcousticBassDrum  -- Midi Key 35
      | BassDrum1         -- Midi Key 36
      | SideStick         -- ...
      | AcousticSnare | HandClap      | ElectricSnare | LowFloorTom
      | ClosedHiHat   | HighFloorTom  | PedalHiHat    | LowTom
      | OpenHiHat     | LowMidTom     | HiMidTom      | CrashCymbal1
      | HighTom       | RideCymbal1   | ChineseCymbal | RideBell
      | Tambourine    | SplashCymbal  | Cowbell       | CrashCymbal2
      | Vibraslap     | RideCymbal2   | HiBongo       | LowBongo
      | MuteHiConga   | OpenHiConga   | LowConga      | HighTimbale
      | LowTimbale    | HighAgogo     | LowAgogo      | Cabasa
      | Maracas       | ShortWhistle  | LongWhistle   | ShortGuiro
      | LongGuiro     | Claves        | HiWoodBlock   | LowWoodBlock
      | MuteCuica     | OpenCuica     | MuteTriangle
      | OpenTriangle      -- Midi Key 81
   deriving (Show, Eq, Ord, Ix, Enum, Bounded)

-- http://oxygen.cside6.com/gallery/ins_gm.html

instance Random Drum where
   random  = boundedEnumRandom
   randomR = enumRandomR

instance Arbitrary Drum where
   arbitrary = chooseEnum
