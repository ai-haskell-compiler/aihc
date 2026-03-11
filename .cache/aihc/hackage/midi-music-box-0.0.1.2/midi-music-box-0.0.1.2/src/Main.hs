{-# LANGUAGE FlexibleContexts #-}
module Main where

import qualified Note

import qualified Sound.MIDI.Message.Class.Query as Query

import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg
import qualified Sound.MIDI.File.Load as Load
import qualified Sound.MIDI.File as MidiFile

import qualified Data.EventList.Absolute.TimeBody as AbsEventList
import qualified Data.EventList.Relative.TimeBody as EventList

import qualified Diagrams.Backend.Postscript.CmdLine as PS
import qualified Diagrams.Backend.CmdLine as Cmd
import Diagrams.Prelude (Diagram)
import Diagrams.Prelude (translateX, translateY, translate, r2, alignTL)
import Diagrams.Prelude (hsep, vsep)
import Diagrams.Prelude (circle, rect, hrule, vrule)
import Diagrams.Prelude (lwO, text, fontSizeL)
import Diagrams.Prelude (fc, lc, yellow, blue, darkgreen, white, grey, black)
import Diagrams.Prelude ((#), (<$>), (<>))

import qualified Options.Applicative as OP

import qualified System.IO as IO
import Text.Printf (hPrintf, printf)

import Control.Monad (when, )

import qualified Data.Map as Map; import Data.Map (Map, )
import qualified Data.List.HT as ListHT
import qualified Data.List as List
import qualified Data.Monoid.HT as MnHT
import qualified Data.Monoid as Mn
import Data.Foldable (foldMap, fold, )
import Data.Tuple.HT (mapSnd, )


type Diag = Diagram PS.B

globalScale :: Double
globalScale = 1/7

horsep, versep :: Double
horsep = 40*globalScale
versep = 80*globalScale

horlen, verlen :: Int -> Double
horlen n = fromIntegral n * horsep
verlen n = fromIntegral n * versep

normalLW, thickLW :: Diag -> Diag
normalLW = lwO (5*globalScale)
thickLW = lwO (8*globalScale)


data MusicScale =
   MusicScale {
      scaleLabels :: [Char],
      greenLines :: [Bool],
      scaleNoteMap :: Map Int (Bool, Int),
      horizontalMargin, horizontalExtraMargin :: Double
   }

musicScale15, musicScale30 :: MusicScale
musicScale15 =
   musicScaleFromNotes Note.pitches15 (65*globalScale, 100*globalScale)
musicScale30 =
   musicScaleFromNotes Note.pitches30 (3*horsep, 5*horsep)

musicScaleFromNotes :: [(Bool, (Char, Int))] -> (Double,Double) -> MusicScale
musicScaleFromNotes notes (hormargin, horextramargin) =
   let (greenLines_, (scaleLabels_,ps)) = mapSnd unzip $ unzip notes
   in MusicScale {
         horizontalMargin = hormargin,
         horizontalExtraMargin = horextramargin,
         scaleLabels = scaleLabels_,
         greenLines = greenLines_,
         scaleNoteMap =
            Map.fromList $ concat $
            zipWith3
               (\pos p k ->
                  take k $ zip [p..] $ (True,pos) : repeat (False,pos))
               [0..] ps (ListHT.mapAdjacent subtract ps ++ [1])
      }


labels :: [Char] -> Diag
labels = hsep horsep . map (\char -> text [char] # fontSizeL horsep)

long :: [Bool] -> Int -> Diag
long greenLs numIntervals =
   hsep horsep $
   map (\(wid, col) ->
          vrule (verlen numIntervals) # wid # lc col) $
   map
      (\isGreen -> if isGreen then (thickLW, darkgreen) else (normalLW, black))
      greenLs

across :: Int -> Int -> Diag
across numLong numIntervals =
   vsep versep $
   take (succ numIntervals) $ cycle $
      let len = horlen (numLong-1)
      in  map (# normalLW) [hrule len, hrule len # lc grey]

grid :: MusicScale -> Cutter -> Int -> Diag
grid musicScale (Cutter cutter) numIntervals =
  let numLong = length $ scaleLabels musicScale
      hormargin = horizontalMargin musicScale
      horextramargin = horizontalExtraMargin musicScale
  in
   MnHT.when (not cutter)
      (alignTL (long (greenLines musicScale) numIntervals)) <>
   MnHT.when (not cutter) (alignTL (across numLong numIntervals)) <>
   translateX (-hormargin)
      (alignTL (vrule (verlen numIntervals) # normalLW # lc grey)) <>
   translateX (horlen (numLong-1) + hormargin)
      (alignTL (vrule (verlen numIntervals) # normalLW # lc grey)) <>
   MnHT.when (not cutter)
      (translateY (0.5*versep) $ labels $ scaleLabels musicScale) <>
   (translateX (-horextramargin) $ translateY (verlen 2) $ alignTL $ lc white $
    rect (horlen (numLong-1) + 2*horextramargin) (verlen (numIntervals+4)))


dots :: Diameter -> Cutter -> [(DotType, (Int, Double))] -> Diag
dots (Diameter diameterRel) (Cutter cutter) poss =
   flip foldMap poss $
   \(typ, (x,y)) ->
      translate (r2 (horlen x, - versep * y)) $
      let radius = horsep*diameterRel/2
          warning txt =
            (fontSizeL horsep $ text txt) <>
            (lwO 0 $ fc yellow $ circle (horsep*0.4))
      in if cutter
            then
               case typ of
                  Valid -> normalLW $ circle radius
                  _ -> Mn.mempty
            else
               case typ of
                  Valid -> normalLW $ fc blue $ circle radius
                  Semitone -> warning "#"
                  TooLow -> warning "!"
                  TooHigh -> warning "!"


data DotType = Valid | Semitone | TooLow | TooHigh
   deriving (Eq, Ord)

layoutDots ::
   Map Int (Bool, Int) ->
   TimeStep -> VoiceMsg.Pitch -> MidiFile.T -> [(DotType, (Int, Double))]
layoutDots
      noteMap (TimeStep timeStep) zeroKey (MidiFile.Cons typ division tracks) =
   map (\(t, (dottyp,p)) -> (dottyp, (p,t))) $
   AbsEventList.toPairList $
   AbsEventList.mapTime ((/timeStep) . realToFrac) $
   AbsEventList.mapMaybe
      (\ev -> do
         (_c, (_v, p, True)) <- Query.noteExplicitOff ev
         let pz = VoiceMsg.subtractPitch zeroKey p
         return $
            case Map.lookup pz noteMap of
               Just (s, n) -> (if s then Valid else Semitone, n)
               Nothing ->
                  let ( pmin, (_, nmin)) = Map.findMin noteMap
                      (_pmax, (_, nmax)) = Map.findMax noteMap
                  in  if pz < pmin
                        then (TooLow,  nmin)
                        else (TooHigh, nmax)) $
   EventList.toAbsoluteEventList 0 $
   MidiFile.mergeTracks typ $
   map (MidiFile.secondsFromTicks division) tracks


musicScaleMap :: Map String MusicScale
musicScaleMap =
   Map.fromList $
      ("major15", musicScale15) :
      ("mixed30", musicScale30) :
      []

instance Cmd.Parseable MusicScale where
   parser =
      OP.option
         (OP.eitherReader $ \str ->
            maybe
               (Left $
                printf "unknown name '%s', must be one of %s" str
                  (List.intercalate ", " $ Map.keys musicScaleMap))
               Right $
            Map.lookup str musicScaleMap)
         (OP.long "music-scale" Mn.<>
          OP.metavar "NAME" Mn.<>
          OP.value musicScale15 Mn.<>
          OP.help "Type of music box")


newtype ZeroKey = ZeroKey Int

instance Cmd.Parseable ZeroKey where
   parser =
      OP.option (ZeroKey <$> OP.auto)
         (OP.long "zerokey" Mn.<>
          OP.metavar "INT" Mn.<>
          OP.value (ZeroKey 60) Mn.<>
          OP.help "MIDI key for the lowest note line")


newtype TimeStep = TimeStep Double

instance Cmd.Parseable TimeStep where
   parser =
      OP.option (TimeStep <$> OP.auto)
         (OP.long "timestep" Mn.<>
          OP.metavar "SECONDS" Mn.<>
          OP.value (TimeStep 0.1) Mn.<>
          OP.help "time step between lines")


newtype Cutter = Cutter Bool

instance Cmd.Parseable Cutter where
   parser =
      Cutter <$>
      OP.flag False True
         (OP.long "cutter" Mn.<>
          OP.help "stripped graphics ready for laser cutter")


newtype Diameter = Diameter Double

instance Cmd.Parseable Diameter where
   parser =
      OP.option (Diameter <$> OP.auto)
         (OP.long "hole-diameter" Mn.<>
          OP.metavar "RATIO" Mn.<>
          OP.value (Diameter 0.5) Mn.<>
          OP.help "size of a hole relative to line distance")


newtype Input = Input FilePath

instance Cmd.Parseable Input where
   parser = OP.argument (Input <$> OP.str) (OP.metavar "INPUT")


diag ::
   MusicScale -> TimeStep -> ZeroKey -> Diameter -> Cutter -> Input -> IO Diag
diag musicScale timeStep (ZeroKey zeroKey) diameter cutter (Input path) = do
   midi <- Load.fromFile path
   let cloud =
         layoutDots (scaleNoteMap musicScale)
            timeStep (VoiceMsg.toPitch zeroKey) midi
       sorted = Map.fromListWith (++) $ map (mapSnd (:[])) cloud
       warning typ msg =
         let n = length $ fold $ Map.lookup typ sorted
         in  when (n > 0) $ hPrintf IO.stderr "Warning: %i %s\n" n msg
   warning Semitone "semitones"
   warning TooLow "notes are too low"
   warning TooHigh "notes are too high"
   return $
      dots diameter cutter cloud <>
      (grid musicScale cutter $ ceiling $ maximum $ map (snd . snd) cloud)

main :: IO ()
main = PS.mainWith diag
