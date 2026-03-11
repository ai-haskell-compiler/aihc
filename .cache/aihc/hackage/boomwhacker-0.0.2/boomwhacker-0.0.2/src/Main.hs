module Main where

import qualified Options.Applicative as OP

import qualified Graphics.PDF as PDF

import qualified Shell.Utility.Verbosity as Verbosity
import qualified Shell.Utility.Log as Log
import Shell.Utility.ParseArgument (parseNumber)
import Shell.Utility.Exit (exitFailureMsg)

import qualified Sound.MIDI.Message.Class.Query as Query
import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg
import qualified Sound.MIDI.General as GeneralMidi
import qualified Sound.MIDI.File.Load as MidiLoad
import qualified Sound.MIDI.File.Event.Meta as MetaEvent
import qualified Sound.MIDI.File.Event as MidiEvent
import qualified Sound.MIDI.File as MidiFile

import qualified Data.EventList.Absolute.TimeBody as AbsEventList
import qualified Data.EventList.Relative.TimeBody as EventList
import qualified Numeric.NonNegative.Wrapper as NonNegW

import qualified Data.Time.Format as TimeFmt
import qualified Data.Time as Time
import qualified Data.Array as Array
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.IntSet as IntSet
import qualified Data.Traversable as Trav
import qualified Data.Foldable as Fold
import qualified Data.FixedLengthList as FL
import qualified Data.NonEmpty.Class as NonEmptyC
import qualified Data.Sequence as Seq
import qualified Data.List as List
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Array (Array, listArray, (!))
import Data.Semigroup ((<>))
import Data.Tuple.HT (mapFst, mapPair, swap, thd3)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.String (fromString)
import Data.Complex (Complex((:+)))

import qualified Control.Monad.Trans.State as MS
import Control.Monad (join, when, guard, mfilter)
import Control.Applicative (liftA2, (<$>), (<*>), (<|>))

import Text.Printf (printf)



{- |
Terminated tubes sorted with respect to upper boundary
and unterminated tubes sorted with respect to note.
-}
type VisibleTubes = (Map Double [(Double, Int)], IntMap Double)

{- |
This also handles three kinds of corruption:
NoteOff without NoteOn, NoteOn without NoteOff,
duplicate NoteOn (which is kind of special case of NoteOn without NoteOff)
-}
welcomeNextEvent ::
   (Double, (Int, Bool)) -> VisibleTubes -> VisibleTubes
welcomeNextEvent (timeStamp, (pitch, noteOn)) (terminated, unterminated) =
   (case IntMap.lookup pitch unterminated of
      Nothing -> terminated
      Just timeStart ->
         Map.insertWith (++) timeStamp [(timeStart, pitch)] terminated
    ,
    if noteOn
      then IntMap.insert pitch timeStamp unterminated
      else IntMap.delete pitch unterminated)

farewellEvents :: Double -> VisibleTubes -> VisibleTubes
farewellEvents time (terminated, unterminated) =
   (thd3 $ Map.splitLookup time terminated, unterminated)

filterNotes ::
   (Query.C ev) =>
   VoiceMsg.Pitch ->
   AbsEventList.T Double ev -> AbsEventList.T Double (Int, Bool)
filterNotes zeroKey =
   AbsEventList.mapMaybe
      (\ev -> do
         (c, (_v, p, noteOn)) <- Query.noteExplicitOff ev
         guard $ c /= GeneralMidi.drumChannel
         return (VoiceMsg.subtractPitch zeroKey p, noteOn))

windowInitialize ::
   Double -> [(Double, (Int, Bool))] ->
   (VisibleTubes, [(Double, (Int, Bool))])
windowInitialize time events =
   case span ((<=time) . fst) events of
      (displayed, remaining) ->
         (foldl (flip welcomeNextEvent) (Map.empty, IntMap.empty) displayed,
          remaining)

windowMove ::
   (Double, Double) ->
   (VisibleTubes, [(Double, (Int, Bool))]) ->
   (VisibleTubes, [(Double, (Int, Bool))])
windowMove (newFrom, newTo) (currentDisplay, events) =
   case span ((<=newTo) . fst) events of
      (newDisplayed, remaining) ->
         (foldl (flip welcomeNextEvent)
            (farewellEvents newFrom currentDisplay)
            newDisplayed,
          remaining)

data Tube =
   Tube {
      tubeFrom :: Double,
      tubeTo :: Maybe Double,
      tubePitch :: Int
   }

shiftTube :: Double -> Tube -> Tube
shiftTube d (Tube from mTo pitch) = (Tube (from+d) (fmap (+d) mTo) pitch)

windowLayout :: VisibleTubes -> [Tube]
windowLayout (terminated, unterminated) =
   Fold.fold
      (Map.mapWithKey
         (\to -> map (\(from, pitch) -> Tube from (Just to) pitch))
         terminated)
   ++
   map
      (\(pitch, from) -> Tube from Nothing pitch)
      (IntMap.toList unterminated)

minimalDistanceToTubeHeads :: [Tube] -> IntMap Double
minimalDistanceToTubeHeads =
   IntMap.fromListWith min .
   map (\(Tube from _mTo pitch) -> (pitch, -from)) .
   filter (\(Tube from _mTo _pitch) -> from<=0)


defaultTitleDuration :: (Fractional t) => t
defaultTitleDuration = 2

titleFadeDuration :: (Fractional t) => t
titleFadeDuration = 1

alphaForTitle :: (RealFrac a) => a -> a -> a
alphaForTitle titleDuration t = min 1 ((titleDuration-t)/titleFadeDuration)

layoutTitle :: (RealFrac t) => t -> t -> [[(t, text)]] -> [[(t, t, text)]]
layoutTitle frameRate titleDuration =
   map (map (\(t,text) -> (1, alphaForTitle titleDuration t, text))) .
   addCaptionTails (ceiling (titleDuration*frameRate)) (recip frameRate)

layoutCounters :: (RealFrac t) => t -> [[(t, text)]] -> [[(t, t, text)]]
layoutCounters frameRate =
   map (map (\(t,text) ->
         let dt = max 0 (t-0.05)
             z = 1 + dt * 2
         in (5/z, 1 - dt/1.0, text))) .
   addCaptionTails (ceiling (1.0*frameRate)) (recip frameRate)

zipSemi :: (Semigroup a) => [a] -> [a] -> [a]
zipSemi (x:xs) (y:ys) = (x<>y) : zipSemi xs ys
zipSemi [] ys = ys
zipSemi xs [] = xs

addCaptionTails :: (Num t) => Int -> t -> [[(t,a)]] -> [[(t,a)]]
addCaptionTails numFrames d =
   foldr
      (\cs xs ->
         flip zipSemi ([]:xs) $
         List.transpose $
         map
            (\(t,a) ->
               map (\ti -> (ti,a)) $
               take numFrames $ iterate (d+) t)
            cs)
      []

chop :: (Ord t) => [t] -> [(t,a)] -> [[(t,a)]]
chop ts evs0 =
   snd $ Trav.mapAccumL (\evs t -> swap $ span ((<t).fst) evs) evs0 ts

filterTitle :: AbsEventList.T Double MidiEvent.T -> [(Double, String)]
filterTitle =
   AbsEventList.toPairList .
   AbsEventList.mapMaybe
      (\ev ->
         case ev of
            MidiEvent.MetaEvent (MetaEvent.TrackName str) -> Just str
            _ -> Nothing)

filterLyrics :: AbsEventList.T Double MidiEvent.T -> [(Double, String)]
filterLyrics =
   AbsEventList.toPairList .
   AbsEventList.mapMaybe
      (\ev ->
         case ev of
            MidiEvent.MetaEvent (MetaEvent.Lyric str) -> Just str
            _ -> Nothing)


seqViewL :: Seq a -> Maybe (a, Seq a)
seqViewL xs =
   case Seq.viewl xs of
      Seq.EmptyL -> Nothing
      y Seq.:< ys -> Just (y,ys)

formatPitch :: VoiceMsg.Pitch -> String
formatPitch p =
   case divMod (fromEnum p) (snd $ Array.bounds noteNames) of
      (octave, pitchClass) -> printf "%s%d" (noteNames ! pitchClass) octave

formatSeconds :: Double -> String
formatSeconds =
   TimeFmt.formatTime TimeFmt.defaultTimeLocale "%T%03Q" .
      Time.timeToTimeOfDay . realToFrac

detectCorruptNotes ::
   (Num time) =>
   (time -> String) ->
   AbsEventList.T time MidiEvent.T -> AbsEventList.T time String
detectCorruptNotes formatTimeStamp =
   flip MS.evalState IntMap.empty
   .
   AbsEventList.traverseWithTime
      (\t (p,noteOn) -> do
         noteOns <- MS.get
         let pInt = fromEnum p
         let prompt :: String
             prompt =
               printf "%s, pitch %d (%s)"
                  (formatTimeStamp t) pInt (formatPitch p)
         if noteOn
            then
               case mfilter (not . Fold.null) $ IntMap.lookup pInt noteOns of
                  Nothing -> do
                     MS.put $ IntMap.insert pInt (Seq.singleton t) noteOns
                     return ""
                  Just startTimes -> do
                     MS.put $
                        IntMap.insert pInt
                           (startTimes <> Seq.singleton t) noteOns
                     return $
                        printf "%s: NoteOn after unfinished NoteOns at %s"
                           prompt
                           (List.intercalate ", " $ Fold.toList $
                              fmap formatTimeStamp startTimes)
            else
               case IntMap.lookup pInt noteOns >>= seqViewL of
                  Nothing ->
                     return $ printf "%s: NoteOff without NoteOn" prompt
                  Just (_startTime, startTimes) -> do
                     MS.put $ IntMap.insert pInt startTimes noteOns
                     return "")
   .
   AbsEventList.mapMaybe
      (\ev -> do
         (c, (_v, p, noteOn)) <- Query.noteExplicitOff ev
         guard $ c /= GeneralMidi.drumChannel
         return (p, noteOn))

mergeTracksToAbsoluteTicks :: MidiFile.T -> AbsEventList.T Integer MidiEvent.T
mergeTracksToAbsoluteTicks (MidiFile.Cons typ _division tracks) =
   AbsEventList.mapTime NonNegW.toNumber $
   EventList.toAbsoluteEventList 0 $
   MidiFile.mergeTracks typ tracks

mergeTracksToAbsoluteSeconds :: MidiFile.T -> AbsEventList.T Double MidiEvent.T
mergeTracksToAbsoluteSeconds (MidiFile.Cons typ division tracks) =
   AbsEventList.mapTime realToFrac $
   EventList.toAbsoluteEventList 0 $
   MidiFile.secondsFromTicks division $
   MidiFile.mergeTracks typ tracks

selectTracks :: [Int] -> MidiFile.T -> Either String MidiFile.T
selectTracks [] midi = Right midi
selectTracks trackNos (MidiFile.Cons typ division tracks) =
   MidiFile.Cons typ division <$>
   let trackMap = IntMap.fromList $ zip [1..] tracks in
   Trav.forM trackNos $ \trackNo ->
      maybe (Left $ printf "track %d not available" trackNo) Right $
      IntMap.lookup trackNo trackMap


noteLetters :: [Char]
noteLetters =
   ['C', '#', 'D', '#', 'E', 'F', '#', 'G', '#', 'A', '#', 'B', 'C']

noteNameList :: [String]
noteNameList =
   ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B=H", "C"]

noteNames :: Array Int String
noteNames = listArray (0, length noteNameList - 1) noteNameList


type List3 = FL.T3
type RGB = List3 Double

noteColors :: Array Int RGB
noteColors =
   let rgb = FL.consAll3
       xs =
         rgb  90   0   0 :
         rgb  90  15   0 :
         rgb  95  35   0 :
         rgb  95  60   5 :
         rgb 100  75   0 :
         rgb  25 100  25 :
         rgb   5  70  20 :
         rgb   5  45  20 :
         rgb   0  20  65 :
         rgb  35   0  70 :
         rgb  55   0  55 :
         rgb  75  35  75 :
         rgb  90   0   0 :
         []
   in listArray (0, length xs - 1) $ map (fmap (0.01*)) xs

greyRGB :: Double -> RGB
greyRGB = NonEmptyC.repeat

grey :: Double -> PDF.Color
grey brightness = PDF.Rgb brightness brightness brightness


colorFromPitch :: Bool -> Int -> RGB
colorFromPitch fullRange pitch =
   if fullRange || Array.inRange (0,12) pitch
      then noteColors ! mod pitch (snd $ Array.bounds noteColors)
      else greyRGB 0.5

selectDisplayedCups :: Bool -> Bool -> IntSet -> IntMap (RGB, Bool)
selectDisplayedCups fullRange usedCupsOnly usedCups =
   let pitchRange =
         fromMaybe (0,0) $
         liftA2 (,)
            (fst <$> IntSet.minView usedCups)
            (fst <$> IntSet.maxView usedCups)
   in IntMap.mapWithKey
         (\pitch used -> (colorFromPitch fullRange pitch, used)) $
      IntMap.union (IntMap.fromSet (const True) usedCups) $
      if usedCupsOnly
         then IntMap.empty
         else IntMap.fromSet (const False) $
              IntSet.fromList $ Array.range pitchRange

interpolateColor :: Double -> RGB -> RGB -> RGB
interpolateColor k = NonEmptyC.zipWith (\x y -> (1-k)*x + k*y)


writePDF ::
   FilePath -> Maybe PDF.JpegFile -> Int -> PDF.FontName -> Int ->
   IntMap (RGB,Bool) -> [([Tube], [(Double, Double, String)])] -> IO ()
writePDF
   path mBackground heightPoints fontName fontHeightInt
   displayedCups blocks = do

   let fontHeight = fromIntegral fontHeightInt
       mBackgroundExtent =
         fmap (mapPair (fromIntegral, fromIntegral) . PDF.jpegBounds)
            mBackground
       mBackgroundWidth =
         fmap (\(w,h) -> let k = height / h in (w * k, k)) mBackgroundExtent
       cupsBarWidth = fromIntegral (IntMap.size displayedCups) * fontHeight
       width  = maybe cupsBarWidth (max cupsBarWidth . fst) mBackgroundWidth
       height = fromIntegral heightPoints
       bottom =  0.5 * fontHeight
       gradientHeight = height
       rect = PDF.PDFRect 0 0 width height
   let (bowHeight, tube) =
         if True
            then
               (0.2,
                \(l:+b) (r:+t) -> do
                  let b1 = b - bowHeight*fontHeight
                  let t1 = t - bowHeight*fontHeight
                  PDF.beginPath (l:+b)
                  PDF.curveto (l:+b1) (r:+b1) (r:+b)
                  PDF.lineto (r:+t)
                  PDF.curveto (r:+t1) (l:+t1) (l:+t)
               )
            else (0, \lb rt -> PDF.addShape $ PDF.Rectangle lb rt)

   stdFont <- either (fail . show) return =<< PDF.mkStdFont fontName
   PDF.runPdf path PDF.standardDocInfo rect $ do
      mBackgroundObj <- Trav.traverse PDF.createPDFJpeg mBackground

      fadingMask <- do
         let maskRect =
               PDF.Rectangle
                  (0:+(-bowHeight*fontHeight))
                  (fontHeight:+gradientHeight)
         PDF.createSoftMask maskRect
            (PDF.paintWithShading
               (PDF.AxialShading 0 0 0 gradientHeight
                  (PDF.ColorFunction1 PDF.GraySpace $
                   PDF.Interpolated1 1  1 0))
               (PDF.addShape maskRect))

      let cupRect =
            PDF.Rectangle
               (0:+(-bowHeight*fontHeight))
               (fontHeight:+fontHeight)
      unusedCupMask <- do
         PDF.createSoftMask cupRect $ do
            PDF.fillColor $ grey 0.2
            PDF.fill cupRect

      cupObjs <-
         Trav.sequence $
         flip IntMap.mapWithKey displayedCups $ \pitch (color,used) ->
            fmap (flip (,) (color,used)) $
            PDF.createTransparencyGroup PDF.RGBSpace cupRect $
               drawCup (bowHeight, tube) stdFont fontHeightInt
                  color pitch 1

      Fold.for_ blocks $ \(block, captions) -> do
         page <- PDF.addPage Nothing
         PDF.drawWithPage page $ do
            PDF.fillColor PDF.black
            PDF.fill $ PDF.Rectangle (0:+0) (width:+height)
            Fold.for_ (liftA2 (,) mBackgroundObj mBackgroundWidth) $
               \(backgroundObj, (w,sc)) ->
                  PDF.withNewContext $ do
                     let t = (width - w) / 2
                     PDF.applyMatrix $ PDF.translate (t:+0)
                     PDF.applyMatrix $ PDF.scale sc sc
                     PDF.drawXObject backgroundObj

            PDF.withNewContext $ do
               PDF.applyMatrix $
                  PDF.translate ((width - cupsBarWidth) / 2 :+ bottom)
               drawScene
                  (bowHeight, tube) (fadingMask, unusedCupMask, cupObjs)
                  stdFont fontHeightInt (width,height) block

            drawCaptions stdFont fontHeightInt (width,height) captions

tubeShading :: PDF.PDFFloat -> RGB -> PDF.PDFShading
tubeShading width rgb =
   PDF.AxialShading
      0 0 width 0
      (PDF.ColorFunction1 PDF.RGBSpace $
       let triple = FL.uncurry3 (,,) in
       PDF.linearStitched
         (triple $ interpolateColor 0.8 (greyRGB 0) rgb)
         [(0.15, triple rgb),
          (0.30, triple $ interpolateColor 0.8 (greyRGB 1) rgb),
          (0.50, triple rgb)
         ]
         (triple $ greyRGB 0))

drawCup ::
   (PDF.PDFFloat,
      Complex PDF.PDFFloat -> Complex PDF.PDFFloat -> PDF.Draw ()) ->
   PDF.AnyFont -> Int ->
   RGB -> Int ->
   Double -> PDF.Draw ()
drawCup (bowHeight, tube) stdFont fontHeightInt color pitch flashShift = do
   let fontHeight = fromIntegral fontHeightInt
   let bowHalf = bowHeight/2
   PDF.paintWithShading
      (tubeShading fontHeight $
         interpolateColor flashShift (greyRGB 1) color)
      (tube
         (fontHeight*0.1 :+ fontHeight*(bowHalf-0.1))
         (fontHeight*0.9 :+ fontHeight*(bowHalf+0.9)))

   PDF.fillColor $ grey flashShift
   PDF.setWidth 0.5
   PDF.strokeColor PDF.black
   do
      let label =
            noteNames ! mod pitch (snd $ Array.bounds noteNames)
      let (upper, lower) =
            case break ('='==) label of
               (xs, "") -> ("", xs)
               (xs, ys) -> (xs, ys)
      let textColumns = max (length upper) (length lower)
      let font = PDF.PDFFont stdFont (div fontHeightInt textColumns)
      let upperText = fromString upper
      let lowerText = fromString lower
      let textWidth =
            max
               (PDF.textWidth font upperText)
               (PDF.textWidth font lowerText)
      let textLeft = (fontHeight - textWidth) / 2
      when (not $ null upper) $ PDF.drawText $ do
         PDF.setFont font
         PDF.renderMode PDF.FillAndStrokeText
         PDF.textStart textLeft (fontHeight * 0.4)
         PDF.displayText upperText
      PDF.drawText $ do
         PDF.setFont font
         PDF.renderMode PDF.FillAndStrokeText
         PDF.textStart textLeft 0
         PDF.displayText lowerText

flashPos :: Double
flashPos = 1

drawScene ::
   (PDF.PDFFloat,
      Complex PDF.PDFFloat -> Complex PDF.PDFFloat -> PDF.Draw ()) ->
   (PDF.SoftMask, PDF.SoftMask,
      IntMap (PDF.PDFReference PDF.PDFXForm, (RGB, Bool))) ->
   PDF.AnyFont ->
   Int ->
   (PDF.PDFFloat, PDF.PDFFloat) ->
   [Tube] -> PDF.Draw ()
drawScene
      (bowHeight, tube) (fadingMask, unusedCupMask, cupObjs)
      stdFont fontHeightInt (_width,height) block = do

   let flashVelocity = 1.1
   let fontHeight = fromIntegral fontHeightInt
   let intMapFindIndex k m = IntMap.size $ fst $ IntMap.split k m
   let boxLeftFromPitch pitch =
         fromIntegral (intMapFindIndex pitch cupObjs) * fontHeight
   Fold.for_ block $ \(Tube from mTo pitch) -> do
      let boxLeft = boxLeftFromPitch pitch
      let boxBottom = fontHeight*from
      let boxTop = maybe height ((fontHeight*) . subtract 0.1) mTo
      PDF.withNewContext $ do
         PDF.applyMatrix $
            PDF.translate (boxLeft:+(boxBottom+flashPos*fontHeight))
         PDF.paintWithTransparency fadingMask $
            PDF.paintWithShading
               (tubeShading fontHeight $ fst $ snd $ cupObjs IntMap.! pitch)
               (tube
                  (fontHeight*0.1 :+ 0)
                  (fontHeight*0.9 :+ (boxTop-boxBottom)))

   let headDistances = minimalDistanceToTubeHeads block
   Fold.sequence_ $ flip IntMap.mapWithKey cupObjs $
      \pitch (cupObj, (color, used)) ->
         PDF.withNewContext $ do
      PDF.applyMatrix $ PDF.translate (boxLeftFromPitch pitch :+ 0)
      let flashShift =
            fromMaybe 1 $ do
               guard used
               dist <- IntMap.lookup pitch headDistances
               return $ min 1 $ flashVelocity*dist
      (if used then id else PDF.paintWithTransparency unusedCupMask) $
         if flashShift == 1
            then PDF.drawXObject cupObj
            else drawCup (bowHeight, tube) stdFont fontHeightInt
                    color pitch flashShift

drawCaptions ::
   PDF.AnyFont -> Int ->
   (PDF.PDFFloat, PDF.PDFFloat) ->
   [(PDF.PDFFloat, Double, String)] -> PDF.Draw ()
drawCaptions stdFont fontHeightInt (width,height) captions = do
   let fontHeight = fromIntegral fontHeightInt
   Fold.for_ captions $ \(k,alpha,caption) -> PDF.withNewContext $ do
      let captionLines = map fromString $ lines caption
      let font = PDF.PDFFont stdFont fontHeightInt
      PDF.fillColor PDF.white
      PDF.setWidth 0.5
      PDF.strokeColor PDF.black
      PDF.applyMatrix $ PDF.translate $ (width :+ height)/2
      PDF.applyMatrix $ PDF.scale k k
      PDF.applyMatrix $ PDF.translate $
         (0 :+ fromIntegral (length captionLines) * fontHeight)/2
      PDF.setStrokeAlpha alpha
      PDF.setFillAlpha alpha
      Fold.for_ captionLines $ \captionText -> do
         PDF.applyMatrix $ PDF.translate $ 0 :+ (-fontHeight)
         PDF.withNewContext $ do
            PDF.applyMatrix $ PDF.translate $
               -(PDF.textWidth font captionText :+ 0)/2
            PDF.drawText $ do
               PDF.setFont font
               PDF.renderMode PDF.FillAndStrokeText
               PDF.displayText captionText


substituteWhiteSpace :: String -> String
substituteWhiteSpace =
   map (\c -> case c of '\160' -> ' '; '\9252' -> '\n'; _ -> c)

-- ToDo: import from utility-ht:Data.Maybe.HT
maybePlus :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
maybePlus f mx my = liftA2 f mx my <|> mx <|> my

animate ::
   Double -> Integer -> VoiceMsg.Pitch -> Bool -> Bool ->
   FilePath -> Maybe (String, Maybe Double) -> Int -> Int -> [Int] ->
   FilePath -> FilePath -> IO ()
animate
   timeStep frameRate zeroKey fullRange usedCupsOnly
   backgroundPath forcedTitle heightPoints fontHeight trackNos
      input output = do

   background <-
      if null backgroundPath
         then return Nothing
         else
            either
               (\msg ->
                  exitFailureMsg $
                  printf "background image %s failed: %s" backgroundPath msg)
               (return . Just)
               =<< PDF.readJpegFile backgroundPath
   midi <-
      either fail return . selectTracks trackNos
         =<< MidiLoad.fromFile input
   let tickTrack = mergeTracksToAbsoluteTicks midi
   Fold.for_ (detectCorruptNotes show tickTrack) $ \warning ->
      when (not $ null warning) $ Log.warn Verbosity.normal $ warning ++ "\n"
   let origTrack = mergeTracksToAbsoluteSeconds midi
   Fold.for_ (detectCorruptNotes formatSeconds origTrack) $ \warning ->
      when (not $ null warning) $ Log.warn Verbosity.normal $ warning ++ "\n"
   let mTitleDuration = snd =<< forcedTitle
   let track =
         maybe origTrack (flip AbsEventList.delay origTrack) mTitleDuration
   let duration = AbsEventList.duration track
   let bottom = 0.5
   let height = fromIntegral heightPoints / fromIntegral fontHeight
   let noteEvents = filterNotes zeroKey track
   let tubes =
         AbsEventList.toPairList $
         AbsEventList.mapTime (/timeStep) noteEvents
   let usedCups = IntSet.fromList $ map (fst.snd) tubes
   let start = windowInitialize height tubes
   let frameRateFloat = fromInteger frameRate
   let ts = [0, recip frameRateFloat .. duration]
   let rows = map (/timeStep) ts
   let frames =
         scanl (\display times -> windowMove times display) start $
         map (\row -> (row-bottom-flashPos, row+height-flashPos)) rows
   let lyrics = filterLyrics track
   let firstEventTime =
         fromMaybe defaultTitleDuration $
            mTitleDuration
            <|>
            maybePlus min
               (listToMaybe $ AbsEventList.getTimes noteEvents)
               (listToMaybe $ map fst lyrics)
   writePDF output
         background heightPoints PDF.Helvetica_Bold fontHeight
         (selectDisplayedCups fullRange usedCupsOnly usedCups) $
      zipWith3
         (\row captions ->
            flip (,) captions . map (shiftTube (-row)) . windowLayout . fst)
         rows
         (zipWith (++)
            (layoutTitle frameRateFloat firstEventTime $
             zipWith (\t -> map (mapFst (subtract t))) ts $
             chop (drop 1 ts) $
             maybe id (\(title,_dur) -> ((0, substituteWhiteSpace title):))
               forcedTitle $
             filterTitle track)
            (layoutCounters frameRateFloat $
             zipWith (\t -> map (mapFst (subtract t))) ts $
             chop (drop 1 ts) lyrics))
         frames


info :: OP.Parser a -> OP.ParserInfo a
info p =
   OP.info
      (OP.helper <*> p)
      (OP.fullDesc <>
       OP.progDesc "Generate boomwhacker animation from MIDI file.")

parser :: OP.Parser (IO ())
parser =
   pure animate
   <*>
      OP.option OP.auto
         (OP.long "timestep" <>
          OP.metavar "SECONDS" <>
          OP.value 0.2 <>
          OP.help "time step between virtual character rows")
   <*>
      OP.option OP.auto
         (OP.long "rate" <>
          OP.metavar "FPS" <>
          OP.value 25 <>
          OP.help "frame rate")
   <*>
      (VoiceMsg.toPitch <$>
         OP.option OP.auto
            (OP.long "zerokey" <>
             OP.metavar "INT" <>
             OP.value 60 <>
             OP.help "MIDI key for the left-most tube"))
   <*>
      (OP.switch $
         OP.long "full-range" <>
         OP.help "Draw all bars and cups with colors")
   <*>
      (OP.switch $
         OP.long "used-cups-only" <>
         OP.help "Display only cups that are actually played somewhen")
   <*>
      (OP.strOption $
         OP.long "background" <>
         OP.metavar "JPEG" <>
         OP.value "" <>
         OP.help "Background image")
   <*>
      (OP.optional $
       liftA2 (,)
         (OP.strOption $
            OP.long "title" <>
            OP.metavar "TEXT" <>
            OP.help "Override title in MIDI file")
         (OP.optional $
          OP.option (OP.eitherReader $ parseNumber "duration" (0<) "positive") $
            OP.long "title-duration" <>
            OP.metavar "SECONDS" <>
            OP.help "Duration of title appearance including fading"))
   <*>
      (OP.option (OP.eitherReader $ parseNumber "height" (0<) "positive") $
         OP.long "height" <>
         OP.metavar "POINTS" <>
         OP.value 720 <>
         OP.help "Height of the paper in typographical points")
   <*>
      (OP.option (OP.eitherReader $ parseNumber "font height" (0<) "positive") $
         OP.long "font-height" <>
         OP.metavar "POINTS" <>
         OP.value 80 <>
         OP.help "Font height")
   <*>
      OP.many
         (OP.option
               (OP.eitherReader $ parseNumber "track number" (0<) "positive") $
            OP.long "track" <>
            OP.metavar "ONEBASED" <>
            OP.help "Select input track")
   <*>
      OP.strArgument
         (OP.metavar "INPUT" <>
          OP.help "Input MIDI file")
   <*>
      OP.strArgument
         (OP.metavar "OUTPUT" <>
          OP.help "Output PDF file")

main :: IO ()
main = join $ OP.execParser $ info parser
