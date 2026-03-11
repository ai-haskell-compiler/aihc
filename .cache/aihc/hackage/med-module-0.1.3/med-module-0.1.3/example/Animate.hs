module Main where

import qualified Animate.Option as Option
import qualified Options.Applicative as OP
import System.FilePath(dropExtension, (<.>))

import qualified Sound.MED.Generic as MED
import qualified Sound.MED.Generic.Block as MEDBlock
import qualified Sound.MED.Generic.Tempo as MEDTempo
import qualified Sound.MED.Generic.PlaySeq as MEDPlaySeq
import Sound.MED.Generic.Block(Note,Inst,Cmd,Val)
import Sound.MED.Basic.Human(human)

import qualified Graphics.PDF as PDF

import qualified Data.Array as Array
import qualified Data.Foldable as Fold
import qualified Data.NonEmpty as NonEmpty
import qualified Data.List.Reverse.StrictSpine as ListRev
import qualified Data.List.Match as Match
import qualified Data.List.HT as ListHT
import qualified Data.List as List
import Data.Array (Array, listArray, (!))
import Data.Maybe.HT (toMaybe)
import Data.Maybe (fromMaybe, isJust)
import Data.Tuple.HT (mapSnd, fst3, thd3)
import Text.Printf (printf)
import Data.String (fromString)
import Data.Monoid (Any(Any,getAny))
import Data.Complex (Complex((:+)))

import qualified Control.Monad.Trans.Writer as MW
import qualified Control.Monad.Trans.Class as MT
import Control.Monad(when, void, msum)
import Control.Applicative((<$>), (<|>))

main :: IO ()
main = do
  (options, mmds) <- OP.execParser $ Option.info Option.parser
  mapM_ (animate options) mmds

animate :: Option.Option -> FilePath -> IO ()
animate opt file = do
    med <- MED.load file
    let initTempo = MED.tempo med
    void $ printf "tempo primary: %d, secondary %d\n"
      (MEDTempo.primary initTempo) (MEDTempo.secondary initTempo)
    sequ <-
      case MED.playseqs med of
        sequ:_ -> return sequ
        [] -> ioError $ userError "no play sequence available"
    putStrLn $ human sequ
    let blocks = map MEDBlock.seqdata $ MED.blocks med
    let loop = Option.repeatUntil opt
    let allLines = flattenLines (isJust loop) blocks $ MEDPlaySeq.indices sequ
    let durations =
          maybe id takeUntilTime loop $ map MEDTempo.toTime $
          NonEmpty.tail $ NonEmpty.scanl (foldl MEDTempo.update) initTempo $
          map (concatMap thd3 . snd . snd . snd) allLines
    let pre = Option.contextSize opt
    let post = Option.contextSize opt
    let numLines = length durations + post
    let usedTracks =
          foldl (zipWithPad (||)) [] $
          map (notePattern . snd . snd) $ take numLines allLines
    void $ putStrLn $ "used tracks: " ++
      unwords (map (\b -> if b then "X" else "_") usedTracks)
    let stripLine = map snd . filter fst . zip usedTracks
    let strippedLines =
          if Option.stripNotelessTracks opt
            then map (mapSnd (mapSnd (mapSnd stripLine))) allLines
            else allLines
    let countCommands =
          if Option.stripZeroCommands opt
            then numNonZeroCommands else numCommands
    let maxCmdsPerTrack =
          map (max 1) $ foldl (zipWithPad max) [] $
          map (countCommands . snd . snd) $ take numLines strippedLines
    void $ putStrLn $
      "maximum commands per track: " ++
      List.intercalate ", " (map show maxCmdsPerTrack)
    let formattedLines = map (highlightLine maxCmdsPerTrack) strippedLines
    let textWidth = maximum $ map (length . snd) $ take numLines formattedLines
    writePDF (dropExtension file <.> "pdf")
        (Option.font opt) (Option.height opt) (pre,post) textWidth $
      zip (brightnesses (Option.fadeOut opt) durations) $
      map (take (pre+1+post)) $ ListHT.tails $
      replicate pre ((False,False),"") ++ formattedLines
    when (not $ null $ Option.framePattern opt) $
      writeFile (dropExtension file <.> "cue") $ concat $
        zipWith (printf "file '%s'\nduration %.5f\n")
          (map (formatFramePath $ Option.framePattern opt) [1 ..])
          durations

brightnesses :: Double -> [Double] -> [Double]
brightnesses fadeOutDur durations =
  let totalTime = sum durations
      fadeOutStart = totalTime - fadeOutDur
  in  map
        (\time ->
          if time < fadeOutStart || fadeOutDur <= 0
            then 1
            else (totalTime-time) / fadeOutDur) $
      NonEmpty.init $ NonEmpty.scanl (+) 0 durations

takeUntilTime :: Double -> [Double] -> [Double]
takeUntilTime =
  let go _ [] = []
      go end (dur:durs) = if dur>=end then [end] else dur : go (end-dur) durs
  in  go

{- |
Traverse playing sequence but ignore control commands.
If the playing sequence is finite, the flattened list of lines will be, too.
-}
flattenLinesSimple :: Bool -> [[line]] -> [Int] -> [(Bool,(Int,line))]
flattenLinesSimple loop blocks =
  let blockArr = arrayFromList blocks
  in  (if loop then cycle else id) .
      concatMap (addTrailingTrue . zip [0..] . (blockArr !))

flattenLinesArr :: Bool -> [[line]] -> [Int] -> [(Bool,(Int,line))]
flattenLinesArr loop blocks =
  let blockArr = arrayFromList $ map arrayFromList blocks
  in  (if loop then cycle else id) .
      concatMap (addTrailingTrue . Array.assocs . (blockArr !))

addTrailingTrue :: [a] -> [(Bool,a)]
addTrailingTrue xs = zip (Match.replicate (drop 1 xs) False ++ [True]) xs

flattenLines ::
  Bool -> [[MEDBlock.Line]] -> [Int] -> [(Bool,(Int,MEDBlock.Line))]
flattenLines loop blocks sequ =
  let blockArr = arrayFromList $ map arrayFromList blocks
      sequArr = arrayFromList sequ
      sequLength = length sequ
      accessLine (SongPos sequIdx line) =
        let block = blockArr!(sequArr!sequIdx)
        in  ((line, block!line), Array.rangeSize $ Array.bounds block)
  in  List.unfoldr
        (fmap $ \pos ->
          let (line,blockLength) = accessLine pos
              (endOfBlock, newSongPos) =
                nextSongPos loop sequLength blockLength pos $
                concatMap thd3 $ snd $ snd line
          in  ((endOfBlock, line), newSongPos))
        (Just $ SongPos 0 0)

data SongPos = SongPos {posSequ, posLine :: Int}

nextSongPos ::
  Bool -> Int -> Int -> SongPos -> [(Cmd, Val)] -> (Bool, Maybe SongPos)
nextSongPos loop sequLength blockLength pos cmds =
  let newSongPos =
        MW.runWriterT $
          adjustSongPos loop sequLength blockLength =<<
          cumulatedSongPosUpdate loop pos cmds
  in  (maybe False (getAny.snd) newSongPos, fst<$>newSongPos)

data SongPosCmds =
  SongPosCmds {
    posCmd0F00, posCmd0FFE :: Bool,
    posCmd1Dxx :: Maybe Int,
    posCmd0Bxx :: Maybe Int
  }

{-
If there are multiple flow control commands in a line
then the behavior of MED is complex and undocumented.
Here is, what I found out:
@0FFE@ overrides all other three flow control commands.
@1Dxx@ overrides @0F00@,
@0Bxx@ and @1Dxx@ are combined, as well as @0Bxx@ and @0F00@.
If there are multiple @0Bxx@ commands in a line
then the right-most and then the deep-most wins
(deep with respect to command pages).
The same applies to multiple @1Dxx@ commands.
-}
cumulatedSongPosUpdate ::
  Bool -> SongPos -> [(Cmd, Val)] -> MW.WriterT Any Maybe SongPos
cumulatedSongPosUpdate loop pos@(SongPos sequIdx _line) cmds0 =
  let cum = foldl update start cmds0
      start =
        SongPosCmds {
          posCmd0F00 = False, posCmd0FFE = False,
          posCmd1Dxx = Nothing, posCmd0Bxx = Nothing
        }
      update cmds cmdVal =
        case cmdVal of
          (0x0F, 0x00) -> cmds{posCmd0F00 = True}
          (0x0F, 0xFE) -> cmds{posCmd0FFE = True}
          (0x1D, line) -> cmds{posCmd1Dxx = Just line}
          (0x0B, idx ) -> cmds{posCmd0Bxx = Just idx}
          _ -> cmds
      continue = flip (,) (Any False); break_ = flip (,) (Any True)
      breakBlock =
        case (posCmd0Bxx cum, posCmd1Dxx cum <|> toMaybe (posCmd0F00 cum) 0) of
          (Nothing, Nothing) -> Just $ continue $ incSongPos pos
          (Just idx, Nothing) -> toMaybe loop $ break_ $ SongPos idx 0
          (Nothing, Just line) -> Just $ break_ $ SongPos (sequIdx+1) line
          (Just idx, Just line) -> toMaybe loop $ break_ $ SongPos (idx+1) line
  in  if posCmd0FFE cum
        then MT.lift Nothing
        else MW.WriterT breakBlock


{- |
Simpler alternative to 'cumulatedSongPosUpdate'.
-}
leftMostSongPosUpdate ::
  Bool -> SongPos -> [(Cmd, Val)] -> MW.WriterT Any Maybe SongPos
leftMostSongPosUpdate loop pos =
  fromMaybe (return $ incSongPos pos) . msum . map (updateSongPos loop pos)

adjustSongPos :: Bool -> Int -> Int -> SongPos -> MW.WriterT Any Maybe SongPos
adjustSongPos loop sequLength blockLength pos0 =
  let endOfBlock = posLine pos0 >= blockLength
      pos1 = if endOfBlock then SongPos (posSequ pos0 + 1) 0 else pos0
  in  MW.WriterT $
        toMaybe (posSequ pos1 < sequLength) (pos1, Any endOfBlock)
        <|>
        toMaybe loop (SongPos 0 0, Any True)

incSongPos :: SongPos -> SongPos
incSongPos (SongPos sequIdx line) = SongPos sequIdx (line+1)

updateSongPos ::
  Bool -> SongPos -> (Cmd, Val) -> Maybe (MW.WriterT Any Maybe SongPos)
updateSongPos loop (SongPos sequIdx _line) cmdVal =
  fmap (MW.WriterT . fmap (flip (,) (Any True))) $
  case cmdVal of
    (0x0F, 0xFE) -> Just Nothing
    (0x0F, 0x00) -> Just $ Just $ SongPos (sequIdx+1) 0
    (0x1D, line) -> Just $ Just $ SongPos (sequIdx+1) line
    (0x0B, idx ) -> Just $ toMaybe loop $ SongPos idx 0
    _ -> Nothing


arrayFromList :: [a] -> Array Int a
arrayFromList xs = listArray (0, length xs - 1) xs

formatFramePath :: String -> Int -> FilePath
formatFramePath = printf


numCommands :: MEDBlock.Line -> [Int]
numCommands = map (length . thd3) . snd

numNonZeroCommands :: MEDBlock.Line -> [Int]
numNonZeroCommands =
   map (length . ListRev.dropWhile ((0,0) ==) . thd3) . snd

notePattern :: MEDBlock.Line -> [Bool]
notePattern = map ((/=0) . fst3) . snd

zipWithPad :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWithPad f =
   let go xs []         = xs
       go [] ys         = ys
       go (x:xs) (y:ys) = f x y : go xs ys
   in  go


highlightLine :: [Int] -> (Bool, (Int, MEDBlock.Line)) -> ((Bool,Bool),String)
highlightLine maxCmdsPerTrack (endOfBlock, (i, (highlight, ds))) =
  ((Fold.or highlight, endOfBlock), humanLine maxCmdsPerTrack i ds)

humanLine :: [Int] -> Int -> [(Note, Inst, [(Cmd, Val)])] -> String
humanLine maxCmdsPerTrack i ds =
  let punwords = concatMap (' ':)
      hCV = maybe "    " (uncurry $ printf "%02X%02X")
      hTrack maxCmds (n, j, cvs) =
         printf "%s %02X%s"
            (MEDBlock.notes!!n) j
            (punwords $ map hCV $
             ListHT.padRight Nothing maxCmds $ map Just cvs)
  in  printf "%04X:%s" i $ punwords $ zipWith hTrack maxCmdsPerTrack ds


writePDF ::
  FilePath -> Option.Font -> Int -> (Int,Int) -> Int ->
  [(Double, [((Bool,Bool),String)])] -> IO ()
writePDF path font heightInt (pre,post) textWidth blocks = do
  let height = fromIntegral heightInt
      width = fromIntegral textWidth * fontWidth + 2*left
      top = height - fontHeight - (height - fontHeight*(preR+1+postR))/2
      left = 80
      barWidth n = fromIntegral n * fontWidth + 2*barBorderH
      fontWidth = fontHeight * Option.fontRelativeWidth font
      fontHeight = fromIntegral $ Option.fontHeight font
      barBorderH = 5
      barBorderV = 3
      postR = fromIntegral post
      preR = fromIntegral pre
      rect = PDF.PDFRect 0 0 width height
      grey brightness = PDF.Rgb brightness brightness brightness
      rectangleBySize leftBottom extent =
        PDF.Rectangle leftBottom (leftBottom+extent)
      horizRule xl xr y = PDF.Line xl y xr y
  normalFont <-
    either (fail . show) return =<< PDF.mkStdFont (Option.fontNormal font)
  highlightFont <-
    either (fail . show) return =<< PDF.mkStdFont (Option.fontHighlight font)
  PDF.runPdf path PDF.standardDocInfo rect $
    Fold.for_ blocks $ \(brightness,block) -> do
      page <- PDF.addPage Nothing
      PDF.drawWithPage page $ do
        PDF.fillColor (grey brightness)
        PDF.fill $ PDF.Rectangle (0:+0) (width:+height)
        PDF.fillColor (grey (0.8*brightness))
        PDF.fill $
          rectangleBySize
            ((left-barBorderH) :+ (top-preR*fontHeight-barBorderV))
            ((barWidth $ length $ snd $ block!!pre) :+ fontHeight)
        Fold.for_ (zip (iterate (subtract fontHeight) top) block) $
          \(y, ((highlight,endOfBlock),line)) -> do
            PDF.fillColor PDF.black
            PDF.drawText $ do
              PDF.setFont $
                PDF.PDFFont
                  (if highlight then highlightFont else normalFont)
                  (Option.fontHeight font)
              PDF.textStart left y
              PDF.renderMode PDF.FillText
              PDF.displayText $ fromString line
            PDF.strokeColor PDF.black
            when endOfBlock $
              PDF.stroke $
                horizRule
                  (left - barBorderH)
                  (left - barBorderH + barWidth (length line))
                  (y - barBorderV)
