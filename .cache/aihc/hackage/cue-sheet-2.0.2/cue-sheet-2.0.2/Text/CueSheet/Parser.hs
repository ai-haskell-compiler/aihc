{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Text.CueSheet.Parser
-- Copyright   :  © 2016–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- The modules contains a CUE sheet parser. You probably want to import
-- "Text.CueSheet" instead.
module Text.CueSheet.Parser
  ( parseCueSheet,
    CueParserFailure (..),
    Eec (..),
  )
where

import Control.Monad (void)
import Control.Monad.State.Strict (StateT, execStateT, gets, modify)
import Data.ByteString (ByteString)
import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as E
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics
import Numeric.Natural
import Text.CueSheet.Types
import Text.Megaparsec
import Text.Megaparsec.Byte
import qualified Text.Megaparsec.Byte.Lexer as L

----------------------------------------------------------------------------
-- Types

-- | Extended error component with support for storing number of track
-- declaration in which a parsing error has occurred.
data Eec = Eec (Maybe Natural) CueParserFailure
  deriving (Show, Eq, Ord, Data, Typeable, Generic)

instance ShowErrorComponent Eec where
  showErrorComponent (Eec mtrack failure') =
    showErrorComponent failure'
      ++ "\n"
      ++ maybe "" (\n -> "in declaration of the track " ++ show n) mtrack

-- | The enumeration of all failures that may happen during running of
-- 'parseCueSheet'.
data CueParserFailure
  = -- | A wrapper for a trivial error
    CueParserTrivialError (Maybe (ErrorItem Word8)) (Set (ErrorItem Word8))
  | -- | We ran into an invalid media catalog number
    CueParserInvalidCatalog Text
  | -- | We ran into an invalid text literal
    CueParserInvalidCueText Text
  | -- | We spotted a track out of order
    CueParserTrackOutOfOrder
  | -- | We ran into an invalid ISRC
    CueParserInvalidTrackIsrc Text
  | -- | We ran into an invalid number of seconds
    CueParserInvalidSeconds Natural
  | -- | We ran into an invalid number of frames
    CueParserInvalidFrames Natural
  | -- | We spotted a track index out of order
    CueParserTrackIndexOutOfOrder
  deriving (Show, Eq, Ord, Data, Typeable, Generic)

instance ShowErrorComponent CueParserFailure where
  showErrorComponent = \case
    CueParserTrivialError us es ->
      init $
        parseErrorTextPretty
          (TrivialError undefined us es :: ParseError ByteString Eec)
    CueParserInvalidCatalog txt ->
      "the value \"" ++ T.unpack txt ++ "\" is not a valid Media Catalog Number"
    CueParserInvalidCueText txt ->
      "the value \"" ++ T.unpack txt ++ "\" is not a valid CUE text literal"
    CueParserTrackOutOfOrder ->
      "this track appears out of order"
    CueParserInvalidTrackIsrc txt ->
      "\"" ++ T.unpack txt ++ "\" is not a valid ISRC"
    CueParserInvalidSeconds n ->
      "\"" ++ show n ++ "\" is not a valid number of seconds"
    CueParserInvalidFrames n ->
      "\"" ++ show n ++ "\" is not a valid number of frames"
    CueParserTrackIndexOutOfOrder ->
      "this index appears out of order"

-- | Type of parser we use here, it's not public.
type Parser a = StateT Context (Parsec Eec ByteString) a

-- | Context of parsing. This is passed around in 'StateT'. We need all of
-- this to signal parse errors on duplicate declarations of things that
-- should only be declared once according to description of the format, to
-- validate track numbers, etc.
data Context = Context
  { -- | Current state of CUE sheet we parse. When a part\/parameter of CUE
    -- sheet is parsed, this thing is updated.
    contextCueSheet :: !CueSheet,
    -- | Temporary storage for parsed files (we can't store it in the
    -- 'CueSheet' because it does not allow empty list of files).
    contextFiles :: ![CueFile],
    -- | Similar to 'contextFiles', collection of tracks but for current
    -- file.
    contextTracks :: ![CueTrack],
    -- | Number of tracks we have parsed so far, to avoid traversing lists
    -- again and again.
    contextTrackCount :: !Natural,
    -- | Temporary storage for collection indices for current track.
    contextIndices :: ![CueTime],
    -- | Similarly for indices.
    contextIndexCount :: !Natural
  }

-- | Parse a CUE sheet from a lazy 'BL.ByteString'.
parseCueSheet ::
  -- | File name to include in error messages
  String ->
  -- | CUE sheet to parse as a lazy 'BL.ByteString'
  ByteString ->
  -- | 'ParseError' or result
  Either (ParseErrorBundle ByteString Eec) CueSheet
parseCueSheet = parse (contextCueSheet <$> execStateT pCueSheet initContext)
  where
    initContext =
      Context
        { contextCueSheet =
            CueSheet
              { cueCatalog = Nothing,
                cueCdTextFile = Nothing,
                cuePerformer = Nothing,
                cueTitle = Nothing,
                cueSongwriter = Nothing,
                cueFirstTrackNumber = 0,
                cueFiles = dummyFile :| []
              },
          contextFiles = [],
          contextTracks = [],
          contextTrackCount = 0,
          contextIndices = [],
          contextIndexCount = 0
        }

-- | Parse a 'CueSheet'. The result is not returned, but written in the
-- 'Context'.
pCueSheet :: Parser ()
pCueSheet = do
  scn
  void (many pHeaderItem)
  void (some pFile)
  -- NOTE Lens would help here, but let's keep this vanilla.
  modify $ \x ->
    x
      { contextCueSheet =
          (contextCueSheet x)
            { cueFiles = (NE.fromList . reverse . contextFiles) x
            }
      }
  eof

pHeaderItem :: Parser ()
pHeaderItem =
  choice
    [ pCatalog,
      pCdTextFile,
      pPerformer,
      pTitle,
      pSongwriter,
      pRem
    ]

pCatalog :: Parser ()
pCatalog = do
  already <- gets (isJust . cueCatalog . contextCueSheet)
  let f x' =
        let x = T.decodeUtf8 x'
         in case mkMcn x of
              Nothing -> Left (CueParserInvalidCatalog x)
              Just mcn -> Right mcn
  mcn <- labelledLit already f "CATALOG"
  modify $ \x ->
    x
      { contextCueSheet =
          (contextCueSheet x) {cueCatalog = Just mcn}
      }

pCdTextFile :: Parser ()
pCdTextFile = do
  already <- gets (isJust . cueCdTextFile . contextCueSheet)
  cdTextFile <- T.decodeUtf8 <$> labelledLit already Right "CDTEXTFILE"
  modify $ \x ->
    x
      { contextCueSheet =
          (contextCueSheet x)
            { cueCdTextFile = Just (T.unpack cdTextFile)
            }
      }

pPerformer :: Parser ()
pPerformer = do
  already <- gets (isJust . cuePerformer . contextCueSheet)
  let f x' =
        let x = T.decodeUtf8 x'
         in case mkCueText x of
              Nothing -> Left (CueParserInvalidCueText x)
              Just txt -> Right txt
  performer <- labelledLit already f "PERFORMER"
  modify $ \x ->
    x
      { contextCueSheet =
          (contextCueSheet x) {cuePerformer = Just performer}
      }

pTitle :: Parser ()
pTitle = do
  already <- gets (isJust . cueTitle . contextCueSheet)
  let f x' =
        let x = T.decodeUtf8 x'
         in case mkCueText x of
              Nothing -> Left (CueParserInvalidCueText x)
              Just txt -> Right txt
  title <- labelledLit already f "TITLE"
  modify $ \x ->
    x
      { contextCueSheet =
          (contextCueSheet x) {cueTitle = Just title}
      }

pSongwriter :: Parser ()
pSongwriter = do
  already <- gets (isJust . cueSongwriter . contextCueSheet)
  let f x' =
        let x = T.decodeUtf8 x'
         in case mkCueText x of
              Nothing -> Left (CueParserInvalidCueText x)
              Just txt -> Right txt
  songwriter <- labelledLit already f "SONGWRITER"
  modify $ \x ->
    x
      { contextCueSheet =
          (contextCueSheet x) {cueSongwriter = Just songwriter}
      }

pRem :: Parser ()
pRem = do
  void (symbol "REM")
  takeWhileP (Just "character") (/= 10) *> char 10 *> scn

pFile :: Parser ()
pFile = do
  void (symbol "FILE")
  filename <- T.decodeUtf8 <$> lexeme stringLit
  let pFiletype =
        choice
          [ Binary <$ symbol "BINARY",
            Motorola <$ symbol "MOTOROLA",
            Aiff <$ symbol "AIFF",
            Wave <$ symbol "WAVE",
            MP3 <$ symbol "MP3"
          ]
  filetype <- pFiletype <* eol <* scn
  void (some (pTrack <|> pRem))
  tracks <- gets contextTracks
  let newFile =
        CueFile
          { cueFileName = T.unpack filename,
            cueFileType = filetype,
            cueFileTracks = NE.fromList (reverse tracks)
          }
  modify $ \x ->
    x
      { contextFiles = newFile : contextFiles x,
        contextTracks = []
      }

pTrack :: Parser ()
pTrack = do
  void (symbol "TRACK")
  trackOffset <- gets (cueFirstTrackNumber . contextCueSheet)
  trackCount <- gets contextTrackCount
  let firstTrack = trackCount == 0
      f x =
        if firstTrack || x == trackOffset + trackCount
          then Right x
          else Left CueParserTrackOutOfOrder
  n <- withCheck f (lexeme L.decimal)
  let pTrackType =
        choice
          [ CueTrackAudio <$ symbol "AUDIO",
            CueTrackCdg <$ symbol "CDG",
            CueTrackMode1_2048 <$ symbol "MODE1/2048",
            CueTrackMode1_2352 <$ symbol "MODE1/2352",
            CueTrackMode2_2336 <$ symbol "MODE2/2336",
            CueTrackMode2_2352 <$ symbol "MODE2/2352",
            CueTrackCdi2336 <$ symbol "CDI/2336",
            CueTrackCdi2352 <$ symbol "CDI/2352"
          ]
  trackType <- pTrackType <* eol <* scn
  let newTrack = dummyTrack {cueTrackType = trackType}
  modify $ \x ->
    x
      { contextTracks = newTrack : contextTracks x,
        contextTrackCount = trackCount + 1,
        contextCueSheet =
          let old = contextCueSheet x
           in if firstTrack
                then old {cueFirstTrackNumber = n}
                else old
      }
  inTrack n $ do
    void (many pTrackHeaderItem)
    index0 <- (optional . try . pIndex) 0
    modify $ \x ->
      x
        { contextTracks = changingFirstOf (contextTracks x) $ \t ->
            t {cueTrackPregapIndex = index0}
        }
    modify $ \x ->
      x
        { contextIndices = [],
          contextIndexCount = 0
        }
    let grabIndex = do
          next <- (+ 1) <$> gets contextIndexCount
          nextIndex <- pIndex next
          modify $ \x ->
            x
              { contextIndices = nextIndex : contextIndices x,
                contextIndexCount = next
              }
    void (some (grabIndex <|> pRem))
    modify $ \x ->
      x
        { contextTracks = changingFirstOf (contextTracks x) $ \t ->
            t {cueTrackIndices = (NE.fromList . reverse . contextIndices) x}
        }
    void (optional pPostgap)

pTrackHeaderItem :: Parser ()
pTrackHeaderItem =
  choice
    [ pFlags,
      pIsrc,
      pTrackPerformer,
      pTrackTitle,
      pTrackSongwriter,
      pRem,
      pPregap
    ]

pFlags :: Parser ()
pFlags = do
  already <- gets (seenFlags . head . contextTracks)
  failAtIf already "FLAGS"
  void (some pFlag) <* eol <* scn

-- | A helper data type for track flags.
data CueTrackFlag = DCP | FourCH | PRE | SCMS

pFlag :: Parser ()
pFlag = do
  flag <-
    choice
      [ DCP <$ symbol "DCP",
        FourCH <$ symbol "4CH",
        PRE <$ symbol "PRE",
        SCMS <$ symbol "SCMS"
      ]
  modify $ \x ->
    x
      { contextTracks = changingFirstOf (contextTracks x) $ \t ->
          case flag of
            DCP -> t {cueTrackDigitalCopyPermitted = True}
            FourCH -> t {cueTrackFourChannelAudio = True}
            PRE -> t {cueTrackPreemphasisEnabled = True}
            SCMS -> t {cueTrackSerialCopyManagement = True}
      }

pIsrc :: Parser ()
pIsrc = do
  already <- gets (isJust . cueTrackIsrc . head . contextTracks)
  let f x' =
        let x = T.decodeUtf8 x'
         in case mkIsrc x of
              Nothing -> Left (CueParserInvalidTrackIsrc x)
              Just isrc -> Right isrc
  isrc <- labelledLit already f "ISRC"
  modify $ \x ->
    x
      { contextTracks = changingFirstOf (contextTracks x) $ \t ->
          t {cueTrackIsrc = Just isrc}
      }

pTrackPerformer :: Parser ()
pTrackPerformer = do
  already <- gets (isJust . cueTrackPerformer . head . contextTracks)
  let f x' =
        let x = T.decodeUtf8 x'
         in case mkCueText x of
              Nothing -> Left (CueParserInvalidCueText x)
              Just txt -> Right txt
  performer <- labelledLit already f "PERFORMER"
  modify $ \x ->
    x
      { contextTracks = changingFirstOf (contextTracks x) $ \t ->
          t {cueTrackPerformer = Just performer}
      }

pTrackTitle :: Parser ()
pTrackTitle = do
  already <- gets (isJust . cueTrackTitle . head . contextTracks)
  let f x' =
        let x = T.decodeUtf8 x'
         in case mkCueText x of
              Nothing -> Left (CueParserInvalidCueText x)
              Just txt -> Right txt
  title <- labelledLit already f "TITLE"
  modify $ \x ->
    x
      { contextTracks = changingFirstOf (contextTracks x) $ \t ->
          t {cueTrackTitle = Just title}
      }

pTrackSongwriter :: Parser ()
pTrackSongwriter = do
  already <- gets (isJust . cueTrackSongwriter . head . contextTracks)
  let f x' =
        let x = T.decodeUtf8 x'
         in case mkCueText x of
              Nothing -> Left (CueParserInvalidCueText x)
              Just txt -> Right txt
  songwriter <- labelledLit already f "SONGWRITER"
  modify $ \x ->
    x
      { contextTracks = changingFirstOf (contextTracks x) $ \t ->
          t {cueTrackSongwriter = Just songwriter}
      }

pPregap :: Parser ()
pPregap = do
  already <- gets (isJust . cueTrackPregap . head . contextTracks)
  failAtIf already "PREGAP"
  time <- lexeme cueTime <* eol <* scn
  modify $ \x ->
    x
      { contextTracks = changingFirstOf (contextTracks x) $ \t ->
          t {cueTrackPregap = Just time}
      }

pPostgap :: Parser ()
pPostgap = do
  already <- gets (isJust . cueTrackPostgap . head . contextTracks)
  failAtIf already "POSTGAP"
  time <- lexeme cueTime <* eol <* scn
  modify $ \x ->
    x
      { contextTracks = changingFirstOf (contextTracks x) $ \t ->
          t {cueTrackPostgap = Just time}
      }

pIndex :: Natural -> Parser CueTime
pIndex n = do
  void (symbol "INDEX")
  let f x =
        if x == n
          then Right ()
          else Left CueParserTrackIndexOutOfOrder
  withCheck f (lexeme naturalLit)
  lexeme cueTime <* eol <* scn

cueTime :: Parser CueTime
cueTime = do
  minutes <- naturalLit
  void (char 58)
  let checkSeconds n =
        if n < 60
          then Right n
          else Left (CueParserInvalidSeconds n)
      checkFrames n =
        if n < 75
          then Right n
          else Left (CueParserInvalidFrames n)
  seconds <- withCheck checkSeconds naturalLit
  void (char 58)
  frames <- withCheck checkFrames naturalLit
  case fromMmSsFf minutes seconds frames of
    Nothing -> empty -- NOTE must be always valid, we checked already
    Just x -> return x

----------------------------------------------------------------------------
-- Helpers

-- | Parse something and then check if it's OK. If it's not OK, the error
-- will be reported with position at the start of the offending lexeme,
-- otherwise the lexeme is parsed as usual.
withCheck :: (a -> Either CueParserFailure b) -> Parser a -> Parser b
withCheck check p = do
  o <- getOffset
  r <- p
  case check r of
    Left custom -> do
      setOffset o
      (fancyFailure . E.singleton . ErrorCustom) (Eec Nothing custom)
    Right x -> return x

-- | If the first argument is 'True' and we can parse the given command,
-- fail pointing at the beginning of the command and report it as something
-- unexpected.
failAtIf :: Bool -> ByteString -> Parser ()
failAtIf shouldFail command = do
  let p = void (symbol command)
  lookAhead p
  if shouldFail
    then empty
    else p

-- | Indicate that the inner parser belongs to declaration of a track with
-- the given index. The index of the track will be added to 'ParseError's to
-- help the user find where the error happened.
inTrack :: Natural -> Parser a -> Parser a
inTrack n = region f
  where
    f (TrivialError pos us es) =
      FancyError pos . E.singleton $
        ErrorCustom (Eec (Just n) (CueParserTrivialError us es))
    f (FancyError pos xs) = FancyError pos (E.map g xs)
    g (ErrorCustom (Eec mn x)) = ErrorCustom (Eec (mn <|> Just n) x)
    g e = e

-- | A labelled literal (a helper for common case).
labelledLit ::
  -- | Should we instantly fail when command is parsed?
  Bool ->
  -- | How to judge the result
  (ByteString -> Either CueParserFailure a) ->
  -- | Name of the command to grab
  ByteString ->
  Parser a
labelledLit shouldFail check command = do
  failAtIf shouldFail command
  withCheck check (lexeme stringLit) <* eol <* scn

-- | String literal with support for quotation.
stringLit :: Parser ByteString
stringLit =
  (quoted <?> "quoted string literal")
    <|> (unquoted <?> "unquoted string literal")
  where
    quoted = char 34 *> takeWhileP Nothing f <* char 34
    unquoted = takeWhileP Nothing g
    f x = x /= 10 && x /= 34
    g x = x /= 10 && x /= 9 && x /= 13 && x /= 32

-- | Parse a 'Natural'.
naturalLit :: Parser Natural
naturalLit = L.decimal

-- | Case-insensitive symbol parser.
symbol :: ByteString -> Parser ByteString
symbol s = string' s <* notFollowedBy alphaNumChar <* sc

-- | A wrapper for lexemes.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Space consumer (eats newlines).
scn :: Parser ()
scn = L.space space1 empty empty

-- | Space consumer (does not eat newlines).
sc :: Parser ()
sc = L.space (void $ takeWhile1P Nothing f) empty empty
  where
    f x = x == 32 || x == 9

-- | Determine by 'CueTrack' if we have already parsed FLAGS command.
seenFlags :: CueTrack -> Bool
seenFlags CueTrack {..} =
  or
    [ cueTrackDigitalCopyPermitted,
      cueTrackFourChannelAudio,
      cueTrackPreemphasisEnabled,
      cueTrackSerialCopyManagement
    ]

-- | Apply given function to the first element of the list.
changingFirstOf :: [a] -> (a -> a) -> [a]
changingFirstOf [] _ = []
changingFirstOf (x : xs) f = f x : xs

----------------------------------------------------------------------------
-- Dummies

-- | A dummy file. It's only here because 'CueSheet' can't have an empty
-- list of files and it cannot be a bottom either.
dummyFile :: CueFile
dummyFile =
  CueFile
    { cueFileName = "",
      cueFileType = Wave,
      cueFileTracks = dummyTrack :| []
    }

-- | A dummy track, see 'dummyFile'.
dummyTrack :: CueTrack
dummyTrack =
  CueTrack
    { cueTrackDigitalCopyPermitted = False,
      cueTrackFourChannelAudio = False,
      cueTrackPreemphasisEnabled = False,
      cueTrackSerialCopyManagement = False,
      cueTrackType = CueTrackAudio,
      cueTrackIsrc = Nothing,
      cueTrackTitle = Nothing,
      cueTrackPerformer = Nothing,
      cueTrackSongwriter = Nothing,
      cueTrackPregap = Nothing,
      cueTrackPregapIndex = Nothing,
      cueTrackIndices = CueTime 0 :| [],
      cueTrackPostgap = Nothing
    }
