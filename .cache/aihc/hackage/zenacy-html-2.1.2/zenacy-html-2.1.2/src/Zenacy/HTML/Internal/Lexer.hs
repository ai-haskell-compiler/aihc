{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The HTML Lexer.
module Zenacy.HTML.Internal.Lexer
  ( Lexer(..)
  , LexerOptions(..)
  , LexerSkip(..)
  , lexerNew
  , lexerSetRCDATA
  , lexerSetRAWTEXT
  , lexerSetPLAINTEXT
  , lexerSetScriptData
  , lexerSkipNextLF
  , lexerNext
  ) where

import Zenacy.HTML.Internal.BS
import Zenacy.HTML.Internal.Buffer
import Zenacy.HTML.Internal.Char
import Zenacy.HTML.Internal.Core
import Zenacy.HTML.Internal.Entity
import Zenacy.HTML.Internal.Token
import Control.Monad
  ( forM
  , forM_
  , mapM
  , when
  )
import Control.Monad.Extra
  ( whenM
  , whenJust
  )
import Control.Monad.ST
  ( ST
  )
import Data.STRef
  ( STRef
  , newSTRef
  )
import Data.DList
  ( DList
  )
import qualified Data.DList as D
  ( empty
  , snoc
  , toList
  )
import Data.Map
  ( Map
  )
import qualified Data.Map as Map
  ( fromList
  , lookup
  )
import Data.Maybe
  ( fromMaybe
  , isJust
  )
import Data.Word
  ( Word8
  )
import Data.Default
  ( Default(..)
  )
import Data.Vector.Storable.Mutable
  ( MVector(..)
  )
import qualified Data.Vector.Storable.Mutable as U
  ( new
  , length
  , read
  , write
  , grow
  )

-- | Lexer options type.
data LexerOptions = LexerOptions
  { lexerOptionInput          :: BS
  -- ^ The input to the lexer.
  , lexerOptionLogErrors      :: Bool
  -- ^ Indicates whether errors are logged.
  , lexerOptionIgnoreEntities :: Bool
  -- ^ Indicates that entities should not be tokenized.
  , lexerOptionInitialState   :: Maybe LexerState
  -- ^ Determines the initial state for the lexer when handling fragments.
  } deriving (Show)

-- | Defines the skip mode.
data LexerSkip
  = LexerSkipNone
  | LexerSkipLF
    deriving (Eq, Ord, Show)

-- | Defines the lexer state.
data Lexer s = Lexer
  { lexerData     :: BS
  -- ^ The lexer data.
  , lexerIgnore   :: Bool
  -- ^ Flag to ignore entity processing.
  , lexerLog      :: Bool
  -- ^ Flag to log errors.
  , lexerOffset   :: STRef s Int
  -- ^ The offset in the data for the next word to read.
  , lexerToken    :: STRef s (TokenBuffer s)
  -- ^ The token buffer.
  , lexerBuffer   :: STRef s (Buffer s)
  -- ^ The buffer is used to accumulate characters during
  --   some types of character processing.
  , lexerLast     :: STRef s [Word8]
  -- ^ The last start tag name to have been emitted.
  , lexerState    :: STRef s LexerState
  -- ^ The current lexer state in some cases.
  , lexerReturn   :: STRef s LexerState
  -- ^ The state to return to after character reference processing.
  , lexerSkip     :: STRef s LexerSkip
  -- ^ The skip next linefeed flag.
  , lexerErrors   :: STRef s (DList BS)
  -- ^ The lexer errors.
  , lexerCode     :: STRef s Int
  -- ^ The character reference code.
  }

-- | Default instance for lexer options.
instance Default LexerOptions where
  def = LexerOptions
    { lexerOptionInput          = bsEmpty
    , lexerOptionLogErrors      = False
    , lexerOptionIgnoreEntities = False
    , lexerOptionInitialState   = Nothing
    }

-- | Defines the lexer state.
data LexerState
  = StateData
  | StateRCDATA
  | StateRAWTEXT
  | StateScriptData
  | StatePLAINTEXT
  | StateTagOpen
  | StateEndTagOpen
  | StateTagName
  | StateRCDATALessThan
  | StateRCDATAEndTagOpen
  | StateRCDATAEndTagName
  | StateRAWTEXTLessThan
  | StateRAWTEXTEndTagOpen
  | StateRAWTEXTEndTagName
  | StateScriptDataLessThan
  | StateScriptDataEndTagOpen
  | StateScriptDataEndTagName
  | StateScriptDataEscapeStart
  | StateScriptDataEscapeStartDash
  | StateScriptDataEscaped
  | StateScriptDataEscapedDash
  | StateScriptDataEscapedDashDash
  | StateScriptDataEscapedLessThan
  | StateScriptDataEscapedEndTagOpen
  | StateScriptDataEscapedEndTagName
  | StateScriptDataDoubleEscapedStart
  | StateScriptDataDoubleEscaped
  | StateScriptDataDoubleEscapedDash
  | StateScriptDataDoubleEscapedDashDash
  | StateScriptDataDoubleEscapedLessThan
  | StateScriptDataDoubleEscapeEnd
  | StateBeforeAttrName
  | StateAttrName
  | StateAfterAttrName
  | StateBeforeAttrValue
  | StateAttrValueDoubleQuoted
  | StateAttrValueSingleQuoted
  | StateAttrValueUnquoted
  | StateAfterAttrValue
  | StateSelfClosingStartTag
  | StateBogusComment
  | StateMarkupDeclarationOpen
  | StateCommentStart
  | StateCommentStartDash
  | StateComment
  | StateCommentLessThan
  | StateCommentLessThanBang
  | StateCommentLessThanBangDash
  | StateCommentLessThanBangDashDash
  | StateCommentEndDash
  | StateCommentEnd
  | StateCommentEndBang
  | StateDoctype
  | StateBeforeDoctypeName
  | StateDoctypeName
  | StateAfterDoctypeName
  | StateAfterDoctypePublicKeyword
  | StateBeforeDoctypePublicId
  | StateDoctypePublicIdDoubleQuoted
  | StateDoctypePublicIdSingleQuoted
  | StateAfterDoctypePublicId
  | StateBetweenDoctypePublicAndSystem
  | StateAfterDoctypeSystemKeyword
  | StateBeforeDoctypeSystemId
  | StateDoctypeSystemIdDoubleQuoted
  | StateDoctypeSystemIdSingleQuoted
  | StateAfterDoctypeSystemId
  | StateBogusDoctype
  | StateCDATASection
  | StateCDATASectionBracket
  | StateCDATASectionEnd
  | StateCharacterReference
  | StateNamedCharacterReference
  | StateAmbiguousAmpersand
  | StateNumericCharacterReference
  | StateHexCharacterReferenceStart
  | StateDecimalCharacterReferenceStart
  | StateHexCharacterReference
  | StateDecimalCharacterReference
  | StateNumericCharacterReferenceEnd
    deriving (Show, Eq, Ord)

lexerDispatch :: LexerState -> Lexer s -> ST s ()
lexerDispatch = \case
  StateData ->
    doData
  StateRCDATA ->
    doRCDATA
  StateRAWTEXT ->
    doRAWTEXT
  StateScriptData ->
    doScriptData
  StatePLAINTEXT ->
    doPLAINTEXT
  StateTagOpen ->
    doTagOpen
  StateEndTagOpen ->
    doEndTagOpen
  StateTagName ->
    doTagName
  StateRCDATALessThan ->
    doRCDATALessThan
  StateRCDATAEndTagOpen ->
    doRCDATAEndTagOpen
  StateRCDATAEndTagName ->
    doRCDATAEndTagName
  StateRAWTEXTLessThan ->
    doRAWTEXTLessThan
  StateRAWTEXTEndTagOpen ->
    doRAWTEXTEndTagOpen
  StateRAWTEXTEndTagName ->
    doRAWTEXTEndTagName
  StateScriptDataLessThan ->
    doScriptDataLessThan
  StateScriptDataEndTagOpen ->
    doScriptDataEndTagOpen
  StateScriptDataEndTagName ->
    doScriptDataEndTagName
  StateScriptDataEscapeStart ->
    doScriptDataEscapeStart
  StateScriptDataEscapeStartDash ->
    doScriptDataEscapeStartDash
  StateScriptDataEscaped ->
    doScriptDataEscaped
  StateScriptDataEscapedDash ->
    doScriptDataEscapedDash
  StateScriptDataEscapedDashDash ->
    doScriptDataEscapedDashDash
  StateScriptDataEscapedLessThan ->
    doScriptDataEscapedLessThan
  StateScriptDataEscapedEndTagOpen ->
    doScriptDataEscapedEndTagOpen
  StateScriptDataEscapedEndTagName ->
    doScriptDataEscapedEndTagName
  StateScriptDataDoubleEscapedStart ->
    doScriptDataDoubleEscapedStart
  StateScriptDataDoubleEscaped ->
    doScriptDataDoubleEscaped
  StateScriptDataDoubleEscapedDash ->
    doScriptDataDoubleEscapedDash
  StateScriptDataDoubleEscapedDashDash ->
    doScriptDataDoubleEscapedDashDash
  StateScriptDataDoubleEscapedLessThan ->
    doScriptDataDoubleEscapedLessThan
  StateScriptDataDoubleEscapeEnd ->
    doScriptDataDoubleEscapeEnd
  StateBeforeAttrName ->
    doBeforeAttrName
  StateAttrName ->
    doAttrName
  StateAfterAttrName ->
    doAfterAttrName
  StateBeforeAttrValue ->
    doBeforeAttrValue
  StateAttrValueDoubleQuoted ->
    doAttrValueDoubleQuoted
  StateAttrValueSingleQuoted ->
    doAttrValueSingleQuoted
  StateAttrValueUnquoted ->
    doAttrValueUnquoted
  StateAfterAttrValue ->
    doAfterAttrValue
  StateSelfClosingStartTag ->
    doSelfClosingStartTag
  StateBogusComment ->
    doBogusComment
  StateMarkupDeclarationOpen ->
    doMarkupDeclarationOpen
  StateCommentStart ->
    doCommentStart
  StateCommentStartDash ->
    doCommentStartDash
  StateComment ->
    doComment
  StateCommentLessThan ->
    doCommentLessThan
  StateCommentLessThanBang ->
    doCommentLessThanBang
  StateCommentLessThanBangDash ->
    doCommentLessThanBangDash
  StateCommentLessThanBangDashDash ->
    doCommentLessThanBangDashDash
  StateCommentEndDash ->
    doCommentEndDash
  StateCommentEnd ->
    doCommentEnd
  StateCommentEndBang ->
    doCommentEndBang
  StateDoctype ->
    doDoctype
  StateBeforeDoctypeName ->
    doBeforeDoctypeName
  StateDoctypeName ->
    doDoctypeName
  StateAfterDoctypeName ->
    doAfterDoctypeName
  StateAfterDoctypePublicKeyword ->
    doAfterDoctypePublicKeyword
  StateBeforeDoctypePublicId ->
    doBeforeDoctypePublicId
  StateDoctypePublicIdDoubleQuoted ->
    doDoctypePublicIdDoubleQuoted
  StateDoctypePublicIdSingleQuoted ->
    doDoctypePublicIdSingleQuoted
  StateAfterDoctypePublicId ->
    doAfterDoctypePublicId
  StateBetweenDoctypePublicAndSystem ->
    doBetweenDoctypePublicAndSystem
  StateAfterDoctypeSystemKeyword ->
    doAfterDoctypeSystemKeyword
  StateBeforeDoctypeSystemId ->
    doBeforeDoctypeSystemId
  StateDoctypeSystemIdDoubleQuoted ->
    doDoctypeSystemIdDoubleQuoted
  StateDoctypeSystemIdSingleQuoted ->
    doDoctypeSystemIdSingleQuoted
  StateAfterDoctypeSystemId ->
    doAfterDoctypeSystemId
  StateBogusDoctype ->
    doBogusDoctype
  StateCDATASection ->
    doCDATASection
  StateCDATASectionBracket ->
    doCDATASectionBracket
  StateCDATASectionEnd ->
    doCDATASectionEnd
  StateCharacterReference ->
    doCharacterReference
  StateNamedCharacterReference ->
    doNamedCharacterReference
  StateAmbiguousAmpersand ->
    doAmbiguousAmpersand
  StateNumericCharacterReference ->
    doNumericCharacterReference
  StateHexCharacterReferenceStart ->
    doHexCharacterReferenceStart
  StateDecimalCharacterReferenceStart ->
    doDecimalCharacterReferenceStart
  StateHexCharacterReference ->
    doHexCharacterReference
  StateDecimalCharacterReference ->
    doDecimalCharacterReference
  StateNumericCharacterReferenceEnd ->
    doNumericCharacterReferenceEnd

-- | Makes a new lexer.
lexerNew :: LexerOptions -> ST s (Either BS (Lexer s))
lexerNew o@LexerOptions{..}
  | Just i <- bsElemIndex 0 lexerOptionInput =
      pure $ Left $ bsConcat [ "input contains null at ", bcPack $ show i ]
  | otherwise =
      Right <$> lexerMake o

-- | Makes a new lexer.
lexerMake :: LexerOptions -> ST s (Lexer s)
lexerMake LexerOptions{..} = do
  offset <- newSTRef 0
  state  <- newSTRef $ fromMaybe StateData lexerOptionInitialState
  ret    <- newSTRef StateData
  token  <- tokenBuffer
  buffer <- bufferNew
  last   <- newSTRef []
  skip   <- newSTRef LexerSkipNone
  errors <- newSTRef D.empty
  code   <- newSTRef 0
  pure $ Lexer
    { lexerData   = lexerOptionInput
    , lexerIgnore = lexerOptionIgnoreEntities
    , lexerLog    = lexerOptionLogErrors
    , lexerOffset = offset
    , lexerToken  = token
    , lexerBuffer = buffer
    , lexerLast   = last
    , lexerState  = state
    , lexerReturn = ret
    , lexerSkip   = skip
    , lexerErrors = errors
    , lexerCode   = code
    }

-- | Sets the RCDATA mode.
lexerSetRCDATA :: Lexer s -> ST s ()
lexerSetRCDATA Lexer{..} = wref lexerState StateRCDATA

-- | Sets the raw text mode.
lexerSetRAWTEXT :: Lexer s -> ST s ()
lexerSetRAWTEXT Lexer{..} = wref lexerState StateRAWTEXT

-- | Sets the plain text mode.
lexerSetPLAINTEXT :: Lexer s -> ST s ()
lexerSetPLAINTEXT Lexer{..} = wref lexerState StatePLAINTEXT

-- | Sets the script data mode.
lexerSetScriptData :: Lexer s -> ST s ()
lexerSetScriptData Lexer{..} = wref lexerState StateScriptData

-- | Sets the skip next linefeed flag.
lexerSkipNextLF :: Lexer s -> ST s ()
lexerSkipNextLF Lexer{..} = wref lexerSkip LexerSkipLF

-- | Gets the next token from a lexer.
lexerNext :: Lexer s -> ST s Token
lexerNext x@Lexer {..} =
  skip
  where
    skip = do
      t <- next
      case t of
        TChar 0x10 ->
          rref lexerSkip >>= \case
            LexerSkipLF ->
              wref lexerSkip LexerSkipNone >> skip
            LexerSkipNone ->
              pure t
        _otherwise ->
          pure t

    next = do
      i <- tokenNext lexerToken
      if i == 0
         then do
           tokenReset lexerToken
           s <- rref lexerState
           lexerDispatch s x
           i <- tokenFirst lexerToken
           if i == 0
              then pure TEOF
              else tokenPack i lexerToken
         else do
           tokenPack i lexerToken

-- | Gets the next word from a lexer.
nextWord :: Lexer s -> ST s Word8
nextWord Lexer {..} = do
  offset <- rref lexerOffset
  if | offset < bsLen lexerData -> do
         wref lexerOffset (offset + 1)
         pure $ bsIndex lexerData offset
     | otherwise ->
         pure chrEOF

-- | Gets the next word from a lexer without advancing.
peekWord :: Lexer s -> ST s Word8
peekWord Lexer {..} = do
  offset <- rref lexerOffset
  if | offset < bsLen lexerData ->
         pure $ bsIndex lexerData offset
     | otherwise ->
         pure chrEOF

-- | Moves the lexer back one word.
backWord :: Lexer s -> ST s ()
backWord Lexer {..} = uref lexerOffset (subtract 1)

-- | Skips the specified number of words.
skipWord :: Lexer s -> Int -> ST s ()
skipWord Lexer {..} n = uref lexerOffset (+n)

-- | Gets an indexing function from the current lexer offset.
dataIndexer :: Lexer s -> ST s (Int -> Word8)
dataIndexer Lexer {..} = do
  offset <- rref lexerOffset
  pure $ \i -> bsIndex lexerData (offset + i)

-- | Gets the remaining number of words.
dataRemain :: Lexer s -> ST s Int
dataRemain Lexer {..} = do
  offset <- rref lexerOffset
  pure $ bsLen lexerData - offset

-- | Emits the last token in the token buffer.
emit :: Lexer s -> ST s ()
emit Lexer {..} = do
  tokenTail lexerToken >>= flip tokenTagStartName lexerToken >>= \case
    Just a  -> wref lexerLast a
    Nothing -> pure ()

-- | Emits a character token.
emitChar :: Lexer s -> Word8 -> ST s ()
emitChar x@Lexer {..} w = do
  tokenCharInit w lexerToken
  emit x

-- | Emits the characters in the buffer.
emitBuffer :: Lexer s -> ST s ()
emitBuffer x@Lexer {..} = do
  bufferApply (emitChar x) lexerBuffer
  bufferReset lexerBuffer

-- | Sets the lexer state.
state :: Lexer s -> LexerState -> ST s ()
state Lexer {..} = wref lexerState

-- | Sets the lexer return state.
returnSet :: Lexer s -> LexerState -> ST s ()
returnSet x@Lexer {..} = wref lexerReturn

-- | Gets the lexer return state.
returnGet :: Lexer s -> ST s LexerState
returnGet x@Lexer {..} = rref lexerReturn

-- | Switches to the return state.
returnState :: Lexer s -> ST s ()
returnState x = returnGet x >>= flip lexerDispatch x

-- | Handles parse errors.
parseError :: Lexer s -> BS -> ST s ()
parseError x@Lexer {..} =
  when lexerLog . (uref lexerErrors . flip D.snoc)

-- | Determines if the current token is an appropriate end tag.
appropriateEndTag :: Lexer s -> ST s Bool
appropriateEndTag x@Lexer {..} = do
  tokenTail lexerToken >>= flip tokenTagEndName lexerToken >>= \case
    Just a  -> (==a) <$> rref lexerLast
    Nothing -> pure False

-- | Determines if an attribute value is currently being consumed.
consumingAttibute :: Lexer s -> ST s Bool
consumingAttibute x@Lexer {..} = do
  a <- returnGet x
  pure $ any (==a)
    [ StateAttrValueDoubleQuoted
    , StateAttrValueSingleQuoted
    , StateAttrValueUnquoted
    ]

-- | Flushes the code points stored in the buffer.
flushCodePoints :: Lexer s -> ST s ()
flushCodePoints x@Lexer {..} = do
  a <- consumingAttibute x
  if | a -> do
         bufferApply (flip tokenAttrValAppend lexerToken) lexerBuffer
         bufferReset lexerBuffer
     | otherwise -> do
         emitBuffer x

-- 12.2.5.1 Data state
doData :: Lexer s -> ST s ()
doData x@Lexer {..} = do
  c <- nextWord x
  if | c == chrAmpersand -> do
         returnSet x StateData
         doCharacterReference x
     | c == chrLess -> do
         doTagOpen x
     | c == chrEOF -> do
         tokenEOFInit lexerToken
         emit x
     | otherwise -> do
         emitChar x c
         doData x

-- 12.2.5.2 RCDATA state
doRCDATA :: Lexer s -> ST s ()
doRCDATA x@Lexer {..} = do
  c <- nextWord x
  if | c == chrAmpersand -> do
         returnSet x StateRCDATA
         doCharacterReference x
     | c == chrLess -> do
         doRCDATALessThan x
     | c == chrEOF -> do
         tokenEOFInit lexerToken
         emit x
     | otherwise -> do
         emitChar x c
         doRCDATA x

-- 12.2.5.3 RAWTEXT state
doRAWTEXT :: Lexer s -> ST s ()
doRAWTEXT x@Lexer {..} = do
  c <- nextWord x
  if | c == chrLess -> do
         doRAWTEXTLessThan x
     | c == chrEOF -> do
         tokenEOFInit lexerToken
         emit x
     | otherwise -> do
         emitChar x c
         doRAWTEXT x

-- 12.2.5.4 Script data state
doScriptData :: Lexer s -> ST s ()
doScriptData x@Lexer {..} = do
  c <- nextWord x
  if | c == chrLess -> do
         doScriptDataLessThan x
     | c == chrEOF -> do
         tokenEOFInit lexerToken
         emit x
     | otherwise -> do
         emitChar x c
         doScriptData x

-- 12.2.5.5 PLAINTEXT state
doPLAINTEXT :: Lexer s -> ST s ()
doPLAINTEXT x@Lexer {..} = do
  c <- nextWord x
  if | c == chrEOF -> do
         tokenEOFInit lexerToken
         emit x
     | otherwise -> do
         emitChar x c
         doPLAINTEXT x

-- 12.2.5.6 Tag open state
doTagOpen :: Lexer s -> ST s ()
doTagOpen x@Lexer {..} = do
  c <- nextWord x
  if | c == chrExclamation -> do
         doMarkupDeclarationOpen x
     | c == chrSolidus -> do
         doEndTagOpen x
     | chrASCIIAlpha c -> do
         tokenTagStartInit lexerToken
         backWord x
         doTagName x
     | c == chrQuestion -> do
         parseError x "unexpected-question-mark-instead-of-tag-name"
         tokenCommentInit lexerToken
         backWord x
         doBogusComment x
     | c == chrEOF -> do
         parseError x "eof-before-tag-name"
         tokenCharInit chrLess lexerToken
         tokenEOFInit lexerToken
         emit x
     | otherwise -> do
         parseError x "invalid-first-character-of-tag-name"
         backWord x
         doData x

-- 12.2.5.7 End tag open state
doEndTagOpen :: Lexer s -> ST s ()
doEndTagOpen x@Lexer {..} = do
  c <- nextWord x
  if | chrASCIIAlpha c -> do
         tokenTagEndInit lexerToken
         backWord x
         doTagName x
     | c == chrGreater -> do
         parseError x "missing-end-tag-name"
         doData x
     | c == chrEOF -> do
         parseError x "eof-before-tag-name"
         tokenCharInit chrLess lexerToken
         tokenCharInit chrSolidus lexerToken
         tokenEOFInit lexerToken
         emit x
     | otherwise -> do
         parseError x "invalid-first-character-of-tag-name"
         tokenCommentInit lexerToken
         backWord x
         doBogusComment x

-- 12.2.5.8 Tag name state
doTagName :: Lexer s -> ST s ()
doTagName x@Lexer {..} = do
  c <- nextWord x
  if | c == chrTab ||
       c == chrLF ||
       c == chrFF ||
       c == chrSpace -> do
         doBeforeAttrName x
     | c == chrSolidus -> do
         doSelfClosingStartTag x
     | c == chrGreater -> do
         state x StateData
         emit x
     | chrASCIIUpperAlpha c -> do
         tokenTagNameAppend (chrToLower c) lexerToken
         doTagName x
     | c == chrEOF -> do
         parseError x "eof-in-tag"
         tokenEOFInit lexerToken
         emit x
     | otherwise -> do
         tokenTagNameAppend c lexerToken
         doTagName x

-- 12.2.5.9 RCDATA less-than sign state
doRCDATALessThan :: Lexer s -> ST s ()
doRCDATALessThan x@Lexer {..} = do
  c <- nextWord x
  if | c == chrSolidus -> do
         bufferReset lexerBuffer
         doRCDATAEndTagOpen x
     | otherwise -> do
         tokenCharInit chrLess lexerToken
         backWord x
         doRCDATA x

-- 12.2.5.10 RCDATA end tag open state
doRCDATAEndTagOpen :: Lexer s -> ST s ()
doRCDATAEndTagOpen x@Lexer {..} = do
  c <- nextWord x
  if | chrASCIIAlpha c -> do
         tokenTagEndInit lexerToken
         backWord x
         doRCDATAEndTagName x
     | otherwise -> do
         tokenCharInit chrLess lexerToken
         tokenCharInit chrSolidus lexerToken
         backWord x
         doRCDATA x

-- 12.2.5.11 RCDATA end tag name state
doRCDATAEndTagName :: Lexer s -> ST s ()
doRCDATAEndTagName x@Lexer {..} = do
  c <- nextWord x
  a <- appropriateEndTag x
  if | c == chrTab ||
       c == chrLF ||
       c == chrFF ||
       c == chrSpace -> do
         if a
         then do
           doBeforeAttrName x
         else do
           anythingElse
     | c == chrSolidus -> do
         if a
         then do
           doSelfClosingStartTag x
         else do
           anythingElse
     | c == chrGreater -> do
         if a
         then do
           state x StateData
           emit x
         else do
           anythingElse
     | chrASCIIUpperAlpha c -> do
         tokenTagNameAppend (chrToLower c) lexerToken
         bufferAppend c lexerBuffer
         doRCDATAEndTagName x
     | chrASCIILowerAlpha c -> do
         tokenTagNameAppend c lexerToken
         bufferAppend c lexerBuffer
         doRCDATAEndTagName x
     | otherwise -> do
         anythingElse
  where
    anythingElse = do
      tokenDrop lexerToken
      emitChar x chrLess
      emitChar x chrSolidus
      emitBuffer x
      backWord x
      doRCDATA x

-- 12.2.5.12 RAWTEXT less-than sign state
doRAWTEXTLessThan :: Lexer s -> ST s ()
doRAWTEXTLessThan x@Lexer {..} = do
  c <- nextWord x
  if | c == chrSolidus -> do
         bufferReset lexerBuffer
         doRAWTEXTEndTagOpen x
     | otherwise -> do
         tokenCharInit chrLess lexerToken
         backWord x
         doRAWTEXT x

-- 12.2.5.13 RAWTEXT end tag open state
doRAWTEXTEndTagOpen :: Lexer s -> ST s ()
doRAWTEXTEndTagOpen x@Lexer {..} = do
  c <- nextWord x
  if | chrASCIIAlpha c -> do
         tokenTagEndInit lexerToken
         backWord x
         doRAWTEXTEndTagName x
     | otherwise -> do
         emitChar x chrLess
         emitChar x chrSolidus
         backWord x
         doRAWTEXT x

-- 12.2.5.14 RAWTEXT end tag name state
doRAWTEXTEndTagName :: Lexer s -> ST s ()
doRAWTEXTEndTagName x@Lexer {..} = do
  c <- nextWord x
  a <- appropriateEndTag x
  if | c == chrTab ||
       c == chrLF ||
       c == chrFF ||
       c == chrSpace -> do
         if a
         then do
           doBeforeAttrName x
         else do
           anythingElse
     | c == chrSolidus -> do
         if a
         then do
           doSelfClosingStartTag x
         else do
           anythingElse
     | c == chrGreater -> do
         if a
         then do
           state x StateData
           emit x
         else do
           anythingElse
     | chrASCIIUpperAlpha c -> do
         tokenTagNameAppend (chrToLower c) lexerToken
         bufferAppend c lexerBuffer
         doRAWTEXTEndTagName x
     | chrASCIILowerAlpha c -> do
         tokenTagNameAppend c lexerToken
         bufferAppend c lexerBuffer
         doRAWTEXTEndTagName x
     | otherwise -> do
         anythingElse
  where
    anythingElse = do
      tokenDrop lexerToken
      emitChar x chrLess
      emitChar x chrSolidus
      emitBuffer x
      backWord x
      doRAWTEXT x

-- 12.2.5.15 Script data less-than sign state
doScriptDataLessThan :: Lexer s -> ST s ()
doScriptDataLessThan x@Lexer {..} = do
  c <- nextWord x
  if | c == chrSolidus -> do
         bufferReset lexerBuffer
         doScriptDataEndTagOpen x
     | c == chrExclamation -> do
         tokenCharInit chrLess lexerToken
         tokenCharInit chrExclamation lexerToken
         doScriptDataEscapeStart x
     | otherwise -> do
         tokenCharInit chrLess lexerToken
         backWord x
         doScriptData x

-- 12.2.5.16 Script data end tag open state
doScriptDataEndTagOpen :: Lexer s -> ST s ()
doScriptDataEndTagOpen x@Lexer {..} = do
  c <- nextWord x
  if | chrASCIIAlpha c -> do
         tokenTagEndInit lexerToken
         backWord x
         doScriptDataEndTagName x
     | otherwise -> do
         emitChar x chrLess
         emitChar x chrSolidus
         backWord x
         doScriptData x

-- 12.2.5.17 Script data end tag name state
doScriptDataEndTagName :: Lexer s -> ST s ()
doScriptDataEndTagName x@Lexer {..} = do
  c <- nextWord x
  a <- appropriateEndTag x
  if | c == chrTab ||
       c == chrLF ||
       c == chrFF ||
       c == chrSpace -> do
         if a
         then do
           doBeforeAttrName x
         else do
           anythingElse
     | c == chrSolidus -> do
         if a
         then do
           doSelfClosingStartTag x
         else do
           anythingElse
     | c == chrGreater -> do
         if a
         then do
           state x StateData
           emit x
         else do
           anythingElse
     | chrASCIIUpperAlpha c -> do
         tokenTagNameAppend (chrToLower c) lexerToken
         bufferAppend c lexerBuffer
         doScriptDataEndTagName x
     | chrASCIILowerAlpha c -> do
         tokenTagNameAppend c lexerToken
         bufferAppend c lexerBuffer
         doScriptDataEndTagName x
     | otherwise -> do
         anythingElse
  where
    anythingElse = do
      tokenDrop lexerToken
      emitChar x chrLess
      emitChar x chrSolidus
      emitBuffer x
      backWord x
      doScriptData x

-- 12.2.5.18 Script data escape start state
doScriptDataEscapeStart :: Lexer s -> ST s ()
doScriptDataEscapeStart x@Lexer {..} = do
  c <- nextWord x
  if | c == chrHyphen -> do
         emitChar x chrHyphen
         doScriptDataEscapeStartDash x
     | otherwise -> do
         backWord x
         doScriptData x

-- 12.2.5.19 Script data escape start dash state
doScriptDataEscapeStartDash :: Lexer s -> ST s ()
doScriptDataEscapeStartDash x@Lexer {..} = do
  c <- nextWord x
  if | c == chrHyphen -> do
         emitChar x chrHyphen
         doScriptDataEscapedDashDash x
     | otherwise -> do
         backWord x
         doScriptData x

-- 12.2.5.20 Script data escaped state
doScriptDataEscaped :: Lexer s -> ST s ()
doScriptDataEscaped x@Lexer {..} = do
  c <- nextWord x
  if | c == chrHyphen -> do
         emitChar x chrHyphen
         doScriptDataEscapedDash x
     | c == chrLess -> do
         doScriptDataEscapedLessThan x
     | c == chrEOF -> do
         parseError x "eof-in-script-html-comment-like-text"
         tokenEOFInit lexerToken
         emit x
     | otherwise -> do
         emitChar x c
         doScriptDataEscaped x

-- 12.2.5.21 Script data escaped dash state
doScriptDataEscapedDash :: Lexer s -> ST s ()
doScriptDataEscapedDash x@Lexer {..} = do
  c <- nextWord x
  if | c == chrHyphen -> do
         emitChar x chrHyphen
         doScriptDataEscapedDashDash x
     | c == chrLess -> do
         doScriptDataEscapedLessThan x
     | c == chrEOF -> do
         parseError x "eof-in-script-html-comment-like-text"
         tokenEOFInit lexerToken
         emit x
     | otherwise -> do
         emitChar x c
         doScriptDataEscaped x

-- 12.2.5.22 Script data escaped dash dash state
doScriptDataEscapedDashDash :: Lexer s -> ST s ()
doScriptDataEscapedDashDash x@Lexer {..} = do
  c <- nextWord x
  if | c == chrHyphen -> do
         emitChar x c
         doScriptDataEscapedDashDash x
     | c == chrLess -> do
         doScriptDataEscapedLessThan x
     | c == chrGreater -> do
         emitChar x c
         doScriptData x
     | c == chrEOF -> do
         parseError x "eof-in-script-html-comment-like-text"
         tokenEOFInit lexerToken
         emit x
     | otherwise -> do
         emitChar x c
         doScriptDataEscaped x

-- 12.2.5.23 Script data escaped less-than sign state
doScriptDataEscapedLessThan :: Lexer s -> ST s ()
doScriptDataEscapedLessThan x@Lexer {..} = do
  c <- nextWord x
  if | c == chrSolidus -> do
         bufferReset lexerBuffer
         doScriptDataEscapedEndTagOpen x
     | chrASCIIAlpha c -> do
         bufferReset lexerBuffer
         emitChar x chrLess
         backWord x
         doScriptDataDoubleEscapedStart x
     | otherwise -> do
         emitChar x chrLess
         backWord x
         doScriptDataEscaped x

-- 12.2.5.24 Script data escaped end tag open state
doScriptDataEscapedEndTagOpen :: Lexer s -> ST s ()
doScriptDataEscapedEndTagOpen x@Lexer {..} = do
  c <- nextWord x
  if | chrASCIIAlpha c -> do
         tokenTagEndInit lexerToken
         backWord x
         doScriptDataEscapedEndTagName x
     | otherwise -> do
         emitChar x chrLess
         emitChar x chrSolidus
         backWord x
         doScriptDataEscaped x

-- 12.2.5.25 Script data escaped end tag name state
doScriptDataEscapedEndTagName :: Lexer s -> ST s ()
doScriptDataEscapedEndTagName x@Lexer {..} = do
  c <- nextWord x
  a <- appropriateEndTag x
  if | c == chrTab ||
       c == chrLF ||
       c == chrFF ||
       c == chrSpace -> do
         if a
         then do
           doBeforeAttrName x
         else do
           anythingElse
     | c == chrSolidus -> do
         if a
         then do
           doSelfClosingStartTag x
         else do
           anythingElse
     | c == chrGreater -> do
         if a
         then do
           state x StateData
           emit x
         else do
           anythingElse
     | chrASCIIUpperAlpha c -> do
         tokenTagNameAppend (chrToLower c) lexerToken
         bufferAppend c lexerBuffer
         doScriptDataEscapedEndTagName x
     | chrASCIILowerAlpha c -> do
         tokenTagNameAppend c lexerToken
         bufferAppend c lexerBuffer
         doScriptDataEscapedEndTagName x
     | otherwise -> do
         anythingElse
  where
    anythingElse = do
      tokenDrop lexerToken
      emitChar x chrLess
      emitChar x chrSolidus
      emitBuffer x
      backWord x
      state x StateScriptDataEscaped
      emit x

-- 12.2.5.26 Script data double escape start state
doScriptDataDoubleEscapedStart :: Lexer s -> ST s ()
doScriptDataDoubleEscapedStart x@Lexer {..} = do
  c <- nextWord x
  if | c == chrTab ||
       c == chrLF ||
       c == chrFF ||
       c == chrSpace ||
       c == chrSolidus ||
       c == chrGreater -> do
         bufferContains (bsUnpack "script") lexerBuffer >>= \case
           True -> do
             doScriptDataDoubleEscaped x
           False -> do
             tokenCharInit c lexerToken
             state x StateScriptDataEscaped
             emit x
     | chrASCIIUpperAlpha c -> do
         bufferAppend (chrToLower c) lexerBuffer
         tokenCharInit c lexerToken
         state x StateScriptDataDoubleEscapedStart
         emit x
     | chrASCIILowerAlpha c -> do
         bufferAppend c lexerBuffer
         tokenCharInit c lexerToken
         state x StateScriptDataDoubleEscapedStart
         emit x
     | otherwise -> do
         backWord x
         doScriptDataEscaped x

-- 12.2.5.27 Script data double escaped state
doScriptDataDoubleEscaped :: Lexer s -> ST s ()
doScriptDataDoubleEscaped x@Lexer {..} = do
  c <- nextWord x
  if | c == chrHyphen -> do
         tokenCharInit c lexerToken
         state x StateScriptDataDoubleEscapedDash
         emit x
     | c == chrLess -> do
         tokenCharInit c lexerToken
         state x StateScriptDataDoubleEscapedLessThan
         emit x
     | c == chrEOF -> do
         parseError x "eof-in-script-html-comment-like-text"
         tokenEOFInit lexerToken
         emit x
     | otherwise -> do
         tokenCharInit c lexerToken
         emit x

-- 12.2.5.28 Script data double escaped dash state
doScriptDataDoubleEscapedDash :: Lexer s -> ST s ()
doScriptDataDoubleEscapedDash x@Lexer {..} = do
  c <- nextWord x
  if | c == chrHyphen -> do
         tokenCharInit c lexerToken
         state x StateScriptDataDoubleEscapedDashDash
         emit x
     | c == chrLess -> do
         tokenCharInit c lexerToken
         state x StateScriptDataDoubleEscapedLessThan
         emit x
     | c == chrEOF -> do
         parseError x "eof-in-script-html-comment-like-text"
         tokenEOFInit lexerToken
         emit x
     | otherwise -> do
         tokenCharInit c lexerToken
         state x StateScriptDataDoubleEscaped
         emit x

-- 12.2.5.29 Script data double escaped dash dash state
doScriptDataDoubleEscapedDashDash :: Lexer s -> ST s ()
doScriptDataDoubleEscapedDashDash x@Lexer {..} = do
  c <- nextWord x
  if | c == chrHyphen -> do
         emitChar x c
         doScriptDataDoubleEscapedDashDash x
     | c == chrLess -> do
         tokenCharInit c lexerToken
         state x StateScriptDataDoubleEscapedLessThan
         emit x
     | c == chrGreater -> do
         tokenCharInit c lexerToken
         state x StateScriptData
         emit x
     | c == chrEOF -> do
         parseError x "eof-in-script-html-comment-like-text"
         tokenEOFInit lexerToken
         emit x
     | otherwise -> do
         tokenCharInit c lexerToken
         state x StateScriptDataDoubleEscaped
         emit x

-- 12.2.5.30 Script data double escaped less-than sign state
doScriptDataDoubleEscapedLessThan :: Lexer s -> ST s ()
doScriptDataDoubleEscapedLessThan x@Lexer {..} = do
  c <- nextWord x
  if | c == chrSolidus -> do
         bufferReset lexerBuffer
         tokenCharInit c lexerToken
         state x StateScriptDataDoubleEscapeEnd
         emit x
     | otherwise -> do
         backWord x
         doScriptDataDoubleEscaped x

-- 12.2.5.31 Script data double escape end state
doScriptDataDoubleEscapeEnd :: Lexer s -> ST s ()
doScriptDataDoubleEscapeEnd x@Lexer {..} = do
  c <- nextWord x
  if | c == chrTab ||
       c == chrLF ||
       c == chrFF ||
       c == chrSpace ||
       c == chrSolidus ||
       c == chrGreater -> do
         bufferContains (bsUnpack "script") lexerBuffer >>= \case
           True -> do
             doScriptDataEscaped x
           False -> do
             tokenCharInit c lexerToken
             state x StateScriptDataDoubleEscaped
             emit x
     | chrASCIIUpperAlpha c -> do
         bufferAppend (chrToLower c) lexerBuffer
         emitChar x c
         doScriptDataDoubleEscapeEnd x
     | chrASCIILowerAlpha c -> do
         bufferAppend c lexerBuffer
         emitChar x c
         doScriptDataDoubleEscapeEnd x
     | otherwise -> do
         backWord x
         doScriptDataDoubleEscaped x

-- 12.2.5.32 Before attribute name state
doBeforeAttrName :: Lexer s -> ST s ()
doBeforeAttrName x@Lexer {..} = do
  c <- nextWord x
  if | c == chrTab ||
       c == chrLF ||
       c == chrFF ||
       c == chrSpace -> do
         doBeforeAttrName x
     | c == chrSolidus ||
       c == chrGreater ||
       c == chrEOF -> do
         backWord x
         doAfterAttrName x
     | c == chrEqual -> do
         parseError x "unexpected-equals-sign-before-attribute-name"
         tokenAttrInit lexerToken
         tokenAttrNameAppend c lexerToken
         doAttrName x
     | otherwise -> do
         tokenAttrInit lexerToken
         backWord x
         doAttrName x

-- 12.2.5.33 Attribute name state
doAttrName :: Lexer s -> ST s ()
doAttrName x@Lexer {..} = do
  c <- nextWord x
  if | c == chrTab ||
       c == chrLF ||
       c == chrFF ||
       c == chrSpace ||
       c == chrSolidus ||
       c == chrGreater ||
       c == chrEOF -> do
         checkAttr
         backWord x
         doAfterAttrName x
     | c == chrEqual -> do
         checkAttr
         doBeforeAttrValue x
     | chrASCIIUpperAlpha c -> do
         tokenAttrNameAppend (chrToLower c) lexerToken
         doAttrName x
     | c == chrQuote ||
       c == chrApostrophe ||
       c == chrLess -> do
         parseError x "unexpected-character-in-attribute-name"
         tokenAttrNameAppend c lexerToken
         doAttrName x
     | otherwise -> do
         tokenAttrNameAppend c lexerToken
         doAttrName x
  where
    checkAttr = do
      i <- tokenTail lexerToken
      whenM (tokenAttrNamePrune i lexerToken) $ do
        parseError x "duplicate-attribute"

-- 12.2.5.34 After attribute name state
doAfterAttrName :: Lexer s -> ST s ()
doAfterAttrName x@Lexer {..} = do
  c <- nextWord x
  if | c == chrTab ||
       c == chrLF ||
       c == chrFF ||
       c == chrSpace -> do
         doAfterAttrName x
     | c == chrSolidus -> do
         doSelfClosingStartTag x
     | c == chrEqual -> do
         doBeforeAttrValue x
     | c == chrGreater -> do
         state x StateData
         emit x
     | c == chrEOF -> do
         parseError x "eof-in-tag"
         tokenEOFInit lexerToken
         emit x
     | otherwise -> do
         tokenAttrInit lexerToken
         backWord x
         doAttrName x

-- 12.2.5.35 Before attribute value state
doBeforeAttrValue :: Lexer s -> ST s ()
doBeforeAttrValue x@Lexer {..} = do
  c <- nextWord x
  if | c == chrTab ||
       c == chrLF ||
       c == chrFF ||
       c == chrSpace -> do
         doBeforeAttrValue x
     | c == chrQuote -> do
         doAttrValueDoubleQuoted x
     | c == chrApostrophe -> do
         doAttrValueSingleQuoted x
     | c == chrGreater -> do
         parseError x "missing-attribute-value"
         state x StateData
         emit x
     | otherwise -> do
         backWord x
         doAttrValueUnquoted x

-- 12.2.5.36 Attribute value (double-quoted) state
doAttrValueDoubleQuoted :: Lexer s -> ST s ()
doAttrValueDoubleQuoted x@Lexer {..} = do
  c <- nextWord x
  if | c == chrQuote -> do
         doAfterAttrValue x
     | c == chrAmpersand -> do
         returnSet x StateAttrValueDoubleQuoted
         doCharacterReference x
     | c == chrEOF -> do
         parseError x "eof-in-tag"
         tokenEOFInit lexerToken
         emit x
     | otherwise -> do
         tokenAttrValAppend c lexerToken
         doAttrValueDoubleQuoted x

-- 12.2.5.37 Attribute value (single-quoted) state
doAttrValueSingleQuoted :: Lexer s -> ST s ()
doAttrValueSingleQuoted x@Lexer {..} = do
  c <- nextWord x
  if | c == chrApostrophe -> do
         doAfterAttrValue x
     | c == chrAmpersand -> do
         returnSet x StateAttrValueSingleQuoted
         doCharacterReference x
     | c == chrEOF -> do
         parseError x "eof-in-tag"
         tokenEOFInit lexerToken
         emit x
     | otherwise -> do
         tokenAttrValAppend c lexerToken
         doAttrValueSingleQuoted x

-- 12.2.5.38 Attribute value (unquoted) state
doAttrValueUnquoted :: Lexer s -> ST s ()
doAttrValueUnquoted x@Lexer {..} = do
  c <- nextWord x
  if | c == chrTab ||
       c == chrLF ||
       c == chrFF ||
       c == chrSpace -> do
         doBeforeAttrName x
     | c == chrAmpersand -> do
         returnSet x StateAttrValueUnquoted
         doCharacterReference x
     | c == chrGreater -> do
         state x StateData
         emit x
     | c == chrQuote ||
       c == chrApostrophe ||
       c == chrLess ||
       c == chrEqual ||
       c == chrGrave -> do
         parseError x "unexpected-character-in-unquoted-attribute-value"
         tokenAttrValAppend c lexerToken
         doAttrValueUnquoted x
     | c == chrEOF -> do
         parseError x "eof-in-tag"
         tokenEOFInit lexerToken
         emit x
     | otherwise -> do
         tokenAttrValAppend c lexerToken
         doAttrValueUnquoted x

-- 12.2.5.39 After attribute value (quoted) state
doAfterAttrValue :: Lexer s -> ST s ()
doAfterAttrValue x@Lexer {..} = do
  c <- nextWord x
  if | c == chrTab ||
       c == chrLF ||
       c == chrFF ||
       c == chrSpace -> do
         doBeforeAttrName x
     | c == chrSolidus -> do
         doSelfClosingStartTag x
     | c == chrGreater -> do
         state x StateData
         emit x
     | c == chrEOF -> do
         parseError x "eof-in-tag"
         tokenEOFInit lexerToken
         emit x
     | otherwise -> do
         parseError x "missing-whitespace-between-attributes"
         backWord x
         doBeforeAttrName x

-- 12.2.5.40 Self-closing start tag state
doSelfClosingStartTag :: Lexer s -> ST s ()
doSelfClosingStartTag x@Lexer {..} = do
  c <- nextWord x
  if | c == chrGreater -> do
         tokenTagStartSetSelfClosing lexerToken
         state x StateData
         emit x
     | c == chrEOF -> do
         parseError x "eof-in-tag"
         tokenEOFInit lexerToken
         emit x
     | otherwise -> do
         parseError x "unexpected-solidus-in-tag"
         backWord x
         doBeforeAttrName x

-- 12.2.5.41 Bogus comment state
doBogusComment :: Lexer s -> ST s ()
doBogusComment x@Lexer {..} = do
  c <- nextWord x
  if | c == chrGreater -> do
         state x StateData
         emit x
     | c == chrEOF -> do
         tokenEOFInit lexerToken
         emit x
     | otherwise -> do
         tokenCommentAppend c lexerToken
         doBogusComment x

-- 12.2.5.42 Markup declaration open state
doMarkupDeclarationOpen :: Lexer s -> ST s ()
doMarkupDeclarationOpen x@Lexer {..} = do
  f <- dataIndexer x
  n <- dataRemain x
  if | n > 1 &&
       f 0 == chrHyphen &&
       f 1 == chrHyphen -> do
         skipWord x 2
         tokenCommentInit lexerToken
         doCommentStart x
     | n > 6 &&
       (f 0 == 0x44 || f 0 == 0x64) &&    -- D or d
       (f 1 == 0x4F || f 1 == 0x6F) &&    -- O or o
       (f 2 == 0x43 || f 2 == 0x63) &&    -- C or c
       (f 3 == 0x54 || f 3 == 0x74) &&    -- T or t
       (f 4 == 0x59 || f 4 == 0x79) &&    -- Y or y
       (f 5 == 0x50 || f 5 == 0x70) &&    -- P or p
       (f 6 == 0x45 || f 6 == 0x65) -> do -- E or e
         skipWord x 7
         doDoctype x
     | n > 6 &&
       f 0 == 0x5B &&    -- [
       f 1 == 0x43 &&    -- C
       f 2 == 0x44 &&    -- D
       f 3 == 0x41 &&    -- A
       f 4 == 0x54 &&    -- T
       f 5 == 0x41 &&    -- A
       f 6 == 0x5B -> do -- [
         skipWord x 7
  -- The standard says to check if the parser has an adjusted current node
  -- that is not in the HTML namespace, and if so then CDATA sections are
  -- allowed.  For now we assume that condition is true and we do not
  -- check with the parser to verify.  CDATA sections in HTML are probably
  -- not common and in most cases they are going to be in the correct
  -- locations anyway.
         if True -- TODO
            then do
              state x StateCDATASection
              doCDATASection x
            else do
              parseError x "cdata-in-html-content"
              tokenCommentInit lexerToken
              mapM_ (flip tokenCommentAppend lexerToken)
                [ 0x5B, 0x43, 0x44, 0x41, 0x54, 0x41, 0x5B ]
              doBogusComment x
     | otherwise -> do
         parseError x "incorrectly-opened-comment"
         tokenCommentInit lexerToken
         doBogusComment x

-- 12.2.5.43 Comment start state
doCommentStart :: Lexer s -> ST s ()
doCommentStart x@Lexer {..} = do
  c <- nextWord x
  if | c == chrHyphen -> do
         doCommentStartDash x
     | c == chrGreater -> do
         parseError x "abrupt-closing-of-empty-comment"
         state x StateData
         emit x
     | otherwise -> do
         backWord x
         doComment x

-- 12.2.5.44 Comment start dash state
doCommentStartDash :: Lexer s -> ST s ()
doCommentStartDash x@Lexer {..} = do
  c <- nextWord x
  if | c == chrHyphen -> do
         doCommentEnd x
     | c == chrGreater -> do
         parseError x "abrupt-closing-of-empty-comment"
         state x StateData
         emit x
     | c == chrEOF -> do
         parseError x "eof-in-comment"
         tokenEOFInit lexerToken
         emit x
     | otherwise -> do
         tokenCommentAppend chrHyphen lexerToken
         backWord x
         doComment x

-- 12.2.5.45 Comment state
doComment :: Lexer s -> ST s ()
doComment x@Lexer {..} = do
  c <- nextWord x
  if | c == chrLess -> do
         tokenCommentAppend c lexerToken
         doCommentLessThan x
     | c == chrHyphen -> do
         doCommentEndDash x
     | c == chrEOF -> do
         parseError x "eof-in-comment"
         tokenEOFInit lexerToken
         emit x
     | otherwise -> do
         tokenCommentAppend c lexerToken
         doComment x

-- 12.2.5.46 Comment less-than sign state
doCommentLessThan :: Lexer s -> ST s ()
doCommentLessThan x@Lexer {..} = do
  c <- nextWord x
  if | c == chrExclamation -> do
         tokenCommentAppend c lexerToken
         doCommentLessThanBang x
     | c == chrLess -> do
         tokenCommentAppend c lexerToken
         doCommentLessThan x
     | otherwise -> do
         backWord x
         doComment x

-- 12.2.5.47 Comment less-than sign bang state
doCommentLessThanBang :: Lexer s -> ST s ()
doCommentLessThanBang x@Lexer {..} = do
  c <- nextWord x
  if | c == chrHyphen -> do
         doCommentLessThanBangDash x
     | otherwise -> do
         backWord x
         doComment x

-- 12.2.5.48 Comment less-than sign bang dash state
doCommentLessThanBangDash :: Lexer s -> ST s ()
doCommentLessThanBangDash x@Lexer {..} = do
  c <- nextWord x
  if | c == chrHyphen -> do
         doCommentLessThanBangDashDash x
     | otherwise -> do
         backWord x
         doCommentEndDash x

-- 12.2.5.49 Comment less-than sign bang dash dash state
doCommentLessThanBangDashDash :: Lexer s -> ST s ()
doCommentLessThanBangDashDash x@Lexer {..} = do
  c <- nextWord x
  if | c == chrHyphen ||
       c == chrEOF -> do
         backWord x
         doCommentEnd x
     | otherwise -> do
         parseError x "nested-comment"
         backWord x
         doCommentEnd x

-- 12.2.5.50 Comment end dash state
doCommentEndDash :: Lexer s -> ST s ()
doCommentEndDash x@Lexer {..} = do
  c <- nextWord x
  if | c == chrHyphen -> do
         doCommentEnd x
     | c == chrEOF -> do
         parseError x "eof-in-comment"
         tokenEOFInit lexerToken
         emit x
     | otherwise -> do
         tokenCommentAppend chrHyphen lexerToken
         backWord x
         doComment x

-- 12.2.5.51 Comment end state
doCommentEnd :: Lexer s -> ST s ()
doCommentEnd x@Lexer {..} = do
  c <- nextWord x
  if | c == chrGreater -> do
         state x StateData
         emit x
     | c == chrExclamation -> do
         doCommentEndBang x
     | c == chrHyphen -> do
         tokenCommentAppend chrHyphen lexerToken
         doCommentEnd x
     | c == chrEOF -> do
         parseError x "eof-in-comment"
         tokenEOFInit lexerToken
         emit x
     | otherwise -> do
         tokenCommentAppend chrHyphen lexerToken
         tokenCommentAppend chrHyphen lexerToken
         backWord x
         doComment x

-- 12.2.5.52 Comment end bang state
doCommentEndBang :: Lexer s -> ST s ()
doCommentEndBang x@Lexer {..} = do
  c <- nextWord x
  if | c == chrHyphen -> do
         tokenCommentAppend chrHyphen lexerToken
         tokenCommentAppend chrHyphen lexerToken
         tokenCommentAppend chrExclamation lexerToken
         doCommentEndDash x
     | c == chrGreater -> do
         parseError x "incorrectly-closed-comment"
         state x StateData
         emit x
     | c == chrEOF -> do
         parseError x "eof-in-comment"
         tokenEOFInit lexerToken
         emit x
     | otherwise -> do
         tokenCommentAppend chrHyphen lexerToken
         tokenCommentAppend chrHyphen lexerToken
         tokenCommentAppend chrExclamation lexerToken
         backWord x
         doComment x

-- 12.2.5.53 DOCTYPE state
doDoctype :: Lexer s -> ST s ()
doDoctype x@Lexer {..} = do
  c <- nextWord x
  if | c == chrTab ||
       c == chrLF ||
       c == chrFF ||
       c == chrSpace -> do
         doBeforeDoctypeName x
     | c == chrGreater -> do
         backWord x
         doBeforeDoctypeName x
     | c == chrEOF -> do
         parseError x "eof-in-doctype"
         tokenDoctypeInit lexerToken
         tokenDoctypeSetForceQuirks lexerToken
         tokenEOFInit lexerToken
         emit x
     | otherwise -> do
         parseError x "missing-whitespace-before-doctype-name"
         backWord x
         doBeforeDoctypeName x

-- 12.2.5.54 Before DOCTYPE name state
doBeforeDoctypeName :: Lexer s -> ST s ()
doBeforeDoctypeName x@Lexer {..} = do
  c <- nextWord x
  if | c == chrTab ||
       c == chrLF ||
       c == chrFF ||
       c == chrSpace -> do
         doBeforeDoctypeName x
     | chrASCIIUpperAlpha c -> do
         tokenDoctypeInit lexerToken
         tokenDoctypeNameAppend (chrToLower c) lexerToken
         doDoctypeName x
     | c == chrGreater -> do
         parseError x "missing-doctype-name"
         tokenDoctypeInit lexerToken
         tokenDoctypeSetForceQuirks lexerToken
         state x StateData
         emit x
     | c == chrEOF -> do
         parseError x "eof-in-doctype"
         tokenDoctypeInit lexerToken
         tokenDoctypeSetForceQuirks lexerToken
         tokenEOFInit lexerToken
         emit x
     | otherwise -> do
         tokenDoctypeInit lexerToken
         tokenDoctypeNameAppend c lexerToken
         doDoctypeName x

-- 12.2.5.55 DOCTYPE name state
doDoctypeName :: Lexer s -> ST s ()
doDoctypeName x@Lexer {..} = do
  c <- nextWord x
  if | c == chrTab ||
       c == chrLF ||
       c == chrFF ||
       c == chrSpace -> do
         doAfterDoctypeName x
     | c == chrGreater -> do
         state x StateData
         emit x
     | chrASCIIUpperAlpha c -> do
         tokenDoctypeNameAppend (chrToLower c) lexerToken
         doDoctypeName x
     | c == chrEOF -> do
         parseError x "eof-in-doctype"
         tokenDoctypeSetForceQuirks lexerToken
         tokenEOFInit lexerToken
         emit x
     | otherwise -> do
         tokenDoctypeNameAppend c lexerToken
         doDoctypeName x

-- 12.2.5.56 After DOCTYPE name state
doAfterDoctypeName :: Lexer s -> ST s ()
doAfterDoctypeName x@Lexer {..} = do
  c <- nextWord x
  -- Get data indexer after the character.
  f <- dataIndexer x
  if | c == chrTab ||
       c == chrLF ||
       c == chrFF ||
       c == chrSpace -> do
         doAfterDoctypeName x
     | c == chrGreater -> do
         state x StateData
         emit x
     | c == chrEOF -> do
         parseError x "eof-in-doctype"
         tokenDoctypeSetForceQuirks lexerToken
         tokenEOFInit lexerToken
         emit x
     | (c == 0x50 || c == 0x70) &&        -- P or p
       (f 0 == 0x55 || f 0 == 0x75) &&    -- U or u
       (f 1 == 0x42 || f 1 == 0x62) &&    -- B or b
       (f 2 == 0x4C || f 2 == 0x6C) &&    -- L or l
       (f 3 == 0x49 || f 3 == 0x69) &&    -- I or i
       (f 4 == 0x43 || f 4 == 0x63) -> do -- C or c
         skipWord x 5
         doAfterDoctypePublicKeyword x
     | (c == 0x53 || c == 0x73) &&        -- S or s
       (f 0 == 0x59 || f 0 == 0x79) &&    -- Y or y
       (f 1 == 0x53 || f 1 == 0x73) &&    -- S or s
       (f 2 == 0x54 || f 2 == 0x74) &&    -- T or t
       (f 3 == 0x45 || f 3 == 0x65) &&    -- E or e
       (f 4 == 0x4D || f 4 == 0x6D) -> do -- M or m
         skipWord x 5
         doAfterDoctypeSystemKeyword x
     | otherwise -> do
         parseError x "invalid-character-sequence-after-doctype-name"
         tokenDoctypeSetForceQuirks lexerToken
         doBogusDoctype x

-- 12.2.5.57 After DOCTYPE public keyword state
doAfterDoctypePublicKeyword :: Lexer s -> ST s ()
doAfterDoctypePublicKeyword x@Lexer {..} = do
  c <- nextWord x
  if | c == chrTab ||
       c == chrLF ||
       c == chrFF ||
       c == chrSpace -> do
         doBeforeDoctypePublicId x
     | c == chrQuote -> do
         parseError x "missing-whitespace-after-doctype-public-keyword"
         tokenDoctypePublicIdInit lexerToken
         doDoctypePublicIdDoubleQuoted x
     | c == chrApostrophe -> do
         parseError x "missing-whitespace-after-doctype-public-keyword"
         tokenDoctypePublicIdInit lexerToken
         doDoctypePublicIdSingleQuoted x
     | c == chrGreater -> do
         parseError x "missing-doctype-public-identifier"
         tokenDoctypeSetForceQuirks lexerToken
         state x StateData
         emit x
     | c == chrEOF -> do
         parseError x "eof-in-doctype"
         tokenDoctypeSetForceQuirks lexerToken
         tokenEOFInit lexerToken
         emit x
     | otherwise -> do
         parseError x "missing-quote-before-doctype-public-identifier"
         tokenDoctypeSetForceQuirks lexerToken
         doBogusDoctype x

-- 12.2.5.58 Before DOCTYPE public identifier state
doBeforeDoctypePublicId :: Lexer s -> ST s ()
doBeforeDoctypePublicId x@Lexer {..} = do
  c <- nextWord x
  if | c == chrTab ||
       c == chrLF ||
       c == chrFF ||
       c == chrSpace -> do
         doBeforeDoctypePublicId x
     | c == chrQuote -> do
         tokenDoctypePublicIdInit lexerToken
         doDoctypePublicIdDoubleQuoted x
     | c == chrApostrophe -> do
         tokenDoctypePublicIdInit lexerToken
         doDoctypePublicIdSingleQuoted x
     | c == chrGreater -> do
         parseError x "missing-doctype-public-identifier"
         tokenDoctypeSetForceQuirks lexerToken
         state x StateData
         emit x
     | c == chrEOF -> do
         parseError x "eof-in-doctype"
         tokenDoctypeSetForceQuirks lexerToken
         tokenEOFInit lexerToken
         emit x
     | otherwise -> do
         parseError x "missing-quote-before-doctype-public-identifier"
         tokenDoctypeSetForceQuirks lexerToken
         doBogusDoctype x

-- 12.2.5.59 DOCTYPE public identifier (double-quoted) state
doDoctypePublicIdDoubleQuoted :: Lexer s -> ST s ()
doDoctypePublicIdDoubleQuoted x@Lexer {..} = do
  c <- nextWord x
  if | c == chrQuote -> do
         doAfterDoctypePublicId x
     | c == chrGreater -> do
         parseError x "abrupt-doctype-public-identifier"
         tokenDoctypeSetForceQuirks lexerToken
         state x StateData
         emit x
     | c == chrEOF -> do
         parseError x "eof-in-doctype"
         tokenDoctypeSetForceQuirks lexerToken
         tokenEOFInit lexerToken
         emit x
     | otherwise -> do
         tokenDoctypePublicIdAppend c lexerToken
         doDoctypePublicIdDoubleQuoted x

-- 12.2.5.60 DOCTYPE public identifier (single-quoted) state
doDoctypePublicIdSingleQuoted :: Lexer s -> ST s ()
doDoctypePublicIdSingleQuoted x@Lexer {..} = do
  c <- nextWord x
  if | c == chrApostrophe -> do
         doAfterDoctypePublicId x
     | c == chrGreater -> do
         parseError x "abrupt-doctype-public-identifier"
         tokenDoctypeSetForceQuirks lexerToken
         state x StateData
         emit x
     | c == chrEOF -> do
         parseError x "eof-in-doctype"
         tokenDoctypeSetForceQuirks lexerToken
         tokenEOFInit lexerToken
         emit x
     | otherwise -> do
         tokenDoctypePublicIdAppend c lexerToken
         doDoctypePublicIdSingleQuoted x

-- 12.2.5.61 After DOCTYPE public identifier state
doAfterDoctypePublicId :: Lexer s -> ST s ()
doAfterDoctypePublicId x@Lexer {..} = do
  c <- nextWord x
  if | c == chrTab ||
       c == chrLF ||
       c == chrFF ||
       c == chrSpace -> do
         doBetweenDoctypePublicAndSystem x
     | c == chrGreater -> do
         state x StateData
         emit x
     | c == chrQuote -> do
         parseError x "missing-whitespace-between-doctype-public-and-system-identifiers"
         tokenDoctypeSystemIdInit lexerToken
         doDoctypeSystemIdDoubleQuoted x
     | c == chrApostrophe -> do
         parseError x "missing-whitespace-between-doctype-public-and-system-identifiers"
         tokenDoctypeSystemIdInit lexerToken
         doDoctypeSystemIdSingleQuoted x
     | c == chrEOF -> do
         parseError x "eof-in-doctype"
         tokenDoctypeSetForceQuirks lexerToken
         tokenEOFInit lexerToken
         emit x
     | otherwise -> do
         parseError x "missing-quote-before-doctype-system-identifier"
         tokenDoctypeSetForceQuirks lexerToken
         doBogusDoctype x

-- 12.2.5.62 Between DOCTYPE public and system identifiers state
doBetweenDoctypePublicAndSystem :: Lexer s -> ST s ()
doBetweenDoctypePublicAndSystem x@Lexer {..} = do
  c <- nextWord x
  if | c == chrTab ||
       c == chrLF ||
       c == chrFF ||
       c == chrSpace -> do
         doBetweenDoctypePublicAndSystem x
     | c == chrGreater -> do
         state x StateData
         emit x
     | c == chrQuote -> do
         tokenDoctypeSystemIdInit lexerToken
         doDoctypeSystemIdDoubleQuoted x
     | c == chrApostrophe -> do
         tokenDoctypeSystemIdInit lexerToken
         doDoctypeSystemIdSingleQuoted x
     | c == chrEOF -> do
         parseError x "eof-in-doctype"
         tokenDoctypeSetForceQuirks lexerToken
         tokenEOFInit lexerToken
         emit x
     | otherwise -> do
         parseError x "missing-quote-before-doctype-system-identifier"
         tokenDoctypeSetForceQuirks lexerToken
         doBogusDoctype x

-- 12.2.5.63 After DOCTYPE system keyword state
doAfterDoctypeSystemKeyword :: Lexer s -> ST s ()
doAfterDoctypeSystemKeyword x@Lexer {..} = do
  c <- nextWord x
  if | c == chrTab ||
       c == chrLF ||
       c == chrFF ||
       c == chrSpace -> do
         doBeforeDoctypeSystemId x
     | c == chrQuote -> do
         parseError x "missing-whitespace-after-doctype-system-keyword"
         tokenDoctypeSystemIdInit lexerToken
         doDoctypeSystemIdDoubleQuoted x
     | c == chrApostrophe -> do
         parseError x "missing-whitespace-after-doctype-system-keyword"
         tokenDoctypeSystemIdInit lexerToken
         doDoctypeSystemIdSingleQuoted x
     | c == chrGreater -> do
         parseError x "missing-doctype-system-identifier"
         tokenDoctypeSetForceQuirks lexerToken
         state x StateData
         emit x
     | c == chrEOF -> do
         parseError x "eof-in-doctype"
         tokenDoctypeSetForceQuirks lexerToken
         tokenEOFInit lexerToken
         emit x
     | otherwise -> do
         parseError x "missing-quote-before-doctype-system-identifier"
         tokenDoctypeSetForceQuirks lexerToken
         doBogusDoctype x

-- 12.2.5.64 Before DOCTYPE system identifier state
doBeforeDoctypeSystemId :: Lexer s -> ST s ()
doBeforeDoctypeSystemId x@Lexer {..} = do
  c <- nextWord x
  if | c == chrTab ||
       c == chrLF ||
       c == chrFF ||
       c == chrSpace -> do
         doBeforeDoctypeSystemId x
     | c == chrQuote -> do
         tokenDoctypeSystemIdInit lexerToken
         doDoctypeSystemIdDoubleQuoted x
     | c == chrApostrophe -> do
         tokenDoctypeSystemIdInit lexerToken
         doDoctypeSystemIdSingleQuoted x
     | c == chrGreater -> do
         parseError x "missing-doctype-system-identifier"
         tokenDoctypeSetForceQuirks lexerToken
         state x StateData
         emit x
     | c == chrEOF -> do
         parseError x "eof-in-doctype"
         tokenDoctypeSetForceQuirks lexerToken
         tokenEOFInit lexerToken
         emit x
     | otherwise -> do
         parseError x "missing-quote-before-doctype-system-identifier"
         tokenDoctypeSetForceQuirks lexerToken
         doBogusDoctype x

-- 12.2.5.65 DOCTYPE system identifier (double-quoted) state
doDoctypeSystemIdDoubleQuoted :: Lexer s -> ST s ()
doDoctypeSystemIdDoubleQuoted x@Lexer {..} = do
  c <- nextWord x
  if | c == chrQuote -> do
         doAfterDoctypeSystemId x
     | c == chrGreater -> do
         parseError x "abrupt-doctype-system-identifier"
         tokenDoctypeSetForceQuirks lexerToken
         state x StateData
         emit x
     | c == chrEOF -> do
         parseError x "eof-in-doctype"
         tokenDoctypeSetForceQuirks lexerToken
         tokenEOFInit lexerToken
         emit x
     | otherwise -> do
         tokenDoctypeSystemIdAppend c lexerToken
         doDoctypeSystemIdDoubleQuoted x

-- 12.2.5.66 DOCTYPE system identifier (single-quoted) state
doDoctypeSystemIdSingleQuoted :: Lexer s -> ST s ()
doDoctypeSystemIdSingleQuoted x@Lexer {..} = do
  c <- nextWord x
  if | c == chrApostrophe -> do
         doAfterDoctypeSystemId x
     | c == chrGreater -> do
         parseError x "abrupt-doctype-system-identifier"
         tokenDoctypeSetForceQuirks lexerToken
         state x StateData
         emit x
     | c == chrEOF -> do
         parseError x "eof-in-doctype"
         tokenDoctypeSetForceQuirks lexerToken
         tokenEOFInit lexerToken
         emit x
     | otherwise -> do
         tokenDoctypeSystemIdAppend c lexerToken
         doDoctypeSystemIdSingleQuoted x

-- 12.2.5.67 After DOCTYPE system identifier state
doAfterDoctypeSystemId :: Lexer s -> ST s ()
doAfterDoctypeSystemId x@Lexer {..} = do
  c <- nextWord x
  if | c == chrTab ||
       c == chrLF ||
       c == chrFF ||
       c == chrSpace -> do
         doAfterDoctypeSystemId x
     | c == chrGreater -> do
         state x StateData
         emit x
     | c == chrEOF -> do
         parseError x "eof-in-doctype"
         tokenDoctypeSetForceQuirks lexerToken
         tokenEOFInit lexerToken
         emit x
     | otherwise -> do
         parseError x "unexpected-character-after-doctype-system-identifier"
         doBogusDoctype x

-- 12.2.5.68 Bogus DOCTYPE state
doBogusDoctype :: Lexer s -> ST s ()
doBogusDoctype x@Lexer {..} = do
  c <- nextWord x
  if | c == chrGreater -> do
         state x StateData
         emit x
     | c == chrEOF -> do
         tokenEOFInit lexerToken
         emit x
     | otherwise -> do
         doBogusDoctype x

-- 12.2.5.69 CDATA section state
doCDATASection :: Lexer s -> ST s ()
doCDATASection x@Lexer {..} = do
  c <- nextWord x
  if | c == chrBracketRight -> do
         doCDATASectionBracket x
     | c == chrEOF -> do
         parseError x "eof-in-cdata"
         tokenEOFInit lexerToken
         emit x
     | otherwise -> do
         emitChar x c
         doCDATASection x

-- 12.2.5.70 CDATA section bracket state
doCDATASectionBracket :: Lexer s -> ST s ()
doCDATASectionBracket x@Lexer {..} = do
  c <- nextWord x
  if | c == chrBracketRight -> do
         doCDATASectionEnd x
     | otherwise -> do
         tokenCharInit chrBracketRight lexerToken
         backWord x
         doCDATASection x

-- 12.2.5.71 CDATA section end state
doCDATASectionEnd :: Lexer s -> ST s ()
doCDATASectionEnd x@Lexer {..} = do
  c <- nextWord x
  if | c == chrBracketRight -> do
         emitChar x c
         doCDATASectionEnd x
     | c == chrGreater -> do
         doData x
     | otherwise -> do
         emitChar x chrBracketRight
         emitChar x chrBracketRight
         backWord x
         doCDATASection x

-- 12.2.5.72 Character reference state
doCharacterReference :: Lexer s -> ST s ()
doCharacterReference x@Lexer {..} = do
  bufferReset lexerBuffer
  bufferAppend chrAmpersand lexerBuffer
  c <- nextWord x
  if | lexerIgnore -> do
         flushCodePoints x
         backWord x
         returnState x
     | chrASCIIAlphanumeric c -> do
         backWord x
         doNamedCharacterReference x
     | c == chrNumberSign -> do
         bufferAppend c lexerBuffer
         doNumericCharacterReference x
     | otherwise -> do
         flushCodePoints x
         backWord x
         returnState x

-- 12.2.5.73 Named character reference state
doNamedCharacterReference :: Lexer s -> ST s ()
doNamedCharacterReference x@Lexer {..} = do
  o <- rref lexerOffset
  case entityMatch (bsDrop o lexerData) of
    Just (p, v, _) -> do
      skipWord x $ bsLen p
      forM_ (bsUnpack p) $
        flip bufferAppend lexerBuffer
      attr <- consumingAttibute x
      semi <- pure $ bsLast p == Just chrSemicolon
      c    <- peekWord x
      if | attr
         , not semi
         , c == chrSemicolon || chrASCIIAlphanumeric c -> do
             flushCodePoints x
             returnState x
         | otherwise -> do
             when (not semi) $
               parseError x "missing-semicolon-after-character-reference"
             bufferReset lexerBuffer
             forM_ (bsUnpack v) $
               flip bufferAppend lexerBuffer
             flushCodePoints x
             returnState x
    Nothing -> do
      flushCodePoints x
      doAmbiguousAmpersand x

-- 12.2.5.74 Ambiguous ampersand state
doAmbiguousAmpersand :: Lexer s -> ST s ()
doAmbiguousAmpersand x@Lexer {..} = do
  c <- nextWord x
  if | chrASCIIAlphanumeric c -> do
         consumingAttibute x >>= \case
           True  -> tokenAttrValAppend c lexerToken
           False -> emitChar x c
         doAmbiguousAmpersand x
     | c == chrSemicolon -> do
         parseError x "unknown-named-character-reference"
         backWord x
         returnState x
     | otherwise -> do
         backWord x
         returnState x

-- 12.2.5.75 Numeric character reference state
doNumericCharacterReference :: Lexer s -> ST s ()
doNumericCharacterReference x@Lexer {..} = do
  wref lexerCode 0
  c <- nextWord x
  if | c == chrUpperX || c == chrLowerX -> do
         bufferAppend c lexerBuffer
         doHexCharacterReferenceStart x
     | otherwise -> do
         backWord x
         doDecimalCharacterReference x

-- 12.2.5.76 Hexademical character reference start state
doHexCharacterReferenceStart :: Lexer s -> ST s ()
doHexCharacterReferenceStart x@Lexer {..} = do
  c <- nextWord x
  if | chrASCIIHexDigit c -> do
         backWord x
         doHexCharacterReference x
     | otherwise -> do
         parseError x "absence-of-digits-in-numeric-character-reference"
         flushCodePoints x
         backWord x
         returnState x

-- 12.2.5.77 Decimal character reference start state
doDecimalCharacterReferenceStart :: Lexer s -> ST s ()
doDecimalCharacterReferenceStart x@Lexer {..} = do
  c <- nextWord x
  if | chrASCIIDigit c -> do
         backWord x
         doDecimalCharacterReference x
     | otherwise -> do
         parseError x "absence-of-digits-in-numeric-character-reference"
         flushCodePoints x
         backWord x
         returnState x

-- 12.2.5.78 Hexademical character reference state
doHexCharacterReference :: Lexer s -> ST s ()
doHexCharacterReference x@Lexer {..} = do
  c <- nextWord x
  if | chrASCIIDigit c -> do
         uref lexerCode $ \y -> 16 * y + (fromIntegral c - 0x30)
         doHexCharacterReference x
     | chrASCIIUpperHexDigit c -> do
         uref lexerCode $ \y -> 16 * y + (fromIntegral c - 0x37)
         doHexCharacterReference x
     | chrASCIILowerHexDigit c -> do
         uref lexerCode $ \y -> 16 * y + (fromIntegral c - 0x57)
         doHexCharacterReference x
     | c == chrSemicolon -> do
         doNumericCharacterReferenceEnd x
     | otherwise -> do
         parseError x "missing-semicolon-after-character-reference"
         backWord x
         doNumericCharacterReferenceEnd x

-- 12.2.5.79 Decimal character reference state
doDecimalCharacterReference :: Lexer s -> ST s ()
doDecimalCharacterReference x@Lexer {..} = do
  c <- nextWord x
  if | chrASCIIDigit c -> do
         uref lexerCode $ \y -> 10 * y + (fromIntegral c - 0x30)
         doDecimalCharacterReference x
     | c == chrSemicolon -> do
         doNumericCharacterReferenceEnd x
     | otherwise -> do
         parseError x "missing-semicolon-after-character-reference"
         backWord x
         doNumericCharacterReferenceEnd x

-- 12.2.5.80 Numeric character reference end state
doNumericCharacterReferenceEnd :: Lexer s -> ST s ()
doNumericCharacterReferenceEnd x@Lexer {..} = do
  c <- rref lexerCode
  let n = fromIntegral c
  if | c == 0 -> do
         parseError x "null-character-reference"
         wref lexerCode 0xFFFD
     | c > 0x10FFFF -> do
         parseError x "character-reference-outside-unicode-range"
         wref lexerCode 0xFFFD
     | chrSurrogate c -> do
         parseError x "surrogate-character-reference"
         wref lexerCode 0xFFFD
     | chrNonCharacter c -> do
         parseError x "noncharacter-character-reference"
     | c == 0x0D || (chrWord8 c && chrControl n && not (chrWhitespace n)) -> do
         parseError x "control-character-reference"
         whenJust (Map.lookup c codeMap) $ wref lexerCode
     | otherwise ->
         pure ()
  bufferReset lexerBuffer
  forM_ (chrUTF8 c) $ flip bufferAppend lexerBuffer
  flushCodePoints x
  returnState x

-- | Character code map.
codeMap :: Map Int Int
codeMap = Map.fromList
  [ (0x80, 0x20AC)
  , (0x82, 0x201A)
  , (0x83, 0x0192)
  , (0x84, 0x201E)
  , (0x85, 0x2026)
  , (0x86, 0x2020)
  , (0x87, 0x2021)
  , (0x88, 0x02C6)
  , (0x89, 0x2030)
  , (0x8A, 0x0160)
  , (0x8B, 0x2039)
  , (0x8C, 0x0152)
  , (0x8E, 0x017D)
  , (0x91, 0x2018)
  , (0x92, 0x2019)
  , (0x93, 0x201C)
  , (0x94, 0x201D)
  , (0x95, 0x2022)
  , (0x96, 0x2013)
  , (0x97, 0x2014)
  , (0x98, 0x02DC)
  , (0x99, 0x2122)
  , (0x9A, 0x0161)
  , (0x9B, 0x203A)
  , (0x9C, 0x0153)
  , (0x9E, 0x017E)
  , (0x9F, 0x0178)
  ]
