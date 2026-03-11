{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Defines a token type used by the lexer.
module Zenacy.HTML.Internal.Token
  ( Token(..)
  , TokenBuffer(..)
  , TAttr(..)
  , tokenAttr
  , tokenHasAttr
  , tokenGetAttr
  , tokenGetAttrVal
  , tokenBuffer
  , tokenCapacity
  , tokenReset
  , tokenTail
  , tokenFirst
  , tokenNext
  , tokenCount
  , tokenOffset
  , tokenList
  , tokenDrop
  , tokenHasEOF
  , tokenSlice
  , tokenTagStartName
  , tokenTagEndName
  , tokenDoctypeType
  , tokenTagStartType
  , tokenTagEndType
  , tokenCommentType
  , tokenCharType
  , tokenEOFType
  , tokenDoctypeInit
  , tokenDoctypeNameAppend
  , tokenDoctypeSetForceQuirks
  , tokenDoctypePublicIdInit
  , tokenDoctypePublicIdAppend
  , tokenDoctypeSystemIdInit
  , tokenDoctypeSystemIdAppend
  , tokenTagStartInit
  , tokenTagStartSetSelfClosing
  , tokenTagEndInit
  , tokenTagNameAppend
  , tokenAttrInit
  , tokenAttrNameAppend
  , tokenAttrValAppend
  , tokenAttrNamePrune
  , tokenCommentInit
  , tokenCommentAppend
  , tokenCharInit
  , tokenEOFInit
  , tokenType
  , tokenSize
  , tokenPack
  ) where

import Zenacy.HTML.Internal.BS
import Zenacy.HTML.Internal.Buffer
import Zenacy.HTML.Internal.Core
import Zenacy.HTML.Internal.Types
import Control.Monad
  ( when
  , forM
  )
import Control.Monad.Extra
  ( anyM
  )
import Control.Monad.ST
  ( ST
  )
import Data.STRef
  ( STRef
  , newSTRef
  , readSTRef
  , writeSTRef
  )
import Data.DList
  ( DList
  )
import qualified Data.DList as D
  ( empty
  , snoc
  , toList
  )
import Data.List
  ( find
  , or
  )
import Data.Maybe
  ( catMaybes
  , isJust
  )
import Data.Vector.Unboxed.Mutable
  ( MVector
  )
import qualified Data.Vector.Unboxed.Mutable as U
  ( new
  , length
  , read
  , write
  , grow
  )
import Data.Word
  ( Word8
  )

-- | Defines the token type.
-- The token type is used for testing and debugging only.
data Token
  = TDoctype
    { tDoctypeName   :: !BS
    , tDoctypeQuirks :: !Bool
    , tDoctypePublic :: !(Maybe BS)
    , tDoctypeSystem :: !(Maybe BS)
    }
  | TStart
    { tStartName     :: !BS
    , tStartClosed   :: !Bool
    , tStartAttr     :: ![TAttr]
    }
  | TEnd
    { tEndName       :: !BS
    }
  | TComment
    { tCommentData   :: !BS
    }
  | TChar
    { tCharData      :: !Word8
    }
  | TEOF
    deriving (Eq, Ord, Show)

-- | An HTML element attribute type.
data TAttr = TAttr
  { tAttrName      :: BS
  , tAttrVal       :: BS
  , tAttrNamespace :: HTMLAttrNamespace
  } deriving (Eq, Ord, Show)

-- | A type of buffer used to hold tokens.
data TokenBuffer s = TokenBuffer
  { tbCntl :: MVector s Int
  , tbData :: MVector s Word8
  }

-- | Makes an attribute.
tokenAttr :: BS -> BS -> TAttr
tokenAttr n v = TAttr n v HTMLAttrNamespaceNone

-- | Determines if a token has an attribute.
tokenHasAttr :: BS -> Token -> Bool
tokenHasAttr x t = isJust $ tokenGetAttr x t

-- | Finds an attribute in a token.
tokenGetAttr :: BS -> Token -> Maybe TAttr
tokenGetAttr x = \case
  TStart{..} -> find (\TAttr{..} -> tAttrName == x) tStartAttr
  _otherwise -> Nothing

-- | Finds an attribute value for a token.
tokenGetAttrVal :: BS -> Token -> Maybe BS
tokenGetAttrVal x t = tAttrVal <$> tokenGetAttr x t

-- | Makes a new token buffer.
tokenBuffer :: ST s (STRef s (TokenBuffer s))
tokenBuffer = do
  c <- U.new 100
  d <- U.new 100
  r <- newSTRef (TokenBuffer c d)
  tokenReset r
  pure r

-- | Gets the capacity of the buffer.
tokenCapacity :: STRef s (TokenBuffer s) -> ST s (Int, Int)
tokenCapacity r = do
  TokenBuffer{..} <- readSTRef r
  pure (U.length tbCntl, U.length tbData)

-- | Size of the buffer header.
headerSize :: Int
headerSize = 4

-- | Resets a token buffer.
tokenReset :: STRef s (TokenBuffer s) -> ST s ()
tokenReset r = do
  TokenBuffer{..} <- readSTRef r
  U.write tbCntl 0 1 -- data buffer length (skip first byte)
  U.write tbCntl 1 0 -- token list head offset
  U.write tbCntl 2 0 -- token list tail offset
  U.write tbCntl 3 0 -- token emit offset
  U.write tbData 0 0 -- first byte is unsed

-- | Get the token buffer tail offset.
tokenTail :: STRef s (TokenBuffer s) -> ST s Int
tokenTail r = do
  TokenBuffer{..} <- readSTRef r
  U.read tbCntl 2

-- | Positions the emitter to the first token and returns its offset.
tokenFirst :: STRef s (TokenBuffer s) -> ST s Int
tokenFirst r = do
  TokenBuffer{..} <- readSTRef r
  h <- U.read tbCntl 1
  U.write tbCntl 3 h
  pure h

-- | Positions the emitter to the next token and returns its offset.
tokenNext :: STRef s (TokenBuffer s) -> ST s Int
tokenNext r = do
  TokenBuffer{..} <- readSTRef r
  t <- U.read tbCntl 2
  i <- U.read tbCntl 3
  n <- U.read tbCntl (i + 1)
  if i == t
     then pure 0
     else do
       let j = i + n
       U.write tbCntl 3 j
       pure j

-- | Counts the number of tokens in the buffer.
tokenCount :: STRef s (TokenBuffer s) -> ST s Int
tokenCount r = tokenFold (\i n -> n + 1) 0 r

-- | Gets a list of the tokens in the buffer.
tokenOffset :: STRef s (TokenBuffer s) -> ST s [Int]
tokenOffset r = D.toList <$> tokenFold (flip D.snoc) D.empty r

-- | Gets a list of the tokens in the buffer.
tokenList :: STRef s (TokenBuffer s) -> ST s [Token]
tokenList r = tokenOffset r >>= mapM (flip tokenPack r)

-- | Performs a fold over the token offsets in the buffer.
tokenFold :: (Int -> a -> a) -> a -> STRef s (TokenBuffer s) -> ST s a
tokenFold f a r = do
  TokenBuffer{..} <- readSTRef r
  i <- U.read tbCntl 3
  x <- go a tokenFirst
  U.write tbCntl 3 i
  pure x
  where
    go b g = do
      i <- g r
      if i == 0
         then pure b
         else go (f i b) tokenNext

-- | Drops the last token from the buffer.
tokenDrop :: STRef s (TokenBuffer s) -> ST s ()
tokenDrop r = do
  TokenBuffer{..} <- readSTRef r
  headPtr <- U.read tbCntl 1
  tailPtr <- U.read tbCntl 2
  when (tailPtr > 0) $ do
    if tailPtr == headPtr
       then tokenReset r
       else go tbCntl tailPtr headPtr
  where
    go d t i = do
      n <- U.read d (i + offsetSize)
      if t == i + n
         then U.write d 2 i
         else go d t (i + n)

-- | Returns whether a buffer includes an EOF token.
tokenHasEOF :: STRef s (TokenBuffer s) -> ST s Bool
tokenHasEOF r =
  tokenOffset r >>= anyM isEOF
  where
    isEOF i = (== tokenEOFType) <$> tokenType i r

-- | Returns a slice of the data area of a token buffer.
tokenSlice :: Int -> Int -> STRef s (TokenBuffer s) -> ST s [Word8]
tokenSlice offset len r = do
  TokenBuffer{..} <- readSTRef r
  go tbData offset len D.empty
  where
    go d i 0 a = pure $ D.toList a
    go d i n a = do
      w <- U.read d i
      go d (i + 1) (n - 1) $ D.snoc a w

-- | Returns the start tag name at for the token at an offset.
tokenTagStartName :: Int -> STRef s (TokenBuffer s) -> ST s (Maybe [Word8])
tokenTagStartName = tagNameIfType tokenTagStartType

-- | Returns the end tag name at for the token at an offset.
tokenTagEndName :: Int -> STRef s (TokenBuffer s) -> ST s (Maybe [Word8])
tokenTagEndName = tagNameIfType tokenTagEndType

-- | Returns the start tag name at for the token at an offset.
tagNameIfType :: Int -> Int -> STRef s (TokenBuffer s) -> ST s (Maybe [Word8])
tagNameIfType t x r = do
  TokenBuffer{..} <- readSTRef r
  a <- tokenType x r
  if a == t
     then do
       o <- U.read tbCntl (x + 2)
       n <- U.read tbCntl (x + 3)
       Just <$> tokenSlice o n r
     else
       pure Nothing

-- | Defines the type for a DOCTYPE token.
tokenDoctypeType :: Int
tokenDoctypeType = 101

-- | Defines the type for a start tag token.
tokenTagStartType :: Int
tokenTagStartType = 102

-- | Defines the type for a end tag token.
tokenTagEndType :: Int
tokenTagEndType = 103

-- | Defines the type for a comment token.
tokenCommentType :: Int
tokenCommentType = 104

-- | Defines the type for a character token.
tokenCharType :: Int
tokenCharType = 105

-- | Defines the type for an EOF token.
tokenEOFType :: Int
tokenEOFType = 106

tokenInit :: Int -> STRef s (TokenBuffer s) -> ST s (MVector s Int, Int)
tokenInit maxIndex r = do
  t@TokenBuffer{..} <- readSTRef r
  tailPtr <- U.read tbCntl 2
  i <- if tailPtr == 0
       then do
         U.write tbCntl 1 headerSize
         U.write tbCntl 2 headerSize
         pure headerSize
       else do
         n <- U.read tbCntl (tailPtr + offsetSize)
         let j = tailPtr + n
         U.write tbCntl 2 j
         pure j
  c <- ensureCntl (i + maxIndex) t r
  pure (c, i)

-- | Defines the size of the doctype token.
tokenDoctypeSize :: Int
tokenDoctypeSize = 11

-- | Adds a new DOCTYPE token to the lexer.
tokenDoctypeInit :: STRef s (TokenBuffer s) -> ST s ()
tokenDoctypeInit r = do
  (c, i) <- tokenInit 10 r
  U.write c (i + 0) tokenDoctypeType
  U.write c (i + 1) tokenDoctypeSize
  U.write c (i + 2) 0 -- name offset
  U.write c (i + 3) 0 -- name length
  U.write c (i + 4) 0 -- force quirks flag
  U.write c (i + 5) 0 -- public id exists flag
  U.write c (i + 6) 0 -- public id offset
  U.write c (i + 7) 0 -- public id length
  U.write c (i + 8) 0 -- system id exists flag
  U.write c (i + 9) 0 -- system id offset
  U.write c (i + 10) 0 -- system id length

-- | Appends a character to the current DOCTYPE token.
tokenDoctypeNameAppend :: Word8 -> STRef s (TokenBuffer s) -> ST s ()
tokenDoctypeNameAppend = stringAppendTail 2

-- | Sets the force quirks flag for the current DOCTYPE.
tokenDoctypeSetForceQuirks :: STRef s (TokenBuffer s) -> ST s ()
tokenDoctypeSetForceQuirks r = do
  TokenBuffer{..} <- readSTRef r
  i <- U.read tbCntl 2
  U.write tbCntl (i + 4) 1

-- | Initializes the DOCTYPE public ID.
tokenDoctypePublicIdInit :: STRef s (TokenBuffer s) -> ST s ()
tokenDoctypePublicIdInit r = do
  TokenBuffer{..} <- readSTRef r
  i <- U.read tbCntl 2
  U.write tbCntl (i + 5) 1

-- | Appends a character to the DOCTYPE public ID.
tokenDoctypePublicIdAppend :: Word8 -> STRef s (TokenBuffer s) -> ST s ()
tokenDoctypePublicIdAppend = stringAppendTail 6

-- | Initializes the DOCTYPE system ID.
tokenDoctypeSystemIdInit :: STRef s (TokenBuffer s) -> ST s ()
tokenDoctypeSystemIdInit r = do
  TokenBuffer{..} <- readSTRef r
  i <- U.read tbCntl 2
  U.write tbCntl (i + 8) 1

-- | Appends a character to the DOCTYPE system ID.
tokenDoctypeSystemIdAppend :: Word8 -> STRef s (TokenBuffer s) -> ST s ()
tokenDoctypeSystemIdAppend = stringAppendTail 9

-- | Defines the size of an attribute record.
attrSize :: Int
attrSize = 4

-- | Defines the offset to the start of the attributes.
attrStart :: Int
attrStart = 6

-- | Defines the size of the start tag token.
tokenTagStartSize :: Int
tokenTagStartSize = 6

-- | Adds a new start tag to the lexer.
tokenTagStartInit :: STRef s (TokenBuffer s) -> ST s ()
tokenTagStartInit r = do
  (c, i) <- tokenInit 5 r
  U.write c (i + 0) tokenTagStartType
  U.write c (i + 1) tokenTagStartSize
  U.write c (i + 2) 0 -- name offset
  U.write c (i + 3) 0 -- name length
  U.write c (i + 4) 0 -- self closing flag
  U.write c (i + 5) 0 -- attribute count

-- | Defines the size of the end tag token.
tokenTagEndSize :: Int
tokenTagEndSize = 4

-- | Adds a new end tag to the lexer.
tokenTagEndInit :: STRef s (TokenBuffer s) -> ST s ()
tokenTagEndInit r = do
  (c, i) <- tokenInit 3 r
  U.write c (i + 0) tokenTagEndType
  U.write c (i + 1) tokenTagEndSize
  U.write c (i + 2) 0 -- name offset
  U.write c (i + 3) 0 -- name length

-- | Adds a new start tag to the lexer.
tokenTagStartSetSelfClosing :: STRef s (TokenBuffer s) -> ST s ()
tokenTagStartSetSelfClosing r = do
  TokenBuffer{..} <- readSTRef r
  i <- U.read tbCntl 2
  U.write tbCntl (i + 4) 1

-- | Appends a character to a tag name if the token is a tag.
tokenTagNameAppend :: Word8 -> STRef s (TokenBuffer s) -> ST s ()
tokenTagNameAppend = stringAppendTail 2

-- | Starts a new attribute
tokenAttrInit :: STRef s (TokenBuffer s) -> ST s ()
tokenAttrInit r = do
  t@TokenBuffer{..} <- readSTRef r
  tailPtr <- U.read tbCntl 2
  m <- U.read tbCntl (tailPtr + 1)
  n <- U.read tbCntl (tailPtr + 5)
  let i = attrSize * n + attrStart + tailPtr
  c <- ensureCntl (i + 3) t r
  U.write c (i + 0) 0 -- attribute name offset
  U.write c (i + 1) 0 -- attribute name length
  U.write c (i + 2) 0 -- attribute value offset
  U.write c (i + 3) 0 -- attribute value length
  U.write c (tailPtr + 1) (m + 4) -- add attr to size of token
  U.write c (tailPtr + 5) (n + 1)

-- | Appends a character to the latest attribute name.
tokenAttrNameAppend :: Word8 -> STRef s (TokenBuffer s) -> ST s ()
tokenAttrNameAppend x r = do
  t@TokenBuffer{..} <- readSTRef r
  tailPtr <- U.read tbCntl 2
  n <- U.read tbCntl (tailPtr + 5)
  let i = attrSize * (n - 1) + attrStart + tailPtr
  stringAppend (i + 0) x t r

-- | Appends a character to the latest attribute value.
tokenAttrValAppend :: Word8 -> STRef s (TokenBuffer s) -> ST s ()
tokenAttrValAppend x r = do
  t@TokenBuffer{..} <- readSTRef r
  tailPtr <- U.read tbCntl 2
  n <- U.read tbCntl (tailPtr + 5)
  let i = attrSize * (n - 1) + attrStart + tailPtr
  stringAppend (i + 2) x t r

-- | Checks for duplicate attribute names.
--   Refer to section 12.2.5.33 for details.
tokenAttrNamePrune :: Int -> STRef s (TokenBuffer s) -> ST s Bool
tokenAttrNamePrune x r = do
  t@TokenBuffer{..} <- readSTRef r
  a <- U.read tbCntl (x + offsetType)
  if a /= tokenTagStartType
  then pure False
  else do
    attrCount <- U.read tbCntl (x + 5)
    u <- forM [1 .. attrCount] $ \j -> do
      let i = (j - 1) * attrSize + attrStart + x
      no0 <- U.read tbCntl (i + 0)
      nc0 <- U.read tbCntl (i + 1)
      match <- forM [1 .. j - 1] $ \k -> do
        let m = (k - 1) * attrSize + attrStart + x
        no1 <- U.read tbCntl (m + 0)
        nc1 <- U.read tbCntl (m + 1)
        s0 <- tokenSlice no0 nc0 r
        s1 <- tokenSlice no1 nc1 r
        pure $ nc0 == nc1 && s0 == s1
      if or match
      then do
        U.write tbCntl (i + 0) 0
        U.write tbCntl (i + 1) 0
        pure True
      else
        pure False
    pure $ or u

-- | Defines the size of the comment token.
tokenCommentSize :: Int
tokenCommentSize = 4

-- | Adds a new comment token to the lexer.
tokenCommentInit :: STRef s (TokenBuffer s) -> ST s ()
tokenCommentInit r = do
  (c, i) <- tokenInit 3 r
  U.write c (i + 0) tokenCommentType
  U.write c (i + 1) tokenCommentSize
  U.write c (i + 2) 0 -- string offset
  U.write c (i + 3) 0 -- string length

-- | Appends a character to the current comment token.
tokenCommentAppend :: Word8 -> STRef s (TokenBuffer s) -> ST s ()
tokenCommentAppend = stringAppendTail 2

-- | Defines the size of the char token.
tokenCharSize :: Int
tokenCharSize = 3

-- | Initializes a text token.
tokenCharInit :: Word8 -> STRef s (TokenBuffer s) -> ST s ()
tokenCharInit x r = do
  t <- readSTRef r
  (c, i) <- tokenInit 2 r
  U.write c (i + 0) tokenCharType
  U.write c (i + 1) tokenCharSize
  U.write c (i + 2) (fromIntegral x)

-- | Defines the size of the eof token.
tokenEOFSize :: Int
tokenEOFSize = 2

-- | Initializes an EOF token.
tokenEOFInit :: STRef s (TokenBuffer s) -> ST s ()
tokenEOFInit r = do
  (c, i) <- tokenInit 2 r
  U.write c (i + 0) tokenEOFType
  U.write c (i + 1) tokenEOFSize

-- | Defines the token offset for the type field.
offsetType :: Int
offsetType = 0

-- | Defines the token offset for the size field.
offsetSize :: Int
offsetSize = 1

-- | Gets the type of a token.
tokenType :: Int -> STRef s (TokenBuffer s) -> ST s Int
tokenType x r = do
  TokenBuffer{..} <- readSTRef r
  U.read tbCntl (x + offsetType)

-- | Gets the size of a token.
tokenSize :: Int -> STRef s (TokenBuffer s) -> ST s Int
tokenSize x r = do
  TokenBuffer{..} <- readSTRef r
  U.read tbCntl (x + offsetSize)

-- | Unpacks a token at the specified index.
tokenPack :: Int -> STRef s (TokenBuffer s) -> ST s Token
tokenPack x r = do
  TokenBuffer{..} <- readSTRef r
  t <- U.read tbCntl x
  n <- U.read tbCntl 0
  s <- bufferString tbData n
  if | t == tokenDoctypeType ->  packDoctype x tbCntl s
     | t == tokenTagStartType -> packTagStart x tbCntl s
     | t == tokenTagEndType ->   packTagEnd x tbCntl s
     | t == tokenCommentType ->  packComment x tbCntl s
     | t == tokenCharType ->     packChar x tbCntl
     | t == tokenEOFType ->      pure TEOF
     | otherwise ->              pure TEOF

-- | Unpacks a doctype token.
packDoctype :: Int -> MVector s Int -> BS -> ST s Token
packDoctype x tbCntl bs = do
  nameOffset <- U.read tbCntl (x + 2)
  nameLen    <- U.read tbCntl (x + 3)
  quirks     <- U.read tbCntl (x + 4)
  pubExists  <- U.read tbCntl (x + 5)
  pubOffset  <- U.read tbCntl (x + 6)
  pubLen     <- U.read tbCntl (x + 7)
  sysExists  <- U.read tbCntl (x + 8)
  sysOffset  <- U.read tbCntl (x + 9)
  sysLen     <- U.read tbCntl (x + 10)
  pure $ TDoctype
    (bsPart nameOffset nameLen bs)
    (quirks == 1)
    (if pubExists == 1 then Just (bsPart pubOffset pubLen bs) else Nothing)
    (if sysExists == 1 then Just (bsPart sysOffset sysLen bs) else Nothing)

-- | Unpacks an start tag token.
packTagStart :: Int -> MVector s Int -> BS -> ST s Token
packTagStart x tbCntl bs = do
  nameOffset <- U.read tbCntl (x + 2)
  nameLen    <- U.read tbCntl (x + 3)
  selfClose  <- U.read tbCntl (x + 4)
  attrCount  <- U.read tbCntl (x + 5)
  attr <- forM [1 .. attrCount] $ \j -> do
    let i = (j - 1) * attrSize + attrStart + x
    no <- U.read tbCntl (i + 0)
    nc <- U.read tbCntl (i + 1)
    ao <- U.read tbCntl (i + 2)
    ac <- U.read tbCntl (i + 3)
    pure $
      if no > 0
      then Just $ TAttr
        (bsPart no nc bs)
        (bsPart ao ac bs)
        HTMLAttrNamespaceNone
      else Nothing
  pure $ TStart
    (bsPart nameOffset nameLen bs)
    (selfClose == 1)
    (catMaybes attr)

-- | Unpacks an end tag token.
packTagEnd :: Int -> MVector s Int -> BS -> ST s Token
packTagEnd x tbCntl bs = do
  nameOffset <- U.read tbCntl (x + 2)
  nameLen    <- U.read tbCntl (x + 3)
  pure $ TEnd $ bsPart nameOffset nameLen bs

-- | Unpacks a comment token.
packComment :: Int -> MVector s Int -> BS -> ST s Token
packComment x tbCntl bs = do
  o <- U.read tbCntl (x + 2)
  n <- U.read tbCntl (x + 3)
  pure $ TComment $ bsPart o n bs

-- | Unpacks a character token.
packChar :: Int -> MVector s Int -> ST s Token
packChar x tbCntl = do
  c <- U.read tbCntl (x + 2)
  pure $ TChar $ fromIntegral c

-- | Appends a character to a string at an offset in the current token.
stringAppendTail :: Int -> Word8 -> STRef s (TokenBuffer s) -> ST s ()
stringAppendTail offset word r = do
  t@TokenBuffer{..} <- readSTRef r
  tailPtr <- U.read tbCntl 2
  stringAppend (tailPtr + offset) word t r
{-# INLINE stringAppendTail #-}

-- | Appends a character to a string at an offset.
stringAppend :: Int -> Word8 -> TokenBuffer s -> STRef s (TokenBuffer s) -> ST s ()
stringAppend x word t@TokenBuffer{..} r = do
  let y = x + 1
  i <- U.read tbCntl 0
  o <- U.read tbCntl x
  n <- U.read tbCntl y
  d <- ensureData i t r
  U.write d i word
  U.write tbCntl 0 (i + 1)
  U.write tbCntl y (n + 1)
  when (o == 0) $
    U.write tbCntl x i
{-# INLINE stringAppend #-}

-- | Ensures that the control buffer has enough space and grows it if needed.
ensureCntl :: Int -> TokenBuffer s -> STRef s (TokenBuffer s) -> ST s (MVector s Int)
ensureCntl x TokenBuffer{..} r
  | x < U.length tbCntl =
      pure tbCntl
  | otherwise = do
      v <- U.grow tbCntl $ U.length tbCntl
      writeSTRef r $ TokenBuffer v tbData
      pure v
{-# INLINE ensureCntl #-}

-- | Ensures that the data buffer has enough space and grows it if needed.
ensureData :: Int -> TokenBuffer s -> STRef s (TokenBuffer s) -> ST s (MVector s Word8)
ensureData x TokenBuffer{..} r
  | x < U.length tbData =
      pure tbData
  | otherwise = do
      v <- U.grow tbData $ U.length tbData
      writeSTRef r $ TokenBuffer tbCntl v
      pure v
{-# INLINE ensureData #-}
