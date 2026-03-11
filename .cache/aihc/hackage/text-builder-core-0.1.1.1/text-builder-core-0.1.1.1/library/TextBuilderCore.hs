{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module TextBuilderCore
  ( TextBuilder (..),

    -- * Destructors
    isEmpty,
    toText,

    -- * Constructors

    -- ** Text
    string,
    text,
    lazyText,

    -- ** Character
    char,
    unicodeCodepoint,

    -- ** Primitives
    unsafeChars,
    unsafeSeptets,
    unsafeReverseSeptets,
  )
where

import qualified Data.Text as Text
import qualified Data.Text.Array as TextArray
import qualified Data.Text.Internal as TextInternal
import qualified Data.Text.Lazy as TextLazy
import TextBuilderCore.Prelude

#if MIN_VERSION_text(2,0,0)
import qualified TextBuilderCore.Utf8View as Utf8View
#else
import qualified TextBuilderCore.Utf16View as Utf16View
#endif

-- |
-- Composable specification of how to efficiently construct strict 'Text'.
--
-- Provides instances of 'Semigroup' and 'Monoid', which have complexity of /O(1)/.
data TextBuilder
  = TextBuilder
      -- | Estimated maximum size of the byte array to allocate.
      --
      -- If the builder is empty it must be 0.
      -- Otherwise it must be greater than or equal to the amount of bytes to be written.
      --
      -- __Warning:__ Due to \"text\" switching from UTF-16 to UTF-8 since version 2, 'Word16' is used as the byte when the \"text\" version is @<2@ and 'Word8' is used when it's @>=2@.
      Int
      -- | Function that populates a preallocated byte array of the estimated maximum size specified above provided an offset into it and producing the offset after.
      --
      -- __Warning:__ The function must not write outside of the allocated array or bad things will happen to the running app.
      --
      -- __Warning:__ Keep in mind that the array is operating on 'Word8' values starting from @text-2.0@, but prior to it it operates on 'Word16'. This is due to the \"text\" library switching from UTF-16 to UTF-8 after version 2. To deal with this you have the following options:
      --
      -- 1. Restrict the version of the \"text\" library in your package to @>=2@.
      --
      -- 2. Use helpers provided by this library, such as 'unsafeSeptets' and 'unsafeReverseSeptets', which abstract over the differences in the underlying representation.
      --
      -- 3. Use CPP to conditionally compile your code for different versions of \"text\".
      (forall s. TextArray.MArray s -> Int -> ST s Int)

instance IsString TextBuilder where
  fromString = string

instance Show TextBuilder where
  show = show . toText

instance Eq TextBuilder where
  (==) = on (==) toText

instance Semigroup TextBuilder where
  {-# INLINE (<>) #-}
  (<>) (TextBuilder estimatedArraySizeL writeL) (TextBuilder estimatedArraySizeR writeR) =
    TextBuilder
      (estimatedArraySizeL + estimatedArraySizeR)
      ( \array offset -> do
          offsetAfter1 <- writeL array offset
          writeR array offsetAfter1
      )
  stimes n (TextBuilder maxSize write) =
    TextBuilder
      (maxSize * fromIntegral n)
      ( \array ->
          let go n offset =
                if n > 0
                  then do
                    offset <- write array offset
                    go (pred n) offset
                  else return offset
           in go n
      )

instance Monoid TextBuilder where
  {-# INLINE mempty #-}
  mempty = TextBuilder 0 (const return)
  {-# INLINE mconcat #-}
  mconcat list =
    TextBuilder
      (foldl' (\acc (TextBuilder maxSize _) -> acc + maxSize) 0 list)
      ( \array ->
          let go [] offset = return offset
              go (TextBuilder _ write : xs) offset = do
                offsetAfter <- write array offset
                go xs offsetAfter
           in go list
      )

instance Arbitrary TextBuilder where
  arbitrary = text . Text.pack <$> arbitrary
  shrink a = text . Text.pack <$> shrink (Text.unpack (toText a))

-- * Destructors

-- | Execute the builder producing a strict text.
toText :: TextBuilder -> Text
toText (TextBuilder maxSize write) =
  runST $ do
    array <- TextArray.new maxSize
    offsetAfter <- write array 0
    frozenArray <- TextArray.unsafeFreeze array
    return $ TextInternal.text frozenArray 0 offsetAfter

-- | Check whether the builder is empty.
{-# INLINE isEmpty #-}
isEmpty :: TextBuilder -> Bool
isEmpty (TextBuilder maxSize _) = maxSize == 0

-- * Constructors

-- | Construct from a list of characters.
{-# INLINE string #-}
string :: String -> TextBuilder
string string =
  unsafeChars (length string) string

-- | Strict text.
{-# INLINEABLE text #-}
text :: Text -> TextBuilder
#if MIN_VERSION_text(2,0,0)
text (TextInternal.Text array offset length) =
  TextBuilder length \builderArray builderOffset -> do
    TextArray.copyI length builderArray builderOffset array offset
    return $ builderOffset + length
#else
text (TextInternal.Text array offset length) =
  TextBuilder length \builderArray builderOffset -> do
    let builderOffsetAfter = builderOffset + length
    TextArray.copyI builderArray builderOffset array offset builderOffsetAfter
    return builderOffsetAfter
#endif

-- | Lazy text.
{-# INLINE lazyText #-}
lazyText :: TextLazy.Text -> TextBuilder
lazyText =
  TextLazy.foldrChunks (mappend . text) mempty

-- ** Codepoint

-- | Unicode character.
{-# INLINE char #-}
char :: Char -> TextBuilder
char = unicodeCodepoint . ord

-- | Safe Unicode codepoint with invalid values replaced by the @�@ char (codepoint @0xfffd@),
-- which is the same as what @Data.Text.'Data.Text.pack'@ does.
{-# INLINE unicodeCodepoint #-}
unicodeCodepoint :: Int -> TextBuilder
unicodeCodepoint = unsafeUnicodeCodepoint . project
  where
    project x =
      if x .&. 0x1ff800 /= 0xd800
        then x
        else 0xfffd

-- | Unicode codepoint.
--
-- __Warning:__ It is your responsibility to ensure that the codepoint is in proper range,
-- otherwise the produced text will be broken.
-- It must be in the range of 0x0000 to 0x10FFFF.
{-# INLINE unsafeUnicodeCodepoint #-}
unsafeUnicodeCodepoint :: Int -> TextBuilder
#if MIN_VERSION_text(2,0,0)
unsafeUnicodeCodepoint x =
  Utf8View.unicodeCodepoint x unsafeUtf8CodeUnits1 unsafeUtf8CodeUnits2 unsafeUtf8CodeUnits3 unsafeUtf8CodeUnits4
#else
unsafeUnicodeCodepoint x =
  Utf16View.unicodeCodepoint x unsafeUtf16CodeUnits1 unsafeUtf16CodeUnits2
#endif

-- | Single code-unit UTF-8 character.
unsafeUtf8CodeUnits1 :: Word8 -> TextBuilder
#if MIN_VERSION_text(2,0,0)
{-# INLINEABLE unsafeUtf8CodeUnits1 #-}
unsafeUtf8CodeUnits1 unit1 =
  TextBuilder 1 \array offset ->
    TextArray.unsafeWrite array offset unit1
      $> succ offset
#else
{-# INLINE unsafeUtf8CodeUnits1 #-}
unsafeUtf8CodeUnits1 unit1 =
  Utf16View.utf8CodeUnits1 unit1 unsafeUtf16CodeUnits1 unsafeUtf16CodeUnits2
#endif

-- | Double code-unit UTF-8 character.
unsafeUtf8CodeUnits2 :: Word8 -> Word8 -> TextBuilder
#if MIN_VERSION_text(2,0,0)
{-# INLINEABLE unsafeUtf8CodeUnits2 #-}
unsafeUtf8CodeUnits2 unit1 unit2 =
  TextBuilder 2 \array offset -> do
    TextArray.unsafeWrite array offset unit1
    TextArray.unsafeWrite array (offset + 1) unit2
    return $ offset + 2
#else
{-# INLINE unsafeUtf8CodeUnits2 #-}
unsafeUtf8CodeUnits2 unit1 unit2 =
  Utf16View.utf8CodeUnits2 unit1 unit2 unsafeUtf16CodeUnits1 unsafeUtf16CodeUnits2
#endif

-- | Triple code-unit UTF-8 character.
unsafeUtf8CodeUnits3 :: Word8 -> Word8 -> Word8 -> TextBuilder
#if MIN_VERSION_text(2,0,0)
{-# INLINEABLE unsafeUtf8CodeUnits3 #-}
unsafeUtf8CodeUnits3 unit1 unit2 unit3 =
  TextBuilder 3 \array offset -> do
    TextArray.unsafeWrite array offset unit1
    TextArray.unsafeWrite array (offset + 1) unit2
    TextArray.unsafeWrite array (offset + 2) unit3
    return $ offset + 3
#else
{-# INLINE unsafeUtf8CodeUnits3 #-}
unsafeUtf8CodeUnits3 unit1 unit2 unit3 =
  Utf16View.utf8CodeUnits3 unit1 unit2 unit3 unsafeUtf16CodeUnits1 unsafeUtf16CodeUnits2
#endif

-- | UTF-8 character out of 4 code units.
unsafeUtf8CodeUnits4 :: Word8 -> Word8 -> Word8 -> Word8 -> TextBuilder
#if MIN_VERSION_text(2,0,0)
{-# INLINEABLE unsafeUtf8CodeUnits4 #-}
unsafeUtf8CodeUnits4 unit1 unit2 unit3 unit4 =
  TextBuilder 4 \array offset -> do
    TextArray.unsafeWrite array offset unit1
    TextArray.unsafeWrite array (offset + 1) unit2
    TextArray.unsafeWrite array (offset + 2) unit3
    TextArray.unsafeWrite array (offset + 3) unit4
    return $ offset + 4
#else
{-# INLINE unsafeUtf8CodeUnits4 #-}
unsafeUtf8CodeUnits4 unit1 unit2 unit3 unit4 =
  Utf16View.utf8CodeUnits4 unit1 unit2 unit3 unit4 unsafeUtf16CodeUnits1 unsafeUtf16CodeUnits2
#endif

-- | Single code-unit UTF-16 character.
unsafeUtf16CodeUnits1 :: Word16 -> TextBuilder
#if MIN_VERSION_text(2,0,0)
{-# INLINE unsafeUtf16CodeUnits1 #-}
unsafeUtf16CodeUnits1 = unsafeUnicodeCodepoint . fromIntegral
#else
{-# INLINEABLE unsafeUtf16CodeUnits1 #-}
unsafeUtf16CodeUnits1 unit =
  TextBuilder 1 \array offset ->
    TextArray.unsafeWrite array offset unit
      $> succ offset
#endif

-- | Double code-unit UTF-16 character.
unsafeUtf16CodeUnits2 :: Word16 -> Word16 -> TextBuilder
#if MIN_VERSION_text(2,0,0)
{-# INLINE unsafeUtf16CodeUnits2 #-}
unsafeUtf16CodeUnits2 unit1 unit2 = unsafeUnicodeCodepoint cp
  where
    cp = (((fromIntegral unit1 .&. 0x3FF) `shiftL` 10) .|. (fromIntegral unit2 .&. 0x3FF)) + 0x10000
#else
{-# INLINEABLE unsafeUtf16CodeUnits2 #-}
unsafeUtf16CodeUnits2 unit1 unit2 =
  TextBuilder 2 \array offset -> do
    TextArray.unsafeWrite array offset unit1
    TextArray.unsafeWrite array (succ offset) unit2
    return $ offset + 2
#endif

-- * Basic Unsafe Primitives

-- |
-- Helper for constructing from char producers a bit more efficiently than via @(text . fromString)@.
--
-- >>> unsafeChars 3 "123"
-- "123"
--
-- >>> unsafeChars 4 "123"
-- "123"
{-# INLINE unsafeChars #-}
unsafeChars ::
  -- | Maximum size of the provided list of characters.
  --
  -- __Warning__: Must be greater than or equal to the length of the list.
  Int ->
  [Char] ->
  TextBuilder
#if MIN_VERSION_text(2,0,0)
unsafeChars maxChars chars =
  TextBuilder
    (maxChars * 4)
    ( \array ->
        foldr
          ( \char next offset ->
              Utf8View.unicodeCodepoint
                (ord char)
                ( \byte -> do
                    TextArray.unsafeWrite array offset byte
                    next (succ offset)
                )
                ( \byte1 byte2 -> do
                    TextArray.unsafeWrite array offset byte1
                    TextArray.unsafeWrite array (succ offset) byte2
                    next (offset + 2)
                )
                ( \byte1 byte2 byte3 -> do
                    TextArray.unsafeWrite array offset byte1
                    TextArray.unsafeWrite array (succ offset) byte2
                    TextArray.unsafeWrite array (offset + 2) byte3
                    next (offset + 3)
                )
                ( \byte1 byte2 byte3 byte4 -> do
                    TextArray.unsafeWrite array offset byte1
                    TextArray.unsafeWrite array (succ offset) byte2
                    TextArray.unsafeWrite array (offset + 2) byte3
                    TextArray.unsafeWrite array (offset + 3) byte4
                    next (offset + 4)
                )
          )
          return
          chars
    )
#else
unsafeChars maxChars chars =
  TextBuilder
    (maxChars * 2)
    ( \array ->
        foldr
          ( \char next offset ->
              Utf16View.unicodeCodepoint
                (ord char)
                ( \byte -> do
                    TextArray.unsafeWrite array offset byte
                    next (succ offset)
                )
                ( \byte1 byte2 -> do
                    TextArray.unsafeWrite array offset byte1
                    TextArray.unsafeWrite array (succ offset) byte2
                    next (offset + 2)
                )
          )
          return
          chars
    )
#endif

-- |
-- Provides a unified way to deal with the byte array regardless of the version of the @text@ library.
--
-- Keep in mind that prior to @text-2.0@, the array was operating on 'Word16' values due to the library abstracting over @UTF-16@.
-- Starting from @text-2.0@, the array operates on 'Word8' values and the library abstracts over @UTF-8@.
--
-- This function is useful for building ASCII values.
--
-- >>> unsafeSeptets 3 (fmap (+48) [1, 2, 3])
-- "123"
--
-- >>> unsafeSeptets 4 (fmap (+48) [1, 2, 3])
-- "123"
{-# INLINE unsafeSeptets #-}
unsafeSeptets ::
  -- | Maximum size of the byte array to allocate.
  --
  -- Must be greater than or equal to the length of the list.
  --
  -- __Warning:__ If it is smaller, bad things will happen.
  -- We'll be writing outside of the allocated array.
  Int ->
  -- | List of bytes to write.
  --
  -- __Warning:__ It is your responsibility to ensure that the bytes are smaller than 128.
  -- Otherwise the produced text will have a broken encoding.
  --
  -- To ensure of optimization kicking in it is advised to construct the list using 'GHC.List.build'.
  [Word8] ->
  TextBuilder
#if MIN_VERSION_text(2,0,0)
unsafeSeptets maxSize bytes =
  TextBuilder
    maxSize
    ( \array ->
        foldr
          ( \byte next offset -> do
              TextArray.unsafeWrite array offset byte
              next (succ offset)
          )
          return
          bytes
    )
#else
unsafeSeptets maxSize bytes =
  TextBuilder
    maxSize
    ( \array ->
        foldr
          ( \byte next offset -> do
              TextArray.unsafeWrite array offset (fromIntegral byte)
              next (succ offset)
          )
          return
          bytes
    )
#endif

-- | Same as 'unsafeSeptets', but writes the bytes in reverse order and requires the size to be precise.
--
-- >>> unsafeReverseSeptets 3 (fmap (+48) [1, 2, 3])
-- "321"
{-# INLINE unsafeReverseSeptets #-}
unsafeReverseSeptets ::
  -- | Precise amount of bytes in the list.
  --
  -- Needs to be precise, because writing happens in reverse order.
  --
  -- __Warning:__ If it is smaller, bad things will happen.
  -- We'll be writing outside of the allocated array.
  Int ->
  -- | List of bytes to write in reverse order.
  --
  -- __Warning:__ It is your responsibility to ensure that the bytes are smaller than 128.
  -- Otherwise the produced text will have a broken encoding.
  --
  -- To ensure of optimization kicking in it is advised to construct the list using 'GHC.List.build'.
  [Word8] ->
  TextBuilder
#if MIN_VERSION_text(2,0,0)
unsafeReverseSeptets preciseSize bytes =
  TextBuilder
    preciseSize
    ( \array startOffset ->
        let endOffset = startOffset + preciseSize
         in foldr
              ( \byte next offset -> do
                  TextArray.unsafeWrite array offset byte
                  next (pred offset)
              )
              (\_ -> return endOffset)
              bytes
              (pred endOffset)
    )
#else
unsafeReverseSeptets preciseSize bytes =
  TextBuilder
    preciseSize
    ( \array startOffset ->
        let endOffset = startOffset + preciseSize
         in foldr
              ( \byte next offset -> do
                  TextArray.unsafeWrite array offset (fromIntegral byte)
                  next (pred offset)
              )
              (\_ -> return endOffset)
              bytes
              (pred endOffset)
    )
#endif
