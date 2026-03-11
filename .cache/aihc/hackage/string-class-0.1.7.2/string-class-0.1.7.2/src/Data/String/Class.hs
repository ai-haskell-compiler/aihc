{-# LANGUAGE TypeFamilies, FlexibleContexts, TypeSynonymInstances, ExistentialQuantification, DeriveDataTypeable, FlexibleInstances, UndecidableInstances #-}

module Data.String.Class
    ( Stringy
    , StringCells(..)
    , StringCell(..)
    , StringRWIO(..)
    , ConvGenString(..)
    , ConvString(..)
    , ConvStrictByteString(..)
    , ConvLazyByteString(..)
    , ConvText(..)
    , GenString(..)
    , GenStringDefault
    ) where

import Prelude hiding (head, tail, last, init, take, drop, length, null, concat, putStr, getContents)
import Control.Applicative hiding (empty)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as SC
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Int
import qualified Data.List as List
import Data.Semigroup
import Data.Monoid hiding ((<>))
import Data.String (IsString)
import qualified Data.String
import Data.Tagged
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTE
import qualified Data.Text.Lazy.IO as LT
import Data.Typeable
import Data.Word
import qualified System.IO as IO

-- | String super class
class    (StringCells s, StringRWIO s) => Stringy s
instance (StringCells s, StringRWIO s) => Stringy s

-- | Minimal complete definition: StringCellChar; StringCellAltChar; toStringCells; fromStringCells; toMainChar; toAltChar; cons; snoc; either all of head, tail, last, and init, or all of uncons and unsnoc; take, take64 or genericTake; drop, drop64, or genericDrop; and length, length64, or genericLength
class (Eq s, Monoid s, IsString s, Typeable s, StringCell (StringCellChar s), StringCell (StringCellAltChar s), ConvGenString s, ConvString s, ConvStrictByteString s, ConvLazyByteString s, ConvText s, ConvLazyText s) => StringCells s where
    type StringCellChar s
    type StringCellAltChar s

    toStringCells   :: (StringCells s2) => s  -> s2
    fromStringCells :: (StringCells s2) => s2 -> s

    infixr 9 `cons`
    infixr 9 `uncons`
    infixr 9 `altCons`
    infixr 9 `altUncons`
    cons      :: StringCellChar s -> s -> s
    uncons    :: s -> (StringCellChar s, s)
    snoc      :: s -> StringCellChar s -> s
    unsnoc    :: s -> (s, StringCellChar s)
    altCons   :: StringCellAltChar s -> s -> s
    altUncons :: s -> (StringCellAltChar s, s)
    altSnoc   :: s -> StringCellAltChar s -> s
    altUnsnoc :: s -> (s, StringCellAltChar s)

    toMainChar :: (StringCell c) => c -> Tagged s (StringCellChar s)
    toAltChar  :: (StringCell c) => c -> Tagged s (StringCellAltChar s)

    -- | Append two strings
    infixr 9 `append`
    append :: s -> s -> s
    concat :: [s] -> s

    empty :: s
    null :: s -> Bool

    head :: s -> StringCellChar s
    tail :: s -> s
    last :: s -> StringCellChar s
    init :: s -> s
    altHead :: s -> StringCellAltChar s
    altLast :: s -> StringCellAltChar s

    -- | Construction of a string; implementations should behave safely with incorrect lengths
    --
    -- The default implementation of 'unfoldr' is independent from that of 'altUnfoldr',
    -- as well as 'unfoldrN' as and 'altUnfoldrN'.
    unfoldr       ::          (a -> Maybe (StringCellChar    s, a)) -> a -> s
    altUnfoldr    ::          (a -> Maybe (StringCellAltChar s, a)) -> a -> s
    unfoldrN      :: Int   -> (a -> Maybe (StringCellChar    s, a)) -> a -> s
    altUnfoldrN   :: Int   -> (a -> Maybe (StringCellAltChar s, a)) -> a -> s
    unfoldrN64    :: Int64 -> (a -> Maybe (StringCellChar    s, a)) -> a -> s
    altUnfoldrN64 :: Int64 -> (a -> Maybe (StringCellAltChar s, a)) -> a -> s

    unfoldr f b =
        case f b of
            (Just (a, new_b)) -> a `cons` unfoldr f new_b
            (Nothing)         -> empty

    altUnfoldr f b =
        case f b of
            (Just (a, new_b)) -> a `altCons` altUnfoldr f new_b
            (Nothing)         -> empty
    unfoldrN    = const unfoldr
    altUnfoldrN = const altUnfoldr

    unfoldrN64 l f z = unfoldrN (fromIntegral l) f z

    altUnfoldrN64 l f z = altUnfoldrN (fromIntegral l) f z

    -- | Get the character at the given position
    --
    -- Just like 'drop', 'drop64', and the variants of those functions, the
    -- default definitions of these three variants are independent of each
    -- other, and are defined in terms of 'head' and 'tail', which can be
    -- inefficient.
    index   :: s -> Int   -> StringCellChar s
    index64 :: s -> Int64 -> StringCellChar s
    -- | Index a string at any location
    --
    -- Just like the other 'generic' functions of this module, this function
    -- can be significantly slower than 'index', since the function must be
    -- able to support arbitrarily large indices.  Consider using 'index' or
    -- 'index64', even if you need to coerce the index to an 'Int'.
    genericIndex :: (Integral i) => s -> i -> StringCellChar s

    take        :: Int -> s -> s
    take64      :: Int64 -> s -> s
    genericTake :: (Integral i) => i -> s -> s
    drop        :: Int -> s -> s
    drop64      :: Int64 -> s -> s
    genericDrop :: (Integral i) => i -> s -> s

    length        :: s -> Int
    length64      :: s -> Int64
    genericLength :: (Integral i) => s -> i

    safeUncons        :: s -> Maybe ((StringCellChar s), s)
    safeUnsnoc        :: s -> Maybe (s, (StringCellChar s))
    safeAltUncons     :: s -> Maybe ((StringCellAltChar s), s)
    safeAltUnsnoc     :: s -> Maybe (s, (StringCellAltChar s))
    safeHead          :: s -> Maybe (StringCellChar s)
    safeTail          :: s -> Maybe s
    safeLast          :: s -> Maybe (StringCellChar s)
    safeInit          :: s -> Maybe s
    safeAltHead       :: s -> Maybe (StringCellAltChar s)
    safeAltLast       :: s -> Maybe (StringCellAltChar s)
    safeIndex         :: s -> Int   -> Maybe (StringCellChar s)
    safeIndex64       :: s -> Int64 -> Maybe (StringCellChar s)
    safeGenericIndex  :: (Integral i) => s -> i -> Maybe (StringCellChar s)
    safeTake          :: Int -> s -> Maybe s
    safeTake64        :: Int64 -> s -> Maybe s
    safeGenericTake   :: (Integral i) => i -> s -> Maybe s
    safeDrop          :: Int -> s -> Maybe s
    safeDrop64        :: Int64 -> s -> Maybe s
    safeGenericDrop   :: (Integral i) => i -> s -> Maybe s
    safeUncons2       :: s -> Maybe ((StringCellChar s), (StringCellChar s), s)
    safeUncons3       :: s -> Maybe ((StringCellChar s), (StringCellChar s), (StringCellChar s), s)
    safeUncons4       :: s -> Maybe ((StringCellChar s), (StringCellChar s), (StringCellChar s), (StringCellChar s), s)

    infixr 9 `cons2`
    infixr 9 `cons3`
    infixr 9 `cons4`
    infixr 9 `uncons2`
    infixr 9 `uncons3`
    infixr 9 `uncons4`
    cons2   :: StringCellChar s -> StringCellChar s -> s -> s
    cons3   :: StringCellChar s -> StringCellChar s -> StringCellChar s -> s -> s
    cons4   :: StringCellChar s -> StringCellChar s -> StringCellChar s -> StringCellChar s -> s -> s
    uncons2 :: s -> (StringCellChar s, StringCellChar s, s)
    uncons3 :: s -> (StringCellChar s, StringCellChar s, StringCellChar s, s)
    uncons4 :: s -> (StringCellChar s, StringCellChar s, StringCellChar s, StringCellChar s, s)

    altCons c s = cons (s `untagTypeOf` toMainChar c) s
    altSnoc s c = snoc s (s `untagTypeOf` toMainChar c)
    altUncons s = (\ ~(a, s') -> (s `untagTypeOf` toAltChar a, s')) $ uncons s
    altUnsnoc s = (\ ~(s', a) -> (s', s `untagTypeOf` toAltChar a)) $ unsnoc s

    append = mappend
    concat = mconcat
    empty  = mempty
    null   = (== mempty)

    head = fst . uncons
    tail = snd . uncons
    last = snd . unsnoc
    init = fst . unsnoc
    altHead s = (s `untagTypeOf`) . toAltChar . head $ s
    altLast s = (s `untagTypeOf`) . toAltChar . last $ s

    index        s 0 = head s
    index        s n = (flip index $ pred n) . tail $ s
    index64      s 0 = head s
    index64      s n = (flip index64 $ pred n) . tail $ s
    genericIndex s 0 = head s
    genericIndex s n = (flip genericIndex $ pred n) . tail $ s

    take        n s = take64      (fromIntegral n) s
    take64      n s = genericTake (fromIntegral n  :: Integer) s
    genericTake n s = take        (fromIntegral n) s
    drop        n s = drop64      (fromIntegral n) s
    drop64      n s = genericDrop (fromIntegral n  :: Integer) s
    genericDrop n s = drop        (fromIntegral n) s

    length        = fromIntegral . length64
    length64      = (fromIntegral :: Integer -> Int64) . genericLength
    genericLength = fromIntegral . length

    {-
    -- More efficient default implementation provided above
    append a b = case safeUncons a of
        (Just (c, cs)) -> c `cons` append cs b
        (Nothing)      -> a

    concat = foldr append empty
    -}

    uncons s = (head s, tail s)
    unsnoc s = (init s, last s)

    cons2 a b s = a `cons` b `cons` s
    cons3 a b c s = a `cons` b `cons` c `cons` s
    cons4 a b c d s = a `cons` b `cons` c `cons` d `cons` s
    uncons2 s       =
        let (a, s')   = uncons s
            (b, s'')  = uncons s'
        in  (a, b, s'')
    uncons3 s       =
        let (a, s')   = uncons s
            (b, s'')  = uncons s'
            (c, s''') = uncons s''
        in  (a, b, c, s''')
    uncons4 s       =
        let (a, s')    = uncons s
            (b, s'')   = uncons s'
            (c, s''')  = uncons s''
            (d, s'''') = uncons s'''
        in  (a, b, c, d, s'''')

    safeUncons s
        | null s    = Nothing
        | otherwise = Just $ uncons s
    safeUnsnoc s
        | null s    = Nothing
        | otherwise = Just $ unsnoc s
    safeAltUncons s
        | null s    = Nothing
        | otherwise = Just $ altUncons s
    safeAltUnsnoc s
        | null s    = Nothing
        | otherwise = Just $ altUnsnoc s
    safeHead s
        | null s    = Nothing
        | otherwise = Just $ head s
    safeTail s
        | null s    = Nothing
        | otherwise = Just $ tail s
    safeLast s
        | null s    = Nothing
        | otherwise = Just $ last s
    safeInit s
        | null s    = Nothing
        | otherwise = Just $ init s
    safeAltHead s
        | null s    = Nothing
        | otherwise = Just $ altHead s
    safeAltLast s
        | null s    = Nothing
        | otherwise = Just $ altLast s
    safeIndex s n
        | length s <= n = Nothing
        | otherwise     = Just $ s `index` n
    safeIndex64 s n
        | length64 s <= n = Nothing
        | otherwise     = Just $ s `index64` n
    safeGenericIndex s n
        | genericLength s <= n = Nothing
        | otherwise            = Just $ s `genericIndex` n
    safeTake n s
        | n > length s = Nothing
        | otherwise    = Just $ take n s
    safeTake64 n s
        | n > length64 s = Nothing
        | otherwise      = Just $ take64 n s
    safeGenericTake n s
        | n > genericLength s = Nothing
        | otherwise           = Just $ genericTake n s
    safeDrop n s
        | n > length s = Nothing
        | otherwise    = Just $ drop n s
    safeDrop64 n s
        | n > length64 s = Nothing
        | otherwise      = Just $ drop64 n s
    safeGenericDrop n s
        | n > genericLength s = Nothing
        | otherwise           = Just $ genericDrop n s
    safeUncons2 s = do
        (a, s')    <- safeUncons s
        (b, s'')   <- safeUncons s'
        return (a, b, s'')
    safeUncons3 s = do
        (a, s')    <- safeUncons s
        (b, s'')   <- safeUncons s'
        (c, s''')  <- safeUncons s''
        return (a, b, c, s''')
    safeUncons4 s = do
        (a, s')    <- safeUncons s
        (b, s'')   <- safeUncons s'
        (c, s''')  <- safeUncons s''
        (d, s'''') <- safeUncons s'''
        return (a, b, c, d, s'''')

class StringCell c where
    toChar     :: c      -> Char
    toWord8    :: c      -> Word8
    toWord16   :: c      -> Word16
    toWord32   :: c      -> Word32
    toWord64   :: c      -> Word64
    fromChar   :: Char   -> c
    fromWord8  :: Word8  -> c
    fromWord16 :: Word16 -> c
    fromWord32 :: Word32 -> c
    fromWord64 :: Word64 -> c

class ConvGenString s where
    toGenString   :: s -> GenString
    fromGenString :: GenString -> s

class ConvString s where
    toString   :: s -> String
    fromString :: String -> s

class ConvStrictByteString s where
    toStrictByteString :: s -> S.ByteString
    fromStrictByteString :: S.ByteString -> s

class ConvLazyByteString s where
    toLazyByteString :: s -> L.ByteString
    fromLazyByteString :: L.ByteString -> s

class ConvText s where
    toText :: s -> T.Text
    fromText :: T.Text -> s

class ConvLazyText s where
    toLazyText :: s -> LT.Text
    fromLazyText :: LT.Text -> s

-- | Minimal complete definition: 'hGetContents', 'hGetLine', 'hPutStr', and 'hPutStrLn'
class StringRWIO s where
    --- Handles

    -- | Read n bytes *or* characters, depending on the implementation into a
    -- ByteString, directly from the specified Handle
    --
    -- Whether or not this function is lazy depends on the instance; laziness
    -- is preferred.
    hGetContents :: IO.Handle -> IO s

    -- | Read a single line from a handle
    hGetLine :: IO.Handle -> IO s

    -- | Write a string to a handle
    hPutStr :: IO.Handle -> s -> IO ()

    -- | Write a string to a handle, followed by a newline
    --
    -- N.B.: implementations might not define this atomically.  If the state
    -- of being atomic is necessary, one possible solution is to convert a
    -- string to an efficient type for which 'hPutStrLn' is atomic.
    hPutStrLn :: IO.Handle -> s -> IO ()

    --- Special cases for standard input and output

    -- | Take a function of type Text -> Text as its argument
    --
    -- The entire input from the standard input device is passed to this
    -- function as its argument, and the resulting string is output on the
    -- standard output device.
    interact :: (s -> s) -> IO ()
    interact f = putStr . f =<< getContents

    -- | Read all user input on 'stdin' as a single string
    getContents :: IO s
    getContents = hGetContents IO.stdin

    -- | Read a single line of user input from 'stdin'
    getLine :: IO s
    getLine = hGetLine IO.stdin

    -- | Write a string to 'stdout'
    putStr :: s -> IO ()
    putStr = hPutStr IO.stdout

    -- | Write a string to 'stdout', followed by a newline
    putStrLn :: s -> IO ()
    putStrLn = hPutStrLn IO.stdout

    ---

    -- | Read a file and returns the contents of the file as a string
    --
    -- Depending on the instance, this function might expect the file to be
    -- non-binary.  The default definition uses 'openFile' to open the file.
    readFile :: FilePath -> IO s
    readFile fn = hGetContents =<< IO.openFile fn IO.ReadMode

    -- | Write a string to a file
    --
    -- The file is truncated to zero length before writing begins.
    -- The default definition uses 'withFile' to open the file.
    writeFile :: FilePath -> s -> IO ()
    writeFile fn s = IO.withFile fn IO.WriteMode $ \hdl -> hPutStr hdl s

    -- | Write a string to the end of a file
    --
    -- The default definition uses 'withFile' to open the file.
    appendFile :: FilePath -> s -> IO ()
    appendFile fn s = IO.withFile fn IO.AppendMode $ \hdl -> hPutStr hdl s



instance StringCells String where
    type StringCellChar    String = Char
    type StringCellAltChar String = Char

    toStringCells   = fromString
    fromStringCells = toString

    length = List.genericLength
    empty  = []
    null   = List.null
    cons          = (:)
    snoc s c      = s ++ [c]
    safeUncons (x:xs) = Just (x, xs)
    safeUncons _      = Nothing
    uncons (x:xs) = (x, xs)
    uncons _      = error "String.uncons: null string"
    toMainChar    = Tagged . toChar
    toAltChar     = Tagged . toChar
    head          = List.head
    tail          = List.tail
    init          = List.init
    last          = List.last
    unfoldr       = List.unfoldr
    index         = (List.!!)
    index64 s     = index s . fromIntegral
    genericIndex  = List.genericIndex
    take          = List.take
    genericTake   = List.genericTake
    drop          = List.drop
    genericDrop   = List.genericDrop
    append        = (List.++)
    concat        = List.concat

instance StringCells S.ByteString where
    type StringCellChar    S.ByteString = Word8
    type StringCellAltChar S.ByteString = Char

    toStringCells   = fromStrictByteString
    fromStringCells = toStrictByteString

    length          = S.length
    empty           = S.empty
    null            = S.null
    cons            = S.cons
    snoc            = S.snoc
    safeUncons      = S.uncons
    uncons          = maybe (error "StringCells.Data.ByteString.ByteString.uncons: string is null") id . safeUncons
    toMainChar      = Tagged . toWord8
    toAltChar       = Tagged . toChar
    head            = S.head
    tail            = S.tail
    init            = S.init
    last            = S.last
    unfoldr         = S.unfoldr
    altUnfoldr      = SC.unfoldr
    unfoldrN        = ((fst .) .) . S.unfoldrN
    altUnfoldrN     = ((fst .) .) . SC.unfoldrN
    index           = S.index
    index64 s       = index s . fromIntegral
    take            = S.take
    drop            = S.drop
    append          = S.append
    concat          = S.concat

instance StringCells L.ByteString where
    type StringCellChar    L.ByteString = Word8
    type StringCellAltChar L.ByteString = Char

    toStringCells   = fromLazyByteString
    fromStringCells = toLazyByteString

    length64        = L.length
    length          = fromIntegral . length64
    empty           = L.empty
    null            = L.null
    cons            = L.cons
    snoc            = L.snoc
    safeUncons      = L.uncons
    uncons          = maybe (error "StringCells.Data.ByteString.Lazy.ByteString.uncons: string is null") id . safeUncons
    toMainChar      = Tagged . toWord8
    toAltChar       = Tagged . toChar
    head            = L.head
    tail            = L.tail
    init            = L.init
    last            = L.last
    unfoldr         = L.unfoldr
    altUnfoldr      = LC.unfoldr
    index s         = index64 s . fromIntegral
    index64         = L.index
    take64          = L.take
    drop64          = L.drop
    append          = L.append
    concat          = L.concat

instance StringCells T.Text where
    type StringCellChar    T.Text = Char
    type StringCellAltChar T.Text = Char

    toStringCells   = fromText
    fromStringCells = toText

    length          = T.length
    empty           = T.empty
    null            = T.null
    cons            = T.cons
    safeUncons      = T.uncons
    uncons          = maybe (error "StringCells.Data.Text.Text.uncons: string is null") id . safeUncons
    snoc            = T.snoc
    altSnoc         = T.snoc
    toMainChar      = Tagged . toChar
    toAltChar       = Tagged . toChar
    head            = T.head
    tail            = T.tail
    init            = T.init
    last            = T.last
    unfoldr         = T.unfoldr
    altUnfoldr      = T.unfoldr
    unfoldrN        = T.unfoldrN
    altUnfoldrN     = T.unfoldrN
    index           = T.index
    index64 s       = index s . fromIntegral
    append          = T.append
    concat          = T.concat

instance StringCells LT.Text where
    type StringCellChar    LT.Text = Char
    type StringCellAltChar LT.Text = Char

    toStringCells   = fromLazyText
    fromStringCells = toLazyText

    length64        = LT.length
    empty           = LT.empty
    null            = LT.null
    cons            = LT.cons
    safeUncons      = LT.uncons
    uncons          = maybe (error "StringCells.Data.Text.Lazy.Text.uncons: string is null") id . safeUncons
    snoc            = LT.snoc
    altSnoc         = LT.snoc
    toMainChar      = Tagged . toChar
    toAltChar       = Tagged . toChar
    head            = LT.head
    tail            = LT.tail
    init            = LT.init
    last            = LT.last
    unfoldr         = LT.unfoldr
    altUnfoldr      = LT.unfoldr
    unfoldrN64      = LT.unfoldrN
    altUnfoldrN64   = LT.unfoldrN
    index s         = index64 s . fromIntegral
    index64         = LT.index
    append          = LT.append
    concat          = LT.concat

instance StringCell Char where
    toChar     = id
    toWord8    = BI.c2w
    toWord16   = fromIntegral . toWord8
    toWord32   = fromIntegral . toWord8
    toWord64   = fromIntegral . toWord8
    fromChar   = id
    fromWord8  = BI.w2c
    fromWord16 = BI.w2c . fromIntegral
    fromWord32 = BI.w2c . fromIntegral
    fromWord64 = BI.w2c . fromIntegral

instance StringCell Word8 where
    toChar     = BI.w2c
    toWord8    = id
    toWord16   = fromIntegral
    toWord32   = fromIntegral
    toWord64   = fromIntegral
    fromChar   = BI.c2w
    fromWord8  = id
    fromWord16 = fromIntegral
    fromWord32 = fromIntegral
    fromWord64 = fromIntegral

instance StringCell Word16 where
    toChar     = BI.w2c . fromIntegral
    toWord8    = fromIntegral
    toWord16   = id
    toWord32   = fromIntegral
    toWord64   = fromIntegral
    fromChar   = fromIntegral . BI.c2w
    fromWord8  = fromIntegral
    fromWord16 = id
    fromWord32 = fromIntegral
    fromWord64 = fromIntegral

instance StringCell Word32 where
    toChar     = BI.w2c . fromIntegral
    toWord8    = fromIntegral
    toWord16   = fromIntegral
    toWord32   = id
    toWord64   = fromIntegral
    fromChar   = fromIntegral . BI.c2w
    fromWord8  = fromIntegral
    fromWord16 = fromIntegral
    fromWord32 = id
    fromWord64 = fromIntegral

instance StringCell Word64 where
    toChar     = BI.w2c . fromIntegral
    toWord8    = fromIntegral
    toWord16   = fromIntegral
    toWord32   = fromIntegral
    toWord64   = id
    fromChar   = fromIntegral . BI.c2w
    fromWord8  = fromIntegral
    fromWord16 = fromIntegral
    fromWord32 = fromIntegral
    fromWord64 = id

instance ConvGenString GenString where
    toGenString   = id
    fromGenString = id

instance ConvGenString String where
    toGenString      = GenString
    fromGenString _s = case _s of
        (GenString _s) -> toStringCells _s

instance ConvGenString SC.ByteString where
    toGenString      = GenString
    fromGenString _s = case _s of
        (GenString _s) -> toStringCells _s

instance ConvGenString LC.ByteString where
    toGenString      = GenString
    fromGenString _s = case _s of
        (GenString _s) -> toStringCells _s

instance ConvGenString T.Text where
    toGenString      = GenString
    fromGenString _s = case _s of
        (GenString _s) -> toStringCells _s

instance ConvGenString LT.Text where
    toGenString      = GenString
    fromGenString _s = case _s of
        (GenString _s) -> toStringCells _s

instance ConvString GenString where
    toString   = fromGenString
    fromString = toGenString

instance ConvString String where
    toString   = id
    fromString = id

instance ConvString SC.ByteString where
    toString   = SC.unpack
    fromString = SC.pack

instance ConvString LC.ByteString where
    toString   = LC.unpack
    fromString = LC.pack

instance ConvString T.Text where
    toString   = T.unpack
    fromString = T.pack

instance ConvString LT.Text where
    toString   = LT.unpack
    fromString = LT.pack

instance ConvStrictByteString GenString where
    toStrictByteString   = fromGenString
    fromStrictByteString = toGenString

instance ConvStrictByteString String where
    toStrictByteString   = SC.pack
    fromStrictByteString = SC.unpack

instance ConvStrictByteString S.ByteString where
    toStrictByteString   = id
    fromStrictByteString = id

instance ConvStrictByteString L.ByteString where
    toStrictByteString   = L.toStrict
    fromStrictByteString = toLazyByteString

instance ConvStrictByteString T.Text where
    toStrictByteString   = TE.encodeUtf8
    fromStrictByteString = toText

instance ConvStrictByteString LT.Text where
    toStrictByteString   = toStrictByteString . LTE.encodeUtf8
    fromStrictByteString = toLazyText

instance ConvLazyByteString GenString where
    toLazyByteString   = fromGenString
    fromLazyByteString = toGenString

instance ConvLazyByteString String where
    toLazyByteString   = LC.pack
    fromLazyByteString = LC.unpack

instance ConvLazyByteString S.ByteString where
    toLazyByteString   = L.fromStrict
    fromLazyByteString = toStrictByteString

instance ConvLazyByteString L.ByteString where
    toLazyByteString   = id
    fromLazyByteString = id

instance ConvLazyByteString T.Text where
    toLazyByteString   = toLazyByteString . toStrictByteString
    fromLazyByteString = toText

instance ConvLazyByteString LT.Text where
    toLazyByteString   = toLazyByteString . toStrictByteString
    fromLazyByteString = toLazyText

instance ConvText GenString where
    toText   = fromGenString
    fromText = toGenString

instance ConvText String where
    toText   = T.pack
    fromText = T.unpack

instance ConvText S.ByteString where
    toText   = TE.decodeUtf8With TEE.lenientDecode
    fromText = toStrictByteString

instance ConvText L.ByteString where
    toText   = toText . toStrictByteString
    fromText = toLazyByteString

instance ConvText T.Text where
    toText   = id
    fromText = id

instance ConvText LT.Text where
    toText   = LT.toStrict
    fromText = toLazyText

instance ConvLazyText GenString where
    toLazyText   = fromGenString
    fromLazyText = toGenString

instance ConvLazyText String where
    toLazyText   = LT.pack
    fromLazyText = LT.unpack

instance ConvLazyText S.ByteString where
    toLazyText   = LTE.decodeUtf8With TEE.lenientDecode . toLazyByteString
    fromLazyText = toStrictByteString

instance ConvLazyText L.ByteString where
    toLazyText   = LTE.decodeUtf8With TEE.lenientDecode
    fromLazyText = toLazyByteString

instance ConvLazyText T.Text where
    toLazyText   = LT.fromStrict
    fromLazyText = fromLazyText

instance ConvLazyText LT.Text where
    toLazyText   = id
    fromLazyText = id

-- |
--
-- This is minimally defined with 'GenStringDefault'.
instance StringRWIO GenString where
    hGetContents h = genStringFromConConv <$> hGetContents h

    hGetLine h = genStringFromConConv <$> hGetLine h

    hPutStr h s = hPutStr h (genStringConConv s)

    hPutStrLn h s = hPutStrLn h (genStringConConv s)

-- | Type-restricted string conversion used by 'GenString's instance definition for 'StringRWIO'
genStringConConv :: GenString -> GenStringDefault
genStringConConv = toStringCells

-- | Type-restricted string conversion used by 'GenString's instance definition for 'StringRWIO'
genStringFromConConv :: GenStringDefault -> GenString
genStringFromConConv = toStringCells

-- |
--
-- See 'System.IO for documentation of behaviour.
instance StringRWIO String where
    hGetContents = IO.hGetContents

    hGetLine     = IO.hGetLine

    hPutStr      = IO.hPutStr

    hPutStrLn    = IO.hPutStrLn

    interact     = IO.interact

    getContents  = IO.getContents

    getLine      = IO.getLine

    putStr       = IO.putStr

    putStrLn     = IO.putStrLn

    readFile     = IO.readFile

    writeFile    = IO.writeFile

    appendFile   = IO.appendFile

-- |
--
-- See 'Data.ByteString' for documentation of behaviour.
instance StringRWIO S.ByteString where
    hGetContents = S.hGetContents

    hGetLine     = S.hGetLine

    hPutStr      = S.hPutStr

    hPutStrLn    = SC.hPutStrLn

    interact     = S.interact

    getContents  = S.getContents

    getLine      = S.getLine

    putStr       = S.putStr

    putStrLn     = SC.putStrLn

    readFile     = S.readFile

    writeFile    = S.writeFile

    appendFile   = S.appendFile

-- |
--
-- See 'Data.ByteString.Lazy' for documentation of behaviour.
--
-- 'hGetLine' and 'getLine' are defined in terms of 'toStringCells' and the equivalent methods of 'Data.ByteString'.
-- 'hPutStrLn' is defined non-atomically: it is defined as an action that puts the string and then separately puts a newline character string.
instance StringRWIO L.ByteString where
    hGetContents = L.hGetContents

    hGetLine     = (toStringCells <$>) . S.hGetLine

    hPutStr      = L.hPutStr

    hPutStrLn h  = (>> hPutStr h ((toStringCells :: String -> L.ByteString) ['\n'])) . hPutStr h

    interact     = L.interact

    getContents  = L.getContents

    getLine      = toStringCells <$> S.getLine

    putStr       = L.putStr

    putStrLn     = LC.putStrLn

    readFile     = L.readFile

    writeFile    = L.writeFile

    appendFile   = L.appendFile

-- |
--
-- See 'Data.Text.IO' for documentation of behaviour.
instance StringRWIO T.Text where
    hGetContents = T.hGetContents

    hGetLine     = T.hGetLine

    hPutStr      = T.hPutStr

    hPutStrLn    = T.hPutStrLn

    interact     = T.interact

    getContents  = T.getContents

    getLine      = T.getLine

    putStr       = T.putStr

    putStrLn     = T.putStrLn

    readFile     = T.readFile

    writeFile    = T.writeFile

    appendFile   = T.appendFile

-- |
--
-- See 'Data.Text.Lazy.IO' for documentation of behaviour.
instance StringRWIO LT.Text where
    hGetContents = LT.hGetContents

    hGetLine     = LT.hGetLine

    hPutStr      = LT.hPutStr

    hPutStrLn    = LT.hPutStrLn

    interact     = LT.interact

    getContents  = LT.getContents

    getLine      = LT.getLine

    putStr       = LT.putStr

    putStrLn     = LT.putStrLn

    readFile     = LT.readFile

    writeFile    = LT.writeFile

    appendFile   = LT.appendFile

-- | Polymorphic container of a string
--
-- When operations take place on multiple 'GenString's, they are first
-- converted to the type 'GenStringDefault', which are lazy bytestrings,
-- whenever absolutely necessary (which includes testing for equality,
-- appending strings, concatenating lists of strings, empty strings with
-- 'empty', and unfolding), making them the most efficient type for this
-- polymorphic container.
data GenString = forall s. (Stringy s) => GenString {gen_string :: s}
    deriving (Typeable)

toGenDefaultString :: (Stringy s) => s -> GenStringDefault
toGenDefaultString = toStringCells

instance Eq GenString where
    _a == _b = case (_a, _b) of
        ((GenString _a), (GenString _b)) -> toGenDefaultString _a == toGenDefaultString _b
    _a /= _b = case (_a, _b) of
        ((GenString _a), (GenString _b)) -> toGenDefaultString _a /= toGenDefaultString _b

instance IsString GenString where
    fromString = GenString

instance Semigroup GenString where
    (<>) a b = case (a, b) of
        (GenString _a, GenString _b) -> GenString $ append (toGenDefaultString _a) (toGenDefaultString _b)

instance Monoid GenString where
    mempty  = GenString $ (empty :: GenStringDefault)
    mappend = (<>)
    mconcat ss = GenString $ concat . map toGenDefaultString $ ss

instance StringCells GenString where
    -- These associated types were rather arbitrarily chosen
    type StringCellChar GenString = Char
    type StringCellAltChar GenString = Word8

    toStringCells   = fromGenString
    fromStringCells = toGenString

    cons c _s = case _s of
        (GenString _s) -> GenString $ cons (_s `untagTypeOf` toMainChar c) _s
    uncons _s = case _s of
        (GenString _s) -> let (c, s') = uncons _s
                          in  (genStringPhantom `untagTypeOf` toMainChar c, GenString s')
    snoc _s c = case _s of
        (GenString _s) -> GenString $ snoc _s (_s `untagTypeOf` toMainChar c)
    unsnoc _s = case _s of
        (GenString _s) -> let (s', c) = unsnoc _s
                          in  (GenString s', genStringPhantom `untagTypeOf` toMainChar c)

    altCons c _s = case _s of
        (GenString _s) -> GenString $ cons (fromWord8 c) _s
    altUncons _s = case _s of
        (GenString _s) -> let (c, s') = uncons _s
                          in  (genStringPhantom `untagTypeOf` toAltChar c, GenString s')
    altSnoc _s c = case _s of
        (GenString _s) -> GenString $ snoc _s (fromWord8 c)
    altUnsnoc _s = case _s of
        (GenString _s) -> let (s', c) = unsnoc _s
                          in  (GenString s', genStringPhantom `untagTypeOf` toAltChar c)

    toMainChar = Tagged . toChar
    toAltChar  = Tagged . toWord8

    null _s = case _s of
        (GenString _s) -> null _s

    head _s = case _s of
        (GenString _s) -> genStringPhantom `untagTypeOf` toMainChar (head _s)
    tail _s = case _s of
        (GenString _s) -> GenString $ tail _s
    last _s = case _s of
        (GenString _s) -> genStringPhantom `untagTypeOf` toMainChar (last _s)
    init _s = case _s of
        (GenString _s) -> GenString $ init _s
    altHead _s = case _s of
        (GenString _s) -> genStringPhantom `untagTypeOf` toAltChar (head _s)
    altLast _s = case _s of
        (GenString _s) -> genStringPhantom `untagTypeOf` toAltChar (last _s)

    unfoldr       f z = GenString $ (altUnfoldr    f z  :: GenStringDefault)
    altUnfoldr    f z = GenString $ (unfoldr       f z  :: GenStringDefault)
    unfoldrN    n f z = GenString $ (altUnfoldrN n f z  :: GenStringDefault)
    altUnfoldrN n f z = GenString $ (unfoldrN    n f z  :: GenStringDefault)

    index _s i = case _s of
        (GenString _s) -> genStringPhantom `untagTypeOf` toMainChar (index _s i)
    index64 _s i = case _s of
        (GenString _s) -> genStringPhantom `untagTypeOf` toMainChar (index64 _s i)
    genericIndex _s i = case _s of
        (GenString _s) -> genStringPhantom `untagTypeOf` toMainChar (genericIndex _s i)

    take n _s = case _s of
        (GenString _s) -> GenString $ take n _s
    take64 n _s = case _s of
        (GenString _s) -> GenString $ take64 n _s
    genericTake n _s = case _s of
        (GenString _s) -> GenString $ genericTake n _s
    drop n _s = case _s of
        (GenString _s) -> GenString $ drop n _s
    drop64 n _s = case _s of
        (GenString _s) -> GenString $ drop64 n _s
    genericDrop n _s = case _s of
        (GenString _s) -> GenString $ genericDrop n _s

    length _s = case _s of
        (GenString _s) -> length _s
    length64 _s = case _s of
        (GenString _s) -> length64 _s
    genericLength _s = case _s of
        (GenString _s) -> genericLength _s

    safeUncons _s = case _s of
        (GenString _s) -> (\(c, s') -> (genStringPhantom `untagTypeOf` toMainChar c, GenString s')) <$> safeUncons _s
    safeUnsnoc _s = case _s of
        (GenString _s) -> (\(s', c) -> (GenString s', genStringPhantom `untagTypeOf` toMainChar c)) <$> safeUnsnoc _s
    safeAltUncons _s = case _s of
        (GenString _s) -> (\(c, s') -> (genStringPhantom `untagTypeOf` toAltChar c, GenString s')) <$> safeAltUncons _s
    safeAltUnsnoc _s = case _s of
        (GenString _s) -> (\(s', c) -> (GenString s', genStringPhantom `untagTypeOf` toAltChar c)) <$> safeAltUnsnoc _s
    safeHead _s = case _s of
        (GenString _s) -> (genStringPhantom `untagTypeOf`) . toMainChar <$> safeHead _s
    safeTail _s = case _s of
        (GenString _s) -> GenString <$> safeTail _s
    safeLast _s = case _s of
        (GenString _s) -> (genStringPhantom `untagTypeOf`) . toMainChar <$> safeLast _s
    safeInit _s = case _s of
        (GenString _s) -> GenString <$> safeInit _s
    safeAltHead _s = case _s of
        (GenString _s) -> (genStringPhantom `untagTypeOf`) . toAltChar  <$> safeAltHead _s
    safeAltLast _s = case _s of
        (GenString _s) -> (genStringPhantom `untagTypeOf`) . toAltChar  <$> safeAltLast _s
    safeIndex _s i = case _s of
        (GenString _s) -> (genStringPhantom `untagTypeOf`) . toMainChar <$> safeIndex _s i
    safeIndex64 _s i = case _s of
        (GenString _s) -> (genStringPhantom `untagTypeOf`) . toMainChar <$> safeIndex64 _s i
    safeGenericIndex _s i = case _s of
        (GenString _s) -> (genStringPhantom `untagTypeOf`) . toMainChar <$> safeGenericIndex _s i
    safeTake n _s = case _s of
        (GenString _s) -> GenString <$> safeTake n _s
    safeTake64 n _s = case _s of
        (GenString _s) -> GenString <$> safeTake64 n _s
    safeGenericTake n _s = case _s of
        (GenString _s) -> GenString <$> safeGenericTake n _s
    safeDrop n _s = case _s of
        (GenString _s) -> GenString <$> safeDrop n _s
    safeDrop64 n _s = case _s of
        (GenString _s) -> GenString <$> safeDrop64 n _s
    safeGenericDrop n _s = case _s of
        (GenString _s) -> GenString <$> safeGenericDrop n _s
    safeUncons2 _s = case _s of
        (GenString _s) -> (\(a, b, s') -> (genStringPhantom `untagTypeOf` toMainChar a, genStringPhantom `untagTypeOf` toMainChar b, GenString s')) <$> safeUncons2 _s
    safeUncons3 _s = case _s of
        (GenString _s) -> (\(a, b, c, s') -> (genStringPhantom `untagTypeOf` toMainChar a, genStringPhantom `untagTypeOf` toMainChar b, genStringPhantom `untagTypeOf` toMainChar c, GenString s')) <$> safeUncons3 _s
    safeUncons4 _s = case _s of
        (GenString _s) -> (\(a, b, c, d, s') -> (genStringPhantom `untagTypeOf` toMainChar a, genStringPhantom `untagTypeOf` toMainChar b, genStringPhantom `untagTypeOf` toMainChar c, genStringPhantom `untagTypeOf` toMainChar d, GenString s')) <$> safeUncons4 _s

    cons2 a b _s = case _s of
        (GenString _s) -> GenString $ cons2 (_s `untagTypeOf` toMainChar a) (_s `untagTypeOf` toMainChar b) _s
    cons3 a b c _s = case _s of
        (GenString _s) -> GenString $ cons3 (_s `untagTypeOf` toMainChar a) (_s `untagTypeOf` toMainChar b) (_s `untagTypeOf` toMainChar c) _s
    cons4 a b c d _s = case _s of
        (GenString _s) -> GenString $ cons4 (_s `untagTypeOf` toMainChar a) (_s `untagTypeOf` toMainChar b) (_s `untagTypeOf` toMainChar c) (_s `untagTypeOf` toMainChar d) _s
    uncons2 _s = case _s of
        (GenString _s) -> let (a, b, s') = uncons2 _s
                          in  (genStringPhantom `untagTypeOf` toMainChar a, genStringPhantom `untagTypeOf` toMainChar b, GenString s')
    uncons3 _s = case _s of
        (GenString _s) -> let (a, b, c, s') = uncons3 _s
                          in  (genStringPhantom `untagTypeOf` toMainChar a, genStringPhantom `untagTypeOf` toMainChar b, genStringPhantom `untagTypeOf` toMainChar c, GenString s')
    uncons4 _s = case _s of
        (GenString _s) -> let (a, b, c, d, s') = uncons4 _s
                          in  (genStringPhantom `untagTypeOf` toMainChar a, genStringPhantom `untagTypeOf` toMainChar b, genStringPhantom `untagTypeOf` toMainChar c, genStringPhantom `untagTypeOf` toMainChar d, GenString s')

-- | Untag a type with a type restriction
--
-- The first argument is guaranteed to be ignored; thus the value 'undefined'
-- can be passed in its place.
untagTypeOf :: s -> Tagged s b -> b
untagTypeOf _ = untag

-- | Phantom, undefined value only used for convenience
--
-- Users should be careful that this value is never evaluated when using this.
genStringPhantom :: GenString
genStringPhantom = undefined

-- | This type is used by 'GenString' when a concrete string type is needed
type GenStringDefault = L.ByteString
