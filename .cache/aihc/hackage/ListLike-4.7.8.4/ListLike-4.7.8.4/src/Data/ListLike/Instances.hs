{-# LANGUAGE CPP
            ,MultiParamTypeClasses
            ,FlexibleInstances
            ,TypeFamilies
            ,TypeSynonymInstances #-}
{-# OPTIONS -Wno-orphans #-}
#if __GLASGOW_HASKELL__ >= 900
{-# OPTIONS -Wno-incomplete-uni-patterns #-}
#endif

{-
Copyright (C) 2007 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file COPYRIGHT

-}

{- |
   Module     : Data.ListLike.Instances
   Copyright  : Copyright (C) 2007 John Goerzen
   License    : BSD3

   Maintainer : David Fox <dsf@seereason.com>, Andreas Abel
   Stability  : stable
   Portability: portable

Instances of 'Data.ListLike.ListLike' and related classes.
Re-exported by "Data.ListLike".

Written by John Goerzen, jgoerzen\@complete.org
-}

module Data.ListLike.Instances () where
import Prelude hiding (length, head, last, null, tail, map, filter, concat,
                       any, lookup, init, all, foldl, foldr, foldl1, foldr1,
                       maximum, minimum, iterate, span, break, takeWhile,
                       dropWhile, reverse, zip, zipWith, sequence,
                       sequence_, mapM, mapM_, concatMap, and, or, sum,
                       product, repeat, replicate, cycle, take, drop,
                       splitAt, elem, notElem, unzip, lines, words,
                       unlines, unwords)
import qualified Prelude as P
import qualified Data.List as L
import qualified Data.Sequence as S
import           Data.Sequence ((><), (|>), (<|))
import qualified Data.Foldable as F
import           Data.ListLike.Base
import           Data.ListLike.String
import           Data.ListLike.IO
import           Data.ListLike.FoldableLL
import           Data.ListLike.Text ()
import           Data.ListLike.UTF8 ()
import           Data.ListLike.Vector ()
import           Data.Int
#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup (Semigroup(..))
#endif
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
--import qualified Data.Foldable as F
--import qualified Data.Traversable as T
import qualified Data.Array.IArray as A
import           Data.Array.IArray((!), (//), Ix(..))
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.String (IsString)
--import           Data.String.UTF8 (UTF8)
--import qualified Data.String.UTF8 as UTF8
import qualified System.IO as IO
import           Data.Word
import           GHC.Exts (IsList(..))

--------------------------------------------------
-- []

-- Basic list instance is in Base.hs
-- FoldableLL instance implied by Foldable

instance ListLikeIO String Char where
    hGetLine = IO.hGetLine
    hGetContents = IO.hGetContents
    hGet _ c | c <= 0 = return mempty
    hGet h c = cons <$> IO.hGetChar h <*> hGet h (pred c)
    -- hGetNonBlocking h i >>= (return . toString)
    hGetNonBlocking _h _i = error "Unimplemented: hGetNonBlocking in instance ListLikeIO String Char"
    hPutStr = IO.hPutStr
    hPutStrLn = IO.hPutStrLn
    getLine = IO.getLine
    getContents = IO.getContents
    putStr = IO.putStr
    putStrLn = IO.putStrLn
    interact = IO.interact
    readFile = IO.readFile
    writeFile = IO.writeFile

{-
import           Data.ByteString.Internal (createAndTrim)
import qualified System.IO.Error as IO

hGetNonBlocking :: IO.Handle -> Int -> IO BS.ByteString
hGetNonBlocking h i
    | i >  0    = createAndTrim i $ \p -> IO.hGetBufNonBlocking h p i
    | i == 0    = return empty
    | otherwise = illegalBufferSize h "hGetNonBlocking'" i

illegalBufferSize :: IO.Handle -> String -> Int -> IO a
illegalBufferSize handle fn sz =
    ioError (IO.mkIOError IO.illegalOperationErrorType msg (Just handle) Nothing)
    --TODO: System.IO uses InvalidArgument here, but it's not exported :-(
    where
      msg = fn ++ ": illegal ByteString size " ++ showsPrec 9 sz []
-}

instance StringLike String where
    toString = id
    --fromString = id

instance InfiniteListLike [a] a where
    iterate = L.iterate
    repeat = L.repeat
    cycle = L.cycle

--------------------------------------------------
-- ByteString

instance FoldableLL BS.ByteString Word8 where
    foldl = BS.foldl
    foldl' = BS.foldl'
    foldl1 = BS.foldl1
    foldr = BS.foldr
    foldr' = BS.foldr'
    foldr1 = BS.foldr1

#if !MIN_VERSION_bytestring(0,10,12)
instance IsList BS.ByteString where
    type Item BS.ByteString = Word8
    toList = BS.unpack
    fromList = BS.pack
#endif

instance ListLike BS.ByteString Word8 where
    empty = BS.empty
    singleton = BS.singleton
    cons = BS.cons
    snoc = BS.snoc
    append = BS.append
    uncons = BS.uncons
    head = BS.head
    last = BS.last
    tail = BS.tail
    init = BS.init
    null = BS.null
    length = BS.length
    -- map =
    rigidMap = BS.map
    reverse = BS.reverse
    intersperse = BS.intersperse
    concat = BS.concat . toList
    --concatMap =
    rigidConcatMap = BS.concatMap
    any = BS.any
    all = BS.all
    maximum = BS.maximum
    minimum = BS.minimum
    replicate = BS.replicate
    take = BS.take
    drop = BS.drop
    splitAt = BS.splitAt
    takeWhile = BS.takeWhile
    dropWhile = BS.dropWhile
    span = BS.span
    break = BS.break
    group = fromList . BS.group
    inits = fromList . BS.inits
    tails = fromList . BS.tails
    isPrefixOf = BS.isPrefixOf
    isSuffixOf = BS.isSuffixOf
    --isInfixOf = BS.isInfixOf
    elem = BS.elem
    notElem = BS.notElem
    find = BS.find
    filter = BS.filter
    --partition = BS.partition
    index = BS.index
    elemIndex = BS.elemIndex
    elemIndices x = fromList . BS.elemIndices x
    findIndex = BS.findIndex
    findIndices x = fromList . BS.findIndices x
    -- the default definitions don't work well for array-like things, so
    -- do monadic stuff via a list instead
    sequence  = fmap fromList . P.sequenceA  . toList
    mapM func = fmap fromList . P.traverse func . toList
    --nub = BS.nub
    --delete = BS.delete
    --deleteFirsts = BS.deleteFirsts
    --union = BS.union
    --intersect = BS.intersect
    sort = BS.sort
    --insert = BS.insert
    --fromListLike = fromList . toList
    --nubBy = BS.nubBy
    --deleteBy = BS.deleteBy
    --deleteFirstsBy = BS.deleteFirstsBy
    --unionBy = BS.unionBy
    --intersectBy = BS.intersectBy
    groupBy f = fromList . BS.groupBy f
    --sortBy = BS.sortBy
    --insertBy = BS.insertBy
    genericLength = fromInteger . fromIntegral . BS.length
    genericTake i = BS.take (fromIntegral i)
    genericDrop i = BS.drop (fromIntegral i)
    genericSplitAt i = BS.splitAt (fromIntegral i)
    genericReplicate i = BS.replicate (fromIntegral i)

instance ListLikeIO BS.ByteString Word8 where
    hGetLine = BSC.hGetLine
    hGetContents = BS.hGetContents
    hGet = BS.hGet
    hGetNonBlocking = BS.hGetNonBlocking
    hPutStr = BS.hPutStr
    hPutStrLn = BSC.hPutStrLn
    getLine = BSC.getLine
    getContents = BS.getContents
    putStr = BS.putStr
    putStrLn = BSC.putStrLn
    interact = BS.interact
    readFile = BS.readFile
    writeFile = BS.writeFile
    appendFile = BS.appendFile

-- There is no bijection between Strings and ByteStrings that I know
-- of.  The elements of a String are Unicode code points, and while
-- every String can be UTF8-encoded into a ByteString, there are
-- ByteStrings that can not be decoded into valid Strings - notably
-- "\128".  So should ByteString be an instance of StringLike?
-- Probably not.  Unfortunately, this instance is used to implement
-- the ListLikeIO instance for String!  This must not stand.
#if 0
instance StringLike BS.ByteString where
    toString = BSU.toString
    --fromString = BSU.fromString
#endif

--------------------------------------------------
-- ByteString.Lazy

instance FoldableLL BSL.ByteString Word8 where
    foldl = BSL.foldl
    foldl' = BSL.foldl'
    foldl1 = BSL.foldl1
    foldr = BSL.foldr
    --foldr' = BSL.foldr'
    foldr1 = BSL.foldr1

mi64toi :: Maybe Int64 -> Maybe Int
mi64toi Nothing = Nothing
mi64toi (Just x) = Just (fromIntegral x)

#if !MIN_VERSION_bytestring(0,10,12)
instance IsList BSL.ByteString where
    type Item BSL.ByteString = Word8
    toList = BSL.unpack
    fromList = BSL.pack
#endif

instance ListLike BSL.ByteString Word8 where
    empty = BSL.empty
    singleton = BSL.singleton
    cons = BSL.cons
    snoc = BSL.snoc
    append = BSL.append
    uncons = BSL.uncons
    head = BSL.head
    last = BSL.last
    tail = BSL.tail
    init = BSL.init
    null = BSL.null
    length = fromIntegral . BSL.length
    -- map = BSL.map
    rigidMap = BSL.map
    reverse = BSL.reverse
    --intersperse = BSL.intersperse
    concat = BSL.concat . toList
    --concatMap = BSL.concatMap
    rigidConcatMap = BSL.concatMap
    any = BSL.any
    all = BSL.all
    maximum = BSL.maximum
    minimum = BSL.minimum
    replicate i = BSL.replicate (fromIntegral i)
    take i = BSL.take (fromIntegral i)
    drop i = BSL.drop (fromIntegral i)
    splitAt i = BSL.splitAt (fromIntegral i)
    takeWhile = BSL.takeWhile
    dropWhile = BSL.dropWhile
    span = BSL.span
    break = BSL.break
    group = fromList . BSL.group
    inits = fromList . BSL.inits
    tails = fromList . BSL.tails
    isPrefixOf = BSL.isPrefixOf
    --isSuffixOf = BSL.isSuffixOf
    --isInfixOf = BSL.isInfixOf
    elem = BSL.elem
    notElem = BSL.notElem
    find = BSL.find
    filter = BSL.filter
    --partition = BSL.partition
    index l i = BSL.index l (fromIntegral i)
    elemIndex i = mi64toi . BSL.elemIndex i
    --elemIndices x = fromList . L.map fromIntegral . BSL.elemIndices x
    findIndex f = mi64toi . BSL.findIndex f
    --findIndices x = fromList . L.map fromIntegral . BSL.findIndices x
    sequence  = fmap fromList . P.sequenceA  . toList
    mapM func = fmap fromList . P.traverse func . toList
    --sequence = BSL.sequence
    --mapM = BSL.mapM
    --mapM_ = BSL.mapM_
    --nub = BSL.nub
    --delete = BSL.delete
    --deleteFirsts = BSL.deleteFirsts
    --union = BSL.union
    --intersect = BSL.intersect
    --sort = BSL.sort
    --insert = BSL.insert
    --fromListLike = fromList . toList
    --nubBy = BSL.nubBy
    --deleteBy = BSL.deleteBy
    --deleteFirstsBy = BSL.deleteFirstsBy
    --unionBy = BSL.unionBy
    --intersectBy = BSL.intersectBy
    -- BSL.groupBy is broken. groupBy f = fromList . BSL.groupBy f
    -- the below works on ghc but generates a type error on hugs
    -- groupBy func = map fromList . L.groupBy func . toList
    --sortBy = BSL.sortBy
    --insertBy = BSL.insertBy
    genericLength = fromInteger . fromIntegral . BSL.length
    genericTake i = BSL.take (fromIntegral i)
    genericDrop i = BSL.drop (fromIntegral i)
    genericSplitAt i = BSL.splitAt (fromIntegral i)
    genericReplicate i = BSL.replicate (fromIntegral i)

strict2lazy :: BS.ByteString -> IO BSL.ByteString
strict2lazy b = return (BSL.fromChunks [b])
instance ListLikeIO BSL.ByteString Word8 where
    hGetLine h = BSC.hGetLine h >>= strict2lazy
    hGetContents = BSL.hGetContents
    hGet = BSL.hGet
    hGetNonBlocking = BSL.hGetNonBlocking
    hPutStr = BSL.hPut
    -- hPutStrLn = BSLC.hPutStrLn
    getLine = BSC.getLine >>= strict2lazy
    getContents = BSL.getContents
    putStr = BSL.putStr
    putStrLn = BSLC.putStrLn
    interact = BSL.interact
    readFile = BSL.readFile
    writeFile = BSL.writeFile
    appendFile = BSL.appendFile

#if 0
instance StringLike BSL.ByteString where
    toString = BSLU.toString
    --fromString = BSLU.fromString
#endif

--------------------------------------------------
-- Map
-- N.B. the Map instance is broken because it treats the key as part of the
-- element.  Consider:
--  let m = fromList [(False,0)] :: Map Bool Int
--  let m' = cons (False, 1) m
--  m' == fromList [(False,1)] =/= [(False,1), (False,0)]
--  Map isn't a suitable candidate for ListLike...


--------------------------------------------------
-- Arrays

-- This constraint is required for ghc < 8
instance Ix i => FoldableLL (A.Array i e) e where
    foldl = F.foldl
    foldl1 = F.foldl1
    foldl' = F.foldl'
    foldr = F.foldr
    foldr1 = F.foldr1
    foldr' = F.foldr'

instance (Integral i, Ix i) => Semigroup (A.Array i e) where
  l1 <> l2 = A.array (blow, newbhigh) $
               A.assocs l1 ++ zip [bhigh + 1 .. newbhigh] (A.elems l2)
    where
    newlen        = genericLength newelems
    newelems      = A.elems l2
    newbhigh      = bhigh + newlen
    (blow, bhigh) = A.bounds l1

instance (Integral i, Ix i) => Monoid (A.Array i e) where
  mempty  = A.listArray (0, -1) []
  mappend = (<>)

instance (Integral i, Ix i) => IsList (A.Array i e) where
    type Item (A.Array i e) = e
    toList = A.elems
    fromList l = A.listArray (0, genericLength l - 1) l

instance (Integral i, Ix i) => ListLike (A.Array i e) e where
    empty = mempty
    singleton i = A.listArray (0, 0) [i]
    cons i l =
        -- To add something to the beginning of an array, we must
        -- change the bounds and set the first element.
        (A.ixmap (blow - 1, bhigh) id l) // [(blow - 1, i)]
        where (blow, bhigh) = A.bounds l
    snoc l i =
        -- Here we must change the bounds and set the last element
        (A.ixmap (blow, bhigh + 1) id l) // [(bhigh + 1, i)]
        where (blow, bhigh) = A.bounds l
    append = mappend
    head l = l ! (fst (A.bounds l))
    last l = l ! (snd (A.bounds l))
    tail l = A.array (blow + 1, bhigh) (tail (A.assocs l))
            where (blow, bhigh) = A.bounds l
    init l = A.array (blow, bhigh - 1) (init (A.assocs l))
            where (blow, bhigh) = A.bounds l
    null l = genericLength l == (0::Integer)
    length = genericLength
    -- map
    rigidMap = A.amap
    reverse l = A.listArray (A.bounds l) (L.reverse (A.elems l))
    -- intersperse
    -- concat
    -- concatMap
    -- rigidConcatMap
    any x = L.any x . A.elems
    all x = L.all x . A.elems
    maximum = L.maximum . A.elems
    minimum = L.minimum . A.elems
    replicate = genericReplicate
    take = genericTake
    drop = genericDrop
    -- splitAt
    -- takeWhile
    -- dropWhile
    -- span
    -- break
    -- group
    -- inits
    -- tails
    isPrefixOf l1 l2 = L.isPrefixOf (toList l1) (toList l2)
    isSuffixOf l1 l2 = L.isSuffixOf (toList l1) (toList l2)
    isInfixOf l1 l2 = L.isInfixOf (toList l1) (toList l2)
    elem i l = L.elem i (toList l)
    -- notElem
    filter f = fromList . L.filter f . toList
    -- partition
    index l i = l ! ((fromIntegral i) + offset)
        where offset = (fst $ A.bounds l)
    elemIndex i = L.elemIndex i . toList
    elemIndices i = fromList . L.elemIndices i . toList
    findIndex f = L.findIndex f . toList
    findIndices f = fromList . L.findIndices f . toList
    sequence  = fmap fromList . P.sequenceA  . toList
    mapM func = fmap fromList . P.traverse func . toList
    -- rigidMapM = mapM
    nub = fromList . L.nub . toList
    -- delete
    -- deleteFirsts
    -- union
    -- intersect
    sort l = A.listArray (A.bounds l) (L.sort (A.elems l))
    -- insert
    -- fromListLike
    nubBy f = fromList . L.nubBy f . toList
    -- deleteBy
    -- deleteFirstsBy
    -- unionBy
    -- intersectBy
    -- groupBy
    sortBy f l = A.listArray (A.bounds l) (L.sortBy f (A.elems l))
    -- insertBy
    genericLength l = fromIntegral (bhigh - blow + 1)
        where (blow, bhigh) = A.bounds l
    genericTake count l
        | count > genericLength l = l
        | count <= 0 = empty
        | otherwise = A.listArray (blow, blow + (fromIntegral count) - 1)
                          (L.genericTake count (A.elems l))
        where (blow, _) = A.bounds l
    genericDrop count l = A.listArray (blow + (fromIntegral count), bhigh)
                          (L.genericDrop count (A.elems l))
        where (blow, bhigh) = A.bounds l
    -- geneicSplitAt
    genericReplicate count i = A.listArray (0, (fromIntegral count) - 1)
                                           (L.genericReplicate count i)


instance (Integral i, Ix i) => IsString (A.Array i Char) where
    fromString = fromList

instance (Integral i, Ix i) => StringLike (A.Array i Char) where
    toString = toList
    -- lines
    -- words

instance (Integral i, Ix i) => ListLikeIO (A.Array i Char) Char where
    hGetLine h = IO.hGetLine h >>= (return . fromList)
    hGetContents h = IO.hGetContents h >>= (return . fromList)
    hGet h i = ((hGet h i)::IO String) >>= (return . fromList)
    hGetNonBlocking h i = ((hGetNonBlocking h i):: IO String) >>= (return . fromList)
    hPutStr h = hPutStr h . toString
    hPutStrLn h = hPutStrLn h . toString
    getLine = IO.getLine >>= (return . fromString)
    getContents = IO.getContents >>= (return . fromString)
    putStr = IO.putStr . toString
    putStrLn = IO.putStrLn . toString
    -- interact
    -- readFile
    -- writeFile
    -- appendFile

-- ---------------------------
-- Data.Sequence instances

instance ListLikeIO (S.Seq Char) Char where
    hGetLine h = IO.hGetLine h >>= (return . fromList)
    hGetContents h = IO.hGetContents h >>= (return . fromList)
    hGet h i = ((hGet h i)::IO String) >>= (return . fromList)
    hGetNonBlocking h i = ((hGetNonBlocking h i):: IO String) >>= (return . fromList)
    hPutStr h = hPutStr h . toString
    hPutStrLn h = hPutStrLn h . toString
    getLine = IO.getLine >>= (return . fromString)
    getContents = IO.getContents >>= (return . fromString)
    putStr = IO.putStr . toString
    putStrLn = IO.putStrLn . toString
    -- interact
    -- readFile
    -- writeFile
    -- appendFile

instance StringLike (S.Seq Char) where
    toString = toList
    --fromString = fromList

instance FoldableLL (S.Seq a) a where
    foldl = F.foldl
    foldl' = F.foldl'
    foldl1 = F.foldl1
    foldr = F.foldr
    foldr' = F.foldr'
    foldr1 = F.foldr1

instance ListLike (S.Seq a) a where
    empty = S.empty
    singleton = S.singleton
    cons = (<|)
    snoc = (|>)
    append = (><)
    head s = let (a S.:< _) = S.viewl s in a
    last s = let (_ S.:> a) = S.viewr s in a
    tail s = S.index (S.tails s) 1
    init s = S.index (S.inits s) (S.length s - 1)
    null = S.null
    length = S.length
    map f = fromList . toList . fmap f
    --rigidMap =
    reverse = S.reverse
    --intersperse =
    --concat =
    --concatMap =
    --rigidConcatMap =
    any = F.any
    all = F.all
    maximum = F.maximum
    minimum = F.minimum
    replicate n = S.replicate (if n >= 0 then n else 0)
    take = S.take
    drop = S.drop
    splitAt = S.splitAt
    --takeWhile =
    --dropWhile =
    span = S.spanl
    -- break =
    --group =
    inits = fromList . toList . S.inits
    tails = fromList . toList . S.tails
    --isPrefixOf =
    --isSuffixOf =
    --isInfixOf =
    --elem =
    --notElem =
    --find =
    filter = S.filter
    partition = S.partition
    index = S.index
    elemIndex = S.elemIndexL
    elemIndices p = fromList . S.elemIndicesL p
    findIndex = S.findIndexL
    findIndices p = fromList . S.findIndicesL p
    --sequence =
    --mapM f =
    --nub =
    --delete =
    --deleteFirsts =
    --union =
    --intersect =
    sort = S.sort
    --insert = S.insert
    --fromListLike = fromList . toList
    --nubBy =
    --deleteBy =
    --deleteFirstsBy =
    --unionBy =
    --intersectBy =
    --groupBy f =
    sortBy = S.sortBy
    --insertBy =
    genericLength = fromInteger . fromIntegral . S.length
    genericTake i = S.take (fromIntegral i)
    genericDrop i = S.drop (fromIntegral i)
    genericSplitAt i = S.splitAt (fromIntegral i)
    genericReplicate i = S.replicate (fromIntegral i)
