{-# LANGUAGE CPP
            ,MultiParamTypeClasses
            ,FlexibleInstances
            ,TypeFamilies
            ,TypeSynonymInstances
            ,UndecidableInstances #-}
{-# OPTIONS -Wno-orphans #-}

{- |
Instances of 'Data.ListLike.ListLike' and related classes.
Re-exported by "Data.ListLike".
-}

--------------------------------------------------
-- UTF8 ByteString

module Data.ListLike.UTF8 () where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
--import Control.DeepSeq (NFData(rnf))
import Data.ListLike.Base as LL
import Data.ListLike.FoldableLL
import Data.ListLike.IO
import Data.ListLike.String (StringLike(..))
import Data.Maybe (fromMaybe)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup(..))
#endif
#if !MIN_VERSION_utf8_string(1,0,2)
import Data.String (IsString(fromString))
#endif
import Data.String.UTF8 (UTF8{-, UTF8Bytes-})
import qualified Data.String.UTF8 as UTF8
import GHC.Exts (IsList(..))
--import GHC.Generics

#if 0
utf8rnf :: NFData a => UTF8 a -> ()
utf8rnf = rnf . UTF8.toRep
#endif

instance FoldableLL (UTF8 BS.ByteString) Char where
    foldl = UTF8.foldl
    -- foldl' = UTF8.foldl'
    -- foldl1 = UTF8.foldl1
    foldr = UTF8.foldr
    -- foldr' = UTF8.foldr'
    -- foldr1 = UTF8.foldr1

instance IsList (UTF8 BS.ByteString) where
    type Item (UTF8 BS.ByteString) = Char
    toList = UTF8.toString
    fromList = LL.fromList' -- LL.map id

instance ListLike (UTF8 BS.ByteString) Char where
    empty = mempty
    singleton c = UTF8.fromString [c]
    -- cons = UTF8.cons
    -- snoc = UTF8.snoc
    -- append = UTF8.append
    uncons = UTF8.uncons
    head = fst . fromMaybe (error "head") . uncons
    -- last = UTF8.last
    tail = snd . fromMaybe (error "tail") . uncons
    -- init = UTF8.init
    null s = UTF8.length s == 0
    length = UTF8.length
    -- -- map =
    -- rigidMap = UTF8.map
    -- reverse = UTF8.reverse
    -- intersperse = UTF8.intersperse
    -- concat = UTF8.concat . toList
    -- --concatMap =
    -- rigidConcatMap = UTF8.concatMap
    -- any = UTF8.any
    -- all = UTF8.all
    -- maximum = UTF8.maximum
    -- minimum = UTF8.minimum
    -- replicate = UTF8.replicate
    take = UTF8.take
    drop = UTF8.drop
    splitAt = UTF8.splitAt
    -- takeWhile = UTF8.takeWhile
    -- dropWhile = UTF8.dropWhile
    span = UTF8.span
    break = UTF8.break
    -- group = fromList . UTF8.group
    -- inits = fromList . UTF8.inits
    -- tails = fromList . UTF8.tails
    -- isPrefixOf = UTF8.isPrefixOf
    -- isSuffixOf = UTF8.isSuffixOf
    -- --isInfixOf = UTF8.isInfixOf
    -- elem = UTF8.elem
    -- notElem = UTF8.notElem
    -- find = UTF8.find
    -- filter = UTF8.filter
    -- --partition = UTF8.partition
    -- index = UTF8.index
    -- elemIndex = UTF8.elemIndex
    -- elemIndices x = fromList . UTF8.elemIndices x
    -- findIndex = UTF8.findIndex
    -- findIndices x = fromList . UTF8.findIndices x
    -- -- the default definitions don't work well for array-like things, so
    -- -- do monadic stuff via a list instead
    -- sequence  = fmap fromList . P.sequenceA  . toList
    -- mapM func = fmap fromList . P.traverse func . toList
    -- --nub = UTF8.nub
    -- --delete = UTF8.delete
    -- --deleteFirsts = UTF8.deleteFirsts
    -- --union = UTF8.union
    -- --intersect = UTF8.intersect
    -- sort = UTF8.sort
    -- --insert = UTF8.insert
    --toList = UTF8.toString
    -- fromList = UTF8.pack
    -- fromListLike = fromList . toList
    -- --nubBy = UTF8.nubBy
    -- --deleteBy = UTF8.deleteBy
    -- --deleteFirstsBy = UTF8.deleteFirstsBy
    -- --unionBy = UTF8.unionBy
    -- --intersectBy = UTF8.intersectBy
    -- groupBy f = fromList . UTF8.groupBy f
    -- --sortBy = UTF8.sortBy
    -- --insertBy = UTF8.insertBy
    -- genericLength = fromInteger . fromIntegral . UTF8.length
    -- genericTake i = UTF8.take (fromIntegral i)
    -- genericDrop i = UTF8.drop (fromIntegral i)
    -- genericSplitAt i = UTF8.splitAt (fromIntegral i)
    -- genericReplicate i = UTF8.replicate (fromIntegral i)

instance ListLikeIO (UTF8 BS.ByteString) Char where
    hGetLine h = UTF8.fromRep <$> BSC.hGetLine h
    hGetContents h = UTF8.fromRep <$> BS.hGetContents h
    hGet h n = UTF8.fromRep <$> BS.hGet h n
    hGetNonBlocking h n = UTF8.fromRep <$> BS.hGetNonBlocking h n
    hPutStr h s = BS.hPutStr h (UTF8.toRep s)
    hPutStrLn h s = BSC.hPutStrLn h (UTF8.toRep s)
    -- getLine = BS.getLine
    -- getContents = BS.getContents
    -- putStr = BS.putStr
    -- putStrLn = BSC.putStrLn
    -- interact = BS.interact
    -- readFile = BS.readFile
    -- writeFile = BS.writeFile
    -- appendFile = BS.appendFile

#if !MIN_VERSION_utf8_string(1,0,2)
instance IsString (UTF8 BS.ByteString) where
    fromString = UTF8.fromString
#endif

instance StringLike (UTF8 BS.ByteString) where
    toString = UTF8.toString

instance Semigroup (UTF8 BS.ByteString) where
  a <> b = UTF8.fromRep $ UTF8.toRep a <> UTF8.toRep b

instance Monoid (UTF8 BS.ByteString) where
  mempty  = UTF8.fromString []
  mappend = (<>)

--------------------------------------------------
-- UTF8 Lazy.ByteString

instance FoldableLL (UTF8 BSL.ByteString) Char where
    foldl = UTF8.foldl
    -- foldl' = UTF8.foldl'
    -- foldl1 = UTF8.foldl1
    foldr = UTF8.foldr
    -- foldr' = UTF8.foldr'
    -- foldr1 = UTF8.foldr1

instance IsList (UTF8 BSL.ByteString) where
    type Item (UTF8 BSL.ByteString) = Char
    toList = UTF8.toString
    fromList = LL.fromList' -- LL.map id

instance ListLike (UTF8 BSL.ByteString) Char where
    empty = mempty
    singleton c = UTF8.fromString [c]
    -- cons = UTF8.cons
    -- snoc = UTF8.snoc
    -- append = UTF8.append
    uncons = UTF8.uncons
    head = fst . fromMaybe (error "head") . uncons
    -- last = UTF8.last
    tail = snd . fromMaybe (error "tail") . uncons
    -- init = UTF8.init
    null s = UTF8.length s == 0
    length = fromInteger . toInteger . UTF8.length
    -- -- map =
    -- rigidMap = UTF8.map
    -- reverse = UTF8.reverse
    -- intersperse = UTF8.intersperse
    -- concat = UTF8.concat . toList
    -- --concatMap =
    -- rigidConcatMap = UTF8.concatMap
    -- any = UTF8.any
    -- all = UTF8.all
    -- maximum = UTF8.maximum
    -- minimum = UTF8.minimum
    -- replicate = UTF8.replicate
    take = UTF8.take . fromInteger . toInteger
    drop = UTF8.drop . fromInteger . toInteger
    splitAt = UTF8.splitAt . fromInteger . toInteger
    -- takeWhile = UTF8.takeWhile
    -- dropWhile = UTF8.dropWhile
    span = UTF8.span
    break = UTF8.break
    -- group = fromList . UTF8.group
    -- inits = fromList . UTF8.inits
    -- tails = fromList . UTF8.tails
    -- isPrefixOf = UTF8.isPrefixOf
    -- isSuffixOf = UTF8.isSuffixOf
    -- --isInfixOf = UTF8.isInfixOf
    -- elem = UTF8.elem
    -- notElem = UTF8.notElem
    -- find = UTF8.find
    -- filter = UTF8.filter
    -- --partition = UTF8.partition
    -- index = UTF8.index
    -- elemIndex = UTF8.elemIndex
    -- elemIndices x = fromList . UTF8.elemIndices x
    -- findIndex = UTF8.findIndex
    -- findIndices x = fromList . UTF8.findIndices x
    -- -- the default definitions don't work well for array-like things, so
    -- -- do monadic stuff via a list instead
    -- sequence  = fmap fromList . P.sequenceA  . toList
    -- mapM func = fmap fromList . P.traverse func . toList
    -- --nub = UTF8.nub
    -- --delete = UTF8.delete
    -- --deleteFirsts = UTF8.deleteFirsts
    -- --union = UTF8.union
    -- --intersect = UTF8.intersect
    -- sort = UTF8.sort
    -- --insert = UTF8.insert
    -- toList = UTF8.toString
    -- fromList = UTF8.pack
    -- fromListLike = fromList . toList
    -- --nubBy = UTF8.nubBy
    -- --deleteBy = UTF8.deleteBy
    -- --deleteFirstsBy = UTF8.deleteFirstsBy
    -- --unionBy = UTF8.unionBy
    -- --intersectBy = UTF8.intersectBy
    -- groupBy f = fromList . UTF8.groupBy f
    -- --sortBy = UTF8.sortBy
    -- --insertBy = UTF8.insertBy
    -- genericLength = fromInteger . fromIntegral . UTF8.length
    -- genericTake i = UTF8.take (fromIntegral i)
    -- genericDrop i = UTF8.drop (fromIntegral i)
    -- genericSplitAt i = UTF8.splitAt (fromIntegral i)
    -- genericReplicate i = UTF8.replicate (fromIntegral i)

instance ListLikeIO (UTF8 BSL.ByteString) Char where
    hGetLine h = (UTF8.fromRep . BSL.fromStrict) <$> BSC.hGetLine h
    hGetContents h = (UTF8.fromRep) <$> BSL.hGetContents h
    hGet h n = UTF8.fromRep <$> BSL.hGet h n
    hGetNonBlocking h n = UTF8.fromRep <$> BSL.hGetNonBlocking h n
    hPutStr h s = BSL.hPutStr h (UTF8.toRep s)
    hPutStrLn h s = BSLC.hPutStrLn h (UTF8.toRep s)
    -- getLine = BSL.getLine
    -- getContents = BSL.getContents
    -- putStr = BSL.putStr
    -- putStrLn = BSLC.putStrLn
    -- interact = BSL.interact
    -- readFile = BSL.readFile
    -- writeFile = BSL.writeFile
    -- appendFile = BSL.appendFile

#if !MIN_VERSION_utf8_string(1,0,2)
instance IsString (UTF8 BSL.ByteString) where
    fromString = UTF8.fromString
#endif

instance StringLike (UTF8 BSL.ByteString) where
    toString = UTF8.toString

instance Semigroup (UTF8 BSL.ByteString) where
  a <> b = UTF8.fromRep $ UTF8.toRep a <> UTF8.toRep b

instance Monoid (UTF8 BSL.ByteString) where
  mempty  = UTF8.fromString []
  mappend = (<>)

{-# RULES "fromListLike/a" fromListLike = id :: a -> a #-}
