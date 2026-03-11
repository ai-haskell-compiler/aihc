{-# LANGUAGE MultiParamTypeClasses
            ,FlexibleInstances #-}
{-# OPTIONS -Wno-orphans #-}

module Data.ListLike.Text.TextLazy

where

import           Prelude as P
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TI
import           Data.Text.Encoding (decodeUtf8)
import           Data.ListLike.Base as LL
import           Data.ListLike.FoldableLL
import           Data.ListLike.IO
import           Data.ListLike.String

import qualified Data.ByteString as BS

instance FoldableLL T.Text Char where
    foldl = T.foldl
    foldl' = T.foldl'
    foldl1 = T.foldl1
    foldr = T.foldr
    foldr1 = T.foldr1

instance ListLike T.Text Char where
    empty = T.empty
    singleton = T.singleton
    cons = T.cons
    snoc = T.snoc
    append = T.append
    head = T.head
    last = T.last
    tail = T.tail
    init = T.init
    null = T.null
    length = fromIntegral . T.length
    rigidMap = T.map
    reverse = T.reverse
    intersperse = T.intersperse
    concat = T.concat . toList
    rigidConcatMap = T.concatMap
    any = T.any
    all = T.all
    maximum = T.maximum
    minimum = T.minimum
    replicate n = T.replicate (fromIntegral n) . T.singleton
    take = T.take . fromIntegral
    drop = T.drop . fromIntegral
    splitAt = T.splitAt . fromIntegral
    takeWhile = T.takeWhile
    dropWhile = T.dropWhile
    span = T.span
    break = T.break
    group = fromList . T.group
    inits = fromList . T.inits
    tails = fromList . T.tails
    isPrefixOf = T.isPrefixOf
    isSuffixOf = T.isSuffixOf
    stripPrefix = T.stripPrefix
    stripSuffix = T.stripSuffix
    elem = T.isInfixOf . T.singleton
    find = T.find
    filter = T.filter
    index t = T.index t . fromIntegral
    --toList = T.unpack
    --fromList = T.pack
    --fromListLike = fromList . toList
    groupBy f = fromList . T.groupBy f
    genericLength = fromInteger . fromIntegral . T.length
    genericTake i = T.take (fromIntegral i)
    genericDrop i = T.drop (fromIntegral i)
    genericSplitAt i = T.splitAt (fromIntegral i)
    genericReplicate i = LL.replicate (fromIntegral i)

    sequence  = fmap fromList . P.sequenceA  . toList
    mapM func = fmap fromList . P.traverse func . toList

instance ListLikeIO T.Text Char where
    hGetLine = TI.hGetLine
    hGetContents = TI.hGetContents
    hGet h = fmap (T.fromStrict . decodeUtf8) . BS.hGet h
    hGetNonBlocking h = fmap (T.fromStrict . decodeUtf8) . BS.hGetNonBlocking h
    hPutStr = TI.hPutStr
    hPutStrLn = TI.hPutStrLn
    getLine = TI.getLine
    getContents = TI.getContents
    putStr = TI.putStr
    putStrLn = TI.putStrLn
    interact = TI.interact
    readFile = TI.readFile
    writeFile = TI.writeFile
    appendFile = TI.appendFile

instance StringLike T.Text where
    toString = T.unpack
    words = fromList . T.words
    lines = fromList . T.lines
    unwords = T.unwords . toList
    unlines = T.unlines . toList

    fromText = T.fromStrict
    fromLazyText = id
