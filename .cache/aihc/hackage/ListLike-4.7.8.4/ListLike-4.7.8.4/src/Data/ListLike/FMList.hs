{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}
{-# OPTIONS -Wno-orphans #-}
-- | 'Data.ListLike.ListLike' instances for 'Data.FMList.FMList'
module Data.ListLike.FMList () where
import Prelude hiding (cycle, map)
import Data.ListLike.Base
import Data.ListLike.FoldableLL
--import Data.ListLike.IO
import Data.ListLike.String
import Data.FMList (FMList(..))
import qualified Data.FMList as FM
--import Data.Foldable (Foldable)
import qualified Data.Foldable as F
--import Data.Traversable (Traversable)
--import qualified Data.Traversable as T
import Data.String (IsString)
import qualified Data.String as S
import Control.Monad.Zip (MonadZip)
import qualified Control.Monad.Zip as Z
--import Data.Function
import GHC.Exts (IsList(..))

instance FoldableLL (FMList a) a where
  foldl = F.foldl
  foldr = F.foldr
  foldl1 = F.foldl1
  foldr1 = F.foldr1
  foldl' = F.foldl'
  foldr' = F.foldr'

instance IsList (FMList a) where
  type Item (FMList a) = a
  fromList = FM.fromList
  toList = FM.toList

instance ListLike (FMList a) a where
  empty = FM.empty
  singleton = FM.singleton
  cons = FM.cons
  snoc = FM.snoc
  append = FM.append
  head = FM.head
  tail = FM.tail
  last = FM.last
  init = FM.init
  --fromList = FM.fromList
  --toList = FM.toList
  null = FM.null
  genericLength = FM.genericLength
  length = FM.length
  reverse = FM.reverse
  filter = FM.filter
  take = FM.take
  takeWhile = FM.takeWhile
  drop = FM.drop
  dropWhile = FM.dropWhile

instance InfiniteListLike (FMList a) a where
  iterate = FM.iterate
  repeat = FM.repeat
  cycle a = (a `FM.append` cycle a) `FM.append` a

instance IsString (FMList Char) where
  fromString = FM.fromList

instance StringLike (FMList Char) where
  toString = FM.toList
  lines = map FM.fromList . S.lines . FM.toList
  words = map FM.fromList . S.words . FM.toList
  unlines = FM.fromList . S.unlines . map FM.toList
  unwords = FM.fromList . S.unwords . map FM.toList

instance MonadZip FMList where
  mzipWith = FM.zipWith
