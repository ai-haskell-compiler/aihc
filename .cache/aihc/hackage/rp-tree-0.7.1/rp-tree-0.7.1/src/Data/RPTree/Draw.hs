{-# LANGUAGE FlexibleContexts #-}
{-# language LambdaCase #-}
{-# options_ghc -Wno-unused-imports #-}
{-# options_ghc -Wno-unused-top-binds #-}
module Data.RPTree.Draw (
  -- * CSV
  writeCsv
  , knnWriteCsv
  -- * GraphViz dot
  , writeDot
  -- , draw
                        )where

import Data.Bifoldable (Bifoldable(..))
import Data.Bifunctor (Bifunctor(..))
import Data.Bitraversable (Bitraversable(..))
import Data.List (intercalate)
import Text.Printf (PrintfArg, printf)

-- boxes
import qualified Text.PrettyPrint.Boxes as B (Box, render, emptyBox, vcat, hcat, text, top, bottom, center1)
-- bytestring
import qualified Data.ByteString.Lazy    as LBS (ByteString, writeFile)
import qualified Data.ByteString.Builder as BSB (Builder, toLazyByteString, string7, charUtf8)
-- containers
import qualified Data.Set as S (Set, insert, fromList)
-- mtl
import Control.Monad.State (MonadState(..), modify)
-- text
import qualified Data.Text.Lazy as TL (Text)
import qualified Data.Text.Lazy.Builder as TLB (Builder, toLazyText, fromString)
import qualified Data.Text.Lazy.IO as TL (writeFile)
-- transformers
import Control.Monad.Trans.State (State, evalState)
-- vector
import qualified Data.Vector as V (Vector, replicateM)
import qualified Data.Vector.Generic as VG (Vector(..), map, sum, unfoldr, unfoldrM, length, replicateM, (!))
import qualified Data.Vector.Unboxed as VU (Unbox)

import Data.RPTree.Internal (RPTree(..), RPT(..), DVector, toListDv)


knnWriteCsv ::
  (Foldable t1, Foldable t2, Show a1, Show b1, Show a2, Show b2, VU.Unbox a1, VU.Unbox a2) =>
  FilePath
  -> t1 (V.Vector (DVector a1, b1))
  -> t2 (DVector a2, b2) -- ^ points obtained from knn
  -> IO ()
knnWriteCsv fp ds knnds = TL.writeFile fp $ TLB.toLazyText bld
  where
    bld = toCsvV ds <>
          toCsv knnds

-- | Encode dataset as CSV and save into file
writeCsv :: (Foldable t, VU.Unbox a, Show a, Show b) =>
            FilePath -- ^ path of output file
         -> t (V.Vector (DVector a, b)) -- ^ data point, label
         -> IO ()
writeCsv fp ds = TL.writeFile fp $ TLB.toLazyText $ toCsvV ds

toCsvV :: (Foldable t, VU.Unbox a, Show a, Show b) =>
          t (V.Vector (DVector a, b)) -> TLB.Builder
toCsvV = foldMap toCsv

toCsv :: (Foldable t, Show a, Show b, VU.Unbox a) => t (DVector a, b) -> TLB.Builder
toCsv = foldMap (\(r, i) -> toCsvRow r i <> newline)

toCsvRow :: (Show a, Show b, VU.Unbox a) =>
            DVector a
         -> b
         -> TLB.Builder
toCsvRow dv i = TLB.fromString $ intercalate "," [show x, show y, show i]
  where
    (x:y:_) = toListDv dv

newline :: TLB.Builder
newline = TLB.fromString "\n"

-- toCsv :: (Foldable t, Show a, Show b, VU.Unbox a) =>
--          t (DVector a, b) -> TLB.Builder
-- toCsv = foldMap (\(r, i) -> toCsvRow r i <> newline )


-- | tree to graphviz dot format
writeDot :: Ord t =>
            (t -> String) -- ^ how to render the node content
         -> FilePath -- ^ path of output file
         -> String -- ^ graph name
         -> RPTree d x t
         -> IO ()
writeDot f fp name tt = TL.writeFile fp (toDot f name tt)

toDot :: Ord a => (a -> String) -> String -> RPTree d x a -> TL.Text
toDot f name (RPTree _ tt) = TLB.toLazyText $ open <> x <> close
  where
    x = foldl insf mempty $ toEdges tt
      where
        insf acc = \case
          Edge i1 i2 ->
            acc <> TLB.fromString (unwords [show i1, "->", show i2]) <> newline
          Node i xs -> acc <> TLB.fromString (unwords [show i, nlab ] ) <> newline
            where
              nlab = unwords ["[", "label=\"", f xs,"\"]"]
          BNode i -> acc <> TLB.fromString (unwords [show i, blab]) <> newline
            where
              blab = unwords ["[", "shape=point", "]"]
    open = TLB.fromString $ unwords ["digraph" , name, "{\n"]
    close = TLB.fromString "}"

data G a = Edge Int Int
         | Node Int a -- tip nodes
         | BNode Int -- branching point nodes
         deriving (Eq, Ord, Show)


toEdges :: (Ord a) => RPT d x a -> S.Set (G a)
toEdges = S.fromList . go [] [] . labelBranches
  where
    go s acc = \case
      Tip i1 x ->
        let
          n = Node i1 x
          acc' = push n acc
          acc'' = maybe acc' (\i0 -> push (Edge i0 i1) acc') (pop s)
        in
          acc''
      Bin i1 _ _ tl tr ->
        let
          b1 = BNode i1
          acc' = push b1 acc
          acc'' = case pop s of
            Nothing -> acc'
            Just i0 ->
              let
                e = Edge i0 i1
                b0 = BNode i0
              in push e (push b0 acc')
          s' = push i1 s
        in
          go s' acc'' tl <> go s' acc tr



labelBranches :: Bitraversable t => t x d -> t Int d
labelBranches = flip evalState 0 . bitraverse counter pure

counter :: (MonadState Int m) => x -> m Int
counter _ = do
  i <- get
  modify succ
  pure i

type Stack a = [a]
push :: a -> Stack a -> Stack a
push = (:)
pop :: Stack a -> Maybe a
pop xs
  | null xs = Nothing
  | otherwise = Just $ head xs


-- tt0 :: RPT Integer () [a]
tt0 :: RPT Integer [a1] [a2]
tt0 = Bin [] 0 mempty tl t
  where
    tl = Bin [] 1 mempty (Bin [] 2 mempty t t) (Bin [] 3 mempty t t)
    t = Tip [] []



-- 


-- | Render a tree to stdout
--
-- Useful for debugging
--
-- This should be called only for small trees, otherwise the printed result quickly overflows the screen and becomes hard to read.
--
-- NB : prints distance information rounded to two decimal digits
draw :: (Show a, Boxed a, PrintfArg v) => RPTree v l a -> IO ()
draw = drawRPT . _rpTree

drawRPT :: (Show a, Boxed a, PrintfArg v) => RPT v l a -> IO ()
drawRPT = putStrLn . toStringRPT

toStringRPT :: (Show a, Boxed a, PrintfArg v) => RPT v l a -> String
toStringRPT = B.render . toBox

toBox :: (Show a, Boxed a, PrintfArg v) => RPT v l a -> B.Box
toBox = \case
  (Bin _ thr _ tl tr) ->
    txt (node thr) `stack` (toBox tl `byside` toBox tr)
  Tip _ xs -> boxed xs -- tipData xs -- txt $ show x
  where
    node x = printf "%5.2f" x -- (show x)

class Boxed a where
  boxed :: a -> B.Box
instance (Show a) => Boxed [a] where
  boxed = foldl (\bx x -> bx `stack` txt (show x)) $ B.emptyBox 0 0
instance Boxed () where
  boxed _ = txt "*"

tipData :: (Show a, Foldable t) => t a -> B.Box
tipData = foldl (\bx x -> bx `stack` txt (show x)) $ B.emptyBox 1 1

txt :: String -> B.Box
txt t = spc `byside` B.text t `byside` spc
  where spc = B.emptyBox 1 1

byside :: B.Box -> B.Box -> B.Box
byside l r = B.hcat B.top [l, r]

stack :: B.Box -> B.Box -> B.Box
stack t b = B.vcat B.center1 [t, b]
