{-# LANGUAGE OverloadedLists #-}

{-|
Module:      Data.Diff.Myers
Copyright:   (c) 2023 Tom McLaughlin
License:     BSD3
Stability:   experimental
Portability: portable

This is a fast Haskell implementation of the Myers text diff algorithm[1]. It is heavily inspired by the Python version in [this post](https://blog.robertelder.org/diff-algorithm/), and should have the same @O(min(len(a), len(b)))@ space complexity. (By contrast, the [Diff](https://hackage.haskell.org/package/Diff) package advertises @O(ab)@ space complexity.) The implementation uses unboxed mutable vectors for performance.

This repo also can also build a couple other versions for benchmarking comparison, gated behind flags.

* @-funi_myers@ will build the version from the [uni-util](https://hackage.haskell.org/package/uni-util-2.3.0.3/docs/Util-Myers.html) package.
* @-fdiff_myers@ will use the [Diff](https://hackage.haskell.org/package/Diff) package.

[1]: E. Myers (1986). "An O(ND) Difference Algorithm and Its Variations". Algorithmica. 1 (2): 251–266. CiteSeerX [10.1.1.4.6927](https://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.4.6927). doi:[10.1007/BF01840446](https://doi.org/10.1007%2FBF01840446). S2CID [6996809](https://api.semanticscholar.org/CorpusID:6996809).

-}

module Data.Diff.Myers (
  -- * Pure diffing (uses ST monad)
  diffTexts
  , diffTextsToChangeEvents
  , diffTextsToChangeEventsConsolidate
  , diffTextsToChangeEvents'
  , diffVectors
  , diffStrings

  -- * Lowest level diff function
  , diff

  -- * Working with edit scripts
  , editScriptToChangeEvents
  , consolidateEditScript

  -- * Util
  , fastTextToVector

  -- * Types
  , Edit(..)
  ) where

import Control.Monad.Primitive
import Control.Monad.ST
import Data.Bits
import Data.Diff.Types
import qualified Data.Foldable as F
import Data.Function
import Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Internal.Fusion as TI
import Data.Vector.Unboxed as VU
import Data.Vector.Unboxed.Mutable as VUM
import Prelude hiding (read)


-- | Diff 'Text's to produce an edit script.
diffTexts :: Text -> Text -> Seq Edit
diffTexts left right = runST $
  diff (fastTextToVector left)
       (fastTextToVector right)

-- | Diff 'Text's to produce LSP-style change events.
diffTextsToChangeEvents :: Text -> Text -> [ChangeEvent]
diffTextsToChangeEvents = diffTextsToChangeEvents' id

-- | Diff 'Text's to produce consolidated LSP-style change events.
diffTextsToChangeEventsConsolidate :: Text -> Text -> [ChangeEvent]
diffTextsToChangeEventsConsolidate = diffTextsToChangeEvents' consolidateEditScript

-- | Diff 'Text's with a custom consolidation function.
diffTextsToChangeEvents' :: (Seq Edit -> Seq Edit) -> Text -> Text -> [ChangeEvent]
diffTextsToChangeEvents' consolidateFn left right = F.toList $ editScriptToChangeEvents l r (consolidateFn (runST (diff l r)))
  where
    l = fastTextToVector left
    r = fastTextToVector right

-- | Diff 'VU.Vector's to produce an edit script.
diffVectors :: VU.Vector Char -> VU.Vector Char -> Seq Edit
diffVectors left right = runST $ diff left right

-- | To use in benchmarking against other libraries that use String.
diffStrings :: String -> String -> Seq Edit
diffStrings left right = runST $ do
  let leftThawed = VU.fromList left
  let rightThawed = VU.fromList right
  diff leftThawed rightThawed

-- * Core

diff :: (
  PrimMonad m, Unbox a, Eq a, Show a
  ) => Vector a -> Vector a -> m (Seq Edit)
diff e f = diff' e f 0 0

{-# SPECIALISE diff' :: Vector Char -> Vector Char -> Int -> Int -> ST () (Seq Edit) #-}
{-# SPECIALISE diff' :: Vector Char -> Vector Char -> Int -> Int -> IO (Seq Edit) #-}
diff' :: (
  PrimMonad m, Unbox a, Eq a, Show a
  ) => Vector a -> Vector a -> Int -> Int -> m (Seq Edit)
diff' e f i j = do
  let (bigN, bigM) = (VU.length e, VU.length f)
  let bigZ = (2 * (min bigN bigM)) + 2
  g <- new bigZ
  p <- new bigZ
  diff'' g p e f i j

{-# SPECIALISE diff'' :: MVector (PrimState (ST ())) Int -> MVector (PrimState (ST ())) Int -> Vector Char -> Vector Char -> Int -> Int -> ST () (Seq Edit) #-}
{-# SPECIALISE diff'' :: MVector (PrimState IO) Int -> MVector (PrimState IO) Int -> Vector Char -> Vector Char -> Int -> Int -> IO (Seq Edit) #-}
diff'' :: (
  PrimMonad m, Unbox a, Eq a, Show a
  ) => MVector (PrimState m) Int -> MVector (PrimState m) Int -> Vector a -> Vector a -> Int -> Int -> m (Seq Edit)
diff'' g' p' e f i j = do
  let (bigN, bigM) = (VU.length e, VU.length f)
  let (bigL, bigZ) = (bigN + bigM, (2 * (min bigN bigM)) + 2)

  if | bigN > 0 && bigM > 0 -> do
         let w = bigN - bigM

         -- Clear out the reused memory vectors
         let g = VUM.unsafeSlice 0 bigZ g'
         VUM.set g 0
         let p = VUM.unsafeSlice 0 bigZ p'
         VUM.set p 0

         flip fix 0 $ \loopBaseH -> \case
           h | not (h <= ((bigL `quot` 2) + (if (intMod2 bigL) /= 0 then 1 else 0))) -> return []
           h -> do
             let loopH = loopBaseH (h + 1)
             flip fix (0 :: Int) $ \loopBaseR -> \case
               r | not (r <= 1) -> loopH
               r -> do
                 let loopR = loopBaseR (r + 1)
                 let (c, d, o, m) = if r == 0 then (g, p, 1, 1) else (p, g, 0, -1)
                 flip fix (negate (h - (2 * (max 0 (h - bigM))))) $ \loopBaseK -> \case
                   k | not (k <= (h - (2 * (max 0 (h - bigN))))) -> loopR
                   k -> do
                     let loopK = loopBaseK (k + 2)
                     aInitial <- do
                       prevC <- unsafeRead c ((k-1) `pyMod` bigZ)
                       nextC <- unsafeRead c ((k+1) `pyMod` bigZ)
                       return (if (k == (-h) || (k /= h && (prevC < nextC))) then nextC else prevC + 1)
                     let bInitial = aInitial - k
                     let (s, t) = (aInitial, bInitial)

                     (a, b) <- flip fix (aInitial, bInitial) $ \loop (a', b') -> do
                       if | a' < bigN && b' < bigM -> do
                              let eVal = e `unsafeIndex` (((1 - o) * bigN) + (m*a') + (o - 1))
                              let fVal = f `unsafeIndex` (((1 - o) * bigM) + (m*b') + (o - 1))
                              if | eVal == fVal -> loop (a' + 1, b' + 1)
                                 | otherwise -> pure (a', b')
                          | otherwise -> pure (a', b')

                     write c (k `pyMod` bigZ) a
                     let z = negate (k - w)

                     cVal <- unsafeRead c (k `pyMod` bigZ)
                     dVal <- unsafeRead d (z `pyMod` bigZ)
                     if | (intMod2 bigL == o) && (z >= (negate (h-o))) && (z <= (h-o)) && (cVal + dVal >= bigN) -> do
                            let (bigD, x, y, u, v) = if o == 1 then ((2*h)-1, s, t, a, b) else (2*h, bigN-a, bigM-b, bigN-s, bigM-t)
                            if | bigD > 1 || (x /= u && y /= v) ->
                                  mappend <$> diff'' g p (VU.unsafeSlice 0 x e) (VU.unsafeSlice 0 y f) i j
                                          <*> diff'' g p (VU.unsafeSlice u (bigN - u) e) (VU.unsafeSlice v (bigM - v) f) (i+u) (j+v)
                               | bigM > bigN ->
                                  diff'' g p (VU.unsafeSlice 0 0 e) (VU.unsafeSlice bigN (bigM - bigN) f) (i+bigN) (j+bigN)
                               | bigM < bigN ->
                                  diff'' g p (VU.unsafeSlice bigM (bigN - bigM) e) (VU.unsafeSlice 0 0 f) (i+bigM) (j+bigM)
                               | otherwise -> return []
                        | otherwise -> loopK


     | bigN > 0 -> return [EditDelete i (i + (bigN - 1))]
     | bigM == 0 -> return []
     | otherwise -> return [EditInsert i j (j + (bigM - 1))]

{-# INLINABLE pyMod #-}
pyMod :: Integral a => a -> a -> a
pyMod x y = if y >= 0 then x `mod` y else (x `mod` y) - y

{-# INLINABLE intMod2 #-}
intMod2 :: Int -> Int
intMod2 n = n .&. 1

-- | Convert edit script to LSP-style change events.
editScriptToChangeEvents :: VU.Vector Char -> VU.Vector Char -> Seq Edit -> Seq ChangeEvent
editScriptToChangeEvents left right = go mempty 0 0 0
  where
    go :: Seq ChangeEvent -> Int -> Int -> Int -> Seq Edit -> Seq ChangeEvent
    go seqSoFar _ _ _ Empty = seqSoFar

    -- Implicit unchanged section before delete
    go seqSoFar pos line ch args@((EditDelete from _to) :<| _) |
      pos < from = go seqSoFar from line' ch' args
        where
          (numNewlinesEncountered, lastLineLength) = countNewlinesAndLastLineLength (VU.slice pos (from - pos) left)
          line' = line + numNewlinesEncountered
          ch' | numNewlinesEncountered == 0 = ch + (from - pos)
              | otherwise = lastLineLength
    -- Implicit unchanged section before insert
    go seqSoFar pos line ch args@((EditInsert from _rightFrom _rightTo) :<| _) |
      pos < from = go seqSoFar from line' ch' args
        where
          (numNewlinesEncountered, lastLineLength) = countNewlinesAndLastLineLength (VU.slice pos (from - pos) left)
          line' = line + numNewlinesEncountered
          ch' | numNewlinesEncountered == 0 = ch + (from - pos)
              | otherwise = lastLineLength

    go seqSoFar pos line ch ((EditDelete from to) :<| rest) = go (seqSoFar |> change) pos' line ch rest
      where
        change = ChangeEvent (Range (Position line ch) (Position line' ch')) ""
        pos' = to + 1

        deleted = VU.slice from (to + 1 - from) left
        (numNewlinesInDeleted, lastLineLengthInDeleted) = countNewlinesAndLastLineLength deleted
        line' = line + numNewlinesInDeleted
        ch' = if | numNewlinesInDeleted == 0 -> ch + (to - pos + 1)
                 | otherwise -> lastLineLengthInDeleted

    go seqSoFar pos line ch ((EditInsert _at rightFrom rightTo) :<| rest) = go (seqSoFar |> change) pos' line' ch' rest
      where
        change = ChangeEvent (Range (Position line ch) (Position line ch)) (vectorToText inserted)
        pos' = pos

        inserted = VU.slice rightFrom (rightTo + 1 - rightFrom) right
        (numNewlinesInInserted, lastLineLengthInInserted) = countNewlinesAndLastLineLength inserted
        line' = line + numNewlinesInInserted
        ch' = if | numNewlinesInInserted == 0 -> ch + VU.length inserted
                 | otherwise -> lastLineLengthInInserted

    countNewlinesAndLastLineLength :: VU.Vector Char -> (Int, Int)
    countNewlinesAndLastLineLength = VU.foldl' (\(tot, lastLineLength) ch -> if ch == '\n' then (tot + 1, 0) else (tot, lastLineLength + 1)) (0, 0)

    vectorToText :: VU.Vector Char -> Text
    vectorToText = T.pack . VU.toList

-- * Consolidate edits

-- | Consolidate adjacent edit script entries to shorten the script.
consolidateEditScript :: Seq Edit -> Seq Edit
consolidateEditScript ((EditInsert pos1 from1 to1) :<| (EditInsert pos2 from2 to2) :<| rest)
  | pos1 == pos2 && to1 + 1 == from2 = consolidateEditScript ((EditInsert pos1 from1 to2) <| rest)
consolidateEditScript ((EditDelete from1 to1) :<| (EditDelete from2 to2) :<| rest)
  | to1 + 1 == from2 = consolidateEditScript ((EditDelete from1 to2) <| rest)
consolidateEditScript (x :<| y :<| rest) = x <| (consolidateEditScript (y <| rest))
consolidateEditScript x = x


-- | This is currently the only way to convert a 'Text' to a 'VU.Vector' without extraneous allocations.
-- Taken from https://stackoverflow.com/a/77388392/2659595
-- Once the text library contains a foldM function, we can switch to that and avoid importing internal
-- functions.
-- See https://github.com/haskell/text/pull/543
fastTextToVector :: Text -> VU.Vector Char
fastTextToVector t =
  case TI.stream t of
    TI.Stream step s0 _ -> VU.create $ do
      m <- VUM.new (T.length t)
      let
        go s i =
          case step s of
            TI.Done -> pure ()
            TI.Skip s' -> go s' i
            TI.Yield x s' -> do
              VUM.write m i x
              go s' (i + 1)
      go s0 0
      pure m
