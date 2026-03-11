{-# LANGUAGE RankNTypes #-}
module HaskellWorks.Data.Conduit.Merge
  ( JoinResult (..)
  , joinSources
  , joinResumableSources
  ) where

import Control.Monad       (foldM)
import Control.Monad.Trans (lift)
import Data.Conduit        (ConduitT, SealedConduitT, await, leftover, sealConduitT, yield, ($$++))

{-| A result value of joining two sources.

When sources are joined, the result value can be a value or
it be a leftover on either left or right side in case if one of the
streams is ehausted before another.
-}
data JoinResult a v b
  = LeftoverL a     -- ^ Leftover on the left side, the right side is exhausted
  | JoinValue v     -- ^ Result value
  | LeftoverR b     -- ^ Leftover on the right side, the left side is exhausted
  deriving (Show, Eq)

{-| Joins sources with the provided merging function.
Leftovers are considered valid values and are retuned as a part of a result stream.

@
  import Data.Conduit
  import Data.Conduit.List as CL

  \-\- combining function just sums both values
  comb :: (Ord a, Num a) => a -> a -> ([a], [a], [a])
  comb a b
    | a > b     = ([a - b], [b], [])
    | b > a     = ([], [a], [b - a])
    | otherwise = ([], [a], [])

  let lst1 = CL.sourceList [1,2,3]
  let lst2 = CL.sourceList [1,2,3,4,5]
  runConduit $ joinSources comb lst1 lst2 $$ CL.take 1000

  ['JoinValue' 2,'JoinValue' 4,'JoinValue' 6,'LeftoverR' 4,'LeftoverR' 5]
@
-}
joinSources :: Monad m
            => (a -> b -> ([a], [v], [b]))
            -- ^ Function to merge values.
            --   The result contains values @v@ and possible leftovers @a@ and @b@
            --   for left and right streams.
            -> ConduitT () a m ()
            -- ^ Left side source
            -> ConduitT () b m ()
            -- ^ Right side source
            -> ConduitT () (JoinResult a v b) m ()
            -- ^ Result source that can contain a value or leftovers on each side
joinSources f as bs = joinResumableSources f (sealConduitT as) (sealConduitT bs)


joinResumableSources :: Monad m
            => (a -> b -> ([a], [v], [b]))
            -- ^ Function to merge values.
            --   The result contains values @v@ and possible leftovers @a@ and @b@
            --   for left and right streams.
            -> SealedConduitT () a m ()
            -- ^ Left side source
            -> SealedConduitT () b m ()
            -- ^ Right side source
            -> ConduitT () (JoinResult a v b) m ()
            -- ^ Result source that can contain a value or leftovers on each side
joinResumableSources f = go
  where
    go ras rbs = do
      (ras', ma) <- lift $ ras $$++ await
      (rbs', mb) <- lift $ rbs $$++ await
      case (ma, mb) of
        (Nothing, Nothing) -> pure ()
        (Nothing, Just b)  -> yield (LeftoverR b) >> go ras' rbs'
        (Just a,  Nothing) -> yield (LeftoverL a) >> go ras' rbs'
        (Just a,  Just b)  -> do
          let (ls, vs, rs) = f a b
          mapM_ (yield . JoinValue) vs
          ras'' <- lift $ pushLeftovers ras' ls
          rbs'' <- lift $ pushLeftovers rbs' rs
          go ras'' rbs''
    pushLeftovers s ls = foldM (\vs' l -> fst <$> (vs' $$++ leftover l)) s (reverse ls)

-- fullZip :: Monad m
--         => Source m a
--         -> Source m b
--         -> Source m (Maybe a, Maybe b)
-- fullZip (ConduitM left0) (ConduitM right0) = ConduitM $ \rest -> let
--     go (Leftover left ()) right = go left right
--     go left (Leftover right ())  = go left right
--     go (Done ()) (Done ()) = rest ()
--     go (Done ()) (HaveOutput src close y) = HaveOutput (go (Done ()) src) close (Nothing, Just y)
--     go (HaveOutput src close x) (Done ()) = HaveOutput (go src (Done ())) close (Just x, Nothing)
--     go (Done ()) (PipeM _) = rest ()
--     go (PipeM _) (Done ()) = rest ()
--     go (PipeM mx) (PipeM my) = PipeM (liftM2 go mx my)
--     go (PipeM mx) y@HaveOutput{} = PipeM ((`go` y) <$> mx)
--     go x@HaveOutput{} (PipeM my) = PipeM (go x <$>  my)
--     go (HaveOutput srcx closex x) (HaveOutput srcy closey y) =
--       HaveOutput (go srcx srcy) (closex >> closey) (Just x, Just y)
--     go (NeedInput _ c) right = go (c ()) right
--     go left (NeedInput _ c) = go left (c ())
--     in go (left0 Done) (right0 Done)
