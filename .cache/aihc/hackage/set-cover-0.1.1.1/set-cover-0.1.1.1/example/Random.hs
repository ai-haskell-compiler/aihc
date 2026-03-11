module Random (intSetFromSetAssigns, shuffle) where

import Mastermind.Utility (attach)

import qualified Math.SetCover.Exact as ESC

import System.Random (StdGen, randomR, )

import qualified Control.Monad.Trans.State as MS
import Control.Applicative ((<$>), )

import qualified Data.Map as Map
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set; import Data.Set (Set, )
import qualified Data.Sequence as Seq; import Data.Sequence (Seq, )
import Data.Tuple.HT (mapSnd, )
import Data.Monoid ((<>), )


{- |
This is a variant of 'ESC.intSetFromSetAssigns'
that includes shuffling of elements.
This way, we can make 'consistentCodesRnd' prefer, say, symbol @z@ to @a@.
-}
intSetFromSetAssigns ::
   (Ord a) =>
   [ESC.Assign label (Set a)] ->
   MS.State StdGen [ESC.Assign label IntSet.IntSet]
intSetFromSetAssigns asns = do
   toMapInt <- mapIntFromSet asns
   let toIntSet = IntSet.fromList . Map.elems . toMapInt
   return $ map (fmap toIntSet) asns

-- cf. ESC
mapIntFromSet ::
   (Ord a) =>
   [ESC.Assign label (Set a)] -> MS.State StdGen (Set a -> Map.Map a Int)
mapIntFromSet asns = do
   mapToInt <-
      fmap (Map.fromList . flip zip [0..]) $
      shuffle $ Set.toList $ ESC.unions $ map ESC.labeledSet asns
   return $ Map.intersection mapToInt . constMap ()

-- SetCover.EnumMap
constMap :: (Ord a) => b -> Set.Set a -> Map.Map a b
constMap a = Map.fromAscList . attach a . Set.toAscList


shuffle :: [a] -> MS.State StdGen [a]
shuffle = shuffleSeq . Seq.fromList

shuffleSeq :: Seq a -> MS.State StdGen [a]
shuffleSeq xs =
   if Seq.null xs
      then return []
      else do
         (y, ys) <- select xs
         (y:) <$> shuffleSeq ys

select :: Seq a -> MS.State StdGen (a, Seq a)
select xs = do
   k <- MS.state $ randomR (0, Seq.length xs - 1)
   case mapSnd Seq.viewl $ Seq.splitAt k xs of
      (_, Seq.EmptyL) -> error "Seq.size must have been zero"
      (ys, z Seq.:< zs) -> return (z, ys <> zs)
