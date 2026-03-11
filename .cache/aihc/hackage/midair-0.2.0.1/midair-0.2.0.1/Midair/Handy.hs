{-# LANGUAGE GADTSyntax, NoMonoLocalBinds #-}
{-# LANGUAGE LambdaCase #-}

module Midair.Handy (

   -- * Representations of IO Action Outputs

     Fx(..)
   , runFx

   , sPrint
   , sPutStrLn



   -- * Handy Signal Flow Nodes


   -- ** Numbers

   , sMax
   , sMaxHS
   , sMin
   , sMinHS
   , sSum
   , sSumHS
   , sProduct
   , sProductHS


   -- ** Booleans and Predicates

   , sAnd
   , sOr
   , sAny
   , sAnyHS
   , sAll
   , sAllHS


   -- ** Monoids

   , sMappend
   , sMappendHS
   , sMappendL
   , sMappendLHS
   , sMappendR
   , sMappendRHS


   -- ** \"List\" functions

   , sToList
   , sToListHS
   , sElem
   , sNotElem
   , sMapMaybe
   , sFilterWDefault
   , sFoldHS


   -- ** Counts

   , sCountJust
   , sCountJustHS
   , sCountJustBy
   , sCountJustByHS

   , sCountFires
   , sCountFiresBy
   , sCountFiresByHS
   ) where

import Midair.Core

import Control.Arrow
import Data.Maybe
import Data.Monoid

-- | Representation of side-effecting actions. Usually the final result of
--   the FRP graph
data Fx a where
   Fx_Void :: IO () -> Fx a
   Fx_Return :: IO a -> Fx a

runFx :: Fx a -> IO (Maybe a)
runFx (Fx_Void action) = do
   () <- action
   return Nothing
runFx (Fx_Return action) = Just <$> action


-- | Hot-swap in a fold function
-- 
--   First argument is the default value, in case the graph 
--   you're replacing hasn't fired at all yet
sFoldHS :: c -> (a -> c -> c) -> (Maybe c -> SFlow a c)
sFoldHS defaultVal foldF = \lastValMay ->
   sFold (fromMaybe defaultVal lastValMay) foldF

-- | Handy for developing
sPrint :: Show a => SFlow a (Fx b)
sPrint = sMap (Fx_Void . print)

sPutStrLn :: SFlow String (Fx b)
sPutStrLn = sMap (Fx_Void . putStrLn)

-- | Add all values from the input stream
sSum :: Num n => SFlow n n
sSum = sFold 0 (+)

-- | Hotswap version of 'sSum'
sSumHS :: Num n => Maybe n -> SFlow n n
sSumHS =
   \x -> sFold (fromMaybe 0 x) (+)

-- | Multiply all values from the input stream
sProduct :: Num n => SFlow n n
sProduct = sFold 1 (*)

-- | Hotswap version of 'sProduct'
sProductHS :: Num n => Maybe n -> SFlow n n
sProductHS =
   \x -> sFold (fromMaybe 1 x) (*)

-- | Count of all values -- essentially the number of times the SFlow has fired
-- 
--   (If you want to count e.g. what's passed the predicate in 'sFilter', use
--   'sCountJust' instead)
sCountFires :: Num n => SFlow x n
sCountFires = sCountFiresBy 1

sCountFiresBy :: Num n => n -> SFlow x n
sCountFiresBy stepsToCountBy =
   sFold 0 $ \_ -> (+stepsToCountBy)

sCountFiresByHS :: Num n => n -> (Maybe n -> SFlow x n)
sCountFiresByHS stepsToCountBy = \lastValMay ->
   sFold (fromMaybe 0 lastValMay) $ \_ -> (+stepsToCountBy)

-- | Count the number of 'Just' values we've seen
-- 
--   (Useful after e.g. a 'sFilter')
sCountJust :: Num n => SFlow (Maybe a) n
sCountJust = sCountJustBy 1

-- | Hotswap version of 'sCountJust'
sCountJustHS :: Num n => Maybe n -> SFlow (Maybe a) n
sCountJustHS = sCountJustByHS 1

-- | 'countJust' by steps -- e.g. @[0,3,6]@ instead of @[0,1,2]@
sCountJustBy :: Num n => n -> SFlow (Maybe a) n
sCountJustBy stepsToCountBy = sFold 0 $ \case
   Nothing -> id
   Just _ -> (+ stepsToCountBy)

-- | Hotswap version of 'sCountJustBy'
sCountJustByHS :: Num n => n -> (Maybe n -> SFlow (Maybe a) n)
sCountJustByHS stepsToCountBy = \lastOutput ->
   sFold (fromMaybe 0 lastOutput) $ \case
      Nothing -> id
      Just _ -> (+ stepsToCountBy)

-- | Max of all values
sMax :: Ord x => SFlow x x
sMax = sFoldNoDefault $ \newVal -> \case
   Nothing -> newVal
   Just oldVal -> max oldVal newVal

-- | Hotswap version of 'sMax'
sMaxHS :: Ord x => Maybe x -> SFlow x x
sMaxHS = \lastOutMay -> sFoldAccum lastOutMay $ \newVal -> \case
   Nothing -> newVal
   Just oldVal -> max oldVal newVal

-- | Min of all values
sMin :: Ord x => SFlow x x
sMin = sFoldNoDefault $ \newVal -> \case
   Nothing -> newVal
   Just oldVal -> min oldVal newVal

-- | Hotswap version of 'sMin'
sMinHS :: Ord x => Maybe x -> SFlow x x
sMinHS = \lastOutMay -> sFoldAccum lastOutMay $ \newVal -> \case
   Nothing -> newVal
   Just oldVal -> min oldVal newVal

-- | 'mappend' new values onto the right
sMappendR :: Monoid x => SFlow x x
sMappendR = sFold mempty $
   \newVal oldVal -> oldVal <> newVal

-- | Hotswap version of 'sMappendR'
sMappendRHS :: Monoid x => Maybe x -> SFlow x x
sMappendRHS = \lastOutput ->
   sFold (fromMaybe mempty lastOutput) $
      \newVal oldVal ->
         oldVal <> newVal

-- | 'mappend' new values onto the left
sMappendL :: Monoid x => SFlow x x
sMappendL = sFold mempty $
   \newVal oldVal -> newVal <> oldVal

-- | Hotswap version of 'sMappendL'
sMappendLHS :: Monoid x => Maybe x -> SFlow x x
sMappendLHS = \lastOutput ->
   sFold (fromMaybe mempty lastOutput) $
      \newVal oldVal ->
         newVal <> oldVal

-- | 'mappend' all input signal values
-- 
--   (If you care whether they're appended on the right or left, use 'sMappendL'
--   or 'sMappendR')
sMappend :: Monoid x => SFlow x x
sMappend = sMappendL

-- | Hotswap version of 'sMappend'
sMappendHS :: Monoid x => Maybe x -> SFlow x x
sMappendHS = sMappendLHS

-- | Return True if all input signal values have been True, otherwise False
sAnd :: SFlow Bool Bool
sAnd = sFold True (&&)

-- | Return True if any input signal values have been true
sOr :: SFlow Bool Bool
sOr = sFold False (||)

-- | Return True if any input signals have passed the predicate, False otherwise
sAny :: (a -> Bool) -> SFlow a Bool
sAny predicate = sFoldNoDefault $ sAnyF predicate

sAnyF :: (a -> Bool) -> a -> (Maybe Bool -> Bool)
sAnyF predicate = \inVal -> \case
   Nothing -> predicate inVal
   -- Same as: Just x -> x || predicate inVal:
   Just False -> predicate inVal
   Just True -> True

-- | Hotswap version of 'sAny'
sAnyHS :: (a -> Bool) -> (Maybe Bool -> SFlow a Bool)
sAnyHS predicate = \lastValMay ->
   sFoldAccum lastValMay $ sAnyF predicate

-- | Return True if all input signals have passed the predicate, False otherwise
sAll :: (a -> Bool) -> SFlow a Bool
sAll predicate = sFoldNoDefault $ sAllF predicate

sAllF :: (a -> Bool) -> a -> (Maybe Bool -> Bool)
sAllF predicate = \inVal -> \case
   Nothing -> predicate inVal
   -- Same as: Just x -> x && predicate inVal:
   Just False -> False
   Just True -> predicate inVal

-- | Hotswap version of 'sAll'
sAllHS :: (a -> Bool) -> (Maybe Bool -> SFlow a Bool)
sAllHS predicate = \lastValMaybe ->
   sFoldAccum lastValMaybe $ sAllF predicate

-- sAny :: (a -> Bool)

-- | Returns list of all values - at the head of the list is the most-recent element
sToList :: SFlow a [a]
sToList = sFold [] (:)

sToListHS :: Maybe [a] -> SFlow a [a]
sToListHS = \lastValMaybe -> sFold (fromMaybe [] lastValMaybe) (:)

-- | Return True if the input signal has ever contained the element, else False
sElem :: Eq x => x -> SFlow x Bool
sElem x = sAny (==x)

-- | Return True if the input signal has never contained the element, else False
sNotElem :: Eq x => x -> SFlow x Bool
sNotElem x = sAll (/=x)

-- sTakeWhile
-- sDropWhile
-- sTake
-- sDrop

-- sSplitAt
-- sSpan
-- sBreak
-- sPartition

-- sZip3
-- sZip4
-- sZip5
-- sZip6
-- sZip7
-- sZipWith
-- sZipWith3
-- sZipWith4
-- sZipWith5
-- sZipWith6
-- sZipWith7
-- sUnfold(r)

-- sLefts
-- sRights
-- sEither
-- sPartitionEithers

-- sFromMaybe/sCatMaybes

-- sFirstJust

sMapMaybe :: state -> (update -> Maybe state) -> SFlow update state
sMapMaybe startVal maybeF =
   sFold startVal f
 where
   f update previousVal = case maybeF update of
       Just x -> x
       Nothing -> previousVal

-- Want a shorter name:
sFilterWDefault :: (b -> Bool) -> b -> SFlow b b
sFilterWDefault f defaultVal =
   sMap (fromMaybe defaultVal) <<< sFilter f
