module Test.Data.Char.Frame where

import Test.Utility (checkDuplicates, )

import qualified Data.Char.Frame as Frame

import qualified System.Exit as Exit
import qualified System.IO as IO

import qualified Data.List.HT as ListHT
import qualified Data.Foldable as Fold
import Control.Monad (when, )
import Control.Applicative (pure, )
import Data.Traversable (sequenceA, )


allParts :: [Frame.Parts Bool]
allParts = sequenceA $ pure [False, True]

longParts :: [Frame.Parts Bool]
longParts = filter (ListHT.lengthAtLeast 2 . filter id . Fold.toList) allParts

weightFromBool :: Bool -> Frame.Weight
weightFromBool b =
   if b then Frame.Light else Frame.Empty

checkMatches :: (Frame.Parts Bool -> Char) -> IO ()
checkMatches f = do
   let mismatches =
          filter (uncurry (/=) . snd) $
          map (\parts -> (parts, (Frame.simple parts, f parts))) $
          allParts
   when (not $ null mismatches) $ do
      IO.hPutStrLn IO.stderr "mismatches:"
      mapM_ (IO.hPrint IO.stderr) mismatches
      Exit.exitFailure

test :: IO ()
test = do
   putStrLn "Check consistency of weighted frame elements"
   checkMatches (Frame.weighted . fmap weightFromBool)

   putStrLn "Check consistency of double frame elements"
   checkMatches (Frame.double (Frame.Directions False False))

   putStrLn "Check duplicates of weighted frame elements"
   checkDuplicates (sequenceA $ pure [minBound .. maxBound]) Frame.weighted

   Fold.forM_ (sequenceA $ pure [False, True]) $ \dir -> do
      putStrLn ("Check duplicates of double frame elements, " ++ show dir)
      checkDuplicates longParts (Frame.double dir)
