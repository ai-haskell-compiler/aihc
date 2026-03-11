module Test.Utility where

import qualified System.Exit as Exit
import qualified System.IO as IO

import qualified Data.List.HT as ListHT
import qualified Data.Map as Map

import Control.Monad (when, )


checkDuplicates :: (Show a) => [a] -> (a -> Char) -> IO ()
checkDuplicates xs f = do
   let duplicates =
          Map.toList $
          Map.filter (ListHT.lengthAtLeast 2) $
          Map.fromListWith (++) $
          map (\x -> (f x, [x])) xs
   when (not $ null duplicates) $ do
      IO.hPutStrLn IO.stderr "duplicates:"
      mapM_ (IO.hPrint IO.stderr) duplicates
      Exit.exitFailure
