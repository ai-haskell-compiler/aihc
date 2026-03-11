module Main where

import System.FilePath (takeBaseName)

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Maybe (mapMaybe)
import Data.Foldable (foldMap)


singleton :: FilePath -> Maybe (Map String (Map Char FilePath))
singleton "" = Nothing
singleton str@(c:cs) =
   Just $ Map.singleton (takeBaseName cs) $ Map.singleton c str

group :: [String] -> ([Char], [(String, [FilePath])])
group xs =
   let ys = Map.unionsWith Map.union $ mapMaybe singleton xs
       columns = Set.toList $ foldMap Map.keysSet ys
       emptyColumns = Map.fromList $ map (flip (,) "") columns
   in  (columns,
        Map.toAscList $ fmap (Map.elems . flip Map.union emptyColumns) ys)

formatCSV :: ([Char], [(String, [FilePath])]) -> [String]
formatCSV (header, rows) =
   ("Name," ++ List.intersperse ',' header) :
   map (\(name,cells) -> List.intercalate "," $ name:cells) rows

main :: IO ()
main = interact $ unlines . formatCSV . group . lines
