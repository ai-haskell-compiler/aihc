module EqualFiles where

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL

import System.IO (IOMode(ReadMode), withBinaryFile, openBinaryFile, hClose, )
import Control.Exception (evaluate, )

import Data.List (sort, )
import Data.List.HT (groupBy, )
import Data.Eq.HT (equating, )


groupEqualElems :: (Ord key, Ord elem) => [(elem, key)] -> [[key]]
groupEqualElems =
   map (map snd) . groupBy (equating fst) . sort

{- | Only return sub-lists with more than one element. -}
clusterEqualElems :: (Ord key, Ord elem) => [(elem, key)] -> [[key]]
clusterEqualElems =
   filter (not . null . tail) . groupEqualElems

{- |
This version does not close the files,
since for the most files 'readFile' does not reach the end of the file
and thus leaves the files open.
-}
clusterLeaveOpen :: [FilePath] -> IO [[FilePath]]
clusterLeaveOpen fileNames =
   do files <- mapM BL.readFile fileNames
      return (clusterEqualElems (zip files fileNames))

cluster :: [FilePath] -> IO [[FilePath]]
cluster fileNames =
   do (handles, files) <-
         fmap unzip $
         mapM
            (\fn ->
               do h <- openBinaryFile fn ReadMode
                  fmap ((,) h) $ BL.hGetContents h)
            fileNames
      clusters <-
         mapM (mapM evaluate) $
         clusterEqualElems $ zip files fileNames
      mapM_ hClose handles
      return clusters


readPrefix :: Int -> FilePath -> IO B.ByteString
readPrefix size fileName =
   withBinaryFile fileName ReadMode $ flip B.hGet size

clusterWithPreSort :: Int -> [FilePath] -> IO [[FilePath]]
clusterWithPreSort size fileNames =
   do {- The file prefixes are read strictly
         and thus there is only one file open at a time. -}
      files <- mapM (readPrefix size) fileNames
      {- The files are preclustered according to their prefixes
         and then the preclusters are finally clustered. -}
--      fmap (const []) $ mapM_ (putStrLn . unwords . map show) $
      fmap concat $ mapM cluster $
         clusterEqualElems $ zip files fileNames
