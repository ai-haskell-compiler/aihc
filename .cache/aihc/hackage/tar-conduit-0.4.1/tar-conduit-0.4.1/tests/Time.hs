{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Measure time usage by the tar/untar functions.

module Main where

import           Conduit
import           Control.DeepSeq
import           Control.Monad
import           Criterion.Main
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Tar as Tar
import           Data.Conduit.Tar.Types
import           Data.Monoid
import           GHC.Generics
import           System.Posix.Types

main :: IO ()
main =
  defaultMain
    (concat
       [ [ bench
           ("tar " ++ show count ++ " files")
           (nfIO
              (runConduitRes
                 (CL.sourceList files .| void Tar.tar .| CL.sinkNull)))
         | count :: Int <- [1, 10, 100, 1000, 10000]
         , let !files =
                 force
                   (concat
                      (map
                         (\i -> makeFileN (S8.pack (show i) <> ".txt") 10)
                         [1 :: Int .. count]))
         ]
       , [ bench
           ("tar file of " ++ show bytes ++ " bytes")
           (nfIO
              (runConduitRes
                 (CL.sourceList files .| void Tar.tar .| CL.sinkNull)))
         | bytes :: Int <- [1, 10, 100, 1000, 10000]
         , let !files = force (makeFileN "file.txt" bytes)
         ]
       , [ bench
           ("untar " ++ show count ++ " files")
           (nfIO
              (runConduitRes
                 (CL.sourceList files .| void Tar.tar .|
                  void (Tar.untar (const (return ()))) .|
                  CL.sinkNull)))
         | count :: Int <- [1, 10, 100, 1000, 10000]
         , let !files =
                 force
                   (concat
                      (map
                         (\i -> makeFileN (S8.pack (show i) <> ".txt") 10)
                         [1 :: Int .. count]))
         ]
       , [ bench
           ("untar file of " ++ show bytes ++ " bytes")
           (nfIO
              (runConduitRes
                 (CL.sourceList files .| void Tar.tar .|
                  void (Tar.untar (const (return ()))) .|
                  CL.sinkNull)))
         | bytes :: Int <- [1, 10, 100, 1000, 10000]
         , let !files = force (makeFileN "file.txt" bytes)
         ]
       ])


----------------------------------------------------------------------
-- Helpers

makeFileN :: ByteString -> Int -> [Either FileInfo ByteString]
makeFileN fname bytes =
  let contents = S8.pack (take bytes (cycle "Hello Dave"))
  in [ Left
         FileInfo
         { filePath = fname
         , fileUserId = 0
         , fileUserName = "test"
         , fileGroupId = 0
         , fileGroupName = "test"
         , fileMode = 0
         , fileSize = fromIntegral (S.length contents)
         , fileType = FTNormal
         , fileModTime = 1234
         }
     , Right contents
     ]

----------------------------------------------------------------------
-- NFData helper instances. If ever these instances become available,
-- these can just be removed.

deriving instance Generic FileInfo
instance NFData FileInfo

deriving instance Generic FileType
instance NFData FileType

deriving instance Generic CUid
instance NFData CUid

deriving instance Generic COff
instance NFData COff

deriving instance Generic CGid
instance NFData CGid

deriving instance Generic CMode
instance NFData CMode
