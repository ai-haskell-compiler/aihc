{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators  #-}
module Polysemy.FS where

import Polysemy
import qualified UnliftIO.Path.Directory as U
import Data.Text (Text)
import qualified System.IO.Temp as U
import RIO
import Path
import qualified Data.ByteString as BS

data FSExist m a where
  DoesFileExist :: Path b File -> FSExist m Bool
  DoesDirExist  :: Path b Dir  -> FSExist m Bool

makeSem ''FSExist

data FSRead m a where
  ReadFileBS   :: Path b File -> FSRead m BS.ByteString
  ReadFileUtf8 :: Path b File -> FSRead m Text

makeSem ''FSRead

data FSWrite m a where
  WriteFileBS   :: Path b File -> BS.ByteString -> FSWrite m ()
  WriteFileUtf8 :: Path b File -> Text -> FSWrite m ()

makeSem ''FSWrite

data FSCopy m a where
  CopyFile :: Path b File -> Path b' File -> FSCopy m ()

makeSem ''FSCopy

data FSTemp m a where
  CreateTempDirectory :: FSTemp m (Path Abs Dir)

makeSem ''FSTemp

data FSDir m a where
  CreateDirectory :: Path b Dir -> FSDir m ()
  RemoveDirectory :: Path b Dir -> FSDir m ()

makeSem ''FSDir

runFSExist :: Member (Embed IO) r => Sem (FSExist ': r) a -> Sem r a
runFSExist = interpret \case
  DoesFileExist x -> U.doesFileExist x
  DoesDirExist x  -> U.doesDirectoryExist  x

runFSRead :: Member (Embed IO) r => Sem (FSRead ': r) a -> Sem r a
runFSRead = interpret \case
  ReadFileBS x   -> embed $ BS.readFile (toFilePath x)
  ReadFileUtf8 x -> RIO.readFileUtf8 (toFilePath x)

runFSWrite :: Member (Embed IO) r => Sem (FSWrite ': r) a -> Sem r a
runFSWrite = interpret \case
  WriteFileBS x y -> embed $ BS.writeFile (toFilePath x) y
  WriteFileUtf8 x y -> RIO.writeFileUtf8 (toFilePath x) y

runFSCopy :: Member (Embed IO) r => Sem (FSCopy ': r) a -> Sem r a
runFSCopy = interpret \case
  CopyFile x y -> U.copyFile x y

runFSDir :: Member (Embed IO) r => Sem (FSDir ': r) a -> Sem r a
runFSDir = interpret \case
  CreateDirectory x -> U.createDirectoryIfMissing True x
  RemoveDirectory x -> U.removeDirectoryRecursive x

runFSTemp :: Member (Embed IO) r => Sem (FSTemp ': r) a -> Sem r a
runFSTemp = interpret \case
  CreateTempDirectory -> do
    x <- embed U.getCanonicalTemporaryDirectory
    embed $ U.createTempDirectory x "" >>= parseAbsDir
