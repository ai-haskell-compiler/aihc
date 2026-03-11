{-# LANGUAGE FlexibleContexts #-}

-- | Functions for performing some IO operations on epub files

module Codec.Epub.IO
   ( getPkgXmlFromZip
   , getPkgPathXmlFromZip
   , getPkgPathXmlFromBS
   , getPkgPathXmlFromDir
   , mkEpubArchive
   , readArchive
   , writeArchive
   )
   where

import Codec.Archive.Zip ( Archive, ZipOption (OptRecursive),
  addFilesToArchive, emptyArchive, findEntryByPath, fromArchive, fromEntry, toArchive )
import Control.Arrow.ListArrows ( (>>>), deep )
import Control.Exception ( SomeException, evaluate, try )
import Control.Monad ( (>=>), forM, liftM )
import Control.Monad.Except ( MonadError, throwError )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lazy ( fromChunks )
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Data.List ( (\\), isPrefixOf )
import System.Directory ( doesDirectoryExist, getDirectoryContents, setCurrentDirectory )
import System.FilePath ( (</>) )
import Text.XML.HXT.Arrow.ReadDocument ( readString )
import Text.XML.HXT.Arrow.XmlArrow ( getAttrValue, hasName, isElem )
import Text.XML.HXT.Arrow.XmlState ( no, runX, withValidate )

import Codec.Epub.Util


locateRootFile :: (MonadIO m, MonadError String m) =>
   FilePath -> String -> m FilePath
locateRootFile containerPath' containerDoc = do
   result <- liftIO $ runX (
      readString [withValidate no] containerDoc
      >>> deep (isElem >>> hasName "rootfile")
      >>> getAttrValue "full-path"
      )

   case result of
      (p : []) -> return p
      _        -> throwError $
         "ERROR: rootfile full-path missing from " ++ containerPath'


-- | Extract a file from a zip archive throwing an error on failure
fileFromArchive :: MonadError String m =>
   FilePath -> Archive -> m String
fileFromArchive filePath archive = do
   let mbEntry = findEntryByPath filePath archive
   maybe
      (throwError $ "Unable to locate file " ++ filePath)
      (return . UTF8.toString . fromEntry) mbEntry


{- | The static location of the container.xml, as specified by the
     epub docs
-}
containerPath :: FilePath
containerPath = "META-INF/container.xml"


{- | Get the path and contents of the epub Package Document from
   a ByteString representing an epub (zip) file
-}
getPkgPathXmlFromBS :: (MonadError String m, MonadIO m)
   => BS.ByteString           -- ^ contents of the zip file
   -> m (FilePath, String)    -- ^ path (within the epub archive) and contents of the epub Package Document
getPkgPathXmlFromBS strictBytes = do
   -- Need to turn this strict byte string into a lazy one
   let lazyBytes = fromChunks [strictBytes]
   result <- liftIO $ ( try $ evaluate
      (toArchive lazyBytes) :: IO (Either SomeException Archive) )
   archive <- either (throwError . show) return result

   {- We need to first extract the container.xml file
      It's required to have a certain path and name in the epub
      and contains the path to what we really want, the .opf file.
   -}
   containerDoc <- fileFromArchive containerPath archive

   let cleanedContents = removeIllegalStartChars . removeEncoding
         . removeDoctype $ containerDoc

   rootPath <- locateRootFile containerPath cleanedContents

   -- Now that we have the path to the .opf file, extract it
   rootContents <- fileFromArchive rootPath archive

   return (rootPath, rootContents)


{- | Get the path and contents of the epub Package Document from
   an epub (zip) file
-}
getPkgPathXmlFromZip :: (MonadError String m, MonadIO m)
   => FilePath                -- ^ path to epub zip file
   -> m (FilePath, String)    -- ^ path (within the epub archive) and contents of the epub Package Document
getPkgPathXmlFromZip zipPath = do
   {- Strictly read this file into a ByteString, send to 
      getPkgPathXmlFromBS
   -}
   zipFileBytes <- liftIO $ BS.readFile zipPath
   getPkgPathXmlFromBS zipFileBytes


-- | Get the contents of the epub Package Document from an epub (zip) file
getPkgXmlFromZip :: (MonadError String m, MonadIO m)
   => FilePath  -- ^ path to epub zip file
   -> m String  -- ^ contents of the epub Package Document
getPkgXmlFromZip zipPath = snd `liftM` getPkgPathXmlFromZip zipPath


{- | Get the path and contents of the epub Package Document from
   a directory containing the files from an epub file (as in:
   it's been unzipped into a dir)
-}
getPkgPathXmlFromDir :: (MonadError String m, MonadIO m)
   => FilePath                -- ^ directory path
   -> m (FilePath, String)    -- ^ path (within the epub archive) and contents of the epub Package Document
getPkgPathXmlFromDir dir = do
   {- We need to first extract the container.xml file
      It's required to have a certain path and name in the epub
      and contains the path to what we really want, the .opf file.
   -}
   liftIO $ setCurrentDirectory dir

   containerDoc <- liftIO $ readFile containerPath

   rootPath <- locateRootFile (dir </> containerPath) containerDoc

   -- Now that we have the path to the .opf file, load it
   rootContents <- liftIO $ readFile rootPath

   return (rootPath, rootContents)


{- Recursively get a list of all files starting with the supplied
   parent directory. Excluding the directories themselves and ANY 
   dotfiles.
-}
getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents parent = do
   fullContents <- getDirectoryContents parent
   let contents = filter (not . isPrefixOf ".") fullContents
   paths <- forM contents $ \name -> do
      let path = parent </> name
      isDirectory <- doesDirectoryExist path
      if isDirectory
         then getRecursiveContents path
         else return [path]
   return $ concat paths


{- | Construct a zip Archive containing epub book data from the 
     specified directory
-}
mkEpubArchive :: FilePath -> IO Archive
mkEpubArchive rootDir = do
   setCurrentDirectory rootDir

   let mimetype = ["mimetype"]
   allFiles <- getRecursiveContents "."
   let restFiles = allFiles \\ mimetype

   flip (addFilesToArchive [OptRecursive]) restFiles >=>
      flip (addFilesToArchive []) ["mimetype"]
      $ emptyArchive


-- | Read a zip Archive from disk
readArchive :: FilePath -> IO Archive
readArchive = fmap toArchive . B.readFile


-- | Write a zip Archive to disk using the specified filename
writeArchive :: FilePath -> Archive -> IO ()
writeArchive zipPath = (B.writeFile zipPath) . fromArchive
