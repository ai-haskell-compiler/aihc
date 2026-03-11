{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Conduit.Tar.Windows
    ( getFileInfo
    , restoreFileInternal
    ) where

import           Conduit
import           Control.Monad            (when, unless)
import           Control.Exception.Safe   (tryAny, SomeException, toException)
import           Data.Bits
import qualified Data.ByteString.Char8    as S8
import           Data.Conduit.Tar.Types
import           Data.Either              (partitionEithers)
import           Data.Time.Clock.POSIX
import           Foreign.C.Types          (CTime (..))
import qualified System.Directory         as Dir
import qualified System.PosixCompat.Files as Posix
import qualified System.FilePath as FilePath


getFileInfo :: FilePath -> IO FileInfo
getFileInfo fp = do
    fs <- Posix.getSymbolicLinkStatus fp
    let uid = fromIntegral $ Posix.fileOwner fs
        gid = fromIntegral $ Posix.fileGroup fs
    (fType, fSize) <-
        case () of
            () | Posix.isRegularFile fs     -> return (FTNormal, Posix.fileSize fs)
               | Posix.isDirectory fs       -> return (FTDirectory, 0)
               | otherwise                  -> error $ "Unsupported file type: " ++ fp
    return FileInfo
        { filePath      = encodeFilePath fp
        , fileUserId    = uid
        , fileUserName  = ""
        , fileGroupId   = gid
        , fileGroupName = ""
        , fileMode      = Posix.fileMode fs .&. 0o7777
        , fileSize      = fSize
        , fileType      = fType
        , fileModTime   = Posix.modificationTime fs
        }



-- | See 'Data.Conduit.Tar.restoreFileWithErrors' for documentation
restoreFileInternal ::
       (MonadResource m)
    => Bool
    -> FileInfo
    -> ConduitM S8.ByteString (IO (FileInfo, [SomeException])) m ()
restoreFileInternal lenient fi@FileInfo {..} = do
    let fpStr = decodeFilePath filePath
        tryAnyCond action = if lenient then tryAny action else fmap Right action
        CTime modTimeEpoch = fileModTime
        modTime = posixSecondsToUTCTime (fromIntegral modTimeEpoch)
        restoreTimeAndMode = do
            eExc1 <- tryAnyCond $ Dir.setModificationTime fpStr modTime
            eExc2 <- tryAnyCond $ Posix.setFileMode fpStr fileMode
            return $! fst $ partitionEithers [eExc1, eExc2]
    case fileType of
        FTDirectory -> do
            excs <- liftIO $ do
                Dir.createDirectoryIfMissing True fpStr
                restoreTimeAndMode
            yield $ do
                eExc <- tryAnyCond (Dir.doesDirectoryExist fpStr >>=
                                    (`when` Dir.setModificationTime fpStr modTime))
                return (fi, either ((excs ++) . pure) (const excs) eExc)
        FTNormal -> do
            when lenient $ liftIO $ Dir.createDirectoryIfMissing True $ FilePath.takeDirectory fpStr
            sinkFile fpStr
            excs <- liftIO $ restoreTimeAndMode
            unless (null excs) $ yield $ return (fi, excs)
        ty -> do
            let exc = UnsupportedType ty
            unless lenient $ liftIO $ throwM exc
            yield $ return (fi, [toException exc])

