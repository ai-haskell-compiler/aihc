{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Conduit.Tar.Unix
    ( getFileInfo
    , restoreFileInternal
    ) where

import           Conduit                       hiding (throwM)
import           Control.Exception.Safe
import           Control.Monad                 (void, when, unless)
import           Data.Bits
import qualified Data.ByteString.Char8         as S8
import           Data.Either
import           Data.Conduit.Tar.Types
import           Foreign.C.Types               (CTime (..))
import qualified System.Directory              as Dir
import qualified System.Posix.Files            as Posix
import qualified System.Posix.User             as Posix
import qualified System.FilePath.Posix         as Posix
#if MIN_VERSION_unix(2,8,0)
import qualified System.Posix.User.ByteString as UBS
#endif

getFileInfo :: FilePath -> IO FileInfo
getFileInfo fpStr = do
    let fp = encodeFilePath fpStr
    fs <- Posix.getSymbolicLinkStatus fpStr
    let uid = Posix.fileOwner fs
        gid = Posix.fileGroup fs
    -- Allow for username/group retrieval failure, especially useful for non-tty environment.
    -- Workaround for: https://ghc.haskell.org/trac/ghc/ticket/1487
    -- Moreover, names are non-critical as they are not used during unarchival process
#if MIN_VERSION_unix(2,8,0)
    euEntry :: Either IOException UBS.UserEntry <- try $ Posix.getUserEntryForID uid
    egEntry :: Either IOException UBS.GroupEntry <- try $ Posix.getGroupEntryForID gid
    let
      fileUserName = either (const "") UBS.userName euEntry
      fileGroupName = either (const "") UBS.groupName egEntry
#else
    euEntry :: Either IOException Posix.UserEntry <- try $ Posix.getUserEntryForID uid
    egEntry :: Either IOException Posix.GroupEntry <- try $ Posix.getGroupEntryForID gid
    let
      fileUserName = either (const "") (S8.pack . Posix.userName) euEntry
      fileGroupName = either (const "") (S8.pack . Posix.groupName) egEntry
#endif
    (fType, fSize) <-
        case () of
            () | Posix.isRegularFile fs     -> return (FTNormal, Posix.fileSize fs)
               | Posix.isSymbolicLink fs    -> do
                     ln <- Posix.readSymbolicLink fpStr
                     return (FTSymbolicLink (encodeFilePath ln), 0)
               | Posix.isCharacterDevice fs -> return (FTCharacterSpecial, 0)
               | Posix.isBlockDevice fs     -> return (FTBlockSpecial, 0)
               | Posix.isDirectory fs       -> return (FTDirectory, 0)
               | Posix.isNamedPipe fs       -> return (FTFifo, 0)
               | otherwise                  -> error $ "Unsupported file type: " ++ S8.unpack fp
    return $! FileInfo
        { filePath      = fp
        , fileUserId    = uid
        , fileUserName  = fileUserName
        , fileGroupId   = gid
        , fileGroupName = fileGroupName
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
        restorePermissions = do
            eExc1 <- tryAnyCond $ Posix.setOwnerAndGroup fpStr fileUserId fileGroupId
            eExc2 <- tryAnyCond $ Posix.setFileMode fpStr fileMode
            return $! fst $ partitionEithers [eExc1, eExc2]
        -- | Catch all exceptions, but only if lenient is set to True
    case fileType of
        FTDirectory -> do
            excs <- liftIO $ do
                Dir.createDirectoryIfMissing True fpStr
                restorePermissions
            yield $ do
                eExc <- tryAnyCond (Dir.doesDirectoryExist fpStr >>=
                                    (`when` Posix.setFileTimes fpStr fileModTime fileModTime))
                return (fi, either ((excs ++) . pure) (const excs) eExc)
        FTSymbolicLink link -> do
            excs <- liftIO $ do
                -- Try to unlink any existing file/symlink
                void $ tryAny $ Posix.removeLink fpStr
                when lenient $ Dir.createDirectoryIfMissing True $ Posix.takeDirectory fpStr
                Posix.createSymbolicLink (decodeFilePath link) fpStr
                eExc1 <- tryAnyCond $ Posix.setSymbolicLinkOwnerAndGroup fpStr fileUserId fileGroupId
#if MIN_VERSION_unix(2,7,0)
                -- Try best effort in setting symbolic link modification time.
                let CTime epochInt32 = fileModTime
                    unixModTime = fromInteger (fromIntegral epochInt32)
                eExc2 <- tryAny $ Posix.setSymbolicLinkTimesHiRes fpStr unixModTime unixModTime
#endif
                return $ fst $ partitionEithers [eExc1, eExc2]
            unless (null excs) $ yield (return (fi, excs))
        FTHardLink link -> do
            excs <- liftIO $ do
                let linkedFp = decodeFilePath link
                when lenient $ do
                    linkedFileExists <- Posix.fileExist linkedFp
                    -- If the linked file does not exist (yet), we cannot create a hard link.
                    -- Try to "pre-create" it.
                    unless linkedFileExists $ do
                        Dir.createDirectoryIfMissing True $ Posix.takeDirectory linkedFp
                        writeFile linkedFp ""
                Dir.createDirectoryIfMissing True $ Posix.takeDirectory fpStr
                -- Try to unlink any existing file/hard link
                void $ tryAny $ Posix.removeLink fpStr
                Posix.createLink linkedFp fpStr
                liftIO $ do
                    excs <- restorePermissions
                    eExc <- tryAnyCond $ Posix.setFileTimes fpStr fileModTime fileModTime
                    return (either ((excs ++) . pure) (const excs) eExc)
            unless (null excs) $ yield (return (fi, excs))
        FTNormal -> do
            when lenient $ liftIO $ Dir.createDirectoryIfMissing True $ Posix.takeDirectory fpStr
            sinkFile fpStr
            excs <- liftIO $ do
                excs <- restorePermissions
                eExc <- tryAnyCond $ Posix.setFileTimes fpStr fileModTime fileModTime
                return (either ((excs ++) . pure) (const excs) eExc)
            unless (null excs) $ yield $ return (fi, excs)
        ty -> do
            let exc = UnsupportedType ty
            unless lenient $ liftIO $ throwM exc
            yield $ return (fi, [toException exc])

