{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import           Conduit
import           Control.Exception
import           Control.Monad         (forM_, void, when, zipWithM_)
import           Data.ByteString       as S
import           Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as LS8
import           Data.ByteString.Short (fromShort)
import           Data.Conduit.List
import           Data.Conduit.Tar
import           Data.Int
import           Data.List             (sortOn)
import           Data.Monoid
import           Prelude               as P
import           System.Directory
import qualified System.FilePath as Host
import qualified System.FilePath.Posix as Posix -- always use forward slashes
import           System.IO
import           System.IO             (BufferMode (LineBuffering),
                                        hSetBuffering, stdout)
import           Test.Hspec
import           Test.QuickCheck

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative   (pure, (<$>))
import           Data.Word
#endif

import Data.Conduit.Zlib (ungzip)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    let baseTmp = "tar-conduit-tests"
    isStack <- doesDirectoryExist ".stack-work"
    let testPaths =
               ["src", "README.md", "ChangeLog.md", "LICENSE"]
#ifndef WINDOWS
            <> ["./tests"]
               -- On Windows, the 'stack test' command results in error message:
               --
               --   uncaught exception: IOException of type PermissionDenied
               --   System.Win32File.read: permission denied (Permission denied)
               --
               -- if '.stack-work' is included in the test paths.
            <> [".stack-work" | isStack]
#else
               -- The package does not support symlinks on Windows. See
               -- Data.Conduit.Tar.Windows.getFileInfo.
            <> ["./tests/files"]
#endif
    hspec $ do
        describe "tar/untar" $ do
            let tarUntarContent dir =
                    runConduitRes $
                    yield dir .| void tarFilePath .| untar (const (foldC >>= yield)) .| foldC
            it "content" $ do
                c <- collectContent "src"
                tarUntarContent "src" `shouldReturn` c
        describe "tar/untar/tar" $ do
            around (withTempTarFiles baseTmp) $
                it "structure" $ \(fpIn, hIn, outDir, fpOut) -> do
                    writeTarball hIn testPaths
                    hClose hIn
                    extractTarball fpIn (Just outDir)
                    curDir <- getCurrentDirectory
                    finally
                        (setCurrentDirectory outDir >> createTarball fpOut testPaths)
                        (setCurrentDirectory curDir)
                    tb1 <- readTarballSorted fpIn
                    tb2 <- readTarballSorted fpOut
                    P.length tb1 `shouldBe` P.length tb2
                    zipWithM_ shouldBe (fmap fst tb2) (fmap fst tb1)
                    zipWithM_ shouldBe (fmap snd tb2) (fmap snd tb1)
        describe "untar" $ do
            around (withTempTarFiles baseTmp) $
                it "create-intermediate" $ \(fpIn, hIn, outDir, fpOut) -> do
                    hClose hIn
                    extractTarballLenient "tests/files/subdir.tar" (Just outDir)
                    curDir <- getCurrentDirectory
                    collectContent (outDir Posix.</> "dir/subdir/") `shouldReturn` "Hello World\n"
        describe "ustar" ustarSpec
        describe "GNUtar" gnutarSpec
        describe "pax" paxSpec
        describe "unsupported headers" $ do
            it "associated payload is discarded" $ do
              contents <- readGzipTarball "./tests/files/libpq-0.3.tar.gz"
              let fileNames = filePath . fst  <$> contents
              fileNames `shouldContain` [ "libpq-0.3/"
                                        , "libpq-0.3/.gitignore"
                                        , "libpq-0.3/Database/"
                                        , "libpq-0.3/Database/PQ.hsc"
                                        , "libpq-0.3/LICENSE"
                                        , "libpq-0.3/README.md"
                                        , "libpq-0.3/Setup.hs"
                                        , "libpq-0.3/libpq.cabal"
                                        ]

defFileInfo :: FileInfo
defFileInfo =
    FileInfo
    { filePath = "test-file-name"
    , fileUserId = 1000
    , fileUserName = "test-user-name"
    , fileGroupId = 1000
    , fileGroupName = "test-group-name"
    , fileMode = 0o644
    , fileSize = 0
    , fileType = FTNormal
    , fileModTime = 123456789
    }


fileInfoExpectation :: [(FileInfo, ByteString)] -> IO ()
fileInfoExpectation files = do
    let source = P.concat [[Left fi, Right content] | (fi, content) <- files]
        collectBack fi = do
            content <- foldC
            yield (fi, content)
    result <- runConduit $ sourceList source .| void tar .| untar collectBack .| sinkList
    result `shouldBe` files

data GnuTarFile = GnuTarFile FileInfo (Maybe ByteString) deriving (Show, Eq)

asciiGen :: Int -> Gen ByteString
asciiGen n = S.pack <$> vectorOf n (frequency [(1, pure 0x2f), (20, choose (0x20, 0x7e))])

instance Arbitrary GnuTarFile where
    arbitrary = do
        filePathLen <- (`mod` 4090) <$> arbitrary
        filePath <- ("test-" <>) . S8.filter (/= ('\\')) <$> asciiGen filePathLen
        NonNegative fileUserId64 <- arbitrary
        let fileUserId = fromIntegral (fileUserId64 :: Int64)
        NonNegative fileGroupId64 <- arbitrary
        let fileGroupId = fromIntegral (fileGroupId64 :: Int64)
        fileUserNameLen <- (`mod` 32) <$> arbitrary
        fileUserName <- asciiGen fileUserNameLen
        fileGroupNameLen <- (`mod` 32) <$> arbitrary
        fileGroupName <- asciiGen fileGroupNameLen
        fileMode <- fromIntegral <$> choose (0o000 :: Word, 0o777)
        -- TODO: use `filePathLen` instead, once long link name 'K' is implemented
        linkNameLen <- (`mod` 101) <$> arbitrary
        fileType <-
            oneof
                [ pure FTNormal
                , pure FTDirectory
                , FTSymbolicLink <$> asciiGen linkNameLen
                , FTHardLink <$> asciiGen linkNameLen
                ]
        (fileSize, mContent) <-
            case fileType of
                FTNormal -> do
                    content <- arbitraryByteString
                    return (fromIntegral (S.length content), Just content)
                _ -> return (0, Nothing)
        fileModTime <- fromIntegral <$> (arbitrary :: Gen Int64)
        return (GnuTarFile FileInfo {..} mContent)

arbitraryByteString :: Gen ByteString
arbitraryByteString = do
    maxLen <- arbitrary
    len <- (`mod` (maxLen + 1)) <$> arbitrary
    genFun <- arbitrary
    let strGen x | x < len = Just (genFun x, x + 1)
                 | otherwise = Nothing
    return $ fst $ S.unfoldrN maxLen strGen 0

fileInfoProperty :: [GnuTarFile] -> Property
fileInfoProperty files = either throw (source ===) eResult
  where
    eResult = runConduit $ sourceList source .| void tar .| untar collectBack .| sinkList
    source =
        P.concat [Left fi : maybe [] ((: []) . Right) mContent | GnuTarFile fi mContent <- files]
    collectBack fi = do
        yield $ Left fi
        case fileType fi of
            FTNormal -> do
                content <- foldC
                yield $ Right content
            _ -> return ()


emptyFileInfoExpectation :: FileInfo -> IO ()
emptyFileInfoExpectation fi = fileInfoExpectation [(fi, "")]

ustarSpec :: Spec
ustarSpec = do
    it "minimal" $ do
        emptyFileInfoExpectation defFileInfo
    it "long file name <255" $ do
        emptyFileInfoExpectation $
            defFileInfo {filePath = S8.pack (P.replicate 99 'f' Posix.</> P.replicate 99 'o')}


gnutarSpec :: Spec
gnutarSpec = do
    it "LongLink - a file with long file name" $ do
        emptyFileInfoExpectation $
            defFileInfo
            { filePath =
                  S8.pack (P.replicate 100 'f' Posix.</>
                           P.replicate 100 'o' Posix.</>
                           P.replicate 99 'b')
            }
    it "LongLink - multiple files with long file names" $ do
        fileInfoExpectation
            [ ( defFileInfo
                { filePath =
                      S8.pack (P.replicate 100 'f' Posix.</>
                               P.replicate 100 'o' Posix.</>
                               P.replicate 99 'b')
                , fileSize = 10
                }
              , "1234567890")
            , ( defFileInfo
                { filePath =
                      S8.pack (P.replicate 1000 'g' Posix.</>
                               P.replicate 1000 'o' Posix.</>
                               P.replicate 99 'b')
                , fileSize = 11
                }
              , "abcxdefghij")
            ]
    it "Large User Id" $ do emptyFileInfoExpectation $ defFileInfo {fileUserId = 0o777777777}
    it "All Large Numeric Values" $ do
        emptyFileInfoExpectation $
            defFileInfo
            { fileUserId = 0x7FFFFFFFFFFFFFFF
            , fileGroupId = 0x7FFFFFFFFFFFFFFF
            , fileModTime = fromIntegral (maxBound :: Int64)
            }
    it "Negative Values" $ do
        emptyFileInfoExpectation $ defFileInfo {fileModTime = fromIntegral (minBound :: Int64)}
        emptyFileInfoExpectation $ defFileInfo {fileModTime = -10}
        emptyFileInfoExpectation $ defFileInfo {fileUserId = fromIntegral (minBound :: Int64)}
    it "Negative Size" $
        (emptyFileInfoExpectation (defFileInfo {fileSize = -10}) `shouldThrow`
         (\case
              TarCreationError _ -> True
              _ -> False))
    it "tar/untar Property" $ property fileInfoProperty


withTempTarFiles :: FilePath -> ((FilePath, Handle, FilePath, FilePath) -> IO c) -> IO c
withTempTarFiles base =
    bracket
        (do tmpDir <- getTemporaryDirectory
            (fp1, h1) <- openBinaryTempFile tmpDir (Host.addExtension base ".tar")
            let outPath = Host.dropExtension fp1 ++ ".out"
            return (fp1, h1, outPath, Host.addExtension outPath ".tar")
        )
        (\(fp, h, dirOut, fpOut) -> do
             hClose h
             removeFile fp
             doesDirectoryExist dirOut >>= (`when` removeDirectoryRecursive dirOut)
             doesFileExist fpOut >>= (`when` removeFile fpOut)
        )

-- | Collects all of the files and direcotries from the tarball. Then all of them get sorted, since
-- apparently Windows has no guaranteed order the files within a directory will be listed in upon a
-- tarball creation.
readTarballSorted
  :: FilePath -> IO [(FileInfo, Maybe ByteString)]
readTarballSorted fp =
  sortOn (filePath . fst) <$>
  (runConduitRes $ sourceFileBS fp .| untar grabBoth .| sinkList)

readGzipTarball
  :: FilePath
  -> IO [(FileInfo, Maybe ByteString)]
readGzipTarball fp =
  runConduitRes $ sourceFileBS fp .| ungzip .| untar grabBoth .| sinkList

grabBoth
  :: (Monad m)
  => FileInfo
  -> ConduitM ByteString (FileInfo, Maybe ByteString) m ()
grabBoth fi =
  case fileType fi of
    FTNormal -> do
        content <- foldC
        yield (fi, Just content)
    _ -> yield (fi, Nothing)

collectContent :: FilePath -> IO (ByteString)
collectContent dir =
    runConduitRes $
    sourceDirectoryDeep False dir .| mapMC (\fp -> runConduit (sourceFileBS fp .| foldC)) .| foldC

-- | This test uses untar to process a simple example in the pax interchange
-- format.
paxSpec :: Spec
paxSpec = assert payloadSizeCheck $ do
    it "untarChunksRaw, pax interchange format" $ do
        res <- runConduitRes $
               paxExample
            .| chopEvery bigChopSize
            .| untarChunksRaw
            .| processTarChunks
            .| sinkList
        pure res `shouldReturn`
            [ "/pax-global-header"
            , "payload: 19 comment=Example\n"
            , "/pax-extended-header"
            , "payload: " <> bigPayload1
            , "payload: " <> bigPayload2
            , "original-dir/original-filepath"
            , "payload: " <> smallPayload
            ]
    it "untar, pax interchange format" $ do
        res <- runConduitRes $
               paxExample
            .| chopEvery smallChopSize
            .| untar process
            .| sinkList
        pure res `shouldReturn`
            [ (veryLongFilepath, smallPayload1)
            , (veryLongFilepath, smallPayload2)
            ]
    it "untarRaw, pax interchange format" $ do
        res <- runConduitRes $
               paxExample
            .| chopEvery smallChopSize
            .| untarRaw process
            .| sinkList
        pure res `shouldReturn`
            [ ("original-dir/original-filepath", smallPayload1)
            , ("original-dir/original-filepath", smallPayload2)
            ]
  where
    process fi = awaitForever $ \bs -> yield (filePath fi, bs)
    processTarChunks = awaitForever $ \tc -> yield $ case tc of
        ChunkHeader h -> fromShort $
            headerFileNamePrefix h <> "/" <> headerFileNameSuffix h
        ChunkPayload _ bs -> "payload: " <> bs
        ChunkException e -> "exception: " <> S8.pack (show e)
    chopEvery :: (MonadIO m) => Int -> ConduitT ByteString ByteString m ()
    chopEvery n = chop
      where
        chop = await >>= \case
            Nothing -> pure ()
            Just val -> do
                forM_ (split (S.unpack val)) $ \chunk -> yield (S.pack chunk)
                chop
        split = P.takeWhile (not . P.null)
              . P.map (P.take n)
              . P.iterate (P.drop n)
    bigChopSize = 512
    (bigPayload1, bigPayload2) = S.splitAt bigChopSize veryLongFilepathRecord
    smallChopSize = 4
    (smallPayload1, smallPayload2) = S.splitAt smallChopSize smallPayload
    moreThanHalf s l = l > s && l <= 2 * s
    payloadSizeCheck =
           moreThanHalf bigChopSize (S.length veryLongFilepathRecord)
        && moreThanHalf smallChopSize (S.length smallPayload)


-- | Produces a simple example in the pax interchange format. It has a pax
-- \'global\' header block providing a comment, a pax \'next\' header block
-- providing a very long path (@\"very\very\...\very\long\filepath\"@), and a
-- normal file with filepath @\"original-filepath\"@ and payload @\"payload\".
paxExample :: MonadThrow m => ConduitM a ByteString m ()
paxExample = void $
       yieldMany
           [ Left globalHeader
           , Right globalPayload
           , Left extendedHeader
           , Right extendedPayload
           , Left ustarHeader
           , Right ustarPayload
           ]
    .| void tarEntries
  where
    defaultHeader :: FileOffset -> Header
    defaultHeader offset = Header
        { headerOffset = offset
        , headerPayloadOffset = offset + 512
        , headerFileNameSuffix = mempty
        , headerFileMode = 0o666
        , headerOwnerId = 0x0
        , headerGroupId = 0x0
        , headerPayloadSize = 0x0
        , headerTime = 0x0
        , headerLinkIndicator = 0x0
        , headerLinkName = mempty
        , headerMagicVersion = "ustar"
        , headerOwnerName = "root"
        , headerGroupName = "root"
        , headerDeviceMajor = 0x0
        , headerDeviceMinor = 0x0
        , headerFileNamePrefix = mempty
        }
    nextOffset :: Header -> FileOffset
    nextOffset h =
        let payloadRecordCount = (headerPayloadSize h + 511) `div` 512
        in  headerPayloadOffset h + 512 + payloadRecordCount * 512
    globalHeader = (defaultHeader 0x0)
        { headerFileNameSuffix = "pax-global-header"
        , headerPayloadSize = fromIntegral $ S.length globalPayload
        , headerLinkIndicator = 0x67 -- UTF-8 'g'
        }
    globalPayload = "19 comment=Example\n"
    extendedHeader = (defaultHeader $ nextOffset globalHeader)
        { headerFileNameSuffix = "pax-extended-header"
        , headerPayloadSize = fromIntegral $ S.length extendedPayload
        , headerLinkIndicator = 0x78 -- UTF-8 'x'
        }
    -- The path in the pax extended header should override the filepath
    -- specified in the ustar header.
    extendedPayload = veryLongFilepathRecord
    ustarHeader = (defaultHeader $ nextOffset extendedHeader)
        { headerFileNameSuffix = "original-filepath"
        , headerPayloadSize = fromIntegral $ S.length ustarPayload
        , headerLinkIndicator = 0x30 -- UTF-8 '0'
        , headerFileNamePrefix = "original-dir"
        }
    ustarPayload = smallPayload

-- | A very/very/.../very/long/filepath with 653 bytes.
veryLongFilepath :: ByteString
veryLongFilepath =
    S8.toStrict (LS8.take 640 $ LS8.cycle "very/") <> "long/filepath"

-- | A very, very, ..., very, long filepath record with 663 bytes.
veryLongFilepathRecord :: ByteString
veryLongFilepathRecord = "663 path=" <> veryLongFilepath <> "\n"

-- | A small payload.
smallPayload :: ByteString
smallPayload = "payload"
