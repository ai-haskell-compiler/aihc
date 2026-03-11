{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

-- import Paths_elf
import Prelude as P

import Control.Monad
import Control.Monad.Catch
import Data.Binary
import qualified Data.ByteString as BS
import Data.ByteString.Lazy as BSL
-- import Data.ByteString.Lazy.Char8 as BSC
import Data.Foldable as F
-- import Data.Functor.Identity
import Data.Int
import GHC.IO.Encoding (setLocaleEncoding)
import Prettyprinter
import Prettyprinter.Render.Text
import System.Directory
import System.FilePath
import System.IO as IO
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit

import Control.Exception.ChainedException
import Data.Elf
import Data.Elf.PrettyPrint
import qualified Data.Elf.Headers as H
import Data.Elf.Headers hiding (Header)
import Data.Endian

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM p = foldlM f ([], [])
    where
        f (ts, fs) x = do
            b <- p x
            return $ if b then (x:ts, fs) else (ts, x:fs)

traverseDir :: FilePath -> (FilePath -> IO Bool) -> IO [FilePath]
traverseDir root ok = go root
    where
        go :: FilePath -> IO [FilePath]
        go dir = do
            paths <- P.map (dir </>) <$> listDirectory dir
            (dirPaths, filePaths) <- partitionM doesDirectoryExist paths
            oks <- filterM ok filePaths
            (oks ++) <$> (F.concat <$> mapM go dirPaths)

isElf :: FilePath -> IO Bool
isElf p = if takeExtension p == ".bad" then return False else (elfMagic ==) . decode <$> BSL.readFile p

decodeOrFailAssertion :: Binary a => ByteString -> IO (Int64, a)
decodeOrFailAssertion bs = case decodeOrFail bs of
    Left (_, off, err) -> assertFailure (err ++ " @" ++ show off)
    Right (_, off, a) -> return (off, a)

mkTest'' :: forall a . SingElfClassI a => HeaderXX a -> ByteString -> Assertion
mkTest'' HeaderXX{..} bs = do

    let
        takeLen off len = BSL.take (fromIntegral len) $ BSL.drop (fromIntegral off) bs
        bsSections = takeLen hShOff (hShEntSize * hShNum)
        bsSegments = takeLen hPhOff (hPhEntSize * hPhNum)

    (ss :: [SectionXX a]) <- parseBList hData bsSections
    assertEqual "Section table round trip does not work" bsSections $ serializeBList hData ss

    (ps :: [SegmentXX a]) <- parseBList hData bsSegments
    assertEqual "Segment table round trip does not work" bsSegments $ serializeBList hData ps

mkTest' :: ByteString -> Assertion
mkTest' bs = do
    (off, elfh@(H.Header classS hxx)) <- decodeOrFailAssertion bs
    assertBool "Incorrect header size" (headerSize (fromSingElfClass classS) == off)
    assertEqual "Header round trip does not work" (BSL.take off bs) (encode elfh)

    withSingElfClassI classS mkTest'' hxx bs

mkTest :: FilePath -> TestTree
mkTest p = testCase p $ withBinaryFile p ReadMode (BSL.hGetContents >=> mkTest')

mkGoldenTest' :: FilePath -> FilePath -> (FilePath -> IO (Doc ())) -> FilePath -> TestTree
mkGoldenTest' g o formatFunction file = goldenVsFile file g o mkGoldenTestOutput
    where
        mkGoldenTestOutput :: IO ()
        mkGoldenTestOutput = do
            doc <- formatFunction file
            withFile o WriteMode (`hPutDoc` doc)

mkGoldenTest :: String -> (FilePath -> IO (Doc ())) -> FilePath -> TestTree
mkGoldenTest name formatFunction file = mkGoldenTest' g o formatFunction file
    where
        newBase = "tests" </> file <.> name
        o = newBase <.> "out"
        g = newBase <.> "golden"

mkGoldenTestOSuffix :: String -> String -> (FilePath -> IO (Doc ())) -> FilePath -> TestTree
mkGoldenTestOSuffix name osuffix formatFunction file = mkGoldenTest' g o formatFunction file
    where
        newBase = "tests" </> file <.> name
        o = newBase <.> osuffix <.> "out"
        g = newBase <.> "golden"

------------------------------------------------------------------------------

-- FIXME: define foldMapRBuilderList

index' :: (Integral i, MonadThrow m) => [a] -> i -> m a
index' (x:_) 0 = return x
index' (_:xs) n | n > 0     = index' xs (n-1)
                | otherwise = $chainedError "index': negative argument."
index' _ _                  = $chainedError "index': index too large."

getStringTable :: MonadThrow m => Headers -> BSL.ByteString -> m BSL.ByteString
getStringTable (Headers classS HeaderXX{..} ss _) bs = withSingElfClassI classS
    if hShStrNdx == 0
        then return BSL.empty
        else do
            strs <- index' ss hShStrNdx
            return $ getSectionData bs strs

copyElf :: MonadCatch m => BSL.ByteString -> m BSL.ByteString
copyElf bs = parseElf bs >>= serializeElf

---------------------------------------------------------------------

-- This is for examples/README.md
withHeader :: BSL.ByteString -> (forall a . SingElfClassI a => HeaderXX a -> b) -> Either String b
withHeader bs f =
    case decodeOrFail bs of
        Left (_, _, err) -> Left err
        Right (_, _, (H.Header classS hxx)) -> Right $ withSingElfClassI classS f hxx

printHeaderFile :: FilePath -> IO (Doc ())
printHeaderFile path = do
    bs <- fromStrict <$> BS.readFile path
    $eitherAddContext' $ withHeader bs printHeader

printHeadersFile :: FilePath -> IO (Doc ())
printHeadersFile path = do
    bs <- fromStrict <$> BS.readFile path
    Headers classS hdr ss ps <- parseHeaders bs
    return $ withSingElfClassI classS $ printHeaders hdr ss ps

printStrTableFile :: FilePath -> IO (Doc ())
printStrTableFile path = do
    bs <- readFileLazy path
    hdrs <- parseHeaders bs
    st <- getStringTable hdrs bs
    printStringTable st

printCopyStrTableFile :: FilePath -> IO (Doc ())
printCopyStrTableFile path = do
    bs <- readFileLazy path
    bs' <- copyElf bs
    hdrs <- parseHeaders bs'
    st <- getStringTable hdrs bs'
    printStringTable st

printRBuilderFile :: FilePath -> IO (Doc ())
printRBuilderFile path = do
    bs <- readFileLazy path
    hdrs <- parseHeaders bs
    printLayout hdrs bs

printCopyRBuilderFile :: FilePath -> IO (Doc ())
printCopyRBuilderFile path = do
    bs <- readFileLazy path
    bs' <- copyElf bs
    hdrs <- parseHeaders bs'
    printLayout hdrs bs'

printElfFile :: FilePath -> IO (Doc ())
printElfFile path = do
    bs <- readFileLazy path
    e <- parseElf bs
    printElf e

printCopyElfFile :: FilePath -> IO (Doc ())
printCopyElfFile path = do
    bs <- readFileLazy path
    bs' <- copyElf bs
    e <- parseElf bs'
    printElf e

-----------------------------------------------------------------------

testHeader64 :: H.Header
testHeader64 = H.Header SELFCLASS64 (HeaderXX ELFDATA2LSB 0 0 0 0 0 0 0 0 0 0 0 0 0)

testHeader32 :: H.Header
testHeader32 = H.Header SELFCLASS32 (HeaderXX ELFDATA2MSB 0 0 0 0 0 0 0 0 0 0 0 0 0)

testSection64 :: SectionXX 'ELFCLASS64
testSection64 = SectionXX 0 0 0 0 0 0 0 0 0 0

testSection32 :: SectionXX 'ELFCLASS32
testSection32 = SectionXX 0 0 0 0 0 0 0 0 0 0

testSegment64 :: SegmentXX 'ELFCLASS64
testSegment64 =  SegmentXX 0 0 0 0 0 0 0 0

testSegment32 :: SegmentXX 'ELFCLASS32
testSegment32 =  SegmentXX 0 0 0 0 0 0 0 0

testSymbolTableEntry64 :: SymbolXX 'ELFCLASS64
testSymbolTableEntry64 =  SymbolXX 0 0 0 0 0 0

testSymbolTableEntry32 :: SymbolXX 'ELFCLASS32
testSymbolTableEntry32 =  SymbolXX 0 0 0 0 0 0

mkSizeTest :: Binary a => String -> a -> Int64 -> TestTree
mkSizeTest name v s = testCase name (len @?= s)
    where
        len = BSL.length $ encode v

hdrSizeTests :: TestTree
hdrSizeTests = testGroup "header size" [ mkSizeTest "header 64" testHeader64 (headerSize ELFCLASS64)
                                       , mkSizeTest "header 32" testHeader32 (headerSize ELFCLASS32)

                                       , mkSizeTest "section 64" (Le testSection64) (sectionTableEntrySize ELFCLASS64)
                                       , mkSizeTest "section 32" (Be testSection32) (sectionTableEntrySize ELFCLASS32)

                                       , mkSizeTest "segment 64" (Le testSegment64) (segmentTableEntrySize ELFCLASS64)
                                       , mkSizeTest "segment 32" (Be testSegment32) (segmentTableEntrySize ELFCLASS32)

                                       , mkSizeTest "symbol table entry 64" (Le testSymbolTableEntry64) (symbolTableEntrySize ELFCLASS64)
                                       , mkSizeTest "symbol table entry 32" (Be testSymbolTableEntry32) (symbolTableEntrySize ELFCLASS32)
                                       ]

elfsForHeader :: [String]
elfsForHeader = [ "testdata/orig/bloated"
                , "testdata/orig/tiny"
                , "testdata/orig/vdso"
                ]

main :: IO ()
main = do

    setLocaleEncoding utf8

    elfs <- traverseDir "testdata" isElf

    defaultMain $ testGroup "elf" [ hdrSizeTests
                                  , testGroup "headers round trip"  (mkTest <$> elfs)
                                  , testGroup "elf headers golden"  (mkGoldenTest        "elf_header"      printHeaderFile       <$> elfsForHeader)
                                  , testGroup "header golden"       (mkGoldenTest        "header"          printHeadersFile      <$> elfs)
                                  , testGroup "string table golden" (mkGoldenTest        "strtable"        printStrTableFile     <$> elfs)
                                  , testGroup "layout golden"       (mkGoldenTest        "layout"          printRBuilderFile     <$> elfs)
                                  , testGroup "elf golden"          (mkGoldenTest        "elf"             printElfFile          <$> elfs)
                                  , testGroup "string table copy"   (mkGoldenTestOSuffix "strtable" "copy" printCopyStrTableFile <$> elfs)
                                  , testGroup "layout copy"         (mkGoldenTestOSuffix "layout"   "copy" printCopyRBuilderFile <$> elfs)
                                  , testGroup "elf copy"            (mkGoldenTestOSuffix "elf"      "copy" printCopyElfFile      <$> elfs)
                                  ]
