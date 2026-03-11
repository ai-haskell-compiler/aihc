{-# LANGUAGE CPP                   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
------------------------------------------------------------
-- |
-- Module      :  FileEmbedLzma
-- Copyright   :  (c) 2015-2018 Futurice, 2018 Oleg Grenrus
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
----------------------------------------------------------------------------
module FileEmbedLzma (
    -- * Embed files
    embedByteString,
    embedLazyByteString,
    embedText,
    embedLazyText,
    -- * Embed directories
    embedDir,
    embedRecursiveDir,
    -- * Internal
    -- ** Directory listing
    listDirectoryFiles,
    listRecursiveDirectoryFiles,
    listDirectoryFilesF,
    -- ** Template Haskell
    lazyBytestringE,
    ) where

import Control.Arrow                    (first)
import Control.Monad                    (forM)
import Control.Monad.Trans.State.Strict (runState, state)
import Data.Foldable                    (for_)
import Data.Functor.Compose             (Compose (..))
import Data.Int                         (Int64)
import Data.List                        (sort)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax       (qAddDependentFile)
import System.Directory
       (doesDirectoryExist, getDirectoryContents)
import System.FilePath                  (makeRelative, (</>))
import System.IO.Unsafe                 (unsafePerformIO)

import qualified Codec.Compression.Lzma  as LZMA
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.ByteString.Unsafe  as BS.Unsafe
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as LT
import qualified Data.Text.Lazy.Encoding as LTE

import qualified Data.ByteString.Internal as BS.Internal

import Language.Haskell.TH.Syntax (Bytes (..), liftTyped)

-- $setup
-- >>> :set -XTemplateHaskell -dppr-cols=9999
-- >>> import qualified Data.ByteString.Lazy as LBS
-- >>> import qualified Data.ByteString as BS
-- >>> import qualified Data.Text.Lazy as LT
-- >>> import qualified Data.Text as T

listRecursiveDirectoryFiles :: FilePath -> IO [(FilePath, LBS.ByteString)]
listRecursiveDirectoryFiles = listDirectoryFilesF listRecursiveDirectoryFiles

listDirectoryFiles  :: FilePath -> IO [(FilePath, LBS.ByteString)]
listDirectoryFiles = listDirectoryFilesF (\_ -> return [])

listDirectoryFilesF
    :: (FilePath -> IO [(FilePath, LBS.ByteString)]) -- ^ what to do with a sub-directory
    -> FilePath -> IO [(FilePath, LBS.ByteString)]
listDirectoryFilesF go topdir = do
    names <- getDirectoryContents topdir
    let properNames = filter (`notElem` [".", ".."]) names
    paths <- forM properNames $ \name -> do
        let path = topdir </> name
        isDirectory <- doesDirectoryExist path
        if isDirectory
        then go path
        else do
            contents <- LBS.readFile path
            return [(path, contents)]
    return (concat paths)

makeAllRelative :: FilePath -> [(FilePath, a)] -> [(FilePath, a)]
makeAllRelative topdir = map (first (("/" ++) . makeRelative topdir))

-- | Makes lazy 'LBS.ByteString' expression.
-- Embedded value is compressed with LZMA.
lazyBytestringE :: LBS.ByteString -> Code Q LBS.ByteString
lazyBytestringE lbs =
    [|| LZMA.decompress (LBS.fromStrict (unsafePerformIO (BS.Unsafe.unsafePackAddressLen $$(liftTyped l) $$(unsafeCodeCoerce s)))) ||]
  where
    bs = LBS.toStrict $ LZMA.compressWith params lbs
    s = litE $ bytesPrimL $ bsToBytes bs
    l = BS.length bs

    params = LZMA.defaultCompressParams
        {- doesn't seem to affect much
        { LZMA.compressLevel = LZMA.CompressionLevel9
        , LZMA.compressLevelExtreme = True
        }
        -}

bsToBytes :: BS.ByteString -> Bytes
bsToBytes (BS.Internal.PS fptr off len) = Bytes fptr (fromIntegral off) (fromIntegral len)

makeEmbeddedEntry :: Code Q LBS.ByteString -> (FilePath, (Int64, Int64)) -> Code Q (FilePath, BS.ByteString)
makeEmbeddedEntry name (path, (off, len)) =
    [|| (path, LBS.toStrict (LBS.take len (LBS.drop off $$name))) ||]

concatEntries :: Traversable t => t LBS.ByteString -> (LBS.ByteString, t (Int64, Int64))
concatEntries xs = (bslEndo LBS.empty, ys)
  where
    (ys, (_, bslEndo)) = runState (traverse (state . single) xs) (0, id)

    single
        :: LBS.ByteString                             -- file bytestring
        -> (Int64, LBS.ByteString -> LBS.ByteString)  -- current offset, buffer so far
        -> ((Int64, Int64), (Int64, LBS.ByteString -> LBS.ByteString))
    single bsl (off, endo) = ((off, l), (off + l, endo . LBS.append bsl))
      where
        l = fromIntegral $ LBS.length bsl

-------------------------------------------------------------------------------
-- Directories
-------------------------------------------------------------------------------


-- | Embed a @[('FilePath', 'Data.ByteString.ByteString')]@ list, traversing given directory.
embedDir :: FilePath -> Code Q [(FilePath, BS.ByteString)]
embedDir topdir = joinCode $ do
    pairs' <- runIO $ listDirectoryFiles topdir
    for_ pairs' $ qAddDependentFile . fst
    let pairs = makeAllRelative topdir pairs'
    return $ embedPairs pairs

-- | Embed a @[('FilePath', 'Data.ByteString.ByteString')]@ list, recursively traversing given directory path.
--
-- For example, with @wai-static-app@ this can be used as:
--
-- @
-- staticApp $ embeddedSettings $('embedRecursiveDir' "static")
-- -- is an embedded (no data-files!) equivalent of
-- staticApp $ defaultFileServerSettings "static"
-- @
--
--
-- >>> $$(embedRecursiveDir "example")
-- [("/Example.hs","..."),("/example.txt","Hello from the inside.\n")]
--
-- >>> :t $$(embedRecursiveDir "example")
-- $$(embedRecursiveDir "example") :: [(FilePath, BS.ByteString)]
--
embedRecursiveDir :: FilePath -> Code Q [(FilePath, BS.ByteString)]
embedRecursiveDir topdir = joinCode $ do
    pairs' <- runIO $ listRecursiveDirectoryFiles topdir
    for_ pairs' $ qAddDependentFile . fst
    let pairs = sort (makeAllRelative topdir pairs')
    return $ embedPairs pairs

embedPairs :: [(FilePath, LBS.ByteString)] -> Code Q [(FilePath, BS.ByteString)]
embedPairs pairs = do
    -- we do a hop to only embed single big bytestring.
    -- it's beneficial as lzma have more stuff to compress
    let (bsl, Compose offsets) = concatEntries (Compose pairs)
    let e = [|| let embedded = $$(lazyBytestringE bsl)
                in $$(typedListE (map (makeEmbeddedEntry [|| embedded ||]) offsets)) ||]
    typedSigE e [t| [(FilePath, BS.ByteString)] |]

-------------------------------------------------------------------------------
-- Strings
-------------------------------------------------------------------------------

-- | Embed a lazy 'Data.ByteString.Lazy.ByteString' from a file.
--
-- >>> :t $$(embedLazyByteString "file-embed-lzma.cabal")
-- $$(embedLazyByteString "file-embed-lzma.cabal") :: LBS.ByteString
--
embedLazyByteString :: FilePath -> Code Q LBS.ByteString
embedLazyByteString fp = joinCode $ do
    qAddDependentFile fp
    bsl <- runIO $ LBS.readFile fp
    return $ lazyBytestringE bsl

-- | Embed a strict 'Data.ByteString.ByteString' from a file.
--
-- >>> :t $$(embedByteString "file-embed-lzma.cabal")
-- $$(embedByteString "file-embed-lzma.cabal") :: BS...ByteString
--
embedByteString :: FilePath -> Code Q BS.ByteString
embedByteString fp = [|| LBS.toStrict $$(embedLazyByteString fp) ||]

-- | Embed a lazy 'Data.Text.Lazy.Text' from a UTF8-encoded file.
--
-- >>> :t $$(embedLazyText "file-embed-lzma.cabal")
-- $$(embedLazyText "file-embed-lzma.cabal") :: LT.Text
--
embedLazyText :: FilePath -> Code Q LT.Text
embedLazyText fp = joinCode $ do
    qAddDependentFile fp
    bsl <- runIO $ LBS.readFile fp
    case LTE.decodeUtf8' bsl of
        Left e  -> reportError (show e)
        Right _ -> return ()
    return $ [|| LTE.decodeUtf8 $$(lazyBytestringE bsl) ||]

-- | Embed a strict 'Data.Text.Text' from a UTF8-encoded file.
--
-- >>> :t $$(embedText "file-embed-lzma.cabal")
-- $$(embedText "file-embed-lzma.cabal") :: T...Text
--
embedText :: FilePath -> Code Q T.Text
embedText fp = [|| LT.toStrict $$(embedLazyText fp) ||]

-------------------------------------------------------------------------------
-- TTH helpers
-------------------------------------------------------------------------------

typedSigE :: Code Q a -> Q Type -> Code Q a
typedSigE x a = unsafeCodeCoerce $ sigE (unTypeCode x) a

typedListE :: [Code Q a] -> Code Q [a]
typedListE xs = unsafeCodeCoerce $ listE $ map unTypeCode xs
