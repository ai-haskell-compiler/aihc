module FileEmbedLzma.Untyped (
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

import Language.Haskell.TH (Exp, Q, unTypeCode)

import qualified Data.ByteString.Lazy as LBS
import qualified FileEmbedLzma

-- $setup
-- >>> :set -XTemplateHaskell -dppr-cols=9999
-- >>> import qualified Data.ByteString.Lazy as LBS
-- >>> import qualified Data.ByteString as BS
-- >>> import qualified Data.Text.Lazy as LT
-- >>> import qualified Data.Text as T

-------------------------------------------------------------------------------
-- Directories
-------------------------------------------------------------------------------

-- | Embed a @[('FilePath', 'Data.ByteString.ByteString')]@ list, traversing given directory.
embedDir :: FilePath -> Q Exp
embedDir = unTypeCode . FileEmbedLzma.embedDir

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
-- >>> $(embedRecursiveDir "example")
-- [("/Example.hs","..."),("/example.txt","Hello from the inside.\n")]
--
-- >>> :t $(embedRecursiveDir "example")
-- $(embedRecursiveDir "example") :: [(FilePath, BS.ByteString)]
--
embedRecursiveDir :: FilePath -> Q Exp
embedRecursiveDir = unTypeCode . FileEmbedLzma.embedRecursiveDir

-------------------------------------------------------------------------------
-- Strings
-------------------------------------------------------------------------------

-- | Embed a lazy 'Data.ByteString.Lazy.ByteString' from a file.
--
-- >>> :t $(embedLazyByteString "file-embed-lzma.cabal")
-- $(embedLazyByteString "file-embed-lzma.cabal") :: LBS.ByteString
--
embedLazyByteString :: FilePath -> Q Exp
embedLazyByteString = unTypeCode . FileEmbedLzma.embedLazyByteString

-- | Embed a strict 'Data.ByteString.ByteString' from a file.
--
-- >>> :t $(embedByteString "file-embed-lzma.cabal")
-- $(embedByteString "file-embed-lzma.cabal") :: BS...ByteString
--
embedByteString :: FilePath -> Q Exp
embedByteString = unTypeCode . FileEmbedLzma.embedByteString

-- | Embed a lazy 'Data.Text.Lazy.Text' from a UTF8-encoded file.
--
-- >>> :t $(embedLazyText "file-embed-lzma.cabal")
-- $(embedLazyText "file-embed-lzma.cabal") :: LT.Text
--
embedLazyText :: FilePath -> Q Exp
embedLazyText = unTypeCode . FileEmbedLzma.embedLazyText

-- | Embed a strict 'Data.Text.Text' from a UTF8-encoded file.
--
-- >>> :t $(embedText "file-embed-lzma.cabal")
-- $(embedText "file-embed-lzma.cabal") :: T...Text
--
embedText :: FilePath -> Q Exp
embedText = unTypeCode . FileEmbedLzma.embedText

-------------------------------------------------------------------------------
-- Implementation
-------------------------------------------------------------------------------

-- | Makes lazy 'LBS.ByteString' expression.
-- Embedded value is compressed with LZMA.
lazyBytestringE :: LBS.ByteString -> Q Exp
lazyBytestringE = unTypeCode . FileEmbedLzma.lazyBytestringE

listRecursiveDirectoryFiles :: FilePath -> IO [(FilePath, LBS.ByteString)]
listRecursiveDirectoryFiles = FileEmbedLzma.listRecursiveDirectoryFiles

listDirectoryFiles  :: FilePath -> IO [(FilePath, LBS.ByteString)]
listDirectoryFiles = FileEmbedLzma.listDirectoryFiles

listDirectoryFilesF
    :: (FilePath -> IO [(FilePath, LBS.ByteString)]) -- ^ what to do with a sub-directory
    -> FilePath -> IO [(FilePath, LBS.ByteString)]
listDirectoryFilesF = FileEmbedLzma.listDirectoryFilesF
