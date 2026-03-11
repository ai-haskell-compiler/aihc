{-# LANGUAGE FlexibleInstances #-}

module System.Linux.Btrfs.FilePathLike
    ( FilePathLike(..)
    , RawFilePath
    ) where

import Data.String
import Data.Monoid
import Data.List (dropWhileEnd)
import Foreign.C.String (CString, CStringLen)
import qualified System.Posix.Internals as P
import GHC.IO.Encoding (getFileSystemEncoding)
import qualified GHC.Foreign as GHC
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Char8 as B8
import System.Posix.Types (Fd, FileMode)
import System.Posix.IO hiding (openFd)
import System.Posix.ByteString.FilePath (RawFilePath)
import qualified System.Posix.IO as S (openFd)
import qualified System.Posix.IO.ByteString as B (openFd)
import System.IO.Unsafe (unsafePerformIO)
import Prelude

class (Monoid s, IsString s) => FilePathLike s where
    asString :: s -> String
    peekFilePath :: CString -> IO s
    peekFilePathLen :: CStringLen -> IO s
    withFilePath :: s -> (CString -> IO a) -> IO a
    withFilePathLen :: s -> (CStringLen -> IO a) -> IO a
    unsafeWithFilePathLen :: s -> (CStringLen -> IO a) -> IO a
    openFd :: s -> OpenMode -> OpenFileFlags -> IO Fd
    dropTrailingSlash :: s -> s
    (</>) :: s -> s -> s
    splitFileName :: s -> (s, s)

instance FilePathLike [Char] where
    asString = id
    peekFilePath = P.peekFilePath
    peekFilePathLen = P.peekFilePathLen
    withFilePath = P.withFilePath
    withFilePathLen path action =
        getFileSystemEncoding >>= \enc ->
            GHC.withCStringLen enc path action
    unsafeWithFilePathLen = withFilePathLen
    openFd = S.openFd
    dropTrailingSlash s = if null s' then "/" else s'
      where
        s' = dropWhileEnd (== '/') s
    _ </> s2@('/' : _) = s2
    s1 </> "" = s1
    "" </> s2 = s2
    s1 </> s2 = if s1' == "/" then '/' : s2 else s1' ++ "/" ++ s2
      where
        s1' = dropTrailingSlash s1
    splitFileName s = (if null d then "./" else reverse d, reverse n)
      where
        (n, d) = span (/= '/') $ reverse s

instance FilePathLike B.ByteString where
    asString = convert
    peekFilePath = B.packCString
    peekFilePathLen = B.packCStringLen
    withFilePath = B.useAsCString
    withFilePathLen = B.useAsCStringLen
    unsafeWithFilePathLen = B.unsafeUseAsCStringLen
    openFd = B.openFd
    dropTrailingSlash s = if B.null s' then slashBS else s'
      where
        (s', _) = B8.spanEnd (== '/') s
    s1 </> s2
        | B.null s1 = s2
        | B.null s2 = s1
        | B8.head s2 == '/' = s2
        | otherwise =
            let s1' = dropTrailingSlash s1
            in  if B8.last s1' == '/' then slashBS <> s2
                                      else B.concat [s1', slashBS, s2]
    splitFileName s = (if B.null d then curDir else d, n)
      where
        (d, n) = B8.spanEnd (/= '/') s
        curDir = B8.pack "./"

convert :: (FilePathLike s1, FilePathLike s2) => s1 -> s2
convert s = unsafePerformIO $ unsafeWithFilePathLen s peekFilePathLen

slashBS :: B.ByteString
slashBS = B8.singleton '/'
