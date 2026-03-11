module Retrieve where

import qualified Flatpak

import qualified Cabal.Plan as Plan

import qualified Network.HTTP.Client as Http
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)

import Control.Monad (when)

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Encoding as TextEnc
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.List.HT as ListHT
import Data.Map (Map)
import Data.Tuple.HT (mapFst)
import Data.Bool.HT (if')

import Text.Printf (printf)


formatVersion :: Plan.Ver -> String
formatVersion = Text.unpack . Plan.dispVer

archGHC :: Flatpak.Arch -> String
archGHC arch =
   case arch of
      Flatpak.I386 -> "i386"
      Flatpak.X86_64 -> "x86_64"
      Flatpak.ARM -> "armv7"
      Flatpak.AArch64 -> "aarch64"

ghcDirUrl :: Plan.PkgId -> String
ghcDirUrl (Plan.PkgId (Plan.PkgName compName) compVersion) =
   printf
      "https://downloads.haskell.org/%s/%s/"
      (Text.unpack compName)
      (formatVersion compVersion)

{- |
ghc-8.4.4-x86_64-deb9 is available but fails due to missing libnuma.so.1
-}
ghcNameScheme :: Plan.Ver -> String
ghcNameScheme ghcVer =
   if' (ghcVer >= Plan.Ver [8, 6,1]) "%s-%s-deb9-linux.tar.xz" $
   if' (ghcVer >= Plan.Ver [7,10,3]) "%s-%s-deb8-linux.tar.xz" $
   if' (ghcVer >= Plan.Ver [7, 8,1]) "%s-%s-unknown-linux-deb7.tar.xz" $
   "%s-%s-unknown-linux.tar.bz2"

ghcArchive :: Plan.PkgId -> Flatpak.Arch -> String
ghcArchive compiler@(Plan.PkgId _ compVersion) arch =
   printf (ghcNameScheme compVersion)
      (Text.unpack $ Plan.dispPkgId compiler)
      (archGHC arch)


httpsGet :: String -> IO BL.ByteString
httpsGet url = do
   response <- do
      request <- Http.parseRequest url
      manager <- Http.newManager tlsManagerSettings
      Http.httpLbs request manager
   case statusCode $ Http.responseStatus response of
      200  -> return $ Http.responseBody response
      code -> fail $ "HTTPS get failed with status code: " ++ show code

ghcHashes :: Plan.PkgId -> IO (String, Map BL.ByteString BL.ByteString)
ghcHashes compiler = do
   let sha256Url = ghcDirUrl compiler ++ "SHA256SUMS"
   ghcSha256s <- httpsGet sha256Url
   let (ghcSha256Map, ghcSha256Corrupts) =
         mapFst Map.fromList $
         ListHT.partitionMaybe
            (\line ->
               case BL.words line of
                  [hash,archive] -> Just (archive,hash)
                  _ -> Nothing)
            (BL.lines ghcSha256s)
   when (not $ null ghcSha256Corrupts) $ fail $ unlines $
      (printf "corrupt lines in %s:" sha256Url) :
      map BL.unpack ghcSha256Corrupts
   return (sha256Url,ghcSha256Map)

ghcHash ::
   (String, Map BL.ByteString BL.ByteString) ->
   Plan.PkgId -> Flatpak.Arch -> Either String Plan.Sha256
ghcHash (sha256Url,ghcSha256Map) compiler arch = do
   let ghcName = ghcArchive compiler arch
   let maybeFail msg = maybe (Left msg) Right
   hashStr <-
      maybeFail
         (printf "could not find SHA256 checksum for %s in %s"
            ghcName sha256Url) $
      Map.lookup (BL.pack $ "./"++ghcName) ghcSha256Map
   sha256 <-
      maybeFail ("could not parse SHA256 checksum " ++ BL.unpack hashStr) $
      Plan.parseSha256 $
      TextEnc.decodeLatin1 $ B.concat $ BL.toChunks hashStr
   return sha256
