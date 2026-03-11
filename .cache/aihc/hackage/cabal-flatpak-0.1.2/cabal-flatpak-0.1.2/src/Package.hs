module Package (
   Revision,
   readTar,
   unitsMapFromList,
   scanIndex,
   ) where

import qualified Retrieve

import qualified Cabal.Plan as Plan

import qualified Codec.Compression.GZip as GZip
import qualified Codec.Archive.Tar as Tar
import Codec.Archive.Tar.Entry (fromTarPathToPosixPath, entryTarPath)

import qualified Crypto.Hash.SHA256 as SHA256

import qualified System.Path.PartClass as PartClass
import qualified System.Path as Path

import Control.Applicative (liftA2, (<$>))

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.List as List
import qualified Data.Either.HT as EitherHT
import Data.Map (Map)
import Data.Tuple.HT (mapFst, mapSnd)


readTar ::
   (PartClass.AbsRel absRel) =>
   Path.File absRel -> IO (Tar.Entries Tar.FormatError)
readTar idxTar = do
   let idxTarStr = Path.toString idxTar
   stream <- BL.readFile idxTarStr
   case Path.takeExtensions idxTar of
      ".tar.gz" -> return $ Tar.read $ GZip.decompress stream
      ".tar" -> return $ Tar.read stream
      _ -> fail $ "unknown extension of " ++ idxTarStr

type SHA256 = B.ByteString
type Revision = Int

unitsMapFromList :: [Plan.Unit] -> Map (String,String) Plan.Unit
unitsMapFromList =
   Map.fromList .
   map
      (\unit ->
         let (Plan.PkgId (Plan.PkgName name) version) = Plan.uPId unit
         in ((Text.unpack name, Retrieve.formatVersion version), unit))

maybeFile :: Tar.EntryContent -> Maybe BL.ByteString
maybeFile entry =
   case entry of
      Tar.NormalFile content _size -> Just content
      _ -> Nothing

type Id a = a -> a

forcePair :: a -> b -> (a,b)
forcePair a b = ((,) $! a) $! b

forceNested :: Id ((Map Plan.PkgId Revision, Map Plan.PkgId Revision), SHA256)
forceNested ((count,found), mainHash) =
   forcePair (forcePair count found) mainHash

updateMaps :: (Ord k) =>
   Map k Plan.Unit -> k ->
   (Map Plan.PkgId Revision, Map Plan.PkgId Revision) -> Maybe SHA256 ->
   (Map Plan.PkgId Revision, Map Plan.PkgId Revision)
updateMaps units pkgNameVer (count,found) maybeHash =
   case Map.lookup pkgNameVer units of
      Nothing -> (count,found)
      Just unit ->
         let pkgId = Plan.uPId unit
         in (Map.insertWith (+) pkgId 1 count,
             if Just True ==
                liftA2 (==) maybeHash
                  (Plan.sha256ToByteString <$> Plan.uCabalSha256 unit)
               then Map.insert pkgId (Map.findWithDefault 0 pkgId count) found
               else found)

scanIndex ::
   Tar.Entries Tar.FormatError ->
   (String,String) ->
   Map (String,String) Plan.Unit ->
   Either String (Map Plan.PkgId Revision, SHA256)
scanIndex tar mainPkg units =
   EitherHT.mapRight
      (mapFst $
       \(count,found) ->
         maybe found id $ do
            pkgId <- Plan.uPId <$> Map.lookup mainPkg units
            numRevs <- Map.lookup pkgId count
            return $ Map.insert pkgId (numRevs-1) found) $
   EitherHT.mapLeft (show.fst) $
   Tar.foldlEntries
      (\(maps,mainHash) entry ->
         let path = fromTarPathToPosixPath $ entryTarPath entry
             pkgNameVer =
               mapSnd (takeWhile ('/'/=) . drop 1) $ break ('/'==) path
             maybeHash =
               fmap SHA256.hashlazy $ maybeFile $ Tar.entryContent entry
         in forceNested $
            (if List.isSuffixOf ".cabal" path
               then updateMaps units pkgNameVer maps maybeHash
               else maps,
             case (pkgNameVer == mainPkg, maybeHash) of
                (True, Just hash) -> hash
                _ -> mainHash))
      ((Map.empty, Map.empty), B.empty) tar
