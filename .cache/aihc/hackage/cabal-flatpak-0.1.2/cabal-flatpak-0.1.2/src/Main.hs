module Main where

import qualified Flatpak
import qualified Generate
import qualified Package
import qualified Retrieve

import qualified Cabal.Plan as Plan
import qualified Crypto.Hash.SHA256 as SHA256

import qualified System.Path.Directory as PathDir
import qualified System.Path as Path
import qualified System.Directory as Dir
import System.Path ((</>))

import qualified Option
import qualified Options.Applicative as OP
import qualified Shell.Utility.ParseArgument as ParseArg
import qualified Shell.Utility.Verbosity as Verbosity
import qualified Shell.Utility.Log as Log

import Control.Monad (when)
import Control.Applicative ((<$>))

import qualified Data.Aeson as Json
import qualified Data.Yaml as Yaml
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as Text
import qualified Data.Char as Char
import qualified Data.Traversable as Trav
import qualified Data.Graph as Graph
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List.HT as ListHT
import Data.Tuple.HT (snd3)
import Data.Map (Map)
import Data.Set (Set)

import Text.Printf (printf)


topSort :: Ord a => Map a (Set a) -> [a]
topSort m =
   reverse $
   (\(graph, lookupVertex, _) ->
      map (snd3 . lookupVertex) $ Graph.topSort graph) $
   Graph.graphFromEdges $ Map.elems $
   Map.mapWithKey (\k v -> ((), k, Set.toList v)) m


isYamlFileName :: Path.File absRel -> Bool
isYamlFileName path =
   case map Char.toLower $ Path.takeExtension path of
      ".yaml" -> True
      ".yml" -> True
      _ -> False

findCabalDir :: IO Path.AbsDir
findCabalDir = do
   xdgDir <- Dir.getXdgDirectory Dir.XdgCache "cabal"
   xdgExists <- Dir.doesDirectoryExist xdgDir
   if xdgExists
      then return $ Path.absDir xdgDir
      else do
         appDir <- PathDir.getAppUserDataDirectory "cabal"
         appExists <- PathDir.doesDirectoryExist appDir
         if appExists
            then return appDir
            else
               fail $
               "Cabal cache directory not found under one these directories: "
               ++
               printf "%s, %s" xdgDir (Path.toString appDir)


main :: IO ()
main = do
   opt <- OP.execParser Option.info
   dir <-
      maybe (fmap Path.toAbsRel PathDir.getCurrentDirectory) return $
      Option.projectDir opt
   plan <-
      Plan.findAndDecodePlanJson $
      Plan.ProjectRelativeToDir $ Path.toString dir
   projectJson <-
      if isYamlFileName $ Option.input opt
         then Yaml.decodeFileThrow (Path.toString $ Option.input opt)
         else
            either fail return =<<
            (Json.eitherDecodeFileStrict' $ Path.toString $ Option.input opt)

   {-
   Only cabal >= 2.4.1.0 provides the SHA 256 hashes.
   see: https://hackage.haskell.org/package/cabal-plan-0.5.0.0/docs/Cabal-Plan.html#v:uCabalSha256
   -}
   let minVersion = Plan.Ver [2,4,1,0]
       curVersion = Plan.pjCabalVersion plan
   when (curVersion < minVersion) $ Log.warn Verbosity.normal $
      printf
         ("plan.json is from Cabal-%s, " ++
          "but only version %s and higher provide SHA256 hashes for Cabal files.\n")
         (Retrieve.formatVersion curVersion)
         (Retrieve.formatVersion minVersion)

   let matchExe name comp =
         case comp of Plan.CompNameExe nameExe -> name == nameExe; _ -> False
   let mainExe = Text.pack $ Flatpak.command $ Flatpak.base projectJson
   let mainPackage = Flatpak.mainPackage projectJson
   let units =
         filter
            (\unit ->
               if Generate.matchName mainPackage unit
                  then any (matchExe mainExe) $ Map.keys $ Plan.uComps unit
                  else Map.member Plan.CompNameLib $ Plan.uComps unit) $
         filter ((Plan.UnitTypeBuiltin /=) . Plan.uType) $
         map (Plan.pjUnits plan Map.!) $ topSort $ Plan.planJsonIdGraph plan

   archs <-
      case Option.archs opt of
         [] ->
            let archStr = Text.unpack $ Plan.pjArch plan
            in maybe
                  (fail $ printf "unsupported architecture: %s" archStr)
                  (return . (:[])) $
               ParseArg.enumMaybe Retrieve.archGHC archStr
         archs -> return archs

   archHashes <- do
      let compiler = Plan.pjCompilerId plan
      sha256Map <- Retrieve.ghcHashes compiler
      either fail return $
         Trav.forM archs $ \arch -> do
            sha256 <- Retrieve.ghcHash sha256Map compiler arch
            return (arch, sha256)

   cabalDir <- findCabalDir
   let hackageDir =
         cabalDir </> Path.dir "packages" </> Path.dir "hackage.haskell.org"

   mainVersion <-
      case filter (Generate.matchName mainPackage) units of
         [Plan.Unit{Plan.uPId = Plan.PkgId _pkg ver}] ->
            return $ Retrieve.formatVersion ver
         [] -> fail "main package not found in build plan"
         _ -> fail "main package found multiple times in build plan"
   (revisions, mainCabalHash) <- do
      tar <- Package.readTar $ hackageDir </> Path.file "01-index.tar.gz"
      either fail return $
         Package.scanIndex tar (mainPackage, mainVersion) $
         Package.unitsMapFromList units
   mainTarHash <-
      fmap SHA256.hashlazy $ BL.readFile $ Path.toString $
      hackageDir
         </> Path.dir mainPackage
         </> Path.dir mainVersion
         </> Path.file (printf "%s-%s.tar.gz" mainPackage mainVersion)

   let (revisedUnits, unrevisedUnits) =
         ListHT.partitionMaybe
            (\unit -> (,) unit <$> Map.lookup (Plan.uPId unit) revisions) $
         map
            (\unit ->
               if Generate.matchName mainPackage unit
                  then unit{
                        Plan.uSha256 = Plan.sha256FromByteString mainTarHash,
                        Plan.uCabalSha256 = Plan.sha256FromByteString mainCabalHash
                       }
                  else unit) $
         units

   when (not $ null unrevisedUnits) $ fail $ unlines $
      "units without a SHA256 hash for the Cabal file:" :
      map (Text.unpack . Plan.dispPkgId . Plan.uPId) unrevisedUnits

   (if isYamlFileName $ Option.output opt
      then
         B.writeFile (Path.toString $ Option.output opt) .
         Flatpak.encodeYaml
      else
         BL.writeFile (Path.toString $ Option.output opt) .
         Flatpak.encodeJson) $
      if Option.cabalInstall opt
         then Generate.manifestCabalInstall
                  plan archHashes revisedUnits projectJson
         else Generate.manifest
                  plan archHashes revisedUnits projectJson
