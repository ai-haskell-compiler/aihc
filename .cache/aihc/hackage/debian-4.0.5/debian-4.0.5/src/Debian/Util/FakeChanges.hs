{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
module Debian.Util.FakeChanges (fakeChanges) where

--import Control.Arrow
import Control.Exception
import Control.Monad hiding (mapM)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Data (Data, Typeable)
import Data.Digest.Pure.SHA as SHA
import Data.Foldable (concat, all, foldr)
import Data.List as List (intercalate, nub, partition, isSuffixOf)
import Data.Maybe
import Debian.Pretty (prettyShow)
import Data.Traversable
import Debian.Control
import qualified Debian.Deb as Deb
import Debian.Time
import Network.HostName (getHostName)
import Prelude hiding (concat, foldr, all, mapM, sum)
import System.Environment
import System.FilePath
import System.Posix.Files
import Text.Regex.TDFA

data Error
    = NoDebs
    | TooManyDscs [FilePath]
    | TooManyTars [FilePath]
    | TooManyDiffs [FilePath]
    | UnknownFiles [FilePath]
    | MalformedDebFilename [FilePath]
    | VersionMismatch [Maybe String]
    deriving (Read, Show, Eq, Typeable, Data)

data Files
    = Files { dsc :: Maybe (FilePath, Paragraph)
            , debs :: [(FilePath, Paragraph)]
            , tar :: Maybe FilePath
            , diff :: Maybe FilePath
            }

fakeChanges :: [FilePath] -> IO (FilePath, String)
fakeChanges fps =
    do files <- loadFiles fps
       let version      = getVersion files
           source       = getSource files
           maintainer   = getMaintainer files
           arches       = getArches files
           binArch      = getBinArch files
           dist         = "unstable"
           urgency      = "low"
           (invalid, binaries) = unzipEithers $ map (debNameSplit . fst) (debs files)
       when (not . null $ invalid) (error $ "Some .deb names are invalid: " ++ show invalid)
       uploader <- getUploader
       date <- getCurrentLocalRFC822Time
       fileLines <- mapM mkFileLine fps
       let changes = Control $ return . Paragraph $ map Field
               [ ("Format"," 1.7")
               , ("Date", ' ' : date)
               , ("Source", ' ' : source)
               , ("Binary", ' ' : (intercalate " " $ map (\(n,_,_) -> n) binaries))
               , ("Architecture", ' ' : intercalate " " arches)
               , ("Version", ' ' : version)
               , ("Distribution", ' ' : dist)
               , ("Urgency", ' ' : urgency)
               , ("Maintainer", ' ' : maintainer)
               , ("Changed-By", ' ' : uploader)
               , ("Description", "\n Simulated description")
               , ("Changes", "\n" ++ unlines (map (' ':) [ source ++ " (" ++ version ++") " ++ dist ++ "; urgency=" ++ urgency
                                                         , "."
                                                         , "  * Simulated changes"
                                                         ]
                                             ))
               , ("Files", "\n" ++ unlines fileLines)
               ]
       return $ (concat [ source, "_", version, "_", binArch, ".changes"], prettyShow changes)
--       let (invalid, binaries) = unzipEithers $ map debNameSplit debs
{-
       when (not . null $ invalid) (throwDyn [MalformedDebFilename invalid])
       version <- getVersion dsc debs
       putStrLn version
       source <- getSource dsc debs
       putStrLn source
-}
-- TODO: seems like this could be more aggressive about ensure the
-- versions make sense. Except with packages like libc, the versions
-- don't make sense. Maybe we want a flag that disables version check
-- ?
getVersion :: Files -> String
getVersion files
    | isNothing (dsc files) =
        let versions = map (fieldValue "Version" . snd) (debs files)
        in
          if (all isJust versions) && (length (nub versions) == 1)
          then fromJust (head versions)
          else error (show [VersionMismatch (nub versions)])
    | otherwise =
        case fieldValue "Version" (snd . fromJust $ dsc files) of
          (Just v) -> v
          Nothing  -> error $ "show (dsc files)" ++ " does not have a Version field :("


getSource :: Files -> String
getSource files =
    let dscSource =
            case (dsc files) of
              Nothing -> []
              (Just (fp, p)) ->
                  case fieldValue "Source" p of
                    (Just v) -> [v]
                    Nothing -> error $ fp ++ " does not have a Source field :("
        debSources = map debSource (debs files)
        srcs = nub (dscSource ++ debSources)
    in
      if (singleton srcs)
         then (head srcs)
         else error $ "Could not determine source."
    where
      debSource (deb,p) =
          case (fieldValue "Source" p) of
            (Just v) -> v
            Nothing ->
                case fieldValue "Package" p of
                  (Just v) -> v
                  Nothing -> error $ "Could not find Source or Package field in " ++ deb



getMaintainer :: Files -> String
getMaintainer files
    | isJust (dsc files) =
        let (fp, p) = fromJust (dsc files)
        in
          case fieldValue "Maintainer" p of
            Nothing -> error $ fp ++ " is missing the Maintainer field."
            (Just v) -> v
    | otherwise =
        let maintainers = catMaybes $ map (fieldValue "Maintainer" . snd) (debs files)
            maintainer = nub maintainers
        in
          if singleton maintainer
             then head maintainer
             else error $ "Could not uniquely determine the maintainer: " ++ show maintainer

getArches :: Files -> [String]
getArches files =
    let debArchs = map (fieldValue "Architecture" . snd) (debs files)
        tarArch  = fmap (const "source") (tar files)
        diffArch = fmap (const "source") (diff files)
    in
      nub $ catMaybes (tarArch : diffArch : debArchs)


getBinArch :: Files -> String
getBinArch files =
    let binArch = nub $ mapMaybe (fieldValue "Architecture" . snd) (debs files)
    in
      if singleton binArch
         then head binArch
         else case (filter (/= "all") binArch) of
                [b] -> b
                _ -> error $ "Could not uniquely determine binary architecture: " ++ show binArch

mkFileLine :: FilePath -> IO String
mkFileLine fp
    | ".deb" `isSuffixOf` fp =
        do sum <- L.readFile fp >>= return . show . sha256
           size <- liftM fileSize $ getFileStatus fp
           (Control (p:_)) <- Deb.fields fp
           return $ concat [ " ", sum, " ", show size, " ", fromMaybe "unknown" (fieldValue "Section" p), " "
                           , fromMaybe "optional" (fieldValue "Priority" p), " ", (takeBaseName fp)
                           ]
    | otherwise =
        do sum <- L.readFile fp >>= return . show . sha256
           size <- liftM fileSize $ getFileStatus fp
           return $ concat [ " ", sum, " ", show size, " ", "unknown", " "
                           , "optional"," ", (takeBaseName fp)
                           ]

-- more implementations can be found at:
-- http://www.google.com/codesearch?hl=en&lr=&q=%22%5BEither+a+b%5D+-%3E+%28%5Ba%5D%2C%5Bb%5D%29%22&btnG=Search
unzipEithers :: [Either a b] -> ([a],[b])
unzipEithers = foldr unzipEither ([],[])
    where
      unzipEither (Left l) ~(ls, rs) = (l:ls, rs)
      unzipEither (Right r) ~(ls, rs) = (ls, r:rs)

-- move to different library
debNameSplit :: String -> Either FilePath (String, String, String)
debNameSplit fp =
    case (takeFileName fp) =~ "^(.*)_(.*)_(.*).deb$" of
      [[_, name, version, arch]] -> Right (name, version, arch)
      _ -> Left fp


loadFiles :: [FilePath] -> IO Files
loadFiles files =
       let (dscs', files'') = partition (isSuffixOf ".dsc") files'
           (debs', files') = partition (isSuffixOf ".deb") files
           (tars', files''') = partition (isSuffixOf ".tar.gz") files''
           (diffs', rest) = partition (isSuffixOf ".diff.gz") files'''
           errors = concat [ if (length debs'  < 1) then [NoDebs] else []
                           , if (length dscs'  > 1) then [TooManyDscs dscs']   else []
                           , if (length tars'  > 1) then [TooManyTars tars']   else []
                           , if (length diffs' > 1) then [TooManyDiffs diffs'] else []
                           , if (length rest  > 0) then [UnknownFiles rest]  else []
                           ]
       in
         do when (not . null $ errors) (error $ show errors)
            dsc' <- mapM loadDsc (listToMaybe dscs')
            debs'' <- mapM loadDeb debs'
            return $ Files { dsc = dsc', debs = debs'', tar = listToMaybe tars', diff = listToMaybe diffs' }
         -- if (not . null $ errors) then throwDyn errors else return (debs, listToMaybe dscs, listToMaybe tars, listToMaybe diffs)
    where
      loadDsc :: FilePath -> IO (FilePath, Paragraph)
      loadDsc dsc' =
          do res <- parseControlFromFile dsc'
             case  res of
               (Left e) -> error $ "Error parsing " ++ dsc' ++ "\n" ++ show e
               (Right (Control [p])) -> return (dsc', p)
               (Right c) -> error $ dsc' ++ " did not have exactly one paragraph: " ++ prettyShow c
      loadDeb :: FilePath -> IO (FilePath, Paragraph)
      loadDeb deb =
          do res <- Deb.fields deb
             case res of
               (Control [p]) -> return (deb, p)
               _ -> error $ deb ++ " did not have exactly one paragraph: " ++ prettyShow res


getUploader :: IO String
getUploader =
    do debFullName <-
           do dfn <- try (getEnv "DEBFULLNAME")
              case dfn of
                (Right n) -> return n
                (Left (_ :: SomeException)) ->
                    do dfn' <-try (getEnv "USER")
                       case dfn' of
                         (Right n) -> return n
                         (Left (_ :: SomeException)) -> error $ "Could not determine user name, neither DEBFULLNAME nor USER enviroment variables were set."
       emailAddr <-
           do eml <- try (getEnv "DEBEMAIL")
              case eml of
                (Right e) -> return e
                (Left (_ :: SomeException)) ->
                    do eml' <- try (getEnv "EMAIL")
                       case eml' of
                         (Right e) -> return e
                         (Left (_ :: SomeException)) -> getHostName -- FIXME: this is not a FQDN
       return $ debFullName ++ " <" ++ emailAddr ++ ">"

-- * Utils

singleton :: [a] -> Bool
singleton [_] = True
singleton _ = False
