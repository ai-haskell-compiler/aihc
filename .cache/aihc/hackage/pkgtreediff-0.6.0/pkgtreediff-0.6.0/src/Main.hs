{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative (
#if !MIN_VERSION_simple_cmd_args(0,1,3)
    (<|>),
#endif
#if !MIN_VERSION_base(4,8,0)
    (<$>), (<*>)
#endif
  )
import Control.Concurrent.Async (concurrently)
import Control.Monad
import Control.Monad.Extra (concatMapM)
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Maybe
import Data.RPM.NVRA
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup ((<>))
#endif
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (decodeUtf8)
import Network.HTTP.Client (httpLbs, managerResponseTimeout, newManager,
                            parseRequest, responseBody, responseTimeoutMicro)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Directory
import Network.HTTP.Types (hContentType)
import SimpleCmdArgs
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import System.FilePath.Glob (compile, match)
#if !MIN_VERSION_simple_cmd(0,2,0)
-- for warning
import System.IO (hPutStrLn, stderr)
#endif

import SimpleCmd (cmd, error',
#if MIN_VERSION_simple_cmd(0,2,0)
                  warning
#endif
                 )

import qualified Distribution.Koji as Koji

import Distribution.RPM.PackageTreeDiff
import Paths_pkgtreediff (version)

data Summary = AutoSummary
             | NoSummary
             | ShowSummary
  deriving Eq

data Mode = Added
          | Deleted
          | Updated
          | Downgraded
  deriving Eq

main :: IO ()
main =
  simpleCmdArgs (Just version) "Package tree comparison tool"
  "pkgtreediff compares the packages in two OS trees or instances" $
  compareDirs
    <$> recursiveOpt
    <*> optional subdirOpt
    <*> ignoreVR
    <*> many modeOpt
    <*> summaryOpt
    <*> switchWith 'R' "rst" "Use ReSTructured Text format"
    <*> optional patternOpt
    <*> timeoutOpt
    <*> sourceArg "1"
    <*> sourceArg "2"
  where
    sourceArg :: String -> Parser String
    sourceArg pos = strArg ("URL|DIR|FILE|KOJITAG|CMD" <> pos)

    modeOpt :: Parser Mode
    modeOpt =
      flagWith' Added 'N' "new" "Show only added packages" <|>
      flagWith' Deleted 'D' "deleted" "Show only removed packages" <|>
      flagWith' Updated 'U' "updated" "Show only upgraded packages" <|>
      flagWith' Downgraded 'u' "downgraded" "Show only downgraded packages"

    summaryOpt =
      flagWith' ShowSummary 's' "show-summary" ("Show summary of changes (default when >" <> show summaryThreshold <> " changes)") <|>
      flagWith AutoSummary NoSummary 'S' "no-summary" "Do not display summary"

    -- ignoreArch :: Parser Bool
    -- ignoreArch = switchWith 'A' "ignore-arch" "Ignore arch differences"

    ignoreVR :: Parser Ignore
    ignoreVR =
      flagWith' IgnoreRelease 'R' "ignore-release" "Only show version changes (ignore release)" <|>
      flagWith IgnoreNone IgnoreVersion 'V' "ignore-version" "Only show package changes (ignore version-release)"

    recursiveOpt :: Parser Bool
    recursiveOpt = switchWith 'r' "recursive" "Recursive down into subdirectories"

    subdirOpt :: Parser String
    subdirOpt = strOptionWith 'd' "subdir" "SUBDIR" "Select specific subdir (eg x86_64 or source)"

    patternOpt :: Parser String
    patternOpt = strOptionWith 'p' "pattern" "PKGPATTERN" "Limit packages to glob matches"

    timeoutOpt :: Parser Int
    timeoutOpt = optionalWith auto 't' "timeout" "SECONDS" "Maximum seconds to wait for http response before timing out (default 30)" 30

-- | The threshold for the number of differences for compareDirs to auto-output a summary
summaryThreshold :: Int
summaryThreshold = 20

data SourceType = URL | Tag | Dir | File | Cmd
  deriving Eq

-- >>> kojiUrlTag "koji://tag@fedora"
-- Just ("tag", "https://koji.fedoraproject.org/kojihub")
kojiUrlTag :: String -> Maybe (String, String)
kojiUrlTag s = if not (isKojiScheme s)
               then Nothing
               else case elemIndex '@' s of
                      Just pos -> Just (drop (length kojiScheme) $ take pos s, hubUrl $ drop (pos+1) s)
                      Nothing -> Nothing
  where
    kojiScheme = "koji://"
    isKojiScheme loc = kojiScheme `isPrefixOf` loc
    hubUrl "fedora" = Koji.fedoraKojiHub
    hubUrl "centos" = Koji.centosKojiHub
    hubUrl loc = loc

sourceType :: String -> IO SourceType
sourceType s
  | isHttp s  = return URL
  | isKoji s  = return Tag
  | otherwise = do
      dir <- doesDirectoryExist s
      if dir then return Dir
        else if ' ' `elem` s then return Cmd
             else return File
  where
    isKoji :: String -> Bool
    isKoji loc = isJust (kojiUrlTag loc)
    isHttp :: String -> Bool
    isHttp loc = "http:" `isPrefixOf` loc || "https:" `isPrefixOf` loc

-- | Frontend for the pkgtreediff tool
compareDirs :: Bool -> Maybe String -> Ignore -> [Mode] -> Summary -> Bool
            -> Maybe String -> Int -> String -> String -> IO ()
compareDirs recursive msubdir ignore modes summary rst mpattern timeout tree1 tree2 = do
  (ps1,ps2) <- getTrees tree1 tree2
  let diff = diffPkgs ignore ps1 ps2
  if rst
    then printRST diff
    else mapM_ putStrLn . mapMaybe (showPkgDiff modes) $ diff
  when (summary /= NoSummary && null modes) $
    when (summary == ShowSummary || length diff > summaryThreshold) $ do
    putStrLn ""
    (if rst then printRSTHeader else putStrLn) "Summary"
    let diffsum = changesummary diff
    putStrLn $ "Updated: " <> show (updateSum diffsum)
    putStrLn $ "Downgraded: " <> show (downgradeSum diffsum)
    putStrLn $ "Added: " <> show (newSum diffsum)
    putStrLn $ "Deleted: " <> show (delSum diffsum)
    putStrLn $ "Arch changed: " <> show (archSum diffsum)
    putStrLn $ "Total packages: " <> show (length ps1) <> " -> " <> show (length ps2)
  where
    printRSTHeader name = do
      putStrLn ""
      putStrLn name
      putStrLn $ replicate (length name) '~'
      putStrLn ""
    printRSTElem = putStrLn . mappend "- "
    printRSTDiffElem = printRSTElem . drop 2
    printRST diff = do
      printRSTHeader "Updated"
      mapM_ printRSTElem $ mapMaybe (showPkgDiff modes) [x | x@(PkgUpdate _ _) <- diff]
      printRSTHeader "Downgraded"
      mapM_ printRSTElem $ mapMaybe (showPkgDiff modes) [x | x@(PkgDowngrade _ _) <- diff]
      printRSTHeader "Added"
      mapM_ printRSTDiffElem $ mapMaybe (showPkgDiff modes) [x | x@(PkgAdd _) <- diff]
      printRSTHeader "Removed"
      mapM_ printRSTDiffElem $ mapMaybe (showPkgDiff modes) [x | x@(PkgDel _) <- diff]

    getTrees :: String -> String -> IO ([NVRA],[NVRA])
    getTrees t1 t2 = do
      when (t1 == t2) $ warning "Comparing the same tree!"
      src1 <- sourceType t1
      src2 <- sourceType t2
      mmgr <- if src1 == URL || src2 == URL
        then do
        let ms = responseTimeoutMicro $ timeout * 1000000
        Just <$> newManager tlsManagerSettings {managerResponseTimeout = ms}
        else return Nothing
      let act1 = readPackages src1 mmgr t1
          act2 = readPackages src2 mmgr t2
      if (src1,src2) == (Cmd,Cmd)
        then do
        ps1 <- act1
        ps2 <- act2
        return (ps1,ps2)
        else concurrently act1 act2

    readPackages :: SourceType -> Maybe Manager -> String -> IO [NVRA]
    readPackages source mmgr loc = do
      fs <- case source of
              URL -> httpPackages True (fromJust mmgr) loc
              Tag -> kojiPackages (fromJust (kojiUrlTag loc))
              Dir -> dirPackages True loc
              File -> filePackages loc
              Cmd -> cmdPackages $ words loc
      let ps = map readNVRA $ maybe id (filter . (match . compile)) mpattern fs
      return $ sort ps

    httpPackages :: Bool -> Manager -> String -> IO [String]
    httpPackages recurse mgr url = do
      exists <- httpExists mgr url
      fs <-
        if exists
        then do
          mcontenttype <- lookup hContentType <$> httpFileHeaders mgr url
          if mcontenttype == Just (B.pack "text/plain; charset=UTF-8")
            then do
            request <- parseRequest url
            body <- responseBody <$> httpLbs request mgr
            return $ (filter (".rpm" `isSuffixOf`) . map TL.unpack . TL.lines . decodeUtf8) body
            else map T.unpack . filter (\f -> "/" `T.isSuffixOf` f || ".rpm" `T.isSuffixOf` f) <$> httpDirectory mgr url
        else error' $ "Could not get " <> url
      if (recurse || recursive) && all isDir fs
        then concatMapM (httpPackages False mgr) (map (url </>) (filterSubdir fs))
        else return $ filter (not . isDir) fs

    filterSubdir :: [String] -> [String]
    filterSubdir fs =
      case msubdir of
        Just subdir | (subdir <> "/") `elem` fs -> [subdir]
        _ -> fs

    dirPackages recurse dir = do
      fs <- map (dir </>) . sort <$> listDirectory dir
      alldirs <- mapM doesDirectoryExist fs
      if (recurse || recursive) && and alldirs
        then concatMapM (dirPackages False) (filterSubdir fs)
        -- FIXME strictly should limit to files
        else return $ filter (".rpm" `isSuffixOf`) fs

    isDir = ("/" `isSuffixOf`)

    filePackages file =
      filter (not . isPrefixOf "gpg-pubkey-") . words <$> readFile file

    cmdPackages [] = error' "No command prefix given"
    cmdPackages (c:args) =
      -- use words since container seems to append '\r'
      filter (not . isPrefixOf "gpg-pubkey-") . words <$> cmd c args

    kojiPackages (tag, kojiUrl) = map Koji.kbNvr <$> Koji.kojiListTaggedBuilds kojiUrl True tag

showPkgDiff :: [Mode] -> RPMPkgDiff -> Maybe String
showPkgDiff modes diff =
  case diff of
    PkgAdd p -> maybeShowDiff Added "+ " $ showNVRA p
    PkgDel p -> maybeShowDiff Deleted "- " $ showNVRA p
    PkgUpdate p1 p2 -> maybeShowDiff Updated "" $ showPkgChange p1 p2
    PkgArch p1 p2 -> maybeShowDiff Updated "" $ showArchChange p1 p2
    PkgDowngrade p1 p2 -> maybeShowDiff Downgraded "~ " $ showPkgChange p1 p2
  where
    maybeShowDiff :: Mode -> String -> String -> Maybe String
    maybeShowDiff m prefix ds
      | null modes || (m `elem` modes && length modes > 1)
      = Just $ prefix ++ ds
      | modes == [m] = Just ds
      | otherwise = Nothing

    showPkgChange :: NVRA -> NVRA -> String
    showPkgChange p p' =
      showPkgIdent p <> ": " <> showPkgVerRel p <> " -> " <> showPkgVerRel p'

    showArchChange :: NVRA -> NVRA -> String
    showArchChange p p' =
      rpmName p <> ": " <> rpmDetails p <> " -> " <> rpmDetails p'
      where
        rpmDetails :: NVRA -> String
        rpmDetails pkg = showPkgVerRel pkg <> "." <> rpmArch pkg

data DiffSum = DS {updateSum, downgradeSum, newSum, delSum, archSum :: Int}

emptyDS :: DiffSum
emptyDS = DS 0 0 0 0 0

changesummary :: [RPMPkgDiff] -> DiffSum
changesummary =
  foldl' countDiff emptyDS
  where
    countDiff :: DiffSum -> RPMPkgDiff -> DiffSum
    countDiff ds pd =
      case pd of
        PkgUpdate {} -> ds {updateSum = updateSum ds + 1}
        PkgDowngrade {} -> ds {downgradeSum = downgradeSum ds + 1}
        PkgAdd _ -> ds {newSum = newSum ds + 1}
        PkgDel _ -> ds {delSum = delSum ds + 1}
        PkgArch {} -> ds {archSum = archSum ds + 1}

#if !MIN_VERSION_simple_cmd(0,2,0)
warning :: String -> IO ()
warning = hPutStrLn stderr
#endif
