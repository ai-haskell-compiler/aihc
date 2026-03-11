module Cabal2Spec ( cabal2spec, createSpecFile, ForceBinary, RunTests, CopyrightYear ) where

import Control.Monad
import Data.Char
import Data.List ( delete, nub, sort, (\\), inits, intersect, isPrefixOf, groupBy )
import Data.Time.Clock
import Data.Time.Format
import Distribution.Compiler
import Distribution.License
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.Pretty
import Distribution.Simple.PackageDescription
import Distribution.System
import Distribution.Text
import Distribution.Types.ComponentRequestedSpec
import Distribution.Utils.Path ( getSymbolicPath )
import Distribution.Utils.ShortText ( fromShortText )
import Distribution.Verbosity
import Distribution.Version
import System.FilePath
import System.IO

type ForceBinary = Bool
type RunTests = Bool
type CopyrightYear = Int

cabal2spec :: Platform -> CompilerId -> FlagAssignment -> ForceBinary -> RunTests -> Maybe CopyrightYear
           -> FilePath -> FilePath -> IO ()
cabal2spec platform compilerId flags forceBinary runTests copyrightYear cabalFile specFile = do
  gpd <- readGenericPackageDescription silent cabalFile
  case finalizePD flags requestedComponents (const True) platform (unknownCompilerInfo compilerId NoAbiTag) [] gpd of
    Left missing -> fail ("finalizePD: " ++ show missing)
    Right (pd,_) -> createSpecFile specFile pd forceBinary runTests flags copyrightYear

requestedComponents :: ComponentRequestedSpec
requestedComponents = defaultComponentRequestedSpec

showPkgCfg :: String -> String
showPkgCfg p = "pkgconfig(" ++ p ++ ")"

mkTools :: [String] -> [String]
mkTools tools' = filter excludedTools $ nub $ map mapTools tools'
  where
    excludedTools n = n `notElem` ["ghc", "hsc2hs", "perl"]
    mapTools "gtk2hsC2hs" = "gtk2hs-buildtools"
    mapTools "gtk2hsHookGenerator" = "gtk2hs-buildtools"
    mapTools "gtk2hsTypeGen" = "gtk2hs-buildtools"
    mapTools tool = tool

createSpecFile :: FilePath -> PackageDescription -> ForceBinary -> RunTests -> FlagAssignment -> Maybe CopyrightYear -> IO ()
createSpecFile specFile pkgDesc forceBinary runTests flagAssignment copyrightYear = do
  let deps :: [String]
      deps = map showDevelDep deps' ++ map showProfDep deps'
      deps' :: [String]
      selfdep :: Bool
      (deps', selfdep) = buildDependencies pkgDesc name
      pkgcfgs :: [String]
      pkgcfgs = map showPkgCfg (nub $ map depName $ concatMap pkgconfigDepends buildinfo)
      buildinfo :: [BuildInfo]
      buildinfo = enabledBuildInfos pkgDesc requestedComponents
      tools :: [String]
      tools = mkTools (nub $ map depName (concatMap buildTools buildinfo)) ++ chrpath
      clibs :: [String]
      clibs = nub (map resolveLib (concatMap extraLibs buildinfo))
      chrpath :: [String]
      chrpath = ["chrpath" | selfdep]

      pkg = package pkgDesc
      name = unPackageName (packageName pkg)
      hasExec = hasExes pkgDesc
      hasLib = hasLibs pkgDesc
  (pkgname, binlib) <- getPkgName (Just specFile) pkgDesc forceBinary

  let pkg_name = if pkgname == name then "%{name}" else "%{pkg_name}"
      basename | binlib = "%{pkg_name}"
               | hasExecPkg = name
               | otherwise = "ghc-%{pkg_name}"

      hasExecPkg = binlib || (hasExec && not hasLib)
  -- run commands before opening file to prevent empty file on error
  -- maybe shell commands should be in a monad or something

      testsuiteDeps = testsuiteDependencies pkgDesc name

  h <- openFile specFile WriteMode
  let putHdr hdr val = hPutStrLn h (hdr ++ ":" ++ padding hdr ++ val)
      padding hdr = replicate (14 - length hdr) ' ' ++ " "
      putNewline = hPutStrLn h ""
      put = hPutStrLn h
      putDef v s = put $ unlines [ "%global" +-+ v +-+ s
                                 , "%global pkgver %{pkg_name}-%{version}"
                                 ]
      ghcPkg = if binlib then "-n ghc-%{name}" else ""
      ghcPkgDevel = if binlib then "-n ghc-%{name}-devel" else "devel"

  do
    year <- case copyrightYear of
              Just y -> return (show y)
              Nothing -> formatTime defaultTimeLocale "%Y" <$> getCurrentTime
    put "#"
    put $ "# spec file for package " ++ pkgname
    put "#"
    put $ "# Copyright (c) " ++ year ++ " SUSE LLC"
    put "#"
    put "# All modifications and additions to the file contributed by third parties"
    put "# remain the property of their copyright owners, unless otherwise agreed"
    put "# upon. The license for this file, and modifications and additions to the"
    put "# file, is the same license as for the pristine package itself (unless the"
    put "# license for the pristine package is not an Open Source License, in which"
    put "# case the license is the MIT License). An \"Open Source License\" is a"
    put "# license that conforms to the Open Source Definition (Version 1.9)"
    put "# published by the Open Source Initiative."
    putNewline
    put "# Please submit bugfixes or comments via https://bugs.opensuse.org/"
    put "#"
  putNewline
  putNewline

  -- Some packages conflate the synopsis and description fields.  Ugh.
  let syn = fromShortText (synopsis pkgDesc)
  let initialCapital (c:cs) = toUpper c:cs
      initialCapital [] = []
  let syn' = if badDescription syn then "FIXME" else (unwords . lines . initialCapital) syn
  let summary = rstrip (== '.') . rstrip isSpace $ syn'
  let descr = rstrip isSpace (fromShortText (description pkgDesc))
  let descLines = (formatParagraphs . initialCapital . filterSymbols . finalPeriod) $ if badDescription descr then syn' else descr
      finalPeriod cs = if last cs == '.' then cs else cs ++ "."
      filterSymbols ('@':cs)    = '\'' : filterSymbols cs
      filterSymbols ('\\':c:cs) = c : filterSymbols cs
      filterSymbols (c:cs)      = c : filterSymbols cs
      filterSymbols []          = []
  when hasLib $
    putDef "pkg_name" name

  unless (null testsuiteDeps) $
    if runTests
       then put "%bcond_without tests"
       else put "%bcond_with tests"

  let version = packageVersion pkg
      revision = show $ maybe (0::Int) read (lookup "x-revision" (customFieldsPD pkgDesc))
  putHdr "Name" (if binlib then "%{pkg_name}" else basename)
  putHdr "Version" (display version)
  putHdr "Release" "0"
  putHdr "Summary" summary
  putHdr "License" $ either (show . pretty) showLicense (licenseRaw pkgDesc)
  putHdr "URL" $ "https://hackage.haskell.org/package/" ++ pkg_name
  putHdr "Source0" $ "https://hackage.haskell.org/package/" ++ pkg_name ++ "-%{version}/" ++ pkg_name ++ "-%{version}.tar.gz"
  when (revision /= "0") $
    putHdr "Source1" $ "https://hackage.haskell.org/package/" ++ pkg_name ++ "-%{version}/revision/" ++ revision ++ ".cabal#/" ++ pkg_name ++ ".cabal"
  putHdr "ExcludeArch" "%{ix86}"

  let fixedDeps = ["ghc-Cabal-devel", "ghc-rpm-macros"]
  let alldeps = sort $ nub $ fixedDeps ++ deps ++ tools ++ clibs ++ pkgcfgs ++ ["pkgconfig" | not (null pkgcfgs)]
  let extraTestDeps = sort $ testsuiteDeps \\ deps
  unless (null $ alldeps ++ extraTestDeps) $ do
    mapM_ (putHdr "BuildRequires") alldeps
    unless (null extraTestDeps) $ do
      put "%if %{with tests}"
      mapM_ (putHdr "BuildRequires") extraTestDeps
      put "%endif"

  putNewline

  put "%description"
  mapM_ put descLines

  let wrapGenDesc = wordwrap (79 - max 0 (length pkgname - length pkg_name))

  when hasLib $ do
    when binlib $ do
      put $ "%package" +-+ ghcPkg
      putHdr "Summary" $ "Haskell" +-+ pkg_name +-+ "library"
      putNewline
      put $ "%description" +-+ ghcPkg
      put $ wrapGenDesc $ "This package provides the Haskell" +-+ pkg_name +-+ "shared library."
    put $ "%package" +-+ ghcPkgDevel
    putHdr "Summary" $ "Haskell" +-+ pkg_name +-+ "library development files"
    putHdr "Requires" $ (if binlib then "ghc-%{name}" else "%{name}") +-+ "= %{version}-%{release}"
    putHdr "Requires" "ghc-compiler = %{ghc_version}"
    unless (null $ clibs ++ pkgcfgs) $
      mapM_ (putHdr "Requires") $ sort (clibs ++ pkgcfgs ++ ["pkgconfig" | not (null pkgcfgs)])
    putHdr "Requires(post)" "ghc-compiler = %{ghc_version}"
    putHdr "Requires(postun)" "ghc-compiler = %{ghc_version}"
    putNewline
    put $ "%description" +-+ ghcPkgDevel
    put $ wrapGenDesc $ "This package provides the Haskell" +-+ pkg_name +-+ "library development files."
    put $ unlines
        [ "%package -n ghc-%{pkg_name}-doc"
        , "Summary:        Haskell %{pkg_name} library documentation"
        , "BuildArch:      noarch"
        , "Requires:       ghc-filesystem"
        , ""
        , "%description -n ghc-%{pkg_name}-doc"
        , "This package provides the Haskell %{pkg_name} library documentation."
        , ""
        , ""
        , "%package -n ghc-%{pkg_name}-prof"
        , "Summary:        Haskell %{pkg_name} profiling library"
        , "Requires:       ghc-%{pkg_name}-devel = %{version}-%{release}"
        , "Supplements:    (ghc-%{pkg_name}-devel and ghc-prof)"
        , ""
        , "%description -n ghc-%{pkg_name}-prof"
        , "This package provides the Haskell %{pkg_name} profiling library."
        , ""
        ]

  put "%prep"
  put $ "%autosetup" ++ (if pkgname /= name then " -n %{pkg_name}-%{version}" else "")
  when (revision /= "0") $
    put $ "cp -p %{SOURCE1}" +-+ pkg_name ++ ".cabal"
  putNewline

  put "%build"
  when (flagAssignment /= mempty) $ do
    let cabalFlags = [ "-f" ++ (if b then "" else "-") ++ unFlagName n | (n, b) <- unFlagAssignment flagAssignment ]
    put $ "%define cabal_configure_options " ++ unwords (sort cabalFlags)
  let pkgType = if hasLib then "lib" else "bin"
  put $ "%ghc_" ++ pkgType ++ "_build"
  putNewline

  put "%install"
  put $ "%ghc_" ++ pkgType ++ "_install"

  when selfdep $
    put $ "%ghc_fix_rpath" +-+ "%{pkg_name}-%{version}"

  -- TODO: getSymbolicPath should not be used like this
  let licensefiles = map getSymbolicPath (licenseFiles pkgDesc)

  -- remove docs from datafiles (#38)
  docsUnfiltered <- fmap sort (findDocs (extraSrcFiles pkgDesc ++ extraDocFiles pkgDesc) licensefiles)
  let datafiles = dataFiles pkgDesc
      dupdocs   = docsUnfiltered `intersect` datafiles
      docs      = docsUnfiltered \\ datafiles
  unless (null dupdocs) $
    -- TODO: What does this warning accomplish?
    putStrLn $ "*** " ++ pkgname ++ ": doc files found in datadir:" +-+ unwords (sort dupdocs)
  putNewline

  unless (null testsuiteDeps) $ do
    put "%check"
    put "%cabal_test"
    putNewline

  when hasLib $ do
    let putInstallScript = do
          put "%ghc_pkg_recache"
          putNewline
    put $ "%post" +-+ ghcPkgDevel
    putInstallScript
    put $ "%postun" +-+ ghcPkgDevel
    putInstallScript

  let license_macro = "%license"
  let execs :: [String]
      execs = sort $ map (unUnqualComponentName . exeName) $ filter isBuildable $ executables pkgDesc

  let listDataFiles = unless (null (dataFiles pkgDesc)) $ do
                        put ("%dir %{_datadir}/" ++ pkg_name ++ "-%{version}")
                        mapM_ (put . (("%dir %{_datadir}/" ++ pkg_name ++ "-%{version}/")++) . avoidSquareBrackets) (sort (listDirs (dataFiles pkgDesc)))
                        mapM_ (put . (("%{_datadir}/" ++ pkg_name ++ "-%{version}/")++) . avoidSquareBrackets) (sort (dataFiles pkgDesc))

      listDirs :: [FilePath] -> [FilePath]
      listDirs = nub . concatMap (map joinPath . drop 1 . inits) . nub . map init . filter (\p -> length p > 1) . map splitDirectories

  when hasExecPkg $ do
    put "%files"
    -- Add the license file to the main package only if it wouldn't
    -- otherwise be empty.
    mapM_ (\ l -> put $ license_macro +-+ l) (sort licensefiles)
    unless (null docs) $
      put $ "%doc" +-+ unwords (sort docs)
    mapM_ (\ p -> put $ "%{_bindir}/" ++ (if p == name then "%{name}" else p)) (sort execs)
    listDataFiles
    putNewline

  when hasLib $ do
    let baseFiles = if binlib then "-f ghc-%{name}.files" else "-f %{name}.files"
        develFiles = if binlib then "-f ghc-%{name}-devel.files" else "-f %{name}-devel.files"
    put $ "%files" +-+ ghcPkg +-+ baseFiles
    mapM_ (\ l -> put $ license_macro +-+ l) licensefiles
    unless binlib $
      mapM_ (\ p -> put $ "%{_bindir}/" ++ (if p == name then "%{pkg_name}" else p)) (sort execs)
    unless hasExecPkg listDataFiles
    putNewline
    put $ "%files" +-+ ghcPkgDevel +-+ develFiles
    unless (null docs) $
      put $ "%doc" +-+ unwords (sort docs)
    putNewline
    put "%files -n ghc-%{pkg_name}-doc -f ghc-%{pkg_name}-doc.files"
    mapM_ (\ l -> put $ license_macro +-+ l) licensefiles
    putNewline
    put "%files -n ghc-%{pkg_name}-prof -f ghc-%{pkg_name}-prof.files"
    putNewline

  put "%changelog"
  hClose h


isBuildable :: Executable -> Bool
isBuildable exe = buildable $ buildInfo exe

findDocs :: [FilePath] -> [FilePath] -> IO [FilePath]
findDocs contents licensefiles = do
  let docs = filter likely (sort (nub (map (head . splitDirectories) contents)))
  return $ if null licensefiles
           then docs
           else filter (`notElem` licensefiles) docs
  where names = ["author", "changelog", "changes", "contributors", "copying", "doc",
                 "example", "licence", "license", "news", "readme", "todo"]
        likely name = let lowerName = map toLower name
                      in any (`isPrefixOf` lowerName) names

normalizeVersion :: Version -> Version
normalizeVersion v = case versionNumbers v of
                       [i] -> mkVersion [i,0]
                       _   -> v

showLicense :: License -> String
showLicense (GPL Nothing) = "GPL-1.0-or-later"
showLicense (GPL (Just ver)) = "GPL-" ++ display (normalizeVersion ver) ++ "-or-later"
showLicense (LGPL Nothing) = "LGPL-2.0-or-later"
showLicense (LGPL (Just ver)) = "LGPL-" ++ display (normalizeVersion ver) ++ "-or-later"
showLicense BSD3 = "BSD-3-Clause"
showLicense BSD4 = "BSD-4-Clause"
showLicense MIT = "MIT"
showLicense PublicDomain = "SUSE-Public-Domain"
showLicense AllRightsReserved = "SUSE-NonFree"
showLicense OtherLicense = "Unknown"
showLicense (UnknownLicense l) = "Unknown" +-+ l
showLicense (Apache Nothing) = "Apache-2.0"
showLicense (Apache (Just ver)) = "Apache-" ++ display (normalizeVersion ver)
showLicense (AGPL Nothing) = "AGPL-1.0-or-later"
showLicense (AGPL (Just ver)) = "AGPL-" ++ display (normalizeVersion ver) ++ "-or-later"
showLicense BSD2 = "BSD-2-Clause"
showLicense (MPL ver) = "MPL-" ++ display (normalizeVersion ver)
showLicense ISC = "ISC"
showLicense UnspecifiedLicense = "Unspecified license!"

-- http://rosettacode.org/wiki/Word_wrap#Haskell
wordwrap :: Int -> String -> String
wordwrap maxlen = wrap_ 0 False . words
  where
    wrap_ _ _ [] = "\n"
    wrap_ pos eos (w:ws)
      -- at line start: put down the word no matter what
      | pos == 0 = w ++ wrap_ (pos + lw) endp ws
      | pos + lw + 1 > maxlen - 9 && eos = '\n':wrap_ 0 endp (w:ws)
      | pos + lw + 1 > maxlen = '\n':wrap_ 0 endp (w:ws)
      | otherwise = " " ++ w ++ wrap_ (pos + lw + 1) endp ws
      where
        lw = length w
        endp = last w == '.'

formatParagraphs :: String -> [String]
formatParagraphs = map (wordwrap 79) . paragraphs . lines
  where
    -- from http://stackoverflow.com/questions/930675/functional-paragraphs
    -- using split would be: map unlines . (Data.List.Split.splitWhen null)
    paragraphs :: [String] -> [String]
    paragraphs = map (unlines . filter (not . null)) . groupBy (const $ not . null)

rstrip :: (Char -> Bool) -> String -> String
rstrip p = reverse . dropWhile p . reverse

getPkgName :: Maybe FilePath -> PackageDescription -> Bool -> IO (String, Bool)
getPkgName (Just spec) pkgDesc binary = do
  let name = unPackageName (packageName (package pkgDesc))
      pkgname = takeBaseName spec
      hasLib = hasLibs pkgDesc
  return $ if name == pkgname || binary then (name, hasLib) else (pkgname, False)
getPkgName Nothing pkgDesc binary = do
  let name = unPackageName (packageName (package pkgDesc))
      hasExec = hasExes pkgDesc
      hasLib = hasLibs pkgDesc
  return $ if binary || hasExec && not hasLib then (name, hasLib) else ("ghc-" ++ name, False)

infixr 4 +-+
(+-+) :: String -> String -> String
"" +-+ s = s
s +-+ "" = s
s +-+ t = s ++ " " ++ t

excludedPkgs :: PackageDescription -> String -> Bool
excludedPkgs pkgDesc = flip notElem (subLibs ++ ["ghc-prim", "integer-gmp", "ghc-bignum"])
  where
    subLibs :: [String]
    subLibs = [ unUnqualComponentName ln | l <- subLibraries pkgDesc, LSubLibName ln <- [libName l] ]

-- returns list of deps and whether package is self-dependent
buildDependencies :: PackageDescription -> String -> ([String], Bool)
buildDependencies pkgDesc self =
  let bis   = map libBuildInfo (allLibraries pkgDesc) ++ map buildInfo (executables pkgDesc)
      bdeps = map depName (concatMap targetBuildDepends (filter buildable bis))
      sdeps = maybe [] (map depName . setupDepends) (setupBuildInfo pkgDesc)
      deps  = nub $ bdeps ++ sdeps
  in
    (filter (excludedPkgs pkgDesc) (delete self deps), self `elem` deps && hasExes pkgDesc)

class IsDependency a where
  depName :: a -> String

instance IsDependency Dependency where
  depName (Dependency n _ _) = unPackageName n

instance IsDependency PkgconfigDependency where
  depName (PkgconfigDependency n _) = unPkgconfigName n

instance IsDependency LegacyExeDependency where
  depName (LegacyExeDependency n _) = n

showDevelDep :: String -> String
showDevelDep p = "ghc-" ++ p ++ "-devel"

showProfDep :: String -> String
showProfDep p = "ghc-" ++ p ++ "-prof"

resolveLib :: String -> String
resolveLib "alut" = "freealut-devel"
resolveLib "asound" = "alsa-devel"
resolveLib "blas" = "blas-devel"
resolveLib "bluetooth" = "bluez-devel"
resolveLib "clang" = "clang-devel"
resolveLib "crypt" = "glibc-devel"
resolveLib "crypto" = "libopenssl-devel"
resolveLib "fftw3" = "fftw3-devel"
resolveLib "FLAC" = "flac-devel"
resolveLib "fontconfig" = "fontconfig-devel"
resolveLib "freetype" = "freetype2-devel"
resolveLib "gd" = "gd-devel"
resolveLib "GL" = "Mesa-libGL-devel"
resolveLib "glib-2.0" = "glib2-devel"
resolveLib "GLU" = "glu-devel"
resolveLib "gmp" = "gmp-devel"
resolveLib "gsl" = "gsl-devel"
resolveLib "icudata" = "libicu-devel"
resolveLib "icui18n" = "libicu-devel"
resolveLib "icuuc" = "libicu-devel"
resolveLib "IL" = "DevIL-devel"
resolveLib "Imlib2" = "imlib2-devel"
resolveLib "lapack" = "lapack-devel"
resolveLib "leveldb" = "leveldb-devel"
resolveLib "lmdb" = "lmdb-devel"
resolveLib "lua" = "lua-devel"
resolveLib "luajit" = "luajit-devel"
resolveLib "lzma" = "xz-devel"
resolveLib "m" = "glibc-devel"
resolveLib "magic" = "file-devel"
resolveLib "mpfr" = "mpfr-devel"
resolveLib "odbc" = "unixODBC-devel"
resolveLib "openal" = "openal-soft-devel"
resolveLib "pcre" = "pcre-devel"
resolveLib "png" = "libpng16-compat-devel"
resolveLib "pq" = "postgresql-server-devel"
resolveLib "pthread" = "glibc-devel"
resolveLib "re2" = "re2-devel"
resolveLib "resolv" = "glibc-devel"
resolveLib "ruby" = "ruby-devel"
resolveLib "snappy" = "snappy-devel"
resolveLib "sqlite3" = "sqlite3-devel"
resolveLib "ssl" = "libopenssl-devel"
resolveLib "tag_c" = "libtag-devel"
resolveLib "z" = "zlib-devel"
resolveLib name | "lib" `isPrefixOf` name = name ++ "-devel"
                | otherwise               = "lib" ++ name ++ "-devel"

testsuiteDependencies :: PackageDescription -- ^pkg description
                      -> String             -- ^self
                      -> [String]           -- ^depends
testsuiteDependencies pkgDesc self = map showDevelDep deps ++ map showProfDep deps
  where deps = delete self . filter (excludedPkgs pkgDesc) . nub . map depName $
                 concatMap targetBuildDepends (filter buildable (map testBuildInfo (testSuites pkgDesc)))

badDescription :: String -> Bool
badDescription s = null s
                || "please see readme" `isPrefixOf` map toLower s
                || "please see the readme" `isPrefixOf` map toLower s
                || "see readme" `isPrefixOf` map toLower s
                || "cf readme" `isPrefixOf` map toLower s
                || "please refer to readme" `isPrefixOf` map toLower s
                || "initial project template" `isPrefixOf` map toLower s

-- | @pandoc-2.2.1@ installs a file with square brackets in its name, and that
-- confuses RPM because it thinks those are shell specials.
--
-- TODO: Figure out how this code is supposed to interact with legitimate shell
-- globs, like '*'.

avoidSquareBrackets :: String -> String
avoidSquareBrackets []     = []
avoidSquareBrackets (x:xs)
  | x `elem` "[]"       = '?' : avoidSquareBrackets xs
  | otherwise           = x : avoidSquareBrackets xs
