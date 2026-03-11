module Main where

import Debian.Control  -- (Control(..),lookupP,parseControlFromFile)
import Debian.Relation
import System.Process
import System.Exit
import System.Environment

lookupBuildDeps :: FilePath -> IO [BinPkgName]
lookupBuildDeps fp =
    do control <- parseControlFromFile fp
       case control of
         (Left e) -> error (show e)
         (Right (Control [])) -> error "Empty control file"
         (Right (Control (p:_))) ->
             return $ ((lookupDepends "Build-Depends" p) ++
                       (lookupDepends "Build-Depends-Indep" p))

lookupDepends :: String -> Paragraph' String -> [BinPkgName]
lookupDepends key paragraph =
    case fieldValue key paragraph of
                Nothing -> [] -- (Left $ "could not find key " ++ key)
                (Just relationString) ->
                    case parseRelations relationString of
                      (Left e) -> error (show e)
                      (Right andRelations) ->
                          map pkgName (concatMap (take 1) andRelations)
    where
      pkgName :: Relation -> BinPkgName
      pkgName (Rel name _ _) = name


aptGetInstall :: [String] -> [BinPkgName] -> IO ExitCode
aptGetInstall options pkgnames =
    do (_,_,_,ph)
         <- createProcess $ proc "apt-get" $ ["install"] ++ options ++ map unBinPkgName pkgnames
       waitForProcess ph

main :: IO ()
main
  = do
    options <- getArgs
    lookupBuildDeps "debian/control" >>= aptGetInstall options >>= exitWith
