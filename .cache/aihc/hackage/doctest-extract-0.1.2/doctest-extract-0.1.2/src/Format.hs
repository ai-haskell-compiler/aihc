module Format where

import qualified ModuleName
import Parse (Module(..), Line, Column)
import Test.DocTest.Parse (DocTest(Property, Example))
import Test.DocTest.Location (Located(Located))

import qualified Data.Monoid.HT as Mn
import qualified Data.List.HT as ListHT
import Data.Semigroup ((<>))
import Data.Foldable (foldMap)
import Data.Char (isSpace)

import qualified System.Path.IO as PathIO
import qualified System.Path.PartClass as PathClass
import qualified System.Path as Path
import System.Path.Directory (createDirectoryIfMissing)
import System.Path ((</>))

import Text.Printf (printf)



indentRemainder :: Int -> String -> String
indentRemainder n str =
   let (prefix, suffix) = break isSpace str
   in prefix ++ Mn.when (not $ null suffix) (replicate n ' ' ++ suffix)


type Pos = (Line, Column)

type Flags = (Bool, Bool)

writeTestSuite ::
   (PathClass.AbsRel ar) =>
   Path.Dir ar ->
   ModuleName.T -> Flags -> [Module [Located Pos DocTest]] -> IO ()
writeTestSuite outDir testPrefix flags ms = do
   let testDir = outDir </> ModuleName.dirPath testPrefix
   mapM_ (writeTestModule testDir testPrefix flags) ms

importDriver :: String
importDriver = "import qualified Test.DocTest.Driver as DocTest"

writeTestMain ::
   (PathClass.AbsRel ar) =>
   Bool ->
   Path.File ar -> ModuleName.T ->
   ModuleName.T -> [Module [Located Pos DocTest]] -> IO ()
writeTestMain run path mainName testPrefix ms = do
   let indent = map ("    " ++)
   let prefixed = ModuleName.string . (testPrefix<>) . moduleName
   PathIO.writeFile path $ unlines $
      "-- Do not edit! Automatically created with doctest-extract." :
      printf "module %s where" (ModuleName.string mainName) :
      "" :
      map (printf "import qualified %s" . prefixed) ms ++
      "" :
      importDriver :
      "" :
      (if run
         then
            "main :: IO ()" :
            "main = DocTest.run $ do" :
            []
         else
            "main :: DocTest.T ()" :
            "main = do" :
            []
      )++
      indent (map (printf "%s.test" . prefixed) ms)

writeTestModule ::
   (PathClass.AbsRel ar) =>
   Path.Dir ar -> ModuleName.T -> Flags -> Module [Located Pos DocTest] -> IO ()
writeTestModule testDir testPrefix flags m = do
   let path = testDir </> ModuleName.filePath (moduleName m)
   createDirectoryIfMissing True $ Path.takeDirectory path
   PathIO.writeFile path $ formatTestModule testPrefix flags m

formatTestModule ::
   ModuleName.T -> Flags -> Module [Located Pos DocTest] -> String
formatTestModule testPrefix (verbose,importTested) m =
   let escapedPath = show $ Path.toString $ modulePath m
       formatLinePragma loc =
         printf "{-# LINE %d %s #-}" loc escapedPath
       formatPragma (Located loc lns) =
         unlines $
            formatLinePragma loc :
            map
               (\ln ->
                  Mn.when (not $ null ln) (printf "{-# OPTIONS_GHC %s #-}" ln))
               lns
       formatImport (Located loc lns) =
         unlines $
            formatLinePragma loc :
            map (\(Located col ln) -> indentRemainder col ln) lns
       isExample (Located _loc (Example _ _)) = True; isExample _ = False
       formatTest (Located (loc,col) body) =
         let testCode command mark code =
               (if verbose
                  then [printf " DocTest.printLine %s\n" (show code)] else []) ++
               formatLinePragma loc :
               (' ':command++"(") :
               formatLinePragma loc :
               (case lines code of
                  (':':'{':firstLine) : remainingLines ->
                     (replicate (col + length mark + 3) ' ' ++ firstLine) :
                     ListHT.dropRev 1 remainingLines
                  _ -> [replicate (col + length mark) ' ' ++ code]) ++
               ["  )"]
         in (if verbose
               then printf " DocTest.printLine ('\\n':%s++\":%d:1\")" escapedPath loc
               else printf " DocTest.printPrefix \"%s:%d: \""
                        (ModuleName.string $ moduleName m) loc) :
            case body of
               Property prop -> testCode "DocTest.property" "prop>" prop
               Example str results ->
                  testCode "DocTest.example" ">>>" str ++
                  ("  " ++ showsPrec 11 results "") :
                  []
   in printf
        "-- Do not edit! Automatically created with doctest-extract from %s\n"
        (Path.toString $ modulePath m)
      ++
      foldMap formatPragma (modulePragma m)
      ++
      printf "module %s where\n\n"
         (ModuleName.string $ testPrefix <> moduleName m)
      ++
      Mn.when importTested
         (printf "import %s\n" $ ModuleName.string $ moduleName m)
      ++
      Mn.when (any isExample $ concat $ moduleContent m)
         "import Test.DocTest.Base\n"
      ++
      importDriver ++ "\n\n"
      ++
      foldMap formatImport (moduleSetup m)
      ++
      "\n"
      ++
      "test :: DocTest.T ()\n"
      ++
      "test = do\n"
      ++
      (unlines $ concatMap formatTest $ concat $ moduleContent m)
