module Main where

import qualified Format
import qualified Parse
import qualified Option
import qualified ModuleName

import qualified Options.Applicative as OP
import qualified System.Path.IO as PathIO
import System.Path ((</>))

import System.IO.Error (catchIOError)

import qualified Data.Foldable as Fold
import qualified Data.NonEmpty as NonEmpty
import Data.Traversable (for)
import Data.Foldable (for_)
import Data.Semigroup ((<>))

import Control.Monad (when)
import Control.Applicative (liftA2, (<$>))


{-
'mplus' in base>=4.9 (GHC>=8.0)

Orphan instance importable from transformers:Control.Monad.Trans.Error,
but this is deprecated.
-}
alternative :: IO a -> IO a -> IO a
alternative m n = catchIOError m (const n)

alternatives :: NonEmpty.T [] (IO a) -> IO a
alternatives = Fold.foldr1 alternative


main :: IO ()
main = do
   (inDirs, outDir, testPrefix,
    (executableMain, libraryMain),
    (flags,emitModuleList), moduleNames) <-
      OP.execParser $ Option.info Option.parser

   modules <-
      for moduleNames $ \moduleName -> do
         let altPaths = liftA2 (</>) inDirs (ModuleName.filePaths moduleName)
         alternatives $ flip fmap altPaths $ \path ->
            Parse.parseModule .
            Parse.reindentModule .
            Parse.moduleFromLines moduleName path .
                  Parse.numberedLines . filter ('\r'/=)
               <$> PathIO.readFile path
   Format.writeTestSuite outDir testPrefix flags modules
   for_ executableMain $ \mainPath ->
      Format.writeTestMain True
         (outDir </> mainPath) (ModuleName.singleton "Main") testPrefix modules
   for_ libraryMain $ \mainName -> do
      let fullName = testPrefix <> mainName
      Format.writeTestMain False
         (outDir </> ModuleName.filePath fullName) fullName testPrefix modules
   when emitModuleList $
      putStrLn $ unlines $ map (ModuleName.string . (testPrefix<>)) moduleNames
