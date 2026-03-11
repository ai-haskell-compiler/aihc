module PathFormat where

import qualified System.FilePath as StrPath
import qualified System.Path.PartClass as PathC
import qualified System.Path.Part as PathPart
import qualified System.Path as Path
import System.Path ((</>), )

import qualified Text.Printf as Pf


data File ar = File (Path.Dir ar) FilePath

type AbsRelFile = File PathPart.AbsRel


printf :: (PathC.AbsRel ar) => File ar -> Int -> Path.File ar
printf (File dir fmt) arg =
   dir </> Path.relFile (Pf.printf fmt arg)

parse :: (PathC.AbsRel ar) => String -> Either String (File ar)
parse str = do
   let (dir, file) = StrPath.splitFileName str
   dirPath <- Path.parse dir
   return $ File dirPath file
