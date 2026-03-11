module System.FilePath.TH where

import Prelude

import Control.Monad ((<=<))
import Data.FileEmbed (makeRelativeToProject)
import Language.Haskell.TH (Loc(Loc), Exp, Q, loc_filename, location, runIO, stringE)
import System.Directory (canonicalizePath, getCurrentDirectory)
import System.FilePath ((</>), takeDirectory)

fileRelativeToAbsolute :: String -> Q Exp
fileRelativeToAbsolute = stringE <=< fileRelativeToAbsoluteStr

-- | Use a path relative to the source file in which you're writing the path instead of relative to the working
-- directory in which that source file will be compiled.
--
-- e.g. if this source file is in `my-project/src/Foo/Bar.hs`, `fileRelativeToAbsoluteStr
-- "../../../config/settings.yml"` will load the path at `my-project/config/settings.yml`.
--
-- If this function is provided an absolute path, it will simply canonicalize that path
-- by calling 'System.Directory.canonicalizePath' rather than compute an absolute path from
-- a relative path.
fileRelativeToAbsoluteStr :: String -> Q String
fileRelativeToAbsoluteStr absoluteFilePath@('/':_) =
  runIO . canonicalizePath $ absoluteFilePath
fileRelativeToAbsoluteStr relativeFilePath = do
  Loc {..} <- location
  currentDir <- runIO getCurrentDirectory
  let baseDir = takeDirectory loc_filename
  runIO $ canonicalizePath $ currentDir </> baseDir </> relativeFilePath

fileRelativeToProject :: FilePath -> Q Exp
fileRelativeToProject = stringE <=< makeRelativeToProject
