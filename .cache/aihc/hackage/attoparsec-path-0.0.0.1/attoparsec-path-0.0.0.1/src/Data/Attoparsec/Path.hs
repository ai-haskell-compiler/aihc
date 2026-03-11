module Data.Attoparsec.Path (rootPath, relFilePath, absFilePath, relDirPath, absDirPath) where

import Data.Char (isControl, isSpace)
import Data.Attoparsec.Text (Parser, char, takeWhile1)
import qualified Data.Text as T
import Control.Applicative (Alternative (..))
import Path (Path, File, Dir, Abs, Rel, (</>))
import Unsafe.Coerce (unsafeCoerce)



rootPath :: Parser (Path Abs Dir)
rootPath =
  (unsafeCoerce . (:[])) <$> char '/'

chunkPath :: Parser (Path Rel File)
chunkPath =
  (unsafeCoerce . T.unpack) <$> takeWhile1 (\c -> not (isControl c || isSpace c) && c /= '/')

relFilePath :: Parser (Path Rel File)
relFilePath =
  let parseComplex =
        (\h t -> unsafeCoerce (unsafeCoerce h ++ unsafeCoerce t))
          <$> chunkPath <*> absFilePath
      parseSimple = chunkPath
  in  parseComplex <|> parseSimple

absFilePath :: Parser (Path Abs File)
absFilePath =
  (</>) <$> rootPath <*> relFilePath

relDirPath :: Parser (Path Rel Dir)
relDirPath =
  (\files suffix -> unsafeCoerce (unsafeCoerce files ++ unsafeCoerce suffix))
    <$> relFilePath <*> rootPath

absDirPath :: Parser (Path Abs Dir)
absDirPath =
  (</>) <$> rootPath <*> relDirPath
