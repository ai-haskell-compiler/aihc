module Main where

import           Criterion.Main
import           Data.Maybe          (catMaybes)
import           Data.Text           (Text)
import qualified Data.Text.IO        as Text
import           System.Directory    (getDirectoryContents)
import           System.FilePath

import qualified Language.Lua.Annotated.Parser  as P

main :: IO ()
main = defaultMain
  [ env (loadFiles "lua-5.3.1-tests") $ \files ->
      bench "Parsing Lua files from 5.3.1 test suite" $
        nf (catMaybes . map (either (const Nothing) Just) . map (P.parseText P.chunk)) files
  ]

loadFiles :: FilePath -> IO [Text]
loadFiles root = do
  let isLuaFile file = takeExtension file == ".lua"
      onlyLuaFiles = map (root </>) . filter isLuaFile
  luaFiles <- fmap onlyLuaFiles (getDirectoryContents root)
  mapM Text.readFile luaFiles
