module DownloadDir (
  checkDefaultIsoDir
  )
where

import System.Directory (doesDirectoryExist)
import System.Environment.XDG.UserDir (getUserDir)
import System.FilePath

checkDefaultIsoDir :: IO FilePath
checkDefaultIsoDir = do
  dlDir <- getUserDir "DOWNLOAD"
  dirExists <- doesDirectoryExist dlDir
  let isoDir = dlDir </> "iso"
  isoExists <- doesDirectoryExist isoDir
  return $
    if isoExists
    then isoDir
    else if dirExists
         then dlDir
         else "."
