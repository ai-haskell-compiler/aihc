{-# LANGUAGE FlexibleInstances #-}

module API.RepositoryFilesTests (repositoryFilesTests) where

import API.Common
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import GitLab
import Test.Tasty

-- | https://docs.gitlab.com/ee/api/repository_files.html
repositoryFilesTests :: [TestTree]
repositoryFilesTests =
  -- let fname = "data/api/repository-files/create-new-file-in-repository.json"
  --  in gitlabJsonParserTests
  --       "create-new-file-in-repository"
  --       fname
  --       (parseOne =<< BSL.readFile fname :: IO [Note])
  --       ( do
  --           decodedFile <- parseOne =<< BSL.readFile fname :: IO [Note]
  --           parseOne (encode decodedFile) :: IO [Note]
  --       )
  let fname = "data/api/repository-files/get-file-from-repository.json"
   in gitlabJsonParserTests
        "get-file-from-repository"
        fname
        (parseOne =<< BSL.readFile fname :: IO RepositoryFile)
        ( do
            decodedFile <- parseOne =<< BSL.readFile fname :: IO RepositoryFile
            parseOne (encode decodedFile) :: IO RepositoryFile
        )

-- let fname = "data/api/repository-files/get-file-blame-from-repository.json"
--  in gitlabJsonParserTests
--       "get-file-blame-from-repository"
--       fname
--       (parseOne =<< BSL.readFile fname :: IO RepositoryFile)
--       ( do
--           decodedFile <- parseOne =<< BSL.readFile fname :: IO RepositoryFile
--           parseOne (encode decodedFile) :: IO RepositoryFile
--       )
-- let fname = "data/api/repository-files/update-existing-file-in-repository.json"
--  in gitlabJsonParserTests
--       "update-existing-file-in-repository"
--       fname
--       (parseOne =<< BSL.readFile fname :: IO RepositoryFile)
--       ( do
--           decodedFile <- parseOne =<< BSL.readFile fname :: IO RepositoryFile
--           parseOne (encode decodedFile) :: IO RepositoryFile
--       )
