{-# LANGUAGE FlexibleInstances #-}

module API.PipelinesTests (pipelinesTests) where

import API.Common
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import GitLab
import Test.Tasty

-- | https://docs.gitlab.com/ee/api/pipelines.html
pipelinesTests :: [TestTree]
pipelinesTests =
  let fname = "data/api/pipelines/cancel-pipeline-jobs.json"
   in gitlabJsonParserTests
        "cancel-pipeline-jobs"
        fname
        (parseOne =<< BSL.readFile fname :: IO Pipeline)
        ( do
            decodedFile <- parseOne =<< BSL.readFile fname :: IO Pipeline
            parseOne (encode decodedFile) :: IO Pipeline
        )

{-

cancel-pipeline-jobs.json
pipeline-test-report.json
project-pipelines.json
single-pipeline.json
create-new-pipeline.json
pipeline-test-report-summary.json
retry-jobs-in-pipeline.json
variables-of-pipeline.json

-}
