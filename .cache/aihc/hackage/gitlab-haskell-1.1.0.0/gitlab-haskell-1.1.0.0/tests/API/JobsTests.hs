{-# LANGUAGE FlexibleInstances #-}

module API.JobsTests (jobsTests) where

import API.Common
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import GitLab
import Test.Tasty

-- | https://docs.gitlab.com/ee/api/jobs.html
jobsTests :: [TestTree]
jobsTests =
  concat
    [ let fname = "data/api/jobs/cancel-job.json"
       in gitlabJsonParserTests
            "cancel-job"
            fname
            (parseOne =<< BSL.readFile fname :: IO Job)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO Job
                parseOne (encode decodedFile) :: IO Job
            ),
      let fname = "data/api/jobs/job-tokens-job.json"
       in gitlabJsonParserTests
            "job-tokens-job"
            fname
            (parseOne =<< BSL.readFile fname :: IO Job)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO Job
                parseOne (encode decodedFile) :: IO Job
            ),
      let fname = "data/api/jobs/pipeline-jobs.json"
       in gitlabJsonParserTests
            "pipeline-jobs"
            fname
            (parseOne =<< BSL.readFile fname :: IO [Job])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [Job]
                parseOne (encode decodedFile) :: IO [Job]
            ),
      let fname = "data/api/jobs/project-jobs.json"
       in gitlabJsonParserTests
            "project-jobs"
            fname
            (parseOne =<< BSL.readFile fname :: IO [Job])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [Job]
                parseOne (encode decodedFile) :: IO [Job]
            ),
      let fname = "data/api/jobs/single-job.json"
       in gitlabJsonParserTests
            "single-job"
            fname
            (parseOne =<< BSL.readFile fname :: IO Job)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO Job
                parseOne (encode decodedFile) :: IO Job
            ),
      let fname = "data/api/jobs/erase-job.json"
       in gitlabJsonParserTests
            "erase-job"
            fname
            (parseOne =<< BSL.readFile fname :: IO Job)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO Job
                parseOne (encode decodedFile) :: IO Job
            ),
      let fname = "data/api/jobs/pipeline-bridges.json"
       in gitlabJsonParserTests
            "pipeline-bridges"
            fname
            (parseOne =<< BSL.readFile fname :: IO [Job])
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO [Job]
                parseOne (encode decodedFile) :: IO [Job]
            ),
      let fname = "data/api/jobs/play-job.json"
       in gitlabJsonParserTests
            "play-job"
            fname
            (parseOne =<< BSL.readFile fname :: IO Job)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO Job
                parseOne (encode decodedFile) :: IO Job
            ),
      let fname = "data/api/jobs/retry-job.json"
       in gitlabJsonParserTests
            "retry-job"
            fname
            (parseOne =<< BSL.readFile fname :: IO Job)
            ( do
                decodedFile <- parseOne =<< BSL.readFile fname :: IO Job
                parseOne (encode decodedFile) :: IO Job
            )
    ]
