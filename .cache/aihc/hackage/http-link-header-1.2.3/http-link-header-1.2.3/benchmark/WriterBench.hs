{-# LANGUAGE OverloadedStrings #-}

module WriterBench (benchmarks) where

import           Criterion
import           Data.String              (IsString (..))
import           Data.Text                (Text)
import           Network.HTTP.Link.Types
import           Network.HTTP.Link.Writer
import           Network.URI

instance IsString URI where
    fromString str = case parseURI str of
        Just uri -> uri
        Nothing -> error $ "Failed to parse URI: " ++ str

benchmarks :: [Benchmark]
benchmarks = [
    bench "minimal" $ whnf writeLinkHeaderURI
        [ Link "http://example.com/thing" [ (Rel, "next") ] ]
  , bench "large" $ whnf writeLinkHeaderURI
        [ Link "http://example.com/something_long"
               [ (Rel, "next prev http://hello.world/undefined")
               , (Title, "this is a test benchmark thingy")
               ]
        , Link "https://use.tls.everywhere.pls"
               [ (Rel, "license")
               , (Rev, "author") ]]
  ]

  where
    writeLinkHeaderURI = writeLinkHeader :: [Link URI] -> Text
