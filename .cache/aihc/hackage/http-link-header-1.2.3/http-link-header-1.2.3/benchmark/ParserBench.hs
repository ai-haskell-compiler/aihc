{-# LANGUAGE OverloadedStrings #-}

module ParserBench (benchmarks) where

import           Criterion
import           Network.HTTP.Link.Parser
import           Network.HTTP.Link.Types (Link)
import           Network.URI (URI)

benchmarks :: [Benchmark]
benchmarks = [
    bench "minimal" $ whnf parseLinkHeaderURI "<http://example.com>; rel=\"next\""
  , bench "large" $ whnf parseLinkHeaderURI "\n\t <  http://example.com>; rel=next; title=\"Hello world\",  <ftp://hello.world>; rev=license; someWeirdParam=\"YOLO LOL\", <https://long.ass.domain.name.just.because/lol>; rel=\"something something something http://some.thing/lol/rel\" "
  ]

  where
    parseLinkHeaderURI t = parseLinkHeader t :: Maybe [Link URI]
