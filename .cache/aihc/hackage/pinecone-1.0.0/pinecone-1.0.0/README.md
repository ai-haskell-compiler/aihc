# `pinecone`

This provides a binding to Pinecone's API using `servant`

Example usage:

```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import Pinecone
import Pinecone.Indexes
import Pinecone.Search
import Pinecone.Vectors

import qualified Control.Exception as Exception
import qualified Data.Text as Text
import qualified System.Environment as Environment

main :: IO ()
main = do
    controlEnv <- getClientEnv "https://api.pinecone.io"

    key <- Environment.getEnv "PINECONE_KEY"

    let token = Text.pack key

    let ControlMethods{..} = makeControlMethods controlEnv token

    let open = createIndexWithEmbedding _CreateIndexWithEmbedding
            { name = "test"
            , cloud = AWS
            , region = "us-east-1"
            , embed = EmbedRequest
                { model = "llama-text-embed-v2"
                , metric = Nothing
                , read_parameters = Nothing
                , write_parameters = Nothing
                }
            }

    let close IndexModel{ name } = deleteIndex name

    Exception.bracket open close \IndexModel{ name, host } -> do
        let waitUntilIndexReady = do
                IndexModel{ status } <- describeIndex name

                let Status{ ready } = status

                if ready
                    then return ()
                    else waitUntilIndexReady

        waitUntilIndexReady

        dataEnv <- getClientEnv host

        let DataMethods{..} = makeDataMethods dataEnv token

        upsertText "test" _Record{ id = "hi", text = "Hello, world!" }
        upsertText "test" _Record{ id = "bye", text = "Goodbye, world!" }

        -- Pinecone is eventually consistent, so we have to wait
        let waitUntilVectorsReady = do
                IndexStats{ totalVectorCount } <- getIndexStats _GetIndexStats

                if totalVectorCount == 2
                    then return ()
                    else waitUntilVectorsReady

        waitUntilVectorsReady

        Hits{ hits } <- searchWithText "test" SearchWithText
            { query = _Query{ top_k = 1, input = Just "best greeting"  }
            , fields = Nothing
            , rerank = Nothing
            }

        print (fmap _id hits) -- ["hi"]
```
