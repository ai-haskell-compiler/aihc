{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE OverloadedLists       #-}

module Main where

import Pinecone (DataMethods(..), ControlMethods(..))
import Pinecone.Embed (GenerateVectors(..))
import Pinecone.Rerank (Document(..), Documents(..), RerankResults(..))
import Prelude hiding (id)

import Pinecone.Indexes
    ( Cloud(..)
    , CreateIndexWithEmbedding(..)
    , ConfigureIndex(..)
    , EmbedRequest(..)
    , GetIndexStats(..)
    , IndexModel(..)
    , IndexStats(..)
    , Status(..)
    )
import Pinecone.Search
    ( Hit(..)
    , Hits(..)
    , Matches(..)
    , Query(..)
    , SearchWithText(..)
    , SearchWithVector(..)
    )
import Pinecone.Vectors
    ( DeleteVectors(..)
    , Record(..)
    , UpdateVector(..)
    , UpsertVectors(..)
    , UpsertStats(..)
    , VectorIDs(..)
    , VectorObject(..)
    , Vectors(..)
    )

import qualified Control.Exception as Exception
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Pinecone
import qualified System.Environment as Environment
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit

main :: IO ()
main = do
    controlEnv <- Pinecone.getClientEnv "https://api.pinecone.io"

    key <- Environment.getEnv "PINECONE_KEY"

    let token = Text.pack key

    let ControlMethods{..} = Pinecone.makeControlMethods controlEnv token

    let namespace = "test"

    Tasty.defaultMain do
        HUnit.testCase "Vectors" do
            let open = do
                    createIndexWithEmbedding CreateIndexWithEmbedding
                        { name = "vectors-test"
                        , cloud = AWS
                        , region = "us-east-1"
                        , embed = EmbedRequest
                            { model = "llama-text-embed-v2"
                            , metric = Nothing
                            , read_parameters = Nothing
                            , write_parameters = Nothing
                            }
                        , deletion_protection = Nothing
                        , tags = Nothing
                        }

            let close IndexModel{ name } = deleteIndex name

            Exception.bracket open close \IndexModel{ name, host } -> do
                _ <- generateVectors GenerateVectors
                    { model = "llama-text-embed-v2"
                    , inputs = [ "Hello, world!" ]
                    , parameters = [ ("input_type", "query") ]
                    }

                let hello = Record
                        { id = "hi"
                        , text = "Hello, world"
                        , metadata = Nothing
                        }

                let goodbye = Record
                        { id = "bye"
                        , text = "Goodbye, world"
                        , metadata = Nothing
                        }

                Documents{ data_ = [ Document{ document = Just Record{ id = "hi" } } ] } <- rerankResults RerankResults
                    { model = "bge-reranker-v2-m3"
                    , query = "best greeting"
                    , documents = [ hello, goodbye ]
                    , top_n = Just 1
                    , return_documents = True
                    , parameters = [ ]
                    }

                dataEnv <- Pinecone.getClientEnv host

                let DataMethods{..} = Pinecone.makeDataMethods dataEnv token

                let waitUntilIndexReady = do
                        indexModel <- describeIndex name

                        let IndexModel{ status } = indexModel

                        let Status{ ready } = status

                        if ready
                            then return indexModel
                            else waitUntilIndexReady

                indexModel <- waitUntilIndexReady

                indexes <- listIndexes

                case indexes of
                    [ indexModel₀ ]
                        | indexModel == indexModel₀ -> return ()
                    _ -> HUnit.assertFailure "GET /indexes - wrong models"

                _ <- configureIndex name ConfigureIndex
                    { spec = Nothing
                    , deletion_protection = Nothing
                    , tags = Just [ ("foo", "bar") ]
                    , embed = Nothing
                    }

                IndexModel{ tags = Just [ ("foo", "bar") ] } <- describeIndex name

                UpsertStats{..} <- upsertVectors UpsertVectors
                    { vectors =
                        [ VectorObject
                            { id = "vector-0"
                            , values = Just (Vector.replicate 1024 0.1)
                            , sparseValues = Nothing
                            , metadata = Nothing
                            }
                        ]
                    , namespace = Just namespace
                    }

                HUnit.assertEqual "" 1 upsertedCount

                deleteVectors DeleteVectors
                    { ids = Just [ "vector-0" ]
                    , deleteAll = Nothing
                    , namespace = Just namespace
                    , filter = Nothing
                    }

                upsertText namespace hello

                upsertText namespace goodbye

                updateVector UpdateVector
                    { id = "hi"
                    , values = Nothing
                    , sparseValues = Nothing
                    , setMetadata = Just [ ("category", "greeting") ]
                    , namespace = Just namespace
                    }

                let waitUntilVectorsReady = do
                        IndexStats{..} <- getIndexStats GetIndexStats
                            { filter = Nothing
                            }

                        if totalVectorCount == 2
                            then return ()
                            else waitUntilVectorsReady

                waitUntilVectorsReady

                Vectors{ vectors = [ ("bye", _), ("hi", _) ] } <- fetchVectors [ "hi", "bye" ] (Just namespace)

                VectorIDs{ vectors = [ "bye", "hi" ] } <- listVectorIDs Nothing Nothing Nothing (Just namespace)

                Hits{ hits = [ Hit{ _id = "hi" } ] } <- searchWithText namespace SearchWithText
                    { query = Query
                        { top_k = 1
                        , filter = Nothing
                        , input = Just "Hi!"
                        , vector = Nothing
                        }
                    , fields = Nothing
                    , rerank = Nothing
                    }

                Matches{ matches } <- searchWithVector SearchWithVector
                    { topK = 1
                    , namespace = Just namespace
                    , filter = Nothing
                    , includeValues = Nothing
                    , includeMetadata = Nothing
                    , vector = Just (Vector.replicate 1024 0.1)
                    , sparseVector = Nothing
                    , id = Nothing
                    }

                HUnit.assertEqual "" 1 (Vector.length matches)

                deleteVectors DeleteVectors
                    { ids = Nothing
                    , deleteAll = Just True
                    , namespace = Just namespace
                    , filter = Nothing
                    }

                return ()
