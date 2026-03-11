{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Data.Ollama.Create
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental
Description : Functionality for creating new models in the Ollama client.

This module provides functions to create new models in the Ollama API. Models can be created from:

- Another existing model (using 'from' parameter)
- A safetensors directory (using 'files' parameter)
- A GGUF file (using 'files' parameter)

The create operation is performed via a POST request to the @\/api\/create@ endpoint, with streaming
support for progress updates. The module supports model quantization, custom templates, system prompts,
parameters, and LORA adapters.

Example creating from existing model:

>>> let ops = defaultCreateOps { modelName = "mario", fromModel = Just "llama3.2", systemPrompt = Just "You are Mario from Super Mario Bros." }
>>> createModel ops Nothing
Creating model...
Success

Example quantizing a model:

>>> let ops = defaultCreateOps { modelName = "llama3.2:quantized", fromModel = Just "llama3.2:3b-instruct-fp16", quantizeType = Just Q4_K_M }
>>> createModel ops Nothing
Quantizing model...
Success
-}
module Data.Ollama.Create
  ( -- * Create Model API
    createModel
  , createModelM

    -- * Configuration Types
  , CreateOps (..)
  , defaultCreateOps

    -- * Response Types
  , CreateResp (..)

    -- * Quantization Types
  , QuantizationType (..)
  ) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes)
import Data.Ollama.Common.Config (OllamaConfig)
import Data.Ollama.Common.Types (HasDone (getDone), Message, ModelOptions)
import Data.Ollama.Common.Utils as CU
import Data.Text (Text)
import GHC.Generics (Generic)
import GHC.Int (Int64)

-- | Quantization types supported by Ollama.
data QuantizationType
  = -- | Recommended quantization type
    Q4_K_M
  | -- | Alternative quantization type
    Q4_K_S
  | -- | Recommended quantization type
    Q8_0
  deriving (Show, Eq, Generic)

instance ToJSON QuantizationType where
  toJSON Q4_K_M = String "q4_K_M"
  toJSON Q4_K_S = String "q4_K_S"
  toJSON Q8_0 = String "q8_0"

instance FromJSON QuantizationType where
  parseJSON = withText "QuantizationType" $ \t ->
    case t of
      "q4_K_M" -> pure Q4_K_M
      "q4_K_S" -> pure Q4_K_S
      "q8_0" -> pure Q8_0
      _ -> fail $ "Invalid QuantizationType: " <> show t

-- | Configuration for creating a new model.
data CreateOps = CreateOps
  { modelName :: !Text
  -- ^ The name of the model to create.
  , fromModel :: !(Maybe Text)
  -- ^ Optional name of an existing model to create the new model from.
  , files :: !(Maybe (Map Text Text))
  -- ^ Optional dictionary of file names to SHA256 digests of blobs to create the model from.
  , adapters :: !(Maybe (Map Text Text))
  -- ^ Optional dictionary of file names to SHA256 digests of blobs for LORA adapters.
  , template :: !(Maybe Text)
  -- ^ Optional prompt template for the model.
  , license :: !(Maybe [Text])
  -- ^ Optional list of strings containing the license(s) for the model.
  , systemPrompt :: !(Maybe Text)
  -- ^ Optional system prompt for the model.
  , parameters :: !(Maybe ModelOptions)
  -- ^ Optional parameters for the model.
  , messages :: !(Maybe [Message])
  -- ^ Optional list of message objects used to create a conversation.
  , stream :: !(Maybe Bool)
  -- ^ Optional flag to enable streaming progress updates.
  , quantizeType :: !(Maybe QuantizationType)
  -- ^ Optional quantization type for quantizing a non-quantized model.
  }
  deriving (Show, Eq, Generic)

-- | Default configuration for creating a model.
defaultCreateOps :: Text -> CreateOps
defaultCreateOps name =
  CreateOps
    { modelName = name
    , fromModel = Nothing
    , files = Nothing
    , adapters = Nothing
    , template = Nothing
    , license = Nothing
    , systemPrompt = Nothing
    , parameters = Nothing
    , messages = Nothing
    , stream = Nothing
    , quantizeType = Nothing
    }

-- | Response type for model creation operations.
data CreateResp = CreateResp
  { status :: !Text
  -- ^ The status of the create operation (e.g., "success", "reading model metadata").
  , digest :: !(Maybe Text)
  -- ^ Optional digest (hash) of the model layer being processed.
  , total :: !(Maybe Int64)
  -- ^ Optional total size in bytes for quantization operations.
  , completed :: !(Maybe Int64)
  -- ^ Optional number of bytes completed for quantization operations.
  }
  deriving (Show, Eq, Generic)

instance HasDone CreateResp where
  getDone CreateResp {..} = status == "success"

instance ToJSON CreateOps where
  toJSON CreateOps {..} =
    object $
      catMaybes
        [ Just ("model" .= modelName)
        , ("from" .=) <$> fromModel
        , ("files" .=) <$> files
        , ("adapters" .=) <$> adapters
        , ("template" .=) <$> template
        , ("license" .=) <$> license
        , ("system" .=) <$> systemPrompt
        , ("parameters" .=) <$> parameters
        , ("messages" .=) <$> messages
        , ("stream" .=) <$> stream
        , ("quantize" .=) <$> quantizeType
        ]

instance FromJSON CreateResp where
  parseJSON = withObject "CreateResp" $ \v ->
    CreateResp
      <$> v .: "status"
      <*> v .:? "digest"
      <*> v .:? "total"
      <*> v .:? "completed"

{- | Creates a new model according to the provided configuration.

Sends a POST request to the @\/api\/create@ endpoint to create a model. The model can be created from:

- Another existing model (specify 'fromModel')
- A safetensors directory (specify 'files' with file name to SHA256 digest mappings)
- A GGUF file (specify 'files' with file name to SHA256 digest mapping)

Supports quantization, custom templates, system prompts, parameters, and LORA adapters.
Prints progress messages to the console during creation.
-}
createModel ::
  -- | Model creation configuration
  CreateOps ->
  -- | Optional 'OllamaConfig' (defaults to 'defaultOllamaConfig' if 'Nothing')
  Maybe OllamaConfig ->
  IO ()
createModel createOps mbConfig =
  void $
    withOllamaRequest
      "/api/create"
      "POST"
      (Just createOps)
      mbConfig
      (commonStreamHandler (onToken, pure ()))
  where
    onToken :: CreateResp -> IO ()
    onToken CreateResp {..} = case (total, completed) of
      (Just t, Just c) -> putStrLn $ "Progress: " <> show c <> "/" <> show t <> " bytes"
      _ -> putStrLn $ "Status: " <> show status

{- | MonadIO version of 'createModel' for use in monadic contexts.

Lifts the 'createModel' function into a 'MonadIO' context, allowing it to be used in monadic computations.
-}
createModelM ::
  MonadIO m =>
  CreateOps ->
  Maybe OllamaConfig ->
  m ()
createModelM createOps mbCfg = liftIO $ createModel createOps mbCfg
