{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module GitHub.Types.Repository where


import Control.Applicative
import Control.Monad

import Data.Aeson
import Data.Text

import GitHub.Types.Base



data CreateDeploymentStatusRequest = CreateDeploymentStatusRequest
    { cdsState       :: State
    , cdsTargetUrl   :: Maybe Text
    , cdsDescription :: Maybe Text
    } deriving (Eq, Show)


instance ToJSON CreateDeploymentStatusRequest where
    toJSON CreateDeploymentStatusRequest{..} = object
        [ "state"       .= cdsState
        , "target_url"  .= cdsTargetUrl
        , "description" .= cdsDescription
        ]
