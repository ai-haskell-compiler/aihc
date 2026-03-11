{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Copyright: (c) 2020 Jens Petersen
SPDX-License-Identifier: MIT
Maintainer: Jens Petersen <juhpetersen@gmail.com>

Fedora PDC web api client library
-}

module Fedora.PDC
  ( fedoraPDC
  , pdcArches
  , pdcChangesets
  , pdcComponentBranches
  , pdcComponentBranchSLAs
  , pdcComponentRelationshipTypes
  , pdcComponentSLATypes
  , pdcComposes
  , pdcComposeImages
  , pdcComposeImageRttTests
  , pdcComposeRpms
  , pdcComposeTreeRttTests
  , pdcContentDeliveryContentFormats
  , pdcGlobalComponents
  , pdcImages
  , pdcModules
  , pdcProductVersions
  , pdcProducts
  , pdcReleases
  , pdcWhereToFileBugs
  , pdcRpms
  , queryPDC
  , lookupKey
  , lookupKey'
  , makeKey
  , makeItem
  , maybeKey
  , Query
  , QueryItem
  , getResultsList
  )
where

import Data.Aeson.Types
import Network.HTTP.Query

fedoraPDC :: String
fedoraPDC = "pdc.fedoraproject.org"

-- | Arch List
--
-- https://pdc.fedoraproject.org/rest_api/v1/arches/
pdcArches :: String -> IO [Object]
pdcArches server =
  lookupKey' "results" <$> queryPDC server "arches" []

-- https://pdc.fedoraproject.org/rest_api/v1/auth/ *

-- https://pdc.fedoraproject.org/rest_api/v1/base-products/ no-op?

-- https://pdc.fedoraproject.org/rest_api/v1/bugzilla-components/ no-op?

-- | Changeset List
--
-- https://pdc.fedoraproject.org/rest_api/v1/changesets/
pdcChangesets :: String -> Query -> IO Object
pdcChangesets server =
  queryPDC server "changesets"

-- | Sla To Component Branch List
--
-- https://pdc.fedoraproject.org/rest_api/v1/component-branch-slas/
pdcComponentBranchSLAs :: String -> Query -> IO Object
pdcComponentBranchSLAs server =
  queryPDC server "component-branch-slas"

-- | Component Branch List
--
-- https://pdc.fedoraproject.org/rest_api/v1/component-branches/
pdcComponentBranches :: String -> Query -> IO Object
pdcComponentBranches server =
  queryPDC server "component-branches"

-- | Release Component Relationship Type List
--
-- https://pdc.fedoraproject.org/rest_api/v1/component-relationship-types/
pdcComponentRelationshipTypes :: String -> IO [Object]
pdcComponentRelationshipTypes server =
  lookupKey' "results" <$> queryPDC server "component-relationship-types" []

-- | Sla List
--
-- https://pdc.fedoraproject.org/rest_api/v1/component-sla-types/
pdcComponentSLATypes :: String -> Query -> IO [Object]
pdcComponentSLATypes server params =
  lookupKey' "results" <$> queryPDC server "component-sla-types" params

-- | Compose Image Rtt Test List
--
-- https://pdc.fedoraproject.org/rest_api/v1/compose-image-rtt-tests/ (all untested?)
pdcComposeImageRttTests :: String -> Query -> IO Object
pdcComposeImageRttTests server =
  queryPDC server "compose-image-rtt-tests"

-- | Compose Image Instance
--
-- https://pdc.fedoraproject.org/rest_api/v1/compose-images/ {compose_id}/
pdcComposeImages :: String -> String -> IO Object
pdcComposeImages server compose =
  queryPDC server ("compose-images" +/+ compose ++ "/") []

-- | Compose Rpm List (seems heavy)
--
-- https://pdc.fedoraproject.org/rest_api/v1/compose-rpms/ {compose_id}/
pdcComposeRpms :: String -> String -> IO Object
pdcComposeRpms server compose =
  queryPDC server ("compose-rpms" +/+ compose ++ "/") []

-- | Compose Tree Rtt Test List
--
-- https://pdc.fedoraproject.org/rest_api/v1/compose-tree-rtt-tests/
pdcComposeTreeRttTests :: String -> Query -> IO Object
pdcComposeTreeRttTests server =
  queryPDC server "compose-tree-rtt-tests"

-- | Compose List
--
-- https://pdc.fedoraproject.org/rest_api/v1/composes/
pdcComposes :: String -> Maybe String -> Query -> IO Object
pdcComposes server mcompose params = do
  let path = "composes" +/+ maybe "" (++ "/") mcompose
  queryPDC server path params

-- https://pdc.fedoraproject.org/rest_api/v1/composes/%7Bcompose_id%7D/rpm-mapping/%7Bpackage%7D/

-- | Content Format List
--
-- https://pdc.fedoraproject.org/rest_api/v1/content-delivery-content-formats/
pdcContentDeliveryContentFormats :: String -> Query -> IO [Object]
pdcContentDeliveryContentFormats server params =
  lookupKey' "results" <$> queryPDC server "content-delivery-content-formats" params

-- | Global Component List
--
-- https://pdc.fedoraproject.org/rest_api/v1/global-components/
pdcGlobalComponents :: String -> Query -> IO Object
pdcGlobalComponents server =
   queryPDC server "global-components"

-- | Image List
--
-- https://pdc.fedoraproject.org/rest_api/v1/images/
pdcImages :: String -> Query -> IO Object
pdcImages server =
   queryPDC server "images"

-- | Module List
--
-- https://pdc.fedoraproject.org/rest_api/v1/modules/
pdcModules :: String -> Query -> IO Object
pdcModules server =
  queryPDC server "modules"

-- | Product Version List
--
-- https://pdc.fedoraproject.org/rest_api/v1/product-versions/
pdcProductVersions :: String -> Query -> IO [Object]
pdcProductVersions server params =
  lookupKey' "results" <$> queryPDC server "product-versions" params

-- | Product List
--
-- https://pdc.fedoraproject.org/rest_api/v1/products/
pdcProducts :: String -> Query -> IO [Object]
pdcProducts server params =
  lookupKey' "results" <$> queryPDC server "products" params

-- | Release List
--
-- https://pdc.fedoraproject.org/rest_api/v1/releases/
pdcReleases :: String -> Query -> IO Object
pdcReleases server =
  queryPDC server "releases"

-- | Filter Bugzilla Products And Components List
--
-- https://pdc.fedoraproject.org/rest_api/v1/rpc/where-to-file-bugs/
pdcWhereToFileBugs :: String -> Query -> IO Object
pdcWhereToFileBugs server =
  queryPDC server "rpc/where-to-file-bugs"

-- | Rpm List
--
-- https://pdc.fedoraproject.org/rest_api/v1/rpms/
pdcRpms :: String -> Query -> IO Object
pdcRpms server =
  queryPDC server "rpms"

-- | low-level query
queryPDC :: String -> String -> Query -> IO Object
queryPDC server path params =
  let url = "https://" ++ server +/+ "rest_api/v1" +/+ path
  in webAPIQuery url params

-- | Get results key from a response object
getResultsList :: Object -> [Object]
getResultsList = lookupKey' "results"
