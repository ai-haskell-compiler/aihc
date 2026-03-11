module Iri.Data.Functions where

import Iri.Data.Types
import Iri.Prelude

-- | Try to specialize a general IRI to HTTP
httpIriFromIri :: Iri -> Either Text HttpIri
httpIriFromIri (Iri (Scheme scheme) hierarchy query fragment) =
  do
    security <- case scheme of
      "http" -> Right (Security False)
      "https" -> Right (Security True)
      _ -> Left ("Not an HTTP scheme: " <> (fromString . show) scheme)
    case hierarchy of
      AuthorisedHierarchy (Authority userInfo host port) path -> case userInfo of
        MissingUserInfo -> Right (HttpIri security host port path query fragment)
        PresentUserInfo (User _) _ -> Left ("User Info is present")
      _ -> Left ("Not an authorised hierarchy")

-- | Generalize an HTTP IRI to IRI
iriFromHttpIri :: HttpIri -> Iri
iriFromHttpIri (HttpIri (Security secure) host port path query fragment) =
  Iri scheme hierarchy query fragment
  where
    scheme =
      Scheme (if secure then "https" else "http")
    hierarchy =
      AuthorisedHierarchy (Authority MissingUserInfo host port) path
