{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

{- | Description: Servant spec describing the DoH Api. -}
module OM.DoH.Api (
  DoHApi(..),
  Query(..),
  Response(..),
  DnsMsgCT,
) where


import Data.ByteString (ByteString)
import Data.ByteString.Base64 (decodeBase64Untyped)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import Servant.API (type (:>), Accept(contentType),
  MimeRender(mimeRender), MimeUnrender(mimeUnrender), Get, Post,
  QueryParam', ReqBody, Required, Strict, Summary)
import Servant.API.Generic (GenericMode((:-)))
import Web.HttpApiData (FromHttpApiData(parseUrlPiece))
import qualified Data.ByteString.Lazy as BSL


{- | The DoH api, defined in the "Servant.API.Generic" style. -}
data DoHApi route = DoHApi {
    getQuery :: route :-
      Summary "Submit a raw DNS query on the query string."
      :> QueryParam' '[Required, Strict] "dns" Query
      :> Get '[DnsMsgCT] Response,
    postQuery :: route :-
      Summary "Submit a raw DNS query in the POST body."
      :> ReqBody '[DnsMsgCT] Query
      :> Post '[DnsMsgCT] Response
  }
  deriving stock (Generic)


{- |
  A raw DNS query message. The "Network.DNS" module contains tools for
  encoding and decoding these messages.
-}
newtype Query = Query {unQuery :: ByteString}
  deriving newtype (MimeUnrender DnsMsgCT)
instance FromHttpApiData Query where
  parseUrlPiece txt = Query <$> decodeBase64Untyped (encodeUtf8 txt)


{- |
  A raw DNS response message. The "Network.DNS" module contains tools
  for encoding and decoding these messages.
-}
newtype Response = Response {unResponse :: ByteString}
  deriving newtype (MimeRender DnsMsgCT)


{- | The @application/dns-message@ content type. -}
data DnsMsgCT
instance Accept DnsMsgCT where
  contentType _proxy = "application/dns-message"
instance MimeRender DnsMsgCT ByteString where
  mimeRender _proxy msg = BSL.fromStrict msg
instance MimeUnrender DnsMsgCT ByteString where
  mimeUnrender _proxy = Right . BSL.toStrict


