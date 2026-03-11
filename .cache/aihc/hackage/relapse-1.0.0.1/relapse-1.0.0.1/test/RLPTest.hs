{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module RLPTest where

import           Control.Monad           (sequence)
import           Data.Aeson
import           Data.Aeson.Types        (typeMismatch)
import           Data.ByteString
import qualified Data.ByteString.Lazy    as BL
import qualified Data.Map.Strict         as M
import           Data.Maybe              (fromJust)
import           Data.Semigroup          ((<>))
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as TE
import qualified Data.Vector             as V

import           Development.IncludeFile

import qualified Data.RLP                as RLP

data RLPTestInput = StringInput ByteString
                  | NumberInput Integer
                  | ListInput [RLPTestInput]
                  deriving (Read, Show)

-- The test JSON takes advantage of the fact that you can mix and match types in JSON arrays
-- so naturally, that doesn't play well with haskell, and we can't *REALLY* make FromJSON and ToJSON
-- maintain identity. So we cheat :P. Our biggest issue is that RLP doesn't have an "Integer" type
-- it's effectively stored as a big-endian String. So we need to really handle the case of
-- StringInput == NumberInput and vice versa
instance Eq RLPTestInput where
    (StringInput s1) == (StringInput s2) = s1 == s2
    (NumberInput n1) == (NumberInput n2) = n1 == n2
    (ListInput l1)   == (ListInput l2)   = l1 == l2

    StringInput{}   ==  ListInput{}    = False -- impossible
    (StringInput s) == (NumberInput n) = RLP.unpackBE (unpack s) == n -- todo this case

    NumberInput{}   ==  ListInput{}    = False -- also impossible
    n@NumberInput{} == s@StringInput{} = s == n -- take advantage of the commutative case

    o1 == o2 = False

instance RLP.RLPEncodable RLPTestInput where
    rlpEncode (StringInput s) = RLP.String s
    rlpEncode (NumberInput n) = RLP.rlpEncode n
    rlpEncode (ListInput xs)  = RLP.Array $ RLP.rlpEncode <$> xs

    rlpDecode (RLP.String s) = Right (StringInput s) -- todo this totes wont work for NumInputs
    rlpDecode (RLP.Array xs) = ListInput <$> sequence (RLP.rlpDecode <$> xs)

data RLPTest = RLPTest { input :: RLPTestInput, output :: T.Text }
    deriving (Eq, Read, Show)

instance FromJSON RLPTestInput where
    parseJSON (String s) | T.null s  = return (StringInput "")
                         | otherwise = case T.head s of
                            '#' -> return . NumberInput . read . T.unpack $ T.tail s
                            _   -> return . StringInput $ TE.encodeUtf8 s
    parseJSON (Number n) = return . NumberInput $ round n
    parseJSON (Array a)  = ListInput . V.toList <$> V.forM a parseJSON
    parseJSON x          = typeMismatch "RLPTestInput" x

instance ToJSON RLPTestInput where
    toJSON (StringInput s) = String $ TE.decodeUtf8 s
    toJSON (NumberInput n) = Number $ fromIntegral n
    toJSON (ListInput xs)  = toJSON xs

instance FromJSON RLPTest where
    parseJSON (Object o) = RLPTest <$> (o .: "in") <*> (o .: "out")
    parseJSON x          = typeMismatch "RLPTest" x

instance ToJSON RLPTest where
    toJSON     RLPTest{..} = object [ "in" .= input,   "out" .= output ]
    toEncoding RLPTest{..} = pairs  ( "in" .= input <> "out" .= output )

$(includeFileInSource "test/resources/rlptest.json" "officialRLPTests'")

officialRLPTests :: Either String [(T.Text, RLPTest)]
officialRLPTests = M.toList <$> eitherDecode (BL.fromStrict officialRLPTests')
