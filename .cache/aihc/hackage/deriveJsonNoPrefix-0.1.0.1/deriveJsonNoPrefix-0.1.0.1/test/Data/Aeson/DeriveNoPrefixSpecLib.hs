{-# LANGUAGE TemplateHaskell #-}

module Data.Aeson.DeriveNoPrefixSpecLib where

import           Data.Aeson.DeriveNoPrefix


data SomeRecord = SomeRecord
  { someRecordId :: String
  , someRecordMax :: Double
  , someRecordMin :: Double
  } deriving (Eq, Show)


$(deriveJsonNoTypeNamePrefix ''SomeRecord)
