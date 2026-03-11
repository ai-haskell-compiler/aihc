-- | Parameters and return values for the cruncher
{-# LANGUAGE RecordWildCards #-}

module Exomizer.Data.Args
  ( -- * Options & Info
    CrunchOptions(..)
  , defaultCrunchOptions
  , fixCrunchOptions
  , validCrunchOptions
  , CrunchInfo(..)
  ) where

-- | Parameters for compression
--
-- @since 1.0.0
data CrunchOptions =
  CrunchOptions
    { coEncoding            :: Maybe String -- ^ @since 1.0.0
    , coMaxPasses           :: Int          -- ^ @since 1.0.0
    , coMaxLen              :: Int          -- ^ @since 1.0.0
    , coMaxOffset           :: Int          -- ^ @since 1.0.0
    , coUseLiteralSequences :: Bool         -- ^ @since 1.0.0
    , coFavourSpeed         :: Bool         -- ^ @since 1.0.0
    }
  deriving (Eq, Show)

-- | Default compression parameters
--
-- @since 1.0.0
defaultCrunchOptions :: CrunchOptions
defaultCrunchOptions =
  CrunchOptions
    { coEncoding            = Nothing
    , coMaxPasses           = 65535
    , coMaxLen              = 65535
    , coMaxOffset           = 65535
    , coUseLiteralSequences = True
    , coFavourSpeed         = False
    }

-- | Make sure the options are valid
--
-- @since 1.0.0
fixCrunchOptions :: CrunchOptions -> CrunchOptions
fixCrunchOptions CrunchOptions{..} =
  CrunchOptions
    { coEncoding            = checkEncoding coEncoding
    , coMaxPasses           = 1 `max` coMaxPasses `min` 65535
    , coMaxLen              = 0 `max` coMaxLen `min` 65535
    , coMaxOffset           = 0 `max` coMaxOffset `min` 65535
    , coUseLiteralSequences = coUseLiteralSequences
    , coFavourSpeed         = coFavourSpeed
    }
  where
    checkEncoding enc =
      case (check16Hex . checkComma . check16Hex . checkComma . check4Hex . checkComma . check16Hex) enc of
        Just "" -> enc
        _       -> Nothing
    check16Hex = check4Hex . check4Hex . check4Hex . check4Hex
    check4Hex = checkHex . checkHex . checkHex . checkHex
    checkHex (Just (x:xs))
      | x `elem` ("0123456789abcdefABCDEF" :: String) = Just xs
    checkHex _ = Nothing
    checkComma (Just (',':xs)) = Just xs
    checkComma _               = Nothing

-- | Check if the options are valid
--
-- @since 1.0.0
validCrunchOptions :: CrunchOptions -> Bool
{-# INLINE validCrunchOptions #-}
validCrunchOptions co = co == fixCrunchOptions co

-- | Information about the compressed data (result of the compression)
--
-- @since 1.0.0
data CrunchInfo =
  CrunchInfo
    { ciLiteralSequencesUsed :: Bool   -- ^ @since 1.0.0
    , ciNeededSafetyOffset   :: Int    -- ^ @since 1.0.0
    , ciUsedEncoding         :: String -- ^ @since 1.0.0
    }
  deriving (Eq, Show)
