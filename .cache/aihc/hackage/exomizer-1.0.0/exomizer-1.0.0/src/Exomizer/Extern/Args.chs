-- | Interface for options / result
{-# LANGUAGE RecordWildCards #-}

module Exomizer.Extern.Args
  ( withCrunchOptions
  , withCrunchInfo
  ) where

import           Foreign.C
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Utils
import           Foreign.Ptr

import           Exomizer.Data.Args

#import "exo_helper.h"

-- | @since 1.0.0
withCrunchOptions :: CrunchOptions -> (Ptr CrunchOptions -> IO a) -> IO a
{-# INLINE withCrunchOptions #-}
withCrunchOptions co action =
  let
    CrunchOptions{..} = fixCrunchOptions co
  in
    withNullableCString coEncoding $ \encodingPtr ->
      allocaBytesAligned
        {#sizeof crunch_options#}
        {#alignof crunch_options#}
        $ \coPtr -> do
          {#set crunch_options->exported_encoding#} coPtr encodingPtr
          {#set crunch_options->max_passes#} coPtr (fromIntegral coMaxPasses)
          {#set crunch_options->max_len#} coPtr (fromIntegral coMaxLen)
          {#set crunch_options->max_offset#} coPtr (fromIntegral coMaxOffset)
          {#set crunch_options->use_literal_sequences#} coPtr (fromBool coUseLiteralSequences)
          {#set crunch_options->favor_speed#} coPtr (fromBool coFavourSpeed)
          {#set crunch_options->output_header#} coPtr (fromBool True)
          action coPtr

-- | @since 1.0.0
withCrunchInfo :: (Ptr CrunchInfo -> IO a) -> IO (a, CrunchInfo)
{-# INLINE withCrunchInfo #-}
withCrunchInfo action = do
  allocaBytesAligned
    {#sizeof crunch_info#}
    {#alignof crunch_info#}
    $ \ciPtr -> do
      res <- action ciPtr
      ci <-
        CrunchInfo
          <$> toBool `fmap` {#get crunch_info->literal_sequences_used#} ciPtr
          <*> fromIntegral `fmap` {#get crunch_info->needed_safety_offset#} ciPtr
          <*> (peekCString =<< {#get crunch_info->used_encoding#} ciPtr)
      return (res, ci)

-- helper

withNullableCString :: Maybe String -> (Ptr CChar -> IO a) -> IO a
{-# INLINE withNullableCString #-}
withNullableCString  Nothing      action = action nullPtr
withNullableCString (Just string) action = withCString string action
