-----------------------------------------------------------------------------
-- |
-- Module      :  Sound.ALSA.Mixer
-- Copyright   :  (c) Thomas Tuegel 2011
-- License     :  BSD
--
-- Maintainer  :  Thomas Tuegel <ttuegel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (Linux only)
--
-- This library provides bindings to the Advanced Linux Sound Architecture
-- (ALSA) library API. The portability of this library is limited to
-- systems with ALSA (i.e., Linux systems). The functions in this library
-- throw errors of type 'Sound.ALSA.Exception.T' on failure.
--
-----------------------------------------------------------------------------

module Sound.ALSA.Mixer
    ( -- * Types
      Control(..)
    , Mixer()
    , Channel(..)
    , PerChannel(..)
    , Volume(..)
    , Switch()
    , CUInt
    , CLong
      -- * Functions
      -- ** Mixers
    , controls
    , withMixer
      -- ** Controls
    , getControlByName
    , common
    , playback
    , capture
      -- ** PerChannels
    , channels
    , allChannels
    , joined
    , perChannel
    , getChannel
    , setChannel
      -- * Examples

      -- ** Getting and setting the volume of a Control
      -- $exampleVolume

      -- ** Getting and setting the switch of a Control
      -- $exampleSwitch
    ) where

import Control.Monad ( forM, liftM, when )
import Data.Maybe ( catMaybes )
import Foreign.C.Error ( Errno(..) )
import Foreign.C.Types
import Sound.ALSA.Exception ( catchErrno )
import Sound.ALSA.Mixer.Internal

-- | 'Control' represents one of the controls belonging to an ALSA mixer
-- element. Each control has a number of playback and capture channels.
-- The control may also have a switch and/or a volume capability associated
-- with it. The capability can be common to both playback and capture, or
-- there can be separate capabilities for each.
data Control = Control { index :: CUInt
                       , name :: String
                       , switch :: Either Switch (Maybe Switch, Maybe Switch)
                       , volume :: Either Volume (Maybe Volume, Maybe Volume)
                       }

-- | 'PerChannel' represents a capability that with either a separate value for
-- each channel or with a common value for all channels.
data PerChannel e = Joined { getJoined :: IO e
                           , setJoined :: e -> IO ()
                           , joinedChannels :: [Channel]
                           }
                  | PerChannel { getPerChannel :: IO [(Channel, e)]
                               , setPerChannel :: [(Channel, e)] -> IO ()
                               , perChannels :: [Channel]
                               }

-- | True if the 'PerChannel' object has a common value for all channels.
joined :: PerChannel e -> Bool
joined j@(Joined _ _ _) = True
joined _ = False

-- | True if the 'PerChannel' object has a separate value for each channel.
perChannel :: PerChannel e -> Bool
perChannel p@(PerChannel _ _ _) = True
perChannel _ = False

-- | All channels supported by a 'PerChannel' object.
channels :: PerChannel e -> [Channel]
channels p | joined p = joinedChannels p
           | otherwise = perChannels p

-- | 'Switch' represents a switch capability for controls and channels that can
-- be muted and unmuted.
type Switch = PerChannel Bool

-- | 'Volume' represents a volume capability. There may be a separate value per
-- channel, but each capability has only one range.
data Volume = Volume { getRange :: IO (CLong, CLong)
                       -- ^ Returns the minimum and maximum volumes (unitless).
                     , setRange :: (CLong, CLong) -> IO ()
                       -- ^ Sets the minimum and maximum volumes (unitless).
                     , getRangeDb :: IO (CLong, CLong)
                       -- ^ Returns the minimum and maximum volumes in
                       -- hundredths of a decibel.
                     , value :: PerChannel CLong
                       -- ^ Volume values for each channel.
                     , dB :: PerChannel CLong
                       -- ^ Volume values for each channel in hundredths of
                       -- a decibel.
                     }

-- | Get the value associated with a particular channel, if that channel exists.
getChannel :: Channel -> PerChannel x -> IO (Maybe x)
getChannel c p | joined p = let r | c `elem` channels p =
                                      liftM Just $ getJoined p
                                  | otherwise = return Nothing
                            in r
               | otherwise = liftM (lookup c) $ getPerChannel p

-- | Set the value associated with a particular channel, if that channel exists.
setChannel :: Channel -> PerChannel x -> x -> IO ()
setChannel c p v | joined p = when (c `elem` channels p) $ setJoined p v
                 | otherwise = setPerChannel p [(c, v)]

-- | For a given capability, which may be for either playback or capture, or
-- common to both, return the playback capability if it exists.
playback :: Either a (Maybe a, Maybe a) -> Maybe a
playback (Left _) = Nothing
playback (Right (x, _)) = x

-- | For a given capability, which may be for either playback or capture, or
-- common to both, return the capture capability if it exists.
capture :: Either a (Maybe a, Maybe a) -> Maybe a
capture (Left _) = Nothing
capture (Right (_, x)) = x

-- | For a given capability, which may be for either playback or capture, or
-- common to both, return the common capability if it exists.
common :: Either a (Maybe a, Maybe a) -> Maybe a
common (Left x) = Just x
common (Right _) = Nothing

mkSwitch :: SimpleElement -> IO (Either Switch (Maybe Switch, Maybe Switch))
mkSwitch se = do
    hasPlayChan <- mapM (hasPlaybackChannel se) allChannels
    hasCaptChan <- mapM (hasCaptureChannel se) allChannels
    let pChans = map fst $ filter snd $ zip allChannels hasPlayChan
        cChans = map fst $ filter snd $ zip allChannels hasCaptChan
    hasComSw <- hasCommonSwitch se
    hasPlaySw <- hasPlaybackSwitch se
    hasPlaySwJ <- hasPlaybackSwitchJoined se
    hasCaptSw <- hasCaptureSwitch se
    hasCaptSwJ <- hasCaptureSwitchJoined se
    return $ if hasComSw
                then Left $ if hasPlaySwJ
                              then comJoinedSwitch pChans
                              else comPerChannelSwitch pChans
                else let playSw | not hasPlaySw = Nothing
                                | otherwise = Just
                                    $ if hasPlaySwJ
                                        then playJoinedSwitch pChans
                                        else playPerChannelSwitch pChans
                         captSw | not hasCaptSw = Nothing
                                | otherwise = Just
                                    $ if hasCaptSwJ
                                        then captJoinedSwitch cChans
                                        else captPerChannelSwitch cChans
                     in Right (playSw, captSw)
  where joined fGet fSet chans =
            Joined { getJoined = fGet se (head chans)
                   , setJoined = fSet se (head chans)
                   , joinedChannels = chans
                   }
        perChannel fGet fSet chans =
            PerChannel { getPerChannel = liftM (zip chans)
                            $ mapM (fGet se) chans
                       , setPerChannel = mapM_ (uncurry (fSet se))
                       , perChannels = chans
                       }
        comJoinedSwitch = joined getPlaybackSwitch setPlaybackSwitch
        comPerChannelSwitch = perChannel getPlaybackSwitch setPlaybackSwitch
        playJoinedSwitch = comJoinedSwitch
        playPerChannelSwitch = comPerChannelSwitch
        captJoinedSwitch = joined getCaptureSwitch setCaptureSwitch
        captPerChannelSwitch = perChannel getCaptureSwitch setCaptureSwitch

mkVolume :: SimpleElement -> IO (Either Volume (Maybe Volume, Maybe Volume))
mkVolume se = do
    hasPlayChan <- mapM (hasPlaybackChannel se) allChannels
    hasCaptChan <- mapM (hasCaptureChannel se) allChannels
    let pChans = map fst $ filter snd $ zip allChannels hasPlayChan
        cChans = map fst $ filter snd $ zip allChannels hasCaptChan
    hasComV <- hasCommonVolume se
    hasPlayV <- hasPlaybackVolume se
    hasPlayVJ <- hasPlaybackVolumeJoined se
    hasCaptV <- hasCaptureVolume se
    hasCaptVJ <- hasCaptureVolumeJoined se
    return $
        if hasComV
           then let (v, d) | hasPlayVJ = ( comJoinedVol pChans
                                         , comJoinedDb pChans
                                         )
                           | otherwise = ( comPerChannelVol pChans
                                         , comPerChannelDb pChans
                                         )
                in Left $ playVolume { value = v, dB = d }
           else let playVol | not hasPlayV = Nothing
                            | otherwise =
                                let (v, d) | hasPlayVJ =
                                               ( playJoinedVol pChans
                                               , playJoinedDb pChans
                                               )
                                           | otherwise =
                                               ( playPerChannelVol pChans
                                               , playPerChannelDb pChans
                                               )
                                in Just playVolume { value = v, dB = d }
                    captVol | not hasCaptV = Nothing
                            | otherwise =
                                let (v, d) | hasCaptVJ =
                                               ( captJoinedVol cChans
                                               , captJoinedDb cChans
                                               )
                                           | otherwise =
                                               ( captPerChannelVol cChans
                                               , captPerChannelDb cChans
                                               )
                                in Just $ captVolume { value = v, dB = d }
                in Right (playVol, captVol)
  where j fGet fSet chans =
            Joined { getJoined = fGet se (head chans)
                   , setJoined = fSet se (head chans)
                   , joinedChannels = chans
                   }
        pc fGet fSet chans =
            PerChannel { getPerChannel = liftM (zip chans)
                            $ mapM (fGet se) chans
                       , setPerChannel = mapM_ (uncurry (fSet se))
                       , perChannels = chans
                       }
        playVolume = Volume { getRange = getPlaybackVolumeRange se
                            , setRange = setPlaybackVolumeRange se
                            , getRangeDb = getPlaybackDbRange se
                            , value = undefined
                            , dB = undefined
                            }
        captVolume = Volume { getRange = getCaptureVolumeRange se
                            , setRange = setCaptureVolumeRange se
                            , getRangeDb = getCaptureDbRange se
                            , value = undefined
                            , dB = undefined
                            }
        comJoinedVol = j getPlaybackVolume setPlaybackVolume
        comJoinedDb = j getPlaybackDb setPlaybackDb
        comPerChannelVol = pc getPlaybackVolume setPlaybackVolume
        comPerChannelDb = pc getPlaybackDb setPlaybackDb
        playJoinedVol = comJoinedVol
        playPerChannelVol = comPerChannelVol
        playJoinedDb = comJoinedDb
        playPerChannelDb = comPerChannelDb
        captJoinedVol = j getCaptureVolume setCaptureVolume
        captPerChannelVol = pc getCaptureVolume setCaptureVolume
        captJoinedDb = j getCaptureDb setCaptureDb
        captPerChannelDb = pc getCaptureDb setCaptureDb

-- | All the 'Control' objects associated with a particular 'Mixer'.
controls :: Mixer -> IO [Control]
controls mix = do
    es <- elements mix
    forM es $ \(idN, se) -> do
        n <- getName idN
        i <- getIndex idN
        sw <- mkSwitch se
        v <- mkVolume se
        return $! Control { name = n
                          , index = i
                          , switch = sw
                          , volume = v
                          }

-- | Get the named 'Control', if it exists, from the named 'Mixer'.
getControlByName :: Mixer   -- ^ Mixer
                 -> String  -- ^ Control name
                 -> IO (Maybe Control)
getControlByName mix controlName = do
    cs <- controls mix
    return $ lookup controlName $ zip (map name cs) cs

{- $exampleVolume
This example demonstrates the method of accessing the volume of a Control.
The example function reads the volume and increases it by the value supplied.

>   changeVolumeBy :: CLong -> IO ()
>   changeVolumeBy i =
>       withMixer "default" $ \mixer ->
>         do Just control <- getControlByName mixer "Master"
>            let Just playbackVolume = playback $ volume control
>            (min, max) <- getRange playbackVolume
>            Just vol <- getChannel FrontLeft $ value $ playbackVolume
>            when ((i > 0 && vol < max) || (i < 0 && vol > min))
>              $ setChannel FrontLeft (value $ playbackVolume) $ vol + i

-}

{- $exampleSwitch
This example demonstrates the method of accessing the switch of a Control.
The example function reads the value of the switch and toggles it.

>   toggleMute :: IO ()
>   toggleMute =
>       withMixer "default" $ \mixer ->
>         do Just control <- getControlByName mixer "Master"
>            let Just playbackSwitch = playback $ switch control
>            Just sw <- getChannel FrontLeft playbackSwitch
>            setChannel FrontLeft playbackSwitch $ not sw

-}
