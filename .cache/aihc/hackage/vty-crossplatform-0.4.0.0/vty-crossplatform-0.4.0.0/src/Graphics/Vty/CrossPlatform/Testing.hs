{-# LANGUAGE CPP #-}
module Graphics.Vty.CrossPlatform.Testing
  ( mkDefaultOutput
  )
where

import Graphics.Vty (Output)

import Graphics.Vty.Config (defaultConfig)

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import Graphics.Vty.Platform.Windows.Output (buildOutput)
import Graphics.Vty.Platform.Windows.Settings (defaultSettings)
#else
import Graphics.Vty.Platform.Unix.Output (buildOutput)
import Graphics.Vty.Platform.Unix.Settings (defaultSettings)
#endif

-- | This helper is not intended for end-user consumption; it is exposed
-- only for testing purposes.
mkDefaultOutput :: IO Output
mkDefaultOutput = defaultSettings >>= buildOutput defaultConfig
