{-# LANGUAGE CPP #-}
-- | This module exposes a simple API to initialize Vty in a
-- platform-independent way. This module only provides @mkVty@
-- for initializing the terminal. The rest of the Vty API is
-- accessed through the @vty@ package's API. If you need access to
-- platform-specific settings, it might be best to depend on and use the
-- platform-specific packages directly instead of using this package.
module Graphics.Vty.CrossPlatform
  ( mkVty
  )
where

import Graphics.Vty (Vty)
import Graphics.Vty.Config (VtyUserConfig)

-- Import the platform-specific module that provides 'mkVty'. The
-- convention is that each platform package should export a 'mkVty' with
-- the type signature indicated below, so only branching at import time
-- like this ensures that the build will break if either 1) a platform
-- doesn't provide 'mkVty' or 2) provides it but at a different type.
--
-- This approach works fine for now with just two main platforms as
-- options, but if we need to support a third, we'll probably need to
-- make this check more sophisticated. This approach also has a slight
-- risk in that it may come to a different conclusion about the build
-- platform than the Cabal "if os(...)" check. We could avoid that
-- by using the Cabal API here to do OS detection, but that has the
-- drawback that we'd then be depending on the Cabal library, and that
-- gets to be a big pain when the version of Cabal we depend on happens
-- to be different than the one used to build the 'cabal-install' that
-- got used to do the build.
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import qualified Graphics.Vty.Platform.Windows as Platform
#else
import qualified Graphics.Vty.Platform.Unix as Platform
#endif

-- | Build a 'Vty' handle with the specified configuration.
--
-- This dispatches to the appropriate platform-specific implementation
-- at build time based on the build environment's operating system.
mkVty :: VtyUserConfig
      -- ^ The configuration to use, usually
      -- 'Graphics.Vty.Config.defaultConfig' or the result of
      -- 'Graphics.Vty.Config.userConfig'.
      -> IO Vty
mkVty = Platform.mkVty
