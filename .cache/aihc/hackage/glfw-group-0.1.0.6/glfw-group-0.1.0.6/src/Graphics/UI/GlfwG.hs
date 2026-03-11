{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.UI.GlfwG (

	-- * INIT AND ERROR

	init, setErrorCallback, ErrorCallback, GlfwB.Error, ErrorMessage,

	-- * EVENT

	GlfwB.pollEvents, GlfwB.waitEvents,

	-- * VULKAN

	GlfwB.vulkanSupported, getRequiredInstanceExtensions,

	) where

import Prelude hiding (init)

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.String
import Control.Exception
import Data.Bool
import Data.Text qualified as Txt
import Data.Text.Foreign qualified as Txt

import qualified Graphics.UI.GLFW as GlfwB

init :: (ErrorMessage -> IO a) -> IO a -> IO a
init hdl cmp = GlfwB.init >>= bool
	(hdl $ "Gpu.Vulkan.Khr.Surface.Glfw: " ++
		"GLFW-b.Graphics.UI.GLFW.init return False")
	(finally cmp GlfwB.terminate)

setErrorCallback :: Maybe ErrorCallback -> IO ()
setErrorCallback = GlfwB.setErrorCallback

type ErrorCallback = GlfwB.Error -> ErrorMessage -> IO ()

type ErrorMessage = String

getRequiredInstanceExtensions :: IO [Txt.Text]
getRequiredInstanceExtensions = (cstrToText `mapM`) =<< GlfwB.getRequiredInstanceExtensions

cstrToText :: CString -> IO Txt.Text
cstrToText cs = Txt.peekCStringLen =<< cstringToCStringLen cs

cstringLength :: CString -> IO Int
cstringLength pc = do
	c <- peek pc
	case c of
		0 -> pure 0
		_ -> (+ 1) <$> cstringLength (pc `plusPtr` 1)

cstringToCStringLen :: CString -> IO CStringLen
cstringToCStringLen cs = (cs ,) <$> cstringLength cs
