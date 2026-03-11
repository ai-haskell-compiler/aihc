{-# LANGUAGE CPP #-}
{-|
Module:      Spec.System.Win32Spec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the @Win32@ library.
-}
module Spec.System.Win32Spec (main, spec) where

import Prelude ()
import Prelude.Compat

import Test.Hspec (Spec, hspec, parallel)

#if defined(mingw32_HOST_OS)
import Data.Proxy (Proxy(..))

import Graphics.Win32.GDI.AlphaBlend (BLENDFUNCTION)

import Instances.System.Win32 ()

import Spec.Utils (matchesTextShowSpec)

import System.Win32.Automation.Input (HARDWAREINPUT, INPUT)
import System.Win32.Automation.Input.Key (KEYBDINPUT)
import System.Win32.Automation.Input.Mouse (MOUSEINPUT)
import System.Win32.DebugApi (DebugEventInfo, Exception)
import System.Win32.Exception.Unsupported (Unsupported(..))
import System.Win32.File (BY_HANDLE_FILE_INFORMATION, WIN32_FILE_ATTRIBUTE_DATA)
import System.Win32.Info (ProcessorArchitecture, SYSTEM_INFO)
import System.Win32.Info.Version (ProductType, OSVERSIONINFOEX)
import System.Win32.Mem (MEMORY_BASIC_INFORMATION)
import System.Win32.SimpleMAPI (RecipientClass, Recipient, FileTag, Attachment, Message)
import System.Win32.Time (FILETIME, SYSTEMTIME, TIME_ZONE_INFORMATION, TimeZoneId)

import Test.Hspec (describe)

import TextShow.System.Win32 ()
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
#if defined(mingw32_HOST_OS)
    describe "DebugEventInfo" $
        matchesTextShowSpec (Proxy :: Proxy DebugEventInfo)
    describe "Exception" $
        matchesTextShowSpec (Proxy :: Proxy Exception)
    describe "BY_HANDLE_FILE_INFORMATION" $
        matchesTextShowSpec (Proxy :: Proxy BY_HANDLE_FILE_INFORMATION)
    describe "WIN32_FILE_ATTRIBUTE_DATA" $
        matchesTextShowSpec (Proxy :: Proxy WIN32_FILE_ATTRIBUTE_DATA)
    describe "ProcessorArchitecture" $
        matchesTextShowSpec (Proxy :: Proxy ProcessorArchitecture)
    describe "SYSTEM_INFO" $
        matchesTextShowSpec (Proxy :: Proxy SYSTEM_INFO)
    describe "FILETIME" $
        matchesTextShowSpec (Proxy :: Proxy FILETIME)
    describe "SYSTEMTIME" $
        matchesTextShowSpec (Proxy :: Proxy SYSTEMTIME)
    describe "TIME_ZONE_INFORMATION" $
        matchesTextShowSpec (Proxy :: Proxy TIME_ZONE_INFORMATION)
    describe "TimeZoneId" $
        matchesTextShowSpec (Proxy :: Proxy TimeZoneId)
    describe "BLENDFUNCTION" $
        matchesTextShowSpec (Proxy :: Proxy BLENDFUNCTION)
    describe "KEYBDINPUT" $
        matchesTextShowSpec (Proxy :: Proxy KEYBDINPUT)
    describe "MOUSEINPUT" $
        matchesTextShowSpec (Proxy :: Proxy MOUSEINPUT)
    describe "HARDWAREINPUT" $
        matchesTextShowSpec (Proxy :: Proxy HARDWAREINPUT)
    describe "INPUT" $
        matchesTextShowSpec (Proxy :: Proxy INPUT)
    describe "ProductType" $
        matchesTextShowSpec (Proxy :: Proxy ProductType)
    describe "OSVERSIONINFOEX" $
        matchesTextShowSpec (Proxy :: Proxy OSVERSIONINFOEX)
    describe "MEMORY_BASIC_INFORMATION" $
        matchesTextShowSpec (Proxy :: Proxy MEMORY_BASIC_INFORMATION)
    describe "RecipientClass" $
        matchesTextShowSpec (Proxy :: Proxy RecipientClass)
    describe "Recipient" $
        matchesTextShowSpec (Proxy :: Proxy Recipient)
    describe "FileTag" $
        matchesTextShowSpec (Proxy :: Proxy FileTag)
    describe "Attachment" $
        matchesTextShowSpec (Proxy :: Proxy Attachment)
    describe "Message" $
        matchesTextShowSpec (Proxy :: Proxy Message)
    describe "Unsupported" $
        matchesTextShowSpec (Proxy :: Proxy Unsupported)
#else
    pure ()
#endif
