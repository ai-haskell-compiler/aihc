{-# LANGUAGE CPP               #-}

#if defined(mingw32_HOST_OS)
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -Wno-orphans #-}
#endif

{-|
Module:      TextShow.System.Win32
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instances for data types in the @Win32@ library.
Only provided if using Windows.

/Since: 2/
-}
module TextShow.System.Win32 () where

#if defined(mingw32_HOST_OS)
import Graphics.Win32.GDI.AlphaBlend (BLENDFUNCTION)

import Prelude ()
import Prelude.Compat

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

import TextShow (TextShow(..), fromString)

import TextShow.TH (deriveTextShow)

-- | /Since: 2/
$(deriveTextShow ''Exception)
-- | /Since: 2/
$(deriveTextShow ''DebugEventInfo)
-- | /Since: 2/
$(deriveTextShow ''FILETIME)
-- | /Since: 2/
$(deriveTextShow ''BY_HANDLE_FILE_INFORMATION)
-- | /Since: 2/
$(deriveTextShow ''WIN32_FILE_ATTRIBUTE_DATA)
-- | /Since: 2/
$(deriveTextShow ''ProcessorArchitecture)
-- | /Since: 2/
$(deriveTextShow ''SYSTEM_INFO)
-- | /Since: 2/
$(deriveTextShow ''SYSTEMTIME)
-- | /Since: 2/
$(deriveTextShow ''TIME_ZONE_INFORMATION)
-- | /Since: 2/
$(deriveTextShow ''TimeZoneId)
-- | /Since: 3.6/
$(deriveTextShow ''BLENDFUNCTION)
-- | /Since: 3.6/
$(deriveTextShow ''KEYBDINPUT)
-- | /Since: 3.6/
$(deriveTextShow ''MOUSEINPUT)
-- | /Since: 3.6/
$(deriveTextShow ''HARDWAREINPUT)
-- | /Since: 3.6/
$(deriveTextShow ''INPUT)
-- | /Since: 3.6/
$(deriveTextShow ''ProductType)
-- | /Since: 3.6/
$(deriveTextShow ''OSVERSIONINFOEX)
-- | /Since: 3.6/
$(deriveTextShow ''MEMORY_BASIC_INFORMATION)
-- | /Since: 3.6/
$(deriveTextShow ''RecipientClass)
-- | /Since: 3.6/
$(deriveTextShow ''Recipient)
-- | /Since: 3.6/
$(deriveTextShow ''FileTag)
-- | /Since: 3.6/
$(deriveTextShow ''Attachment)
-- | /Since: 3.6/
$(deriveTextShow ''Message)

-- | /Since: 3.6/
instance TextShow Unsupported where
  showb (MissingLibrary  name reason)
    = "Can't load library \"" <> fromString name <> "\". "  <> fromString reason
  showb (MissingFunction name reason)
    = "Can't find \"" <> fromString name <> "\" function. " <> fromString reason
  showb (MissingValue    name reason)
    = "Can't use \""  <> fromString name <> "\" value. "    <> fromString reason
#endif
