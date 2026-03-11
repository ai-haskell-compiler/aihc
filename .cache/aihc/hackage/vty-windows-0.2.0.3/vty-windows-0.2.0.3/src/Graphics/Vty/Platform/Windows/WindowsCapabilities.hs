-- | This module provides functions to obtain VT escape sequences for the Windows platform
module Graphics.Vty.Platform.Windows.WindowsCapabilities
    ( getStringCapability,
      getIntCapability
    ) where

import qualified Data.Map as M

-- | Lookup for terminal capabilities that have a string value.
-- All Windows supported escape sequences are described here:
-- https://learn.microsoft.com/en-us/windows/console/console-virtual-terminal-sequences
getStringCapability :: String -> Maybe String
getStringCapability cap = M.lookup cap windowsStringCaps

-- | Lookup a terminal capability that has an integer value
getIntCapability :: String -> Maybe Int
getIntCapability cap = M.lookup cap windowsIntCaps

esc :: String -> String
esc code = '\ESC' : code

csi :: String -> String
csi code = esc $ '[' : code

-- | Mapping of capability names to VT escape sequences for the Windows platform
-- All Windows supported escape sequences are described here:
-- https://learn.microsoft.com/en-us/windows/console/console-virtual-terminal-sequences
windowsStringCaps :: M.Map String String
windowsStringCaps = M.fromList
    [ ("sgr0", csi "0m")   -- reset styles and colors
    , ("bold", csi "1m")   -- set bold style
    , ("dim", csi "2m")    -- set dim style
    , ("sitm", csi "3m")   -- set italic style
    , ("smul", csi "4m")   -- set underline style
    , ("rev", csi "7m")    -- set reverse video mode (reverse foreground/background colors)
    , ("invis", csi "8m")  -- set hidden text style
    , ("smxx", csi "9m")   -- set strike-through style
    , ("ritm", csi "23m")  -- exit italic mode
    , ("rmul", csi "24m")  -- exit underline mode
    , ("rmso", csi "27m")  -- exit reverse video mode
    , ("rmxx", csi "29m")  -- exit strikethrough mode
    , ("clear", csi "H" ++ csi "J")   -- move cursor to home row, then clear from cursor to end of screen (whole screen)
    , ("cup", csi "%i%p1%d;%p2%dH")
    , ("civis", csi "?25l")
    , ("cnorm", csi "?25h")
    , ("home", csi "H")
    , ("ed", csi "J")
    , ("el", csi "K")
    , ("kbs", "^H")
    , ("setab", csi "%?%p1%{8}%<%t4%p1%d%e%p1%{16}%<%t10%p1%{8}%-%d%e48;5;%p1%d%;m")
    , ("setaf", csi "%?%p1%{8}%<%t3%p1%d%e%p1%{16}%<%t9%p1%{8}%-%d%e38;5;%p1%d%;m")
    , ("sgr", csi "0%?%p6%t;1%;%?%p2%t;4%;%?%p1%p3%|%t;7%;%?%p4%t;5%;%?%p5%t;2%;%?%p7%t;8%;m")
    , ("smcup", csi "?1049h")   -- switch to the alternate screen buffer
    , ("rmcup", csi "?1049l")   -- exit the alternate screen buffer
    ]

windowsIntCaps :: M.Map String Int
windowsIntCaps = M.fromList 
    [ ("colors", 256)
    ]
