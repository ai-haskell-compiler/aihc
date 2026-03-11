-- | This module provides mappings for keyboard characters and modification keys to VTY event values
module Graphics.Vty.Platform.Windows.Input.Terminfo
  ( classifyMapForTerm
  , universalTable
  , commonVisibleChars
  , specialSupportKeys
  )
where

import Data.Maybe (mapMaybe)
import Graphics.Vty.Input.Events
import qualified Graphics.Vty.Platform.Windows.Input.Terminfo.ANSIVT as ANSIVT

-- | Builds input sequences for all VT sequences available on Windows
classifyMapForTerm :: ClassifyMap
classifyMapForTerm =
    concat $ universalTable
           : ANSIVT.classifyTable

-- | Combined tables.
universalTable :: ClassifyMap
universalTable = concat
    [ commonVisibleChars
    , metaChars
    , otherVisibleChars
    , ctrlChars
    , ctrlMetaChars
    , specialSupportKeys
    ]

-- | Visible characters in the ISO-8859-1 and UTF-8 common set up to
-- but not including those in the range 0xA1 to 0xC1
commonVisibleChars :: ClassifyMap
commonVisibleChars =
    [ ([x], EvKey (KChar x) [])
    | x <- [' ' .. toEnum 0x7F]
    ]

metaChars :: ClassifyMap
metaChars = mapMaybe f commonVisibleChars
  where
    f ([c], _) = Just ('\ESC':[c], EvKey (KChar c) [MMeta])
    f _ = Nothing

otherVisibleChars :: ClassifyMap
otherVisibleChars =
    [ ([x], EvKey (KChar x) [])
    | x <- [toEnum 0x8A .. toEnum 0xC1]
    ]

-- | Non-printable characters in the ISO-8859-1 and UTF-8 common set
-- translated to ctrl + char.
--
-- This treats CTRL-i the same as tab.
ctrlChars :: ClassifyMap
ctrlChars =
    [ ([toEnum x], EvKey (KChar y) [MCtrl])
    | (x,y) <- zip [0..31] ('@':['a'..'z']++['['..'_'])
    , y /= 'i'  -- Resolve issue #3 where CTRL-i hides TAB.
    , y /= 'h'  -- CTRL-h should not hide BS
    ]

-- | Ctrl+Meta+Char
ctrlMetaChars :: ClassifyMap
ctrlMetaChars = mapMaybe f ctrlChars
  where
    f (s, EvKey c m) = Just ('\ESC':s, EvKey c (MMeta:m))
    f _ = Nothing

-- | Escape, backspace, enter, tab.
specialSupportKeys :: ClassifyMap
specialSupportKeys =
    -- special support for ESC
    [ ("\ESC",EvKey KEsc []), ("\ESC\ESC",EvKey KEsc [MMeta])
    -- Special support for backspace
    , ("\DEL", EvKey KBS []), ("\ESC\DEL", EvKey KBS [MMeta]), ("\b", EvKey KBS [MCtrl])
    -- Special support for Enter
    , ("\r",EvKey KEnter []), ("\ESC\^J",EvKey KEnter [MMeta]), ("\n", EvKey KEnter [MCtrl])
    -- explicit support for tab
    , ("\t", EvKey (KChar '\t') []), ("\ESC[Z", EvKey (KChar '\t') [MShift])
    ]
