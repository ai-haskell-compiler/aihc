{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}

{-|
Module:      Web.KeyCode
Copyright:   (C) 2015-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: Portable

Keyboard events in web browsers are often represented as keycodes, which (1) are
difficult to remember, and (2) sometimes vary from browser to browser. This module
allows one to look up a key press's 'KeyCode' and get a plain English description
of the 'Key' that was pressed, to reduce confusion.

/Since: 0.1/
-}
module Web.KeyCode (Key(..), KeyCode, keyCodeLookup, keyCodeMap, isKeyCode) where

import Data.Data (Data)
import Data.IntMap (IntMap, findWithDefault, fromAscList)
import Data.Ix (Ix)
import GHC.Generics (Generic)
import Language.Haskell.TH.Syntax (Lift)

-- | A numeric code representing the value of a pressed 'Key'. Note that a particular
-- 'Key' may not uniquely map to a particular 'KeyCode', as the implementation of
-- key codes is browser-dependent.
--
-- /Since: 0.1/
type KeyCode = Int

-- | Represents a typical keyboard's keys. The lowercase and uppercase variants of any
-- particular key have the same 'KeyCode', so there are not separate constructors for
-- them. There is also an 'UnknownKey' constructor for keys without a particular
-- 'KeyCode'.
--
-- Note that the 'Enum' instance does not correspond to the 'KeyCode's, but is simply
-- provided for convenience.
--
-- /Since: 0.1/
data Key = Backspace
         | Tab
         | NumLock
         | Enter
         | Shift
         | Control
         | Alt
         | Pause
         | CapsLock
         | Escape
         | Space
         | PageUp
         | PageDown
         | End
         | Home
         | ArrowLeft
         | ArrowUp
         | ArrowRight
         | ArrowDown
         | PrintScreen
         | Insert
         | Delete
         | Digit0  -- ^ Without Shift: @0@. With Shift: @)@.
         | Digit1  -- ^ Without Shift: @1@. With Shift: @!@.
         | Digit2  -- ^ Without Shift: @2@. With Shift: @\@@.
         | Digit3  -- ^ Without Shift: @3@. With Shift: @#@.
         | Digit4  -- ^ Without Shift: @4@. With Shift: @$@.
         | Digit5  -- ^ Without Shift: @5@. With Shift: @%@.
         | Digit6  -- ^ Without Shift: @6@. With Shift: @^@.
         | Digit7  -- ^ Without Shift: @7@. With Shift: @&@.
         | Digit8  -- ^ Without Shift: @8@. With Shift: @*@.
         | Digit9  -- ^ Without Shift: @9@. With Shift: @(@.
         | KeyA    -- ^ Without Shift: @a@. With Shift: @A@.
         | KeyB    -- ^ Without Shift: @b@. With Shift: @B@.
         | KeyC    -- ^ Without Shift: @c@. With Shift: @C@.
         | KeyD    -- ^ Without Shift: @d@. With Shift: @D@.
         | KeyE    -- ^ Without Shift: @e@. With Shift: @E@.
         | KeyF    -- ^ Without Shift: @f@. With Shift: @F@.
         | KeyG    -- ^ Without Shift: @g@. With Shift: @G@.
         | KeyH    -- ^ Without Shift: @h@. With Shift: @H@.
         | KeyI    -- ^ Without Shift: @i@. With Shift: @I@.
         | KeyJ    -- ^ Without Shift: @j@. With Shift: @J@.
         | KeyK    -- ^ Without Shift: @k@. With Shift: @K@.
         | KeyL    -- ^ Without Shift: @l@. With Shift: @L@.
         | KeyM    -- ^ Without Shift: @m@. With Shift: @M@.
         | KeyN    -- ^ Without Shift: @n@. With Shift: @N@.
         | KeyO    -- ^ Without Shift: @o@. With Shift: @O@.
         | KeyP    -- ^ Without Shift: @p@. With Shift: @P@.
         | KeyQ    -- ^ Without Shift: @q@. With Shift: @Q@.
         | KeyR    -- ^ Without Shift: @r@. With Shift: @R@.
         | KeyS    -- ^ Without Shift: @s@. With Shift: @S@.
         | KeyT    -- ^ Without Shift: @t@. With Shift: @T@.
         | KeyU    -- ^ Without Shift: @u@. With Shift: @U@.
         | KeyV    -- ^ Without Shift: @v@. With Shift: @V@.
         | KeyW    -- ^ Without Shift: @w@. With Shift: @W@.
         | KeyX    -- ^ Without Shift: @x@. With Shift: @X@.
         | KeyY    -- ^ Without Shift: @y@. With Shift: @Y@.
         | KeyZ    -- ^ Without Shift: @z@. With Shift: @Z@.
         | Command -- ^ Might also be the Windows key or the Super key
         | Numpad0
         | Numpad1
         | Numpad2
         | Numpad3
         | Numpad4
         | Numpad5
         | Numpad6
         | Numpad7
         | Numpad8
         | Numpad9
         | NumpadMultiply
         | NumpadAdd
         | NumpadEnter
         | NumpadSubtract
         | NumpadDecimal
         | NumpadDivide
         | F1
         | F2
         | F3
         | F4
         | F5
         | F6
         | F7
         | F8
         | F9
         | F10
         | F11
         | F12
         | ScrollLock
         | Semicolon    -- ^ Without Shift: @;@. With Shift: @:@.
         | Equals       -- ^ Without Shift: @=@. With Shift: @+@.
         | Comma        -- ^ Without Shift: @,@. With Shift: @<@.
         | Subtract     -- ^ Without Shift: @-@. With Shift: @_@.
         | Period       -- ^ Without Shift: @.@. With Shift: @>@.
         | ForwardSlash -- ^ Without Shift: @/@. With Shift: @?@.
         | Backquote    -- ^ Without Shift: @`@. With Shift: @~@.
         | BracketLeft  -- ^ Without Shift: @[@. With Shift: @{@.
         | Backslash    -- ^ Without Shift: @\\@. With Shift: @|@.
         | BracketRight -- ^ Without Shift: @]@. With Shift: @}@.
         | Apostrophe   -- ^ Without Shift: @\'@. With Shift: @"@.
         | UnknownKey
  deriving ( Bounded
           , Data
           , Enum
           , Eq
           , Generic
           , Ix
           , Lift
           , Ord
           , Read
           , Show
           )

-- | Determine the 'Key' that a 'KeyCode' represents. If one cannot be found,
-- 'UnknownKey' is returned.
--
-- /Since: 0.1/
keyCodeLookup :: KeyCode -> Key
keyCodeLookup key = findWithDefault UnknownKey key keyCodeMap

-- | An map of known 'KeyCode's to 'Key's.
--
-- /Since: 0.1/
keyCodeMap :: IntMap Key
keyCodeMap = fromAscList [
      -- Thanks to David Mauro for his keyCode mapping from the Keypress library
      -- (https://github.com/dmauro/Keypress/blob/e5e95070d81b998b02b2d7f096267b114a3771d7/keypress.coffee#L802-L916)
      -- Licensed under the Apache License, version 2.0
      (  8, Backspace     )
    , (  9, Tab           )
    , ( 12, NumLock       )
    , ( 13, Enter         )
    , ( 16, Shift         )
    , ( 17, Control       )
    , ( 18, Alt           )
    , ( 19, Pause         )
    , ( 20, CapsLock      )
    , ( 27, Escape        )
    , ( 32, Space         )
    , ( 33, PageUp        )
    , ( 34, PageDown      )
    , ( 35, End           )
    , ( 36, Home          )
    , ( 37, ArrowLeft     )
    , ( 38, ArrowUp       )
    , ( 39, ArrowRight    )
    , ( 40, ArrowDown     )
    , ( 44, PrintScreen   )
    , ( 45, Insert        )
    , ( 46, Delete        )
    , ( 48, Digit0        )
    , ( 49, Digit1        )
    , ( 50, Digit2        )
    , ( 51, Digit3        )
    , ( 52, Digit4        )
    , ( 53, Digit5        )
    , ( 54, Digit6        )
    , ( 55, Digit7        )
    , ( 56, Digit8        )
    , ( 57, Digit9        )
    , ( 59, Semicolon     ) -- Firefox oddity
    , ( 61, Equals        ) -- Firefox oddity
    , ( 65, KeyA          )
    , ( 66, KeyB          )
    , ( 67, KeyC          )
    , ( 68, KeyD          )
    , ( 69, KeyE          )
    , ( 70, KeyF          )
    , ( 71, KeyG          )
    , ( 72, KeyH          )
    , ( 73, KeyI          )
    , ( 74, KeyJ          )
    , ( 75, KeyK          )
    , ( 76, KeyL          )
    , ( 77, KeyM          )
    , ( 78, KeyN          )
    , ( 79, KeyO          )
    , ( 80, KeyP          )
    , ( 81, KeyQ          )
    , ( 82, KeyR          )
    , ( 83, KeyS          )
    , ( 84, KeyT          )
    , ( 85, KeyU          )
    , ( 86, KeyV          )
    , ( 87, KeyW          )
    , ( 88, KeyX          )
    , ( 89, KeyY          )
    , ( 90, KeyZ          )
    , ( 91, Command       )
    , ( 92, Command       )
    , ( 93, Command       )
    , ( 96, Numpad0       )
    , ( 97, Numpad1       )
    , ( 98, Numpad2       )
    , ( 99, Numpad3       )
    , (100, Numpad4       )
    , (101, Numpad5       )
    , (102, Numpad6       )
    , (103, Numpad7       )
    , (104, Numpad8       )
    , (105, Numpad9       )
    , (106, NumpadMultiply)
    , (107, NumpadAdd     )
    , (108, NumpadEnter   )
    , (109, NumpadSubtract)
    , (110, NumpadDecimal )
    , (111, NumpadDivide  )
    , (112, F1            )
    , (113, F2            )
    , (114, F3            )
    , (115, F4            )
    , (116, F5            )
    , (117, F6            )
    , (118, F7            )
    , (119, F8            )
    , (120, F9            )
    , (121, F10           )
    , (122, F11           )
    , (123, F12           )
    , (124, PrintScreen   )
    , (144, NumLock       )
    , (145, ScrollLock    )
    , (173, Subtract      ) -- Firefox oddity
    , (186, Semicolon     )
    , (187, Equals        )
    , (188, Comma         )
    , (189, Subtract      )
    , (190, Period        )
    , (191, ForwardSlash  )
    , (192, Backquote     )
    , (219, BracketLeft   )
    , (220, Backslash     )
    , (221, BracketRight  )
    , (222, Apostrophe    )
    , (223, Backquote     )
    , (224, Command       )
    , (225, Alt           )
    ]

-- | Return 'True' if the given 'KeyCode' matches the given 'Key'.
--
-- /Since: 0.2.2/
isKeyCode :: Key -> KeyCode -> Bool
isKeyCode key code = key == keyCodeLookup code
