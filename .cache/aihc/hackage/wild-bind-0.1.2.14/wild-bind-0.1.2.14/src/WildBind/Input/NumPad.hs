{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: WildBind.Input.NumPad
-- Description: Types about number pads
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- Input types for number pad keys.
module WildBind.Input.NumPad
    ( -- * NumLock disabled
      NumPadUnlocked (..)
      -- * NumLock enabled
    , NumPadLocked (..)
    ) where

import           WildBind.Description (Describable (describe))

-- | Number pad key input with NumLock disabled.
data NumPadUnlocked = NumInsert | NumEnd | NumDown | NumPageDown | NumLeft | NumCenter | NumRight | NumHome | NumUp | NumPageUp | NumDivide | NumMulti | NumMinus | NumPlus | NumEnter | NumDelete deriving
    ( Bounded
    , Enum
    , Eq
    , Ord
    , Show
    )

instance Describable NumPadUnlocked where
  describe input = case input of
    NumHome     -> "Home"
    NumUp       -> "↑"
    NumPageUp   -> "PageUp"
    NumLeft     -> "←"
    NumCenter   -> ""
    NumRight    -> "→"
    NumEnd      -> "End"
    NumDown     -> "↓"
    NumPageDown -> "PageDown"
    NumDivide   -> "/"
    NumMulti    -> "*"
    NumMinus    -> "-"
    NumPlus     -> "+"
    NumEnter    -> "Enter"
    NumInsert   -> "Insert"
    NumDelete   -> "Delete"


-- | Number pad key input with NumLock enabled.
data NumPadLocked = NumL0 | NumL1 | NumL2 | NumL3 | NumL4 | NumL5 | NumL6 | NumL7 | NumL8 | NumL9 | NumLDivide | NumLMulti | NumLMinus | NumLPlus | NumLEnter | NumLPeriod deriving
    ( Bounded
    , Enum
    , Eq
    , Ord
    , Show
    )

instance Describable NumPadLocked where
  describe input = case input of
    NumL0      -> "0"
    NumL1      -> "1"
    NumL2      -> "2"
    NumL3      -> "3"
    NumL4      -> "4"
    NumL5      -> "5"
    NumL6      -> "6"
    NumL7      -> "7"
    NumL8      -> "8"
    NumL9      -> "9"
    NumLDivide -> "/"
    NumLMulti  -> "*"
    NumLMinus  -> "-"
    NumLPlus   -> "+"
    NumLEnter  -> "Enter"
    NumLPeriod -> "."

