{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module LengthR where

import Data.List.Length

hello :: LengthR 5 Char
hello = NilR :+ 'h' :+ 'e' :+ 'l' :+ 'l' :+ 'o'
