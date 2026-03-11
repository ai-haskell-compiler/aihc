module Data.Monoid.Colorful.SGR (
  SGRCode,
  sgrCode
) where

import Data.Monoid.Colorful.Term
import Data.Monoid.Colorful.Settings
import Data.Monoid.Colorful.Color
import Data.Word (Word8)
import Data.Bool (bool)

type SGRCode = String

csi :: Char -> [Word8] -> SGRCode
csi cmd args = (("\ESC["++) . go args) [cmd]
  where go [] = id
        go [x] = (show x++)
        go (x:xs@(_:_)) = (show x++) . (';':) . go xs
{-# INLINE csi #-}

sgrCode :: Term -> Settings -> Settings -> SGRCode
sgrCode TermDumb _ _ = ""
sgrCode TermWin  _ _ = ""
sgrCode t old new
  | old == new = ""
  | new == defaultSettings = csi 'm' []
  | otherwise = csi 'm' $
                (flag  settingBlink     5 .
                 flag  settingBold      1 .
                 flag  settingItalic    3 .
                 flag  settingUnderline 4 .
                 flag  settingInvert    7 .
                 color settingFg        0 .
                 color settingBg        10) []

  where

  update :: Eq a => (Settings -> a) -> (a -> ([Word8] -> [Word8])) -> ([Word8] -> [Word8])
  update f g = let new' = f new in bool id (g new') (new' /= f old)

  flag  f n = update f $ bool (20 + n:) (n:)
  color f n = update (reduceColor t . f) $ sgrColorArgs n

sgrColorArgs :: Word8 -> Color -> ([Word8] -> [Word8])
sgrColorArgs n c = case c of
  (Color256 o) -> ([38 + n, 5, o]++)
  (RGB r g b)  -> ([38 + n, 2, r, g, b]++)
  Black        -> (90 + n:)
  Red          -> (91 + n:)
  Green        -> (92 + n:)
  Yellow       -> (93 + n:)
  Blue         -> (94 + n:)
  Magenta      -> (95 + n:)
  Cyan         -> (96 + n:)
  White        -> (97 + n:)
  DullBlack    -> (30 + n:)
  DullRed      -> (31 + n:)
  DullGreen    -> (32 + n:)
  DullYellow   -> (33 + n:)
  DullBlue     -> (34 + n:)
  DullMagenta  -> (35 + n:)
  DullCyan     -> (36 + n:)
  DullWhite    -> (37 + n:)
  DefaultColor -> (39 + n:)
