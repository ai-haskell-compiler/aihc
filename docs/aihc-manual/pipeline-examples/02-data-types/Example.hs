module Example where

data Color = Red | Green | Blue

next :: Color -> Color
next Red = Green
next Green = Blue
next Blue = Red
