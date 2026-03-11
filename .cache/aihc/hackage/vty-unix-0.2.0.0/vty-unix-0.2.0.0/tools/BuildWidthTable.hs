{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
module Main where

import System.Console.ANSI (getCursorPosition)
import Text.Printf (printf)

import Graphics.Vty.UnicodeWidthTable.Main (defaultMain)

charWidth :: Char -> IO Int
charWidth c = do
    printf "\r"
    putChar c
    Just (_, col) <- getCursorPosition
    return col

main :: IO ()
main = defaultMain charWidth
