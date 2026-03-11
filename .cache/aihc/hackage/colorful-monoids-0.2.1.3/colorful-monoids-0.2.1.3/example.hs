{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid.Colorful
import Data.Foldable
import Text.Printf

ansiColors :: [Color]
ansiColors = [ DefaultColor
             , Black
             , Red
             , Green
             , Yellow
             , Blue
             , Magenta
             , Cyan
             , White
             , DullBlack
             , DullRed
             , DullGreen
             , DullYellow
             , DullBlue
             , DullMagenta
             , DullCyan
             , DullWhite
             ]

ansiColorsExample :: IO ()
ansiColorsExample = do
  term <- getTerm
  printColoredS term $ Style Underline $ Style Bold "ANSI Example\n"
  for_ ansiColors $ \c -> do
    printColoredIO term $ Bg c (Value $ printf "%-15s" $ show c)
      <> Fg c (Value $ printf "%-15s" $ show c)
      <> Bg c (Style Invert $ Value $ printf " %-15s" $ show c)
      <> Fg c (Style Invert $ Value $ printf " %-15s" $ show c)
    printColoredS term $ Bg c (Value $ printf "%-15s" $ show c)
      <> Fg c (Value $ printf "%-15s" $ show c)
      <> Bg c (Style Invert $ Value $ printf " %-15s" $ show c)
      <> Fg c (Style Invert $ Value $ printf " %-15s" $ show c)
    putChar '\n'

colors256Example :: IO ()
colors256Example = do
  term <- getTerm
  printColoredS term $ Style Underline $ Style Bold "Color256 Example\n"
  for_ [0..255] $ \c -> do
    printColoredS term $ Bg (Color256 c) (Value $ printf "%02x" c)
      <> Fg (Color256 c) (Value $ printf " %02x" c)
      <> Bg (Color256 c) (Style Invert $ Value $ printf " %02x" c)
      <> Fg (Color256 c) (Style Invert $ Value $ printf " %02x" c)
    putChar '\n'

rgbExample :: IO ()
rgbExample = do
  term <- getTerm
  printColoredS term $ Style Underline $ Style Bold "RGB Example\n"
  for_ [0,64..255] $ \r ->
    for_ [0,64..255] $ \g ->
      for_ [0,64..255] $ \b -> do
        let c = RGB r g b
        printColoredS term $ Bg c (Value $ printf "%-20s" $ show c)
          <> Fg c (Value $ printf " %-20s" $ show c)
          <> Bg c (Style Invert $ Value $ printf " %-20s" $ show c)
          <> Fg c (Style Invert $ Value $ printf " %-20s" $ show c)
        putChar '\n'

specialExample :: IO ()
specialExample = do
  term <- getTerm
  printColoredS term $ Style Underline $ Style Bold "Special Example\n"
  for_ [Bold,Italic,Underline,Invert,Blink] $ \a -> do
    printColoredS term $
      Style a (Value (printf "%-20s" $ show a) <>
               Unstyle a (Value $ printf " %-20s" $ "Not" ++ show a) <>
               Value (printf "%-20s" $ show a))
    putChar '\n'

stackExample :: IO ()
stackExample = do
  term <- getTerm
  printColoredS term $ Style Underline (Style Bold "Stack Example\n") <> loop 0
  putChar '\n'
  where
    loop 8 = mempty
    loop n =
      Bg (Color256 n) $
        Value (replicate (fromIntegral n) ' ') <>
        loop (n + 1) <>
        Value (replicate (fromIntegral n) ' ')

basicExample :: IO ()
basicExample = do
  term <- getTerm
  printColoredS term $ Style Underline (Style Bold "Basic Example\n")
    <> Style Bold "Bold"
    <> Style Italic (Bg Red "Italic Red")
    <> Style Underline "Underline"
  putChar '\n'

reduceExample :: IO ()
reduceExample = do
  printColoredS Term8 $ Style Underline $ Style Bold "Reduction Example\n"
  for_ [0..255] $ \c -> do
    printColoredS Term256 $ Bg (Color256 c) $ Value $ printf "%02x" c
    printColoredS Term8   $ Bg (Color256 c) $ Value $ printf "%02x" c
    putChar '\n'
  for_ [0,64..255] $ \r ->
    for_ [0,64..255] $ \g ->
      for_ [0,64..255] $ \b -> do
        let c = RGB r g b
        printColoredS TermRGB $ Bg c $ Value $ printf "%20s" $ show c
        printColoredS Term256 $ Bg c $ Value $ printf "%20s" $ show c
        printColoredS Term8   $ Bg c $ Value $ printf "%20s" $ show c
        putChar '\n'

main :: IO ()
main = do
  putStrLn "\n"
  ansiColorsExample
  colors256Example
  rgbExample
  specialExample
  stackExample
  basicExample
  reduceExample
  putStrLn "\n"
