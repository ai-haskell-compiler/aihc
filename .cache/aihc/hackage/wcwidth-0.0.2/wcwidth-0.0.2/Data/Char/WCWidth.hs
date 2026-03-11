

{-# LANGUAGE ForeignFunctionInterface
  #-}


{-| A binding for the native 'wcwidth'. It's important that you 'setLocale'
    before using it, like this:

 >  #!/usr/bin/env runhaskell
 >
 >  import Text.Printf
 >  
 >  import System.Locale.SetLocale
 >  import Data.Char.WCWidth
 >
 >  main                     =  do
 >    setLocale LC_ALL (Just "")
 >    sequence_ [ display c | c <- chars ]
 >   where
 >    chars                  =  [minBound..'A']
 >    display c = printf "%04x  %2d  %s\n" (fromEnum c) (wcwidth c) (show c)

    The program file @WCWidthTableaux.hs@ contains a more extensive example of
    using 'wcwidth'.

    Note that this binding to the native implementation gets certain
    characters wrong in obvious ways as well as ways that are problematic for
    indentation based languages. The ASCII tab should be assigned a width of
    8, not -1; and one is likely to find -1 assigned to  numerous obscure
    characters (for example, symbols from the Book of Changes).

 -}


module Data.Char.WCWidth
  ( wcwidth
  , widths
  , ranges
  ) where

import Foreign.C
import Data.List




{-| Widths of all characters. 
 -}
widths                      ::  [ (Char, Int) ]
widths                       =  [ (c, wcwidth c) | c <- [minBound..maxBound] ] 


{-| Characters broken into contiguous ranges with the same width.
 -}
ranges                      ::  [ ((Char, Char), Int) ]
ranges                       =  reverse (foldl' aggregate start (tail widths))
 where
  start                      =  aggregate [] (head widths)
  aggregate [] (c, w)        =  [((c, c), w)]
  aggregate (((a, z), i) : t) (c, w)
    | i == w                 =  ((a, c), i) : t
    | otherwise              =  ((c, c), w) : ((a, z), i) : t


{-| Binding to the native 'wcwidth'. 
 -}
wcwidth                     ::  Char -> Int
wcwidth                      =  fromEnum . native . toEnum . fromEnum


foreign import ccall unsafe "wchar.h wcwidth" native :: CWchar -> CInt


