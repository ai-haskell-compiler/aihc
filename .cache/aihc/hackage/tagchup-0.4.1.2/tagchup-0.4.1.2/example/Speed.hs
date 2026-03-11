module Main where

import qualified Text.HTML.Tagchup.Tag    as Tag
import qualified Text.HTML.Tagchup.Parser as Parser
import qualified Text.HTML.Tagchup.Parser.Stream as Str
import qualified Text.XML.Basic.Name.MixedCase as Name

import System.Time (getClockTime, diffClockTimes, tdSec, tdPicosec, )

import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BL


measureTime :: String -> IO () -> IO ()
measureTime name act =
   do putStr (name++": ")
      timeA <- getClockTime
      act
      timeB <- getClockTime
      let td = diffClockTimes timeB timeA
      print (fromIntegral (tdSec td) +
             fromInteger (tdPicosec td) * 1e-12 :: Double)

number :: Int
number = 100000

main :: IO ()
main =
   do measureTime "String" $ print $
         (last $ Parser.runSoup $ concat $ replicate number "<li>item</li>" :: Tag.T Name.T String)
      measureTime "ByteString.Strict" $ print $
         (last $ Parser.runSoup $ BS.concat $ replicate number $ BS.pack "<li>item</li>" :: Tag.T Name.T String)
      measureTime "ByteString.Lazy" $ print $
         (last $ Parser.runSoup $ BL.concat $ replicate number $ BL.pack "<li>item</li>" :: Tag.T Name.T String)

      measureTime "Pointer.Strict" $ print $
         (last $ Parser.runSoup $ Str.pointerFromByteStringStrict $ BS.concat $ replicate number $ BS.pack "<li>item</li>" :: Tag.T Name.T String)
      measureTime "Pointer.Lazy" $ print $
         (last $ Parser.runSoup $ Str.pointerFromByteStringLazy $ BL.concat $ replicate number $ BL.pack "<li>item</li>" :: Tag.T Name.T String)

{-
Results without inlining on euler:

String: Close "li"
2.182197
ByteString.Strict: Close "li"
2.704024
ByteString.Lazy: Close "li"
3.203038

These figures remain, when Stream methods are inlined.


Results without inlining on anubis:
String: Close "li"
3.507383
ByteString.Strict: Close "li"
4.406978
ByteString.Lazy: Close "li"
5.266979
Pointer.Strict: Close "li"
3.37898
Pointer.Lazy: Close "li"
3.185808


The figures become worse, when MTL and Combinator functions are inlined, too:
String: Close "li"
3.56803
ByteString.String: Close "li"
4.314474
ByteString.Lazy: Close "li"
4.855984

With the main parser functions inlined we get:
tring: Close "li"
2.951519
ByteString.String: Close "li"
2.910444
ByteString.Lazy: Close "li"
3.046311
-}
