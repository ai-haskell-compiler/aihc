{-# LANGUAGE TemplateHaskell #-}
module Language.Elm.CoreSources where

import Data.FileEmbed
import Data.ByteString.Char8 (unpack)

{-|
JavaScript Header to append at the beginning of a complete, linked Elm program.
-}
header :: String
header = "var Elm = Elm || { Native: {} };\n"
  
{-|
Source for the Elm Runtime library
-}
runtime :: String
runtime =  unpack $(embedFile  "src/Native/Runtime.js")

{-|
Dictionary mapping strings of the form "Native.ModuleName" to the JS source code
for that stdlib native module.
-}
nativeSources ::  [(String, String)]
nativeSources = map (\(x,y) -> (x, unpack y)) $ [
        ("Native.Array", $(embedFile  "src/Native/Array.js"))
        , ("Native.Basics", $(embedFile  "src/Native/Basics.js"))
        , ("Native.Bitwise", $(embedFile  "src/Native/Bitwise.js"))
        , ("Native.Char", $(embedFile  "src/Native/Char.js"))
        , ("Native.Color", $(embedFile  "src/Native/Color.js"))
        , ("Native.Date", $(embedFile  "src/Native/Date.js"))
        , ("Native.Debug", $(embedFile  "src/Native/Debug.js"))
        , ("Native.Http", $(embedFile  "src/Native/Http.js"))
        , ("Native.Json", $(embedFile  "src/Native/Json.js"))
        , ("Native.Keyboard", $(embedFile  "src/Native/Keyboard.js"))
        , ("Native.List", $(embedFile  "src/Native/List.js"))
        , ("Native.Mouse", $(embedFile  "src/Native/Mouse.js"))
        , ("Native.Ports", $(embedFile  "src/Native/Ports.js"))
        , ("Native.Regex", $(embedFile  "src/Native/Regex.js"))
        , ("Native.Show", $(embedFile  "src/Native/Show.js"))
        , ("Native.Signal", $(embedFile  "src/Native/Signal.js"))
        , ("Native.String", $(embedFile  "src/Native/String.js"))
        , ("Native.Text", $(embedFile  "src/Native/Text.js"))
        , ("Native.Time", $(embedFile  "src/Native/Time.js"))
        , ("Native.Touch", $(embedFile  "src/Native/Touch.js"))
        , ("Native.Trampoline", $(embedFile  "src/Native/Trampoline.js"))
        , ("Native.Transform2D", $(embedFile  "src/Native/Transform2D.js"))
        , ("Native.Utils", $(embedFile  "src/Native/Utils.js"))
        , ("Native.WebSocket", $(embedFile  "src/Native/WebSocket.js"))
        , ("Native.Window", $(embedFile  "src/Native/Window.js"))
        
        , ("Native.Graphics.Collage", $(embedFile  "src/Native/Graphics/Collage.js"))
        , ("Native.Graphics.Element", $(embedFile  "src/Native/Graphics/Element.js"))
        , ("Native.Graphics.Input", $(embedFile  "src/Native/Graphics/Input.js"))


        ]

{-|
List of .elm sources for the Elm Standard Library
-}        
stdlibSources :: [String]
stdlibSources = map unpack [$(embedFile  "src/Array.elm")
   ,$(embedFile  "src/Array.elm")
   ,$(embedFile  "src/Basics.elm")
   ,$(embedFile  "src/Bitwise.elm")
   ,$(embedFile  "src/Char.elm")
   ,$(embedFile  "src/Color.elm")
   ,$(embedFile  "src/Date.elm")
   ,$(embedFile  "src/Debug.elm")
   ,$(embedFile  "src/Dict.elm")
   ,$(embedFile  "src/Http.elm")
   ,$(embedFile  "src/Keyboard.elm")
   ,$(embedFile  "src/List.elm")
   ,$(embedFile  "src/Maybe.elm")
   ,$(embedFile  "src/Mouse.elm")
   ,$(embedFile  "src/Random.elm")
   ,$(embedFile  "src/Regex.elm")
   ,$(embedFile  "src/Result.elm")
   ,$(embedFile  "src/Set.elm")
   ,$(embedFile  "src/Signal.elm")
   ,$(embedFile  "src/String.elm")
   ,$(embedFile  "src/Text.elm")
   ,$(embedFile  "src/Time.elm")
   ,$(embedFile  "src/Touch.elm")
   ,$(embedFile  "src/Trampoline.elm")
   ,$(embedFile  "src/Transform2D.elm")
   ,$(embedFile  "src/WebSocket.elm")
   ,$(embedFile  "src/Window.elm")
   
   ,$(embedFile  "src/Json/Encode.elm")
   ,$(embedFile  "src/Json/Decode.elm")
   
   ,$(embedFile  "src/Graphics/Collage.elm")
   ,$(embedFile  "src/Graphics/Element.elm")
   ,$(embedFile  "src/Graphics/Input.elm")
   
   ,$(embedFile  "src/Graphics/Input/Field.elm")
   ]
