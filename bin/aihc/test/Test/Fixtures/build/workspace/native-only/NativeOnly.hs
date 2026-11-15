module NativeOnly (nativeAnswer) where

foreign import ccall unsafe "native_answer" nativeAnswer :: Int
