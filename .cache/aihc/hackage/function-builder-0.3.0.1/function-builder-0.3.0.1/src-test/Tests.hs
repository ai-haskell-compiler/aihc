{-# LANGUAGE UndecidableInstances #-}

module Main where


import           Data.FunctionBuilder


data Hello = MkHello String

instance HasFunctionBuilder String Hello where
  type ToFunction String Hello next = next
  toFunctionBuilder (MkHello !s) = immediate s

data HelloP = MkHelloP

instance DynamicContent String HelloP String where
  addParameter _ = deferred ("HelloP: " ++)

instance HasFunctionBuilder String HelloP where
  type ToFunction String HelloP next = String -> next
  toFunctionBuilder = addParameter

data SetStringParam a = MkSetter a String

instance DynamicContent String a String => HasFunctionBuilder String (SetStringParam a) where
  type ToFunction String (SetStringParam a) r = r
  toFunctionBuilder (MkSetter f str) = fillParameter (addParameter f) str



data AssignParameter w a b where
   MkAssignParameter ::DynamicContent w a b => a -> b -> AssignParameter w a b

instance StaticContent w (AssignParameter w a b) where
  addStaticContent (MkAssignParameter !a !b) = fillParameter (addParameter a) b

instance HasFunctionBuilder w (AssignParameter w a b) where
  toFunctionBuilder = addStaticContent


type FBStr a b = FunctionBuilder String a b

main :: IO ()
main =
  if toFunction (toFunctionBuilder (MkHello "test") :: FBStr String String)
     == ("test" :: String)
     && toFunction (addParameter MkHelloP :: FBStr String (String -> String))
                   "test"
     == "HelloP: test"
     && toFunction
          (toFunctionBuilder (MkSetter MkHelloP "test") :: FBStr String String)
     == "HelloP: test"
  then
    return ()
  else
    error "Tests failed."
