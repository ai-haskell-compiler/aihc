{- |
Poor man's Template Haskell:
Generate RULES for handling of primitive number types.
-}
module Main where

import Data.Maybe (fromMaybe, )

import Prelude hiding (fromIntegral, )


pad :: Int -> String -> String
pad n str =
   zipWith fromMaybe
      (replicate n ' ')
      (map Just str ++ repeat Nothing)


machineIntegerTypes :: [String]
machineIntegerTypes =
   do typeSign <- "Int" : "Word" : []
      typeSize <- "" : "8" : "16" : "32" : "64" : []
      return $ typeSign ++ typeSize

functionSignature :: String -> String -> String -> String
functionSignature functionName sourceType targetType =
   functionName ++ " :: " ++ sourceType ++ " -> " ++ targetType

{-
Simply replace NumericPrelude.roundFunc by Prelude98.roundFunc.
This is only sensible where Prelude functions are optimized.
Unfortunately there seems to be no optimization for target type Int8 et.al.
-}
realField :: [String]
realField =
   do sourceType <- "Float" : "Double" : []
      targetType <- machineIntegerTypes
      method <- "round" : "truncate" : "floor" : "ceiling" : []
      let methodPad = pad 8 method
      let signature = functionSignature methodPad sourceType targetType
      return $ "     " ++
         pad 40 ("\"NP." ++ signature ++ "\"") ++
         methodPad ++ " = P." ++ signature ++ ";"

realFieldIndirect :: [String]
realFieldIndirect =
   do targetType <- tail machineIntegerTypes
      method <- "round" : "roundSimple" : "truncate" : "floor" : "ceiling" : []
      let methodPad = pad 11 method
      let signature = functionSignature methodPad "a" targetType
      return $ "     " ++
         pad 33 ("\"NP." ++ signature ++ "\"") ++
         methodPad ++ " = (" ++ functionSignature "P.fromIntegral" "Int" targetType ++ ") . "
             ++ method ++ ";"

splitFractionIndirect :: [String]
splitFractionIndirect =
   do targetType <- tail machineIntegerTypes
      method <- "splitFraction" : []
      let methodPad = pad 13 method
      let signature = functionSignature methodPad "a" ("("++targetType++",a)")
      return $ "     " ++
         pad 40 ("\"NP." ++ signature ++ "\"") ++
         methodPad ++ " = mapFst (" ++ functionSignature "P.fromIntegral" "Int" targetType ++ ") . "
             ++ method ++ ";"


fromIntegral :: [String]
fromIntegral =
   do sourceType <- "Integer" : machineIntegerTypes
      targetType <- "Int" : "Integer" : "Float" : "Double" : []
      let function = "fromIntegral"
      let signature = functionSignature function sourceType targetType
      return $ "     " ++
         pad 40 ("\"NP." ++ signature ++ "\"") ++
         function ++ " = P." ++ signature ++ ";"


main :: IO ()
main =
   putStrLn "module Algebra.RealRing" >>
   mapM_ putStrLn realFieldIndirect >>
   mapM_ putStrLn splitFractionIndirect >>

   putStrLn "module Algebra.ToInteger" >>
   mapM_ putStrLn fromIntegral
