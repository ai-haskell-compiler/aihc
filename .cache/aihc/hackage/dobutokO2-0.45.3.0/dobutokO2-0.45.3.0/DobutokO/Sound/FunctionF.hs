-- |
-- Module      :  DobutokO.Sound.FunctionF
-- Copyright   :  (c) OleksandrZhabenko 2020, 2024
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
--
-- Helps to create experimental music from a file (or its part) and a Ukrainian text. 
-- It can also generate a timbre for the notes. Uses SoX inside.

{-# OPTIONS_GHC -threaded #-}

module DobutokO.Sound.FunctionF (
  -- * Working with OvertonesO and function f
  maybeFFromStrVec
  , fVecCoefs
  , showFFromStrVec
) where

import Text.Read (readMaybe)
import Data.Maybe (isNothing,fromJust,fromMaybe)
import Numeric
import qualified Data.Vector as V
import DobutokO.Sound.Functional.Basics

-- | Gets a function @f::Float -> OvertonesO@ that can be used further. Has two variants with usage of 'closestNote' ('Int' argument is greater than 0)v
--  and without it ('Int' argument is less than 0). For both cases 'String' must be in a form list of tuples of pairs of 'Float' to get somewhat
-- reasonable result. The function @f@ can be shown using a special printing function 'showFFromStrVec'. It is a simplest multiplicative (somewhat
-- acoustically and musically reasonable) form for the function that can provide such a result that fits into the given data.
--
-- > let (y,f1) = fromJust (maybeFFromStrVec 1 3583.9783 "[(25.368,0.2486356),(37.259,0.6464867),(486.153,0.374618646),(789.563,0.463486461)]") in (y,f1 3583.9783)
-- >
-- > (3520.0,[(25.829079975681818,0.2486356),(37.936206670369316,0.6464867),(494.9891484317899,0.374618646),(803.9138234326421,0.463486461)])
-- >
-- > let (y,f1) = fromJust (maybeFFromStrVec (-1) 3583.9783 "[(25.368,0.2486356),(37.259,0.6464867),(486.153,0.374618646),(789.563,0.463486461)]") in (y,f1 3583.9783)
-- > 
-- > (3583.9783,[(25.368,0.2486356),(37.259,0.6464867),(486.153,0.374618646),(789.563,0.463486461)])
-- 
maybeFFromStrVec :: Int -> Float -> String -> Maybe (Float,(Float -> V.Vector (Float,Float)))
maybeFFromStrVec n x ys
  | n == 0 || null ys = Nothing
  | n > 0 = 
     let y = closestNote (if x /= 0.0 then abs x else V.unsafeIndex notes 0)
         v = readMaybe ys::Maybe (V.Vector (Float,Float))
         v2 = fromMaybe V.empty v
         v3 = V.map (\(t,_) -> t / y) v2 in
           if V.null v3 then Nothing
           else Just (y,(\t1 -> V.imap (\i (_,ampl2) -> ((V.unsafeIndex v3 i) * t1,ampl2)) v2))
  | otherwise = 
     let y = (if x /= 0.0 then abs x else V.unsafeIndex notes 0)
         v = readMaybe ys::Maybe (V.Vector (Float,Float))
         v2 = fromMaybe V.empty v
         v3 = V.map (\(t,_) -> t / y) v2 in
           if V.null v3 then Nothing
           else Just (y,(\t1 -> V.imap (\i (_,ampl2) -> ((V.unsafeIndex v3 i) * t1,ampl2)) v2))

-- | Gets multiplication coefficients for @f::Float -> Vector (Float,Float)@ from the 'maybeFFromStrVec' with the same arguments.
fVecCoefs :: Int -> Float -> String -> V.Vector Float
fVecCoefs n x ys =
  let rs = maybeFFromStrVec n x ys in
    case rs of
      Nothing -> V.empty
      _       ->
        let (_,f1) = fromJust rs in
          V.map fst (f1 1)

-- | Experimental 'show' for @f::Float -> Vector (Float,Float)@ that is used only for visualisation. It is correct only with 'maybeFFromStrVec' or
-- equivalent function. Because the shape of the @f@ is known the function can be defined.
-- 
-- > showFFromStrVec (-1) 440 "[(25.358,0.3598),(489.35,0.4588962),(795.35,0.6853)]"
-- > 
-- > "(440.00,(\t -> <(0.05763181818181818 * t, 0.3598),(1.112159090909091 * t, 0.4588962),(1.8076136363636364 * t, 0.6853)>))"
-- 
showFFromStrVec :: Int -> Float -> String -> String
showFFromStrVec n x ys
 | isNothing . maybeFFromStrVec n x $ ys = ""
 | otherwise =
    let (y,f) = fromJust . maybeFFromStrVec n x $ ys
        l = length ("(" ++ (showFFloat Nothing y "") ++ ",(\t -> <(" ++ concat (V.toList . V.map (\z -> (showFFloat Nothing (fst z) $
              " * t, " ++ (showFFloat Nothing (snd z) "),("))) $ (f 1))) in take (l - 2) ("(" ++ (showFFloat Nothing y "") ++ ",(\t -> <("
                ++ concat (V.toList . V.map (\z -> (showFFloat Nothing (fst z) " * t, " ++ (showFFloat Nothing (snd z) "),("))) $ (f 1))) ++ ">))"
