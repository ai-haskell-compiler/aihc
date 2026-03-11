-- |
-- Module      :  DobutokO.Sound.Functional.Elements
-- Copyright   :  (c) OleksandrZhabenko 2020, 2024
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
--
-- Helps to create experimental music from a file (or its part) and a Ukrainian text. 
-- It can also generate a timbre for the notes. Uses SoX inside.

{-# OPTIONS_GHC -threaded #-}

module DobutokO.Sound.Functional.Elements (
  -- * Functions to edit OvertonesO and function f
  renormF
  , renormFD
  , sameOvertone
  , sameOvertoneL  
  , sameFreqF
  , sameFreqFI
  , fAddFElem
  , fRemoveFElem
  , fChangeFElem
  , gAdd01
  , gAdd02
  , gAdd03
  , gAdd04
  , gRem01
  , gRem02
  , gRem03
  -- ** Working with two OvertonesO
  , fAddFElems
  , fRemoveFElems
  , fChangeFElems
  , freqsOverlapOvers
  , elemsOverlapOvers
  , gAdds01
  , gAdds02
) where

import Data.List (sort,sortBy)
import qualified Data.Vector as V
import DobutokO.Sound.Functional.Basics

-- | Renormalizes amplitudes for the frequencies so that the maximum one of them (if 'OvertonesO' is not 'V.empty') is equal by the absolute value
-- to 1.0 and the mutual ratios of the amplitudes are preserved.
renormF :: OvertonesO -> OvertonesO
renormF v
 | V.null v = V.empty
 | otherwise =
    let v1 = V.fromList . sortBy (\(_,y1) (_,y2)-> compare (abs y2) (abs y1)) . V.toList $ v in
      if (\(_,y) -> y == 0.0) . V.unsafeIndex v1 $ 0 then V.empty
      else V.map (\(x,y) -> (x, y / (snd . V.unsafeIndex v1 $ 0))) v1

-- | Renormalizes amplitudes for the frequencies so that the maximum one of them (if 'OvertonesO' is not 'V.empty') is equal by the absolute value
-- to 'Float' argument and the mutual ratios of the amplitudes are preserved.
renormFD :: Float -> OvertonesO -> OvertonesO
renormFD ampl0 v
 | V.null v = V.empty
 | otherwise =
    let v1 = V.fromList . sortBy (\(_,y1) (_,y2)-> compare (abs y2) (abs y1)) . V.toList $ v in
      if (\(_,y) -> y == 0.0) . V.unsafeIndex v1 $ 0 then V.empty
      else V.map (\(x,y) -> (x, ampl0 * y / (snd . V.unsafeIndex v1 $ 0))) v1

-- | Predicate to check whether all tuples in a 'V.Vector' have the same first element.
sameOvertone :: OvertonesO -> Bool
sameOvertone v
 | V.null v = False
 | otherwise = V.all (\(x,_) -> x == (fst . V.unsafeIndex v $ 0)) v

-- | Similar to 'sameOvertone', except that not the 'V.Vector' is checked but a corresponding list.
sameOvertoneL :: [(Float,Float)] -> Bool
sameOvertoneL xs@((x,_):_) = all (\(xn,_) -> xn == x) xs
sameOvertoneL _ = False

-- | @g :: (Float,Float) -> OvertonesO -> OvertonesO@ is a function that defines how the new element is added to the 'OvertonesO'. It depends
-- only on the element being added and the actual 'OvertonesO'. It does not depend on the 'Float' argument for @f :: Float -> OvertonesO@
-- so for different 'Float' for @f@ it gives the same result.
sameFreqF :: Float -> (Float,Float) -> (Float -> OvertonesO) -> ((Float,Float) -> OvertonesO -> OvertonesO) -> OvertonesO
sameFreqF freq (noteN0,amplN0) f g = g (noteN0,amplN0) (f freq)

-- | @g :: (Float,Float) -> OvertonesO -> OvertonesO@ is a function that defines how the new element is added to the 'OvertonesO'.
-- Variant of 'sameFreqF' where g depends only on the elements of the 'OvertonesO', which first elements in the tuples equal to the first element
-- in the @(Float,Float)@. It does not depend on the 'Float' argument for @f :: Float -> OvertonesO@
-- so for different 'Float' for @f@ it gives the same result.
sameFreqFI :: Float -> (Float,Float) -> (Float -> OvertonesO) -> ((Float,Float) -> OvertonesO -> OvertonesO) -> OvertonesO
sameFreqFI freq (noteN0,amplN0) f g = g (noteN0,amplN0) . V.filter (\(x,_) -> x == noteN0) $ f freq

-- | @gAdd :: (Float,Float) -> Float -> (Float -> OvertonesO) -> OvertonesO@ is a function that defines how the element is added
-- to the 'OvertonesO'. 'fAddFElem' is 
-- actually a higher-order function, it changes the function @f@ and returns a new one. It can be an interesting task 
-- (in general) to look at such a function through a prism of notion of operator (mathematical, for example similar to that ones that 
-- are used for quantum mechanics and quantum field theory). 
-- @gAdd@ allows not only to insert an element if missing, but to change all the 'OvertonesO' system. So depending on the complexity,
-- it can produce rather complex behaviour.
fAddFElem :: (Float, Float) -> (Float -> OvertonesO) -> ((Float,Float) -> Float -> (Float -> OvertonesO) -> OvertonesO) ->
  (Float -> OvertonesO)
fAddFElem (noteN, amplN) f gAdd t = gAdd (noteN, amplN) t f

-- | @gRem:: (Float,Float) -> Float -> (Float -> OvertonesO) -> OvertonesO@ is a function that defines how the element is removed
-- from the 'OvertonesO'. 'fRemoveFElem' is
-- actually a higher-order function, it changes the function @f@ and returns a new one. It can be an interesting task 
-- (in general) to look at such a function through a prism of notion of operator (mathematical, for example that ones that are used 
-- for quantum mechanics and quantum field theory). 
-- @gRem@ allows not only to delete an element if existing, but to change all the 'OvertonesO' system. So depending on the complexity,
-- it can produce rather complex behaviour.
fRemoveFElem :: (Float, Float) -> (Float -> OvertonesO) -> ((Float,Float) -> Float -> (Float -> OvertonesO) -> OvertonesO) -> 
  (Float -> OvertonesO)
fRemoveFElem (noteN, amplN) f gRem t = gRem (noteN, amplN) t f

-- | Changes elements of the 'OvertonesO' using two functions. It is a generalization of the 'fAddFElem' and 'fRemoveFElem' functions. For example, if the first 
-- of the two inner functional arguments acts as 'gAdd01' or similar, then it adds element to the 'OvertonesO', if it acts as 'gRem01', then it removes the element. 
-- Its behaviour is defined by the 'Float' parameter (meaning frequency, probably), so you can change elements depending on what point it is applied.
fChangeFElem :: (Float, Float) -> Float -> (Float -> ((Float,Float) -> Float -> (Float -> OvertonesO) -> OvertonesO)) -> (Float -> OvertonesO) -> 
  (Float -> OvertonesO)
fChangeFElem (noteN, amplN) freq h f t = (h freq) (noteN, amplN) t f

-- | Example of the function gAdd for the 'fAddFElem'. If the frequency is already in the 'OvertonesO' then the corresponding amplitude is divided
-- equally between all the elements with the repeated given frequency from @(Float, Float)@. Otherwise, it is just concatenated to the 'OvertonesO'.
gAdd01 :: (Float,Float) -> Float -> (Float -> OvertonesO) -> OvertonesO
gAdd01 (note,ampl) freq f 
 | V.null . f $ freq = V.singleton (note,ampl)
 | otherwise =
    let v1 = renormF . f $ freq in
     let v2 = V.findIndices (\(x,_) -> x == note) v1 in
       if V.null v2 then V.cons (note,ampl) (f freq)
       else renormF . V.imap (\i (t,w) -> if i `V.elem` v2 then (t,w + ampl / fromIntegral (V.length v2)) else (t,w)) $ v1

-- | Can be used to produce an example of the function @gAdd@ for the 'fAddFElem'. Similar to 'gAdd01', but uses its first argument
-- to renorm the result of the 'gAdd01' so that its maximum by absolute value amplitude equals to the first argument.
gAdd02 :: Float -> (Float,Float) -> Float -> (Float -> OvertonesO) -> OvertonesO
gAdd02 amplMax (note,ampl) freq = renormFD amplMax . gAdd01 (note,ampl) freq

-- | Example of the function @gAdd@. for the 'fAddFElem'. If the frequency is not already in the 'OvertonesO' then the corresponding element is added and
-- the 'OvertonesO' are renormed with 'renormF'. Otherwise, the element is tried to be inserted with a new frequency between the greatest by an absolute
-- values notes as an intermediate value with the respective amplitude, or if there is only one element, to produce two elements in
-- the resulting 'V.Vector' with two consequent harmonics.
gAdd03 :: (Float,Float) -> Float -> (Float -> OvertonesO) -> OvertonesO
gAdd03 (note,ampl) freq f 
 | V.null . f $ freq = V.singleton (note,ampl)
 | otherwise =
    let v1 = renormF . f $ freq in
     let v2 = V.findIndices (\(x,_) -> x == note) v1 in
       if V.null v2 then renormF . V.cons (note,ampl) $ f freq
       else
        let xs = sortBy (\(x1,_) (x2,_)-> compare (abs x2) (abs x1)) . V.toList $ v1
            l = V.length v1
            ys = if compare l 1 == GT then ((fst . head $ xs) + (fst . head . tail $ xs) / 2,ampl):xs
                 else [(note,((snd . V.unsafeIndex v1 $ 0) + ampl) / 2),(2 * note,(abs ((snd . V.unsafeIndex v1 $ 0) - ampl)) / 2)] in
                   renormF . V.fromList $ ys

-- | Example of the function gRem for the 'fRemoveFElem'. If the element is already in the 'OvertonesO' then it is removed (if there are more than 5
-- elements already) and 'OvertonesO' are renormalized. Otherwise, all the same for the element already existing elements become less in an amlitude
-- for a numbers that in sum equal to amplitude of the removed element.
gRem01 :: (Float,Float) -> Float -> (Float -> OvertonesO) -> OvertonesO
gRem01 (note,ampl) freq f
  | V.null . f $ freq = V.empty
  | otherwise =
     let v1 = renormF . f $ freq in
     let v2 = V.findIndices (\(x,y) -> x == note && y == ampl) v1 in
       if V.null v2 then
       if compare (V.length v1) 5 == GT then renormF . V.unsafeSlice 0 (V.length v1 - 1) $ v1
       else v1
       else renormF . V.imap (\i (t,w) -> if i `V.elem` v2 then (t,w - ampl / fromIntegral (V.length v2)) else (t,w)) $ v1

-- | Can be used to produce an example of the function @gRem@ for the 'fRemoveFElem'. Similar to 'gRem01', but uses its first argument
-- to renorm the result of the 'gRem01' so that its maximum by absolute value amplitude equals to the first argument.
gRem02 :: Float -> (Float,Float) -> Float -> (Float -> OvertonesO) -> OvertonesO
gRem02 amplMax (note,ampl) freq = renormFD amplMax . gAdd01 (note,ampl) freq

-- | Similar to 'fAddFElem', but instead of one element @(Float,Float)@ it deals with a 'V.Vector' of such elements that is 'OvertonesO'. 
fAddFElems :: OvertonesO -> (Float -> OvertonesO) -> (OvertonesO -> Float -> (Float -> OvertonesO) -> OvertonesO) ->
  (Float -> OvertonesO)
fAddFElems v f gAdds t = gAdds v t f

-- | Similar to 'fRemoveFElem', but instead of one element @(Float,Float)@ it deals with a 'V.Vector' of such elements that is 'OvertonesO'. 
fRemoveFElems :: OvertonesO -> (Float -> OvertonesO) -> (OvertonesO -> Float -> (Float -> OvertonesO) -> OvertonesO) -> 
  (Float -> OvertonesO)
fRemoveFElems v f gRems t = gRems v t f

-- | Similar to 'fChangeFElem', but use another form of the changing function, so it can deal with not only single element of the 'OvertonesO', 
-- but also with several ones.
fChangeFElems :: OvertonesO -> Float -> (Float -> (OvertonesO -> Float -> (Float -> OvertonesO) -> OvertonesO)) -> (Float -> OvertonesO) -> 
  (Float -> OvertonesO)
fChangeFElems v freq h f t = (h freq) v t f

-- | Binary predicate to check whether two given 'OvertonesO' both have the elements with the same first element in the tuples. If 'True' then
-- this means that 'OvertonesO' are at least partially overlaped by the first elements in the tuples (meaning frequencies). 
freqsOverlapOvers :: OvertonesO -> OvertonesO -> Bool
freqsOverlapOvers v1 v2 =
  let [v11,v21] = map (V.map fst) [v1,v2]
      v22 = V.filter (<= V.maximum v11) v21 in
        if V.null v22 then False
        else
          let v12 = V.filter (>= V.minimum v21) v11
              [v13,v23] = map (V.uniq . V.fromList . sort . V.toList) [v12,v22]
              [l1,l2]  = map V.length [v13,v23] in compare (V.length . V.uniq . V.fromList . sort . V.toList . V.concat $ [v13,v23]) (l1 + l2) == LT

-- | Similar to 'freqsOverlapOvers', but checks whether the whole tuples are the same instead of the first elements in the tuples are the same.
elemsOverlapOvers :: OvertonesO -> OvertonesO -> Bool
elemsOverlapOvers v1 v2 =
  let v22 = V.filter (\(x,_) -> x <= fst (V.maximumBy (\(x1,_) (t,_) -> compare x1 t) v1)) v2 in
        if V.null v22 then False
        else
          let v12 = V.filter (\(x,_) -> x >= fst (V.minimumBy (\(x1,_) (t,_) -> compare x1 t) v2)) v1
              [v13,v23] = map (V.uniq . V.fromList . sort . V.toList) [v12,v22]
              [l1,l2]  = map V.length [v13,v23] in compare (V.length . V.uniq . V.fromList . sort . V.toList . V.concat $ [v13,v23]) (l1 + l2) == LT

-- | Example of the function @gAdds@ for the 'fAddFElems'. 
gAdds01 :: OvertonesO -> Float -> (Float -> OvertonesO) -> OvertonesO
gAdds01 v0 freq f 
 | V.null . f $ freq = v0
 | freqsOverlapOvers v0 (f freq) =
     let ys = sortBy (\(x1,_) (x2,_) -> compare x1 x2) . V.toList $ v0
         h ys
          | null ys = []
          | otherwise = (takeWhile (not . (/= head ys)) ys):h (dropWhile (not . (/= head ys)) ys)
         h1 = map (\zs -> (sum . map snd $ zs) / fromIntegral (length zs)) . h
         h2 ys = map (fst . head) (h ys)
         v2   = V.fromList . zip (h2 ys) $ (h1 ys)
         us = sortBy (\(x1,_) (x2,_) -> compare x1 x2) . V.toList $ f freq
         v3   = V.fromList . zip (h2 us) $ (h1 us) in renormF . V.concat $ [v2,v3]
 | otherwise = renormF . V.concat $ [v0, f freq]

-- | Can be used to produce an example of the function @gAdds@ for the 'fAddFElems'. Similar to 'gAdds01', but uses its first argument
-- to renorm the result of the 'gAdds01' so that its maximum by absolute value amplitude equals to the first argument.
gAdds02 :: Float -> OvertonesO -> Float -> (Float -> OvertonesO) -> OvertonesO
gAdds02 amplMax v0 freq = renormFD amplMax . gAdds01 v0 freq

-- | Example of the function @gAdd@. for the 'fAddFElem'. It tries to insert the given ('Float','Float') into the less dense frequency region.
gAdd04 :: (Float,Float) -> Float -> (Float -> OvertonesO) -> OvertonesO
gAdd04 (note,ampl) freq f 
 | V.null . f $ freq = V.singleton (note,ampl)
 | otherwise =
    let v1 = V.fromList . sortBy (\(x1,_) (x2,_) -> compare x1 x2) . V.toList . f $ freq
        v2 = V.zipWith (\(x1,_) (x2,_) -> x2 - x1) v1 (V.unsafeSlice 1 (V.length v1 - 1) v1)
        idxMax = V.maxIndex v2
        newFreq = (fst (V.unsafeIndex v1 (idxMax + 1)) + fst (V.unsafeIndex v1 idxMax)) / 2 in (newFreq,ampl) `V.cons` v1

-- | Example of the function @gRem@ for the 'fRemFElem'. It tries not to remove elements from the less than 6 elements 'OvertonesO' and to remove
-- all the elements in the given range with the width of the twice as many as the second 'Float' in the first argument tuple and the centre
-- in the first 'Float' in the tuple. Similar to somewhat bandreject filter but with more complex behaviour for the sound to be more complex.
gRem03 :: (Float,Float) -> Float -> (Float -> OvertonesO) -> OvertonesO
gRem03 (note,halfwidth) freq f =
 let v1 = V.filter (\(x,_) -> compare (abs (x - note)) halfwidth /= GT) . f $ freq in
   if compare (V.length v1) 5 /= GT then renormF . V.generate 5 $ (\i -> (fromIntegral (i + 1) * note, halfwidth / fromIntegral (i + 3))) 
   else v1
     
