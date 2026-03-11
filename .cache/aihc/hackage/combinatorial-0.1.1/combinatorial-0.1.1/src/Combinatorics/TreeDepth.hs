module Combinatorics.TreeDepth (
   treeDepth,
   treeDepthSeq,
   nodeDepth,
   nodeDegreeProb,
   nodeDegreeExpect,
   ) where

{-
Date: Mon, 18 Apr 2005 18:00:22 +0200
From: Daniel Beer <daniel.beer@informatik.tu-chemnitz.de>
To: Hellseher <lemming@henning-thielemann.de>
Subject: Baum-Stochastik


Nimm folgenden Algorithmus, um einen zufälligen Baum mit n Knoten zu erzeugen:
Starte mit einem einzelnen Knoten (=Wurzel)
Schleife n-1 mal
   wähle beliebigen Knoten v1 aus Graph
   füge neuen Knoten v2 hinzu
   füge Kante (v1,v2) hinzu

So jetzt die Fragen:
a) Kann man den Erwartungswert für die Tiefe des Baums (also längster Pfad von Wurzel zu einem Blatt)
berechnen?
b) Kann man den Erwartungswert für die Anzahl der Blätter berechnen?
c) Erweiterung von (b). Kann man die zu erwartende Verteilung der Ausgangsgrade berechnen (so eine Art
Histogramm, das angibt wie oft welcher Ausgangsgrad erwartungsgemäß vorkommt)?

Natürlich alles in Abhängigkeit von n versteht sich.
-}

import qualified Polynomial as Poly
import qualified Data.Map   as Map
import Data.Ratio ((%), )

{- Instead of handling probabilities
   we make a complete case analysis and
   talk only about the absolute frequencies.
   That is we start with a one-node tree
   then create a new two-node tree from it.
   From (n-1)! n-node trees we create n! new (n+1)-node-trees.
   
   The expectation value of the depth of a node
   is the n-th harmonic number. -}

{-| @nodeDepth !! n !! k@ is the absolute frequency
    of nodes with depth k in trees with n nodes. -}
nodeDepth :: [[Integer]]
nodeDepth = scanl (flip nodeDepthIt) [1] [1 ..]

nodeDepthIt :: Integer -> [Integer] -> [Integer]
nodeDepthIt n = Poly.mul [n,1]

{-| @treeDepth !! n !! m !! k@ is the absolute frequency
    of nodes with depth k in trees with n nodes and depth m.
    This can't work - the function carries not enough information
    for recursive definition.
treeDepth :: [[[Integer]]]
treeDepth = iterate (\ls -> zipWith treeDepthIt ([[]]++ls) (ls++[[0]])) [[1]]

treeDepthIt :: [Integer] -> [Integer] -> [Integer]
treeDepthIt nm0 nm1 =
   foldl1 add [scale (if null nm0 then 0 else last nm0) (nm0 ++ [1]),
               scale (sum (init nm1)) nm1,
               0 : init nm1]
-}


{-|
  Trees are abstracted to lists of integers,
  where each integer denotes the number of nodes
  in the corresponding depth of the tree.
  The number associated with each tree
  is the frequency of this kind of tree
  on random tree generation.
-}
type TreeFreq = Map.Map [Integer] Integer

treeDepth :: [Rational]
treeDepth =
   zipWith (%)
      (map (sum . map (\(xs,c) -> fromIntegral (length xs) * c) . Map.toList)
           treePrototypes)
      (scanl (*) 1 [1 ..])

treeDepthSeq :: [[Integer]]
treeDepthSeq =
   let count = map snd . Map.toList . Map.fromListWith (+) .
          map (\(xs,c) -> (length xs, c)) . Map.toList
   in  map count treePrototypes

treePrototypes :: [TreeFreq]
treePrototypes =
   iterate treeDepthIt (Map.singleton [1] 1)

extendTree :: [Integer] -> [[Integer]]
extendTree tree =
   tail (snd (foldr
      (\x (xs,ys) -> (x:xs, ((x+1):xs) : map (x:) ys)) ([],[]) tree)) ++
      [tree ++ [1]]

treeDepthIt :: TreeFreq -> TreeFreq
treeDepthIt fm =
   Map.fromListWith (+)
      (concatMap (\(xs,c) -> zip (extendTree xs) (map (c*) xs))
                 (Map.toList fm))



{-| @nodeDegree !! n !! k@ is the number of nodes
    with outdegree k in a n-node tree. -}
nodeDegreeProb :: [[Rational]]
nodeDegreeProb = zipWith (\den -> map (%den)) (scanl1 (*) [1 ..]) nodeDegree

nodeDegree :: [[Integer]]
nodeDegree =
   scanl (flip (uncurry nodeDegreeIt)) [1]
      (zip [0 ..] (scanl1 (*) [1 ..]))

nodeDegreeIt :: Integer -> Integer -> [Integer] -> [Integer]
nodeDegreeIt n nFac = Poly.add [nFac] . Poly.mul [n,1]

{-| expected value of node degree -}
nodeDegreeExpect :: [Rational]
nodeDegreeExpect =
   zipWith (%) nodeDegreeExpectAux1 (scanl1 (*) [1 ..])

nodeDegreeExpectTrans :: Integer -> [Integer] -> [Integer]
nodeDegreeExpectTrans s x =
   scanl (\acc (n,c) -> c + n*acc) s
         (zip [1 ..] x)

nodeDegreeExpectAux0, nodeDegreeExpectAux1 :: [Integer]
nodeDegreeExpectAux0 = nodeDegreeExpectTrans 1 (scanl1 (*) [1 ..])
nodeDegreeExpectAux1 = nodeDegreeExpectTrans 0 nodeDegreeExpectAux0
