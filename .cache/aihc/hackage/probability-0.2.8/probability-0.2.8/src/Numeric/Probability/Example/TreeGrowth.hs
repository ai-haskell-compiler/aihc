module Numeric.Probability.Example.TreeGrowth where

import qualified Numeric.Probability.Distribution as Dist
import qualified Numeric.Probability.Transition as Trans
import qualified Numeric.Probability.Random as Rnd
import qualified Numeric.Probability.Trace as Trace
import Numeric.Probability.Simulation ((~..), (~*.), )
import Numeric.Probability.Percentage
    (Dist, Trans, RTrans, Expand, RExpand, Space, )

import Numeric.Probability.Visualize (
      Vis, Color(Green, Red, Blue), Plot,
      fig, figP, figure, title,
      xLabel, yLabel, plotD, color, label,
   )

import qualified Numeric.Probability.Monad as MonadExt


type Height = Int

data Tree = Alive Height | Hit Height | Fallen
	    deriving (Ord,Eq,Show)

grow :: Trans Tree
grow (Alive h) = Dist.normal (map Alive [h+1..h+5])
grow _ = error "TreeGrowth.grow: only alive trees can grow"

hit :: Trans Tree
hit (Alive h) = Dist.certainly (Hit h)
hit _ = error "TreeGrowth.hit: only alive trees can be hit"

fall :: Trans Tree
fall _ = Dist.certainly Fallen

evolve :: Trans Tree
evolve t =
   case t of
      (Alive _) -> Trans.unfold (Dist.enum [90,4,6] [grow,hit,fall]) t
--    (Alive _) -> Trans.unfold (Dist.relative [0.9,0.04,0.06] [grow,hit,fall]) t
      _         -> Dist.certainly t

{- |
tree growth simulation:
 start with seed and run for n generations
-}
seed :: Tree
seed = Alive 0


-- * exact results

-- | @tree n@ : tree distribution after n generations
tree :: Int -> Tree -> Dist Tree
tree n = MonadExt.iterate n evolve

-- | @hist n@ : history of tree distributions for n generations
hist :: Int -> Expand Tree
hist n = Trace.walk n (evolve =<<) . return


-- * simulation results

{- |
Since '(*.)' is overloaded for Trans and RChange,
we can run the simulation ~. directly to @n *. live@.
-}

--simTree k n = k ~. tree n
simTree :: Int -> Int -> RTrans Tree
simTree k n = (k,n) ~*. evolve

simHist :: Int -> Int -> RExpand Tree
simHist k n = (k,n) ~.. evolve

t2 :: Dist Tree
t2  = tree 2 seed

h2 :: Space Tree
h2  = hist 2 seed

sh2, st2 :: IO ()
st2 = Rnd.print $ simTree 2000 2 seed
sh2 = Rnd.print $ simHist 2000 2 seed


-- Alternatives:
--
-- simTree k n = k ~. n *. random evolve
-- simTree k n = (k,n) ~*. evolve


-- take a trace


height :: Tree -> Int
height Fallen = 0
height (Hit h) = h
height (Alive h) = h
{--
myPlot = plotD ((5 *. evolve) (Alive 0) >>= height)

myPlot2 = figP figure{title="Tree Growth",xLabel="Height (m)",
                yLabel="Probability"}
                (autoColor [
		plotD ((5 *. evolve) (Alive 0) >>= height)
		])

--}

p1, p2, p3, p4, p5, p6 :: Vis

p1 = fig [plotD $ Dist.normal ([1..20]::[Int])]

p2 = fig [plotD $ Dist.map height (tree 5 seed)]

p3 = figP figure{title="Tree Growth",
            xLabel="Height (ft)",
            yLabel="Probability"}
	    [plotD $ Dist.map height (tree 5 seed)]


p4 = figP figure{title="Tree Growth",
            xLabel="Height (ft)",
            yLabel="Probability"}
            [heightAtTime 5, heightAtTime 10,heightAtTime 15]

heightAtTime :: Int -> Plot
heightAtTime y = plotD $ Dist.map height (tree y seed)

p5 = figP figure{title="Tree Growth",
            xLabel="Height (ft)",
            yLabel="Probability"}
            (map heightAtTime [3,5,7])

heightCurve :: (Int,Color) -> Plot
heightCurve (n,c) = (heightAtTime n){color=c,label=show n++" Years"}

p6 = figP figure{title="Tree Growth",
            xLabel="Height (ft)",
            yLabel="Probability"}
            (map heightCurve
	    [(3,Blue),(5,Green),(7,Red)])


done :: Tree -> Bool
done (Alive x) = x >= 5
done _ = True

ev5 :: Tree -> Dist Tree
ev5 = MonadExt.while (not . done) evolve
