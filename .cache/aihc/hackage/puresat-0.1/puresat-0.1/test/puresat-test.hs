{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad (forM, forM_)
import Data.Maybe (isJust, isNothing)
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck
import Data.Functor ((<&>))
import Control.Monad.SAT

-- https://fmv.jku.at/papers/BrummayerLonsingBiere-SAT10.pdf
-- Automated Testing and Debugging of SAT and QBF Solvers

main :: IO ()
main = defaultMain $ testGroup "dpll"
    [ testGroup "3Sat"
        [ testProperty "yes" $ forAll (threeSatGen 5) runCNFProblem
        ]
    , testGroup "23Sat"
        [ testProperty "yes" $ forAll (twoThreeSatGen 5) runCNFProblem
        ]
    , testGroup "Prop"
        [ testProperty "yes" $ forAll (propGen) runCNFProblemProp
        ]
    , testProperty "Pigeonhole" $ forAll pigeonholeGen runPigeonhole
    ]



-------------------------------------------------------------------------------
-- ProblemCNF
-------------------------------------------------------------------------------

-- ProblemCNF
data Problem = Problem
    { size     :: Int
    , solution :: [Bool]
    , clauses  :: [[Int]]
    }
  deriving Show

runCNFProblem :: Problem -> Bool
runCNFProblem problem = isJust $ runSATMaybe $ do
    lits <- forM (solution problem) $ \_polarity -> newLit
    forM_ (clauses problem) $ \c -> do
        c' <- forM c $ \l -> do
            let polarity = l >= 0
            let literal  = lits !! (abs l - 1)
            return $ if polarity then literal else neg literal

        addClause c'

    solve_

threeSatGen :: Int -> Gen Problem
threeSatGen r = do
    size <- clamp (10,20) <$> getSize
    solution <- forM [1..size] $ \_ -> arbitrary
    let polarity l = if solution !! (abs l - 1) then l else negate l
    clauses <- forM [1..size * r] $ \_ -> do
        l1 <- chooseInt (1, size)
        l2 <- chooseInt (1, size) `suchThat` \l -> l /= l1
        l3 <- chooseInt (1, size) `suchThat` \l -> l /= l1 && l /= l2
        shuffle $ map polarity [l1, negate l2, negate l3]
    return Problem {..}

twoThreeSatGen :: Int -> Gen Problem
twoThreeSatGen r = do
    size <- clamp (10,100) <$> getSize
    solution <- forM [1..size] $ \_ -> arbitrary
    let polarity l = if solution !! (abs l - 1) then l else negate l

    clauses2 <- forM [1..size] $ \_ -> do
        l1 <- chooseInt (1, size)
        l2 <- chooseInt (1, size) `suchThat` \l -> l /= l1
        shuffle $ map polarity [l1, negate l2]

    clauses3 <- forM [1..size * r] $ \_ -> do
        l1 <- chooseInt (1, size)
        l2 <- chooseInt (1, size) `suchThat` \l -> l /= l1
        l3 <- chooseInt (1, size) `suchThat` \l -> l /= l1 && l /= l2
        shuffle $ map polarity [l1, negate l2, negate l3]

    let clauses = clauses2 ++ clauses3
    return Problem {..}

-------------------------------------------------------------------------------
-- Pigeonhole Prob
-------------------------------------------------------------------------------

data Pigeonhole = Pigeonhole Int [[Int]]
  deriving Show

runPigeonhole :: Pigeonhole -> Bool
runPigeonhole (Pigeonhole n clauses) = isNothing $ runSATMaybe $ do
    lits <- forM [1..n] $ \_-> newLit
    forM_ clauses $ \c -> do
        c' <- forM c $ \l -> do
            let polarity = l >= 0
            let literal  = lits !! (abs l - 1)
            return $ if polarity then literal else neg literal

        addClause c'

    solve_

pigeonholeGen :: Gen Pigeonhole
pigeonholeGen = do
    n <- clamp (2,4) <$> getSize
    let size = n * n + n
    let p i j = 1 + j + i * n
    let somewhere = [ [ p i j | j <- [ 0 .. n - 1 ] ] | i <- [ 0 .. n ] ]
    let onlyone =
          [ [ negate (p i k), negate (p j k) ]
          | k <- [ 0 .. n - 1 ]
          , i <- [ 0 .. n ]
          , j <- [ i + 1 .. n ]
          ]
    let clauses = somewhere ++ onlyone

    return (Pigeonhole size clauses)

-------------------------------------------------------------------------------
-- ProblemProp
-------------------------------------------------------------------------------

data P
    = V Int
    | T
    | F
    | Neg P
    | P :\/: P
    | P :/\: P
    | P :==: P
  deriving Show

data ProblemProp = ProblemProp
    { psize     :: Int
    , psolution :: [Bool]
    , pprop     :: P
    }
  deriving Show

runCNFProblemProp :: ProblemProp -> Bool
runCNFProblemProp problem = isJust $ runSATMaybe $ do
    lits <- forM (psolution problem) $ \_polarity -> newLit
    addProp (go lits (pprop problem))
  where
    go :: [Lit s] -> P -> Prop s
    go lits (V l)      = do
        let polarity = l >= 0
        let literal  = lits !! (abs l - 1)
        lit (if polarity then literal else neg literal)
    go _    T          = true
    go _    F          = false
    go lits (Neg p)    = neg (go lits p)
    go lits (p :/\: q) = go lits p /\ go lits q
    go lits (p :\/: q) = go lits p \/ go lits q
    go lits (p :==: q) = go lits p <-> go lits q

propGen :: Gen ProblemProp
propGen = do
    psize <- clamp (10,100) <$> getSize
    tmp <- forM [1 .. psize] $ \l -> arbitrary >>= \p -> return (l, p)
    let (literals, psolution) = unzip tmp
    -- let polarity l = if psolution !! (abs l - 1) then l else negate l

    pprop <- genT (psize * 10) literals

    return ProblemProp {..}
  where
    genP :: Int -> [Int] -> Gen P
    genP n literals
        | n <= 0    = elements [T,F]
        | n <= 1    = V <$> elements (literals ++ map negate literals)
        | otherwise = oneof
            [ Neg <$> genP (n - 1) literals
            , do
                l <- chooseInt (1, n - 1)
                let r = n - l
                (:/\:) <$> genP l literals <*> genP r literals
            , do
                l <- chooseInt (1, n - 1)
                let r = n - l
                (:\/:) <$> genP l literals <*> genP r literals
            ]

    genT :: Int -> [Int] -> Gen P
    genT n literals
        | n <= 0    = return T
        | n <= 1    = V <$> elements literals
        | otherwise = oneof
            [ Neg <$> genF (n - 1) literals
            , genP (n - 1) literals <&> \p -> p :==: p
            , do
                l <- chooseInt (1, n - 1)
                let r = n - l
                (:/\:) <$> genT l literals <*> genT r literals
            , do
                l <- chooseInt (1, n - 1)
                let r = n - l
                (:\/:) <$> genP l literals <*> genT r literals
            , do
                l <- chooseInt (1, n - 1)
                let r = n - l
                (:==:) <$> genT l literals <*> genT r literals
            , do
                l <- chooseInt (1, n - 1)
                let r = n - l
                (:==:) <$> genT l literals <*> genT r literals
            ]

    genF :: Int -> [Int] -> Gen P
    genF n literals
        | n <= 0    = return F
        | n <= 1    = V . negate <$> elements literals
        | otherwise = oneof
            [ Neg <$> genT (n - 1) literals
            , do
                l <- chooseInt (1, n - 1)
                let r = n - l
                (:\/:) <$> genF l literals <*> genF r literals
            ]

-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------

clamp :: Ord a => (a,a) -> a -> a
clamp (l, u) x = max l (min u x)
