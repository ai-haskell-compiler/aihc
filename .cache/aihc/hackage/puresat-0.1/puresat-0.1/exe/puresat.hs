module Main (main) where

import Control.Monad
import System.Environment

import qualified PureSAT.Main as PureSAT
import PureSAT.Base
import PureSAT.Prim
import qualified PureSAT.DIMACS

main :: IO ()
main = do
    args <- getArgs
    forM_ args $ \arg -> do
        print arg
        dimacs <- PureSAT.DIMACS.parseDimacsFile arg
        let solution = solvePureSAT dimacs
        if null solution
        then do
            putStrLn "p unsat"
        else do
            putStrLn "p sat"
            putStrLn $ unwords $ map show solution

solvePureSAT :: [[Int]] -> [Int]
solvePureSAT clauses = runST $ do
    s <- PureSAT.newSolver

    -- create literal
    literals <- newPrimArray maxLiteral
    forM_ [1..maxLiteral] $ \i -> do
        l <- PureSAT.newLit s
        writePrimArray literals (i - 1) l

    -- addClauses
    forM_ clauses $ \clause -> do
        clause' <- forM clause $ \i -> do
            l <- readPrimArray literals (abs i - 1)
            return $ if i < 0 then PureSAT.neg l else l

        PureSAT.addClause s clause'

    -- solve
    res <- PureSAT.solve s

    -- read back the solution
    if res
    then do
        forM [1..maxLiteral] $ \i -> do
            l <- readPrimArray literals (i - 1)
            x <- PureSAT.modelValue s l
            return $ if x then i else negate i

    else return []
  where
    maxLiteral = foldl' (\acc clause -> foldl' (\x y -> max x (abs y)) acc clause) 0 clauses
