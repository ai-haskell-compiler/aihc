{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Problems from
-- https://www.cs.ubc.ca/~hoos/SATLIB/benchm.html
module Main where

import Control.Monad            (forM, forM_, unless)
import Control.Monad.ST         (runST)
import Data.Foldable            (foldl')
import Data.Primitive           (Prim)
import Data.Primitive.PrimArray (newPrimArray, readPrimArray, writePrimArray)
import System.FilePath          ((</>))
import Test.Tasty               (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit         (assertBool, testCaseSteps)

import qualified PureSAT.Main as PureSAT
import qualified PureSAT.DIMACS
import qualified MiniSat

main :: IO ()
main = defaultMain $ testGroup "dpll-dimacs"
    [ testGroup "aim"
      -- Artificially generated Random-3-SA
        [ testSAT "aim" "aim-100-1_6-no-1"
        -- more
        ]

    , testGroup "ais"
        -- All Intervall Series
        [ testSAT "ais" "ais6"
        , testSAT "ais" "ais8"
        , testSAT "ais" "ais10"
        , testSAT "ais" "ais12"
        ]

    , testGroup "bf"
      -- Circuit fault analysis: bridge fault
        [ testSAT "bf" "bf0432-007"
        , testSAT "bf" "bf1355-075"
        , testSAT "bf" "bf1355-638"
        , testSAT "bf" "bf2670-001"
        ]

    , testGroup "blocksworld"
        -- Planning
        [ testSAT "blocksworld" "anomaly"
        , testSAT "blocksworld" "medium"
        , testSAT "blocksworld" "bw_large.a"
        , testSAT "blocksworld" "bw_large.b"
        , testSAT "blocksworld" "bw_large.c"
        ]

    , testGroup "bmc"
        -- SAT-encoded bounded model checking intances 
        [ testSAT "bmc" "bmc-ibm-2"
        -- , testSAT "bmc" "bmc-ibm-1"
        -- , testSAT "bmc" "bmc-ibm-3"
        -- , testSAT "bmc" "bmc-ibm-4"
        -- , testSAT "bmc" "bmc-ibm-5"
        ]

    , testGroup "dubois"
        -- Randomly generated SAT instances
        [ testSAT "dubois" "dubois100"
        , testSAT "dubois" "dubois20"
        -- , testSAT "dubois" "dubois21"
        -- , testSAT "dubois" "dubois22"
        , testSAT "dubois" "dubois23"
        , testSAT "dubois" "dubois24"
        , testSAT "dubois" "dubois25"
        , testSAT "dubois" "dubois26"
        , testSAT "dubois" "dubois27"
        , testSAT "dubois" "dubois28"
        , testSAT "dubois" "dubois29"
        , testSAT "dubois" "dubois30"
        , testSAT "dubois" "dubois50"
        ]

    -- difficult
    -- , testGroup "hanoi"
    --     -- SAT-encoding of Towers of Hanoi 
    --     [ testSAT "hanoi" "hanoi4"
    --     , testSAT "hanoi" "hanoi5"
    --     ]

    , testGroup "inductive-inference"
        -- Instances from a problem in inductive inference 
        [ testSAT "inductive-inference" "ii8a1"
        , testSAT "inductive-inference" "ii8a2"
        , testSAT "inductive-inference" "ii8a3"
        , testSAT "inductive-inference" "ii8a4"
        , testSAT "inductive-inference" "ii8b1"
        , testSAT "inductive-inference" "ii8b2"
        , testSAT "inductive-inference" "ii8b3"
        , testSAT "inductive-inference" "ii8b4"
        , testSAT "inductive-inference" "ii8c1"
        , testSAT "inductive-inference" "ii8c2"
        , testSAT "inductive-inference" "ii8d1"
        , testSAT "inductive-inference" "ii8d2"
        , testSAT "inductive-inference" "ii8e1"
        , testSAT "inductive-inference" "ii8e2"

        -- , testSAT "inductive-inference" "ii16a1"
        -- , testSAT "inductive-inference" "ii16a2"
        -- , testSAT "inductive-inference" "ii16b1"
        -- , testSAT "inductive-inference" "ii16b2"
        -- , testSAT "inductive-inference" "ii16c1"
        -- , testSAT "inductive-inference" "ii16c2"
        -- , testSAT "inductive-inference" "ii16d1"
        -- , testSAT "inductive-inference" "ii16d2"
        -- , testSAT "inductive-inference" "ii16e1"
        -- , testSAT "inductive-inference" "ii16e2"
        ]

    , testGroup "logicstics"
        -- Planning
        [ testSAT "logistics" "logistics.a"
        , testSAT "logistics" "logistics.b"
        , testSAT "logistics" "logistics.c"
        , testSAT "logistics" "logistics.d"
        ]

    , testGroup "parity"
        -- Instances for problem in learning the parity function
        [ testSAT "parity" "par8-1"
        , testSAT "parity" "par8-1-c"
        , testSAT "parity" "par8-2"
        , testSAT "parity" "par8-2-c"
        , testSAT "parity" "par8-3"
        , testSAT "parity" "par8-3-c"
        , testSAT "parity" "par8-4"
        , testSAT "parity" "par8-4-c"
        , testSAT "parity" "par8-5"
        , testSAT "parity" "par8-5-c"
        -- , testSAT "parity" "par16-1"
        -- , testSAT "parity" "par16-1-c"
        ]

    , testGroup "phole"
        -- Pigeon hole problem
        [ testSAT "phole" "hole6"
        , testSAT "phole" "hole7"
        , testSAT "phole" "hole8"
        ]

    , testGroup "pret"
        -- Encoded 2-colouring forced to be unsatisfiable - 8 instances, all unsatisfiable
        [ testSAT "pret" "pret60_25"
        , testSAT "pret" "pret60_40"
        , testSAT "pret" "pret60_60"
        , testSAT "pret" "pret60_75"
        -- , testSAT "pret" "pret150_25"
        -- , testSAT "pret" "pret150_40"
        -- , testSAT "pret" "pret150_60"
        -- , testSAT "pret" "pret150_75"
        ]

    , testGroup "QG"
        -- SAT-encoded Quasigroup (or Latin square) instances
        [ testSAT "QG" "qg1-07"
        , testSAT "QG" "qg2-07"
        ]

    , testGroup "ssa"
        -- Circuit fault analysis: single-stuck-at fault
        [ testSAT "ssa" "ssa0432-003"
        , testSAT "ssa" "ssa2670-130"
        , testSAT "ssa" "ssa2670-141"
        , testSAT "ssa" "ssa6288-047"
        , testSAT "ssa" "ssa7552-038"
        , testSAT "ssa" "ssa7552-158"
        , testSAT "ssa" "ssa7552-159"
        , testSAT "ssa" "ssa7552-160"
        ]
    ]

testSAT :: FilePath -> FilePath -> TestTree
testSAT dir name = testCaseSteps name $ \info -> do
    dimacs <- PureSAT.DIMACS.parseDimacsFile ("dimacs" </> dir </> (name ++ ".cnf"))
    let solution = solvePureSat dimacs
    if null solution
    then do
        info "UNSAT"
        solution' <- solveMiniSat dimacs
        assertBool "MiniSat doesnt think its unsat" (null solution')
    else do
        info "SAT"
        solution' <- solveMiniSat $ [[x] | x <- solution] ++ dimacs
        assertBool "MiniSat doesnt validate" (not (null solution'))

-------------------------------------------------------------------------------
-- PureSAT
-------------------------------------------------------------------------------

solvePureSat :: [[Int]] -> [Int]
solvePureSat clauses = runST $ do
    s <- PureSAT.newSolver

    -- create literal
    literals <- newPrimArray maxLiteral
    forM_ [1..maxLiteral] $ \i -> do
        l <- PureSAT.newLit s
        writePrimArray literals (i - 1) l

    -- addClauses
    forM_ clauses $ \clause -> unless (null clause) $ do
        clause' <- forM clause $ \i -> do
            l <- readPrimArray literals (abs i - 1)
            return $ if i < 0 then PureSAT.neg l else l

        _ <- PureSAT.addClause s clause'
        return ()

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

solveMiniSat :: [[Int]] -> IO [Int]
solveMiniSat clauses = do
    s <- MiniSat.newSolver

    -- create literal
    literals <- newPrimArray maxLiteral
    forM_ [1..maxLiteral] $ \i -> do
        l <- MiniSat.newLit s
        writePrimArray literals (i - 1) l

    -- addClauses
    forM_ clauses $ \clause -> do
        clause' <- forM clause $ \i -> do
            l <- readPrimArray literals (abs i - 1)
            return $ if i < 0 then MiniSat.neg l else l

        MiniSat.addClause s clause'

    -- solve
    res <- MiniSat.solve s []

    -- read back the solution
    if res
    then do
        forM [1..maxLiteral] $ \i -> do
            l <- readPrimArray literals (i - 1)
            x <- MiniSat.modelValue s l
            return $ case x of
                Nothing    -> i
                Just True  -> i
                Just False -> negate i

    else return []
  where
    maxLiteral = foldl' (\acc clause -> foldl' (\x y -> max x (abs y)) acc clause) 0 clauses

-------------------------------------------------------------------------------
-- orphans
-------------------------------------------------------------------------------

deriving newtype instance Prim MiniSat.Lit
