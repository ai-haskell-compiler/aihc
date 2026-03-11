module Unit
  ( unit_expectExpectedArcReliability
  , unit_expectTransientReliability
  , unit_occurenceMean
  , unit_occurenceVar
  , unit_docTest
  ) where

import           Data.Matrix
import qualified Data.Vector      as Vector
import           Prelude          hiding
                   (pi)
import           Test.DocTest
                   (doctest)
import           Test.Tasty.HUnit
                   (Assertion, (@?), (@?=))

import           MarkovChain

------------------------------------------------------------------------

-- cf. A Simpler and More Direct Derivation of System Reliability Using Markov Chain Usage Models (2017)
-- by Lan Lin, Yufeng Xue and Fengguang Song

-- Transient usage model (transitions from sink omitted).
q :: M
q = fromList 2 3
  [ 0, 0.5, 0.5
  , 0, 0.5, 0.5
  ]

successes :: M
successes = fromList 2 3
  [ 1, 2, 3
  , 4, 5, 6
  ]

failures :: M
failures = fromList 2 3
  [ 1, 0, 1
  , 0, 1, 0
  ]

-- Observed transient success rate.
sr :: M
(sr, _) = expectedArcReliability Nothing (successes, failures)

unit_expectExpectedArcReliability :: Assertion
unit_expectExpectedArcReliability = sr @?= expected
  where
    expected :: M
    expected = fromList 2 3
      [ 2/4, 3/4, 4/6
      , 5/6, 6/8, 7/8
      ]

-- Transient reliability matrix.
tr :: M
tr = fst $ transientReliability q Nothing (successes, failures)

unit_expectTransientReliability :: Assertion
unit_expectTransientReliability =
  (norm_F (tr - expected)) <= 1.0e-6 @? "differs from expected"
  where
    a = (4/6)/2
    b = (3/4)*(7/8)/4
    c = (7/8)/2
    d = 1 - (6/8)/2

    expected :: M
    expected = fromList 2 1
      [ a + b/d
      , c/d
      ]

------------------------------------------------------------------------

-- cf. Computations for Markov Chain Usage Models (2000)
-- by S. J. Prowell

-- Usage model.
p :: M
p = fromList 5 5
  [ 0, 1,    0,   0,    0
  , 0, 0,    0.5, 0.5,  0
  , 0, 0,    0.5, 0.25, 0.25
  , 0, 0.25, 0,   0,    0.75
  , 1, 0,    0,   0,    0
  ]

q' :: M
q' = minorMatrix 5 5 p

-- State occurences.
unit_occurenceMean :: Assertion
unit_occurenceMean =
  norm_F (rowVector actual .- rowVector expected) <= 1.0e-3 @? "differs from expected"
  where
    actual :: V
    actual = getRow 1 (fundamental q')

    expected :: V
    expected = Vector.fromList
      [ 1.0, 1.231, 1.231, 0.9231 ]

unit_occurenceVar :: Assertion
unit_occurenceVar =
  norm_F (rowVector actual .- rowVector expected) <= 1.0e-3 @? "differs from expected"
  where
    actual :: V
    actual = getRow 1 (occVariance (fundamental q'))

    expected :: V
    expected = Vector.fromList
      [ 0, 0.284, 2.556, 0.497 ]

unit_docTest :: IO ()
unit_docTest = doctest ["src/MarkovChain.hs"]
