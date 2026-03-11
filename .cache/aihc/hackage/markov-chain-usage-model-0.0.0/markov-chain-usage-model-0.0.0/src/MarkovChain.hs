module MarkovChain
  ( M
  , V
  , (.*)
  , (./)
  , (.-)
  , (^*)
  , norm_1
  , norm_F
  , fundamental
  , occVariance
  , perron
  , sigma
  , nodeProbabilities
  , expectedLength
  , expectedArcReliability
  , transientReliability
  , singleUseReliability
  -- , singleUseReliabilityIO
  -- , loadStaticMatrix
  -- , load2StaticMatrices
  -- , saveStaticMatrix
  , kullbackLeibler
  )
  where

import           Data.Bifunctor
                   (bimap)
import qualified Data.List          as List
import           Data.Matrix        as Matrix
import           Data.Vector
                   (Vector)
import qualified Data.Vector        as Vector
import           Prelude            hiding
                   (pi)

------------------------------------------------------------------------

type M = Matrix Double
type V = Vector Double

-- point-wise
(.*) :: M -> M -> M
(.*) = elementwise (*)
infixl 7 .*

(./) :: M -> M -> M
(./) = elementwise (/)
infixl 7 ./

(.-) :: M -> M -> M
(.-) = elementwise (-)
infixl 6 .-

-- scalar
(^*) :: Double -> M -> M
(^*) x = fmap (x *)
infixl 7 ^*

(^/) :: Double -> M -> M
(^/) x = fmap (x /)
infixl 7 ^/

norm_1 :: M -> Double
norm_1 = sum . toList

norm_F :: M -> Double
norm_F = sqrt . sum . toList . fmap (^(2 :: Int))

{- $setup
   >>> :set -XFlexibleContexts
   >>> :{
   let
     p :: M
     p = fromList 5 5
       [ 0, 1,    0,   0,    0
       , 0, 0,    0.5, 0.5,  0
       , 0, 0,    0.5, 0.25, 0.25
       , 0, 0.25, 0,   0,    0.75
       , 1, 0,    0,   0,    0
       ]
:}
-}

------------------------------------------------------------------------
-- * Number of occurrences of a state in a test case

-- | The fundamental matrix for absorbing chains. Its (i, j)-th entry is
-- the expected number of occurrences of state j prior to absorption at
-- the sink, given that one starts in state i. So the first row
-- indicates the expected occurence of each state starting from the
-- start state.
--
-- >>> fundamental (minorMatrix 5 5 p :: M)
-- ┌                                                                                 ┐
-- │                 1.0  1.2307692307692306  1.2307692307692308  0.9230769230769231 │
-- │                 0.0  1.2307692307692306  1.2307692307692308  0.9230769230769231 │
-- │                 0.0 0.15384615384615385  2.1538461538461537  0.6153846153846154 │
-- │                 0.0  0.3076923076923077  0.3076923076923077  1.2307692307692308 │
-- └                                                                                 ┘
fundamental
  :: M   -- ^ Reduced matrix (n x n).
  -> M   -- ^ n x n
fundamental m = case inverse (identity (nrows m) .- m) of
  Left err -> error $ "fundamental: " ++ err
  Right r  -> r

-- | Expected variance of the occurrence for each state. The (i, j)-th
-- entry should be read in the same away as that of the fundamental
-- matrix above.
--
-- >>> occVariance (fundamental (minorMatrix 5 5 p :: M))
-- ┌                                                                                 ┐
-- │                 0.0 0.28402366863905315  2.5562130177514795  0.4970414201183433 │
-- │                 0.0 0.28402366863905315  2.5562130177514795  0.4970414201183433 │
-- │                 0.0 0.20118343195266267  2.4852071005917153  0.5207100591715977 │
-- │                 0.0  0.3550295857988165  0.9230769230769231  0.2840236686390534 │
-- └                                                                                 ┘
occVariance
  :: M -- ^ Reduced matrix (n x n).
  -> M -- n x n
occVariance n = n * (2 ^* diagonal 0 (getDiag n) - identity dim) - (n .* n)
  where
    dim = nrows n

------------------------------------------------------------------------
-- * Computing the long-run state probabilities

-- | The Perron eigenvector (long-run occupancies/probabilities of states).
--
-- >>> perron p
-- [0.1857142857142857,0.22857142857142854,0.22857142857142856,0.17142857142857143,0.1857142857142857]
perron :: M -- ^ Transition matrix (n x n).
       -> V
perron p = Vector.map (/ l) n1
  where
    dim = nrows p

    n1 :: V
    n1 = getRow 1 (fundamental (minorMatrix dim dim p)) `Vector.snoc` 1

    l :: Double
    l  = getElem 1 1 $ rowVector n1 * colVector (getDiag (identity dim))

------------------------------------------------------------------------
-- * Sensitivity analysis

-- XXX: Algorithm on p. 31 in report.
_sensitivities :: M -> M
_sensitivities = undefined

------------------------------------------------------------------------
-- * Other long run statistics

-- | Compute the stimulus long-run occupancy.

{- | >>> :{
let s :: M
    s = fromList 4 5
      [ 1,    0,    0,    0,    0
      , 0,    0.5,  0.5,  0,    0
      , 0,    0.5,  0.25, 0.25, 0
      , 0.25, 0,    0,    0.5,  0.25
      ]
in sigma s (perron p)
:}
[0.2807017543859649,0.2807017543859649,0.21052631578947367,0.17543859649122806,5.2631578947368425e-2]
-}
sigma :: M -- ^ Stimulus matrix (n-1 x n).
      -> V -- ^ Perron eigenvector.
      -> V
sigma stimulus pi = Vector.fromList
  [ 1 / (1 - pi Vector.! (pred n))
    * sum [ pi Vector.! (pred i) * getElem i k stimulus
          | i <- [1 .. (pred n)]
          ]
  | k <- [1..n]
  ]
  where
    n = Vector.length pi

-- ------------------------------------------------------------------------
-- -- * Probability of occurrence for states

-- | Compute the probability of occurrence for each state.
--
-- >>> getRow 1 (nodeProbabilities p)
-- [1.0,1.0,0.5714285714285715,0.75]
nodeProbabilities
  :: M -- ^ Transition matrix (n x n).
  -> M -- n-1 x n-1
nodeProbabilities p = n * (diagonal 0 (getDiag (1 ^/ n)))
  where
    n = fundamental (minorMatrix dim dim p)
    dim = nrows p

-- (Algorithm 9 on p. 34.)

-- ------------------------------------------------------------------------

-- | Expected test case length.
--
-- >>> expectedLength (fundamental (minorMatrix 5 5 p :: M))
-- 4.384615384615385
expectedLength
  :: M -- ^ Fundamental matrix (n x n).
  -> Double
expectedLength = norm_1 . rowVector . (getRow 1)

-- ------------------------------------------------------------------------

-- | Success rate matrix.
--
-- >>> expectedArcReliability Nothing (fromList 2 2 [10,10,10,10], fromList 2 2 [0,1,2,3])
-- (┌                                       ┐
-- │ 0.9166666666666666 0.8461538461538461 │
-- │ 0.7857142857142857 0.7333333333333333 │
-- └                                       ┘,┌                                             ┐
-- │  5.876068376068376e-3  9.298393913778529e-3 │
-- │ 1.1224489795918367e-2 1.2222222222222223e-2 │
-- └                                             ┘)
-- >>> :{
--  expectedArcReliability
--    (Just (fromList 2 2 [10,10,10,10], fromList 2 2 [1,1,1,1]))
--    (fromList 2 2 [10,10,10,10], fromList 2 2 [0,1,2,3])
-- :}
-- (┌                                       ┐
-- │ 0.9130434782608695              0.875 │
-- │               0.84 0.8076923076923077 │
-- └                                       ┘,┌                                             ┐
-- │ 3.3081285444234404e-3              4.375e-3 │
-- │  5.169230769230769e-3  5.752794214332676e-3 │
-- └                                             ┘)
expectedArcReliability
  :: Maybe (M, M)
  -> (M, M)
  -> (M, M) -- ^ (mean, variance)
expectedArcReliability mprior (obsSuccs, obsFails) =
  ( alpha ./ (alpha + beta)
  , (alpha .* beta) ./ (ab .* ab .* (ab + ones))
  )
  where
    m = nrows obsSuccs
    n = ncols obsSuccs

    ones :: M
    ones = Matrix.fromList m n [1,1..]

    -- In case no prior information is available, a_{i,j} = b_{i,j} = 1.
    priorSuccs, priorFails :: M
    (priorSuccs, priorFails) =
      maybe (ones, ones) (bimap (+ ones) (+ ones)) mprior

    alpha, beta :: M
    alpha = priorSuccs + obsSuccs
    beta  = priorFails + obsFails

    ab :: M
    ab = alpha + beta

transientReliability
  :: M            -- ^ Reduced transition matrix (n-1 x n).
  -> Maybe (M, M) -- ^ Prior successes and failures (n-1 x n).
  -> (M, M)       -- ^ Observed successes and failures (n-1 x n).
  -> (M, M)       -- ^ Reliability vector and reliability variance vector.
transientReliability q mprior obs =
  (rstar, vstar)
  where
    n = ncols q

    (mean, var) = expectedArcReliability mprior obs

    r1 :: M
    r1 = mean

    fancyR :: M
    fancyR = q .* r1

    fancyRdot :: M -- n-1 x n-1
    fancyRdot = submatrix 1 (pred n) 1 (pred n) fancyR

    w :: M -- n-1 x 1
    w = colVector $ getCol n fancyR

    rstar :: M -- n-1 x 1
    rstar = fundamental fancyRdot * w

    r2 :: M
    r2 = r1 .* r1 + var

    fancyR' :: M -- n-1 x n-1
    fancyR' = q .* r2

    fancyRdot' :: M -- n-1 x n-1
    fancyRdot' = submatrix 1 (pred n) 1 (pred n) fancyR'

    w' :: M -- n-1 x 1
    w' = colVector $ getCol n fancyR'

    rstar' :: M -- n-1 x 1
    rstar' = fundamental fancyRdot' * w'

    vstar = rstar' - rstar .* rstar

singleUseReliability
  :: M            -- ^ Reduced transition matrix (n-1 x n).
  -> Maybe (M, M) -- ^ Prior successes and failures (n-1 x n).
  -> (M, M)       -- ^ Observed successes and failures (n-1 x n).
  -> (Double, Double)
singleUseReliability q mprior obs =
  bimap (head . toList) (head . toList) $ transientReliability q mprior obs

------------------------------------------------------------------------

{-
singleUseReliabilityIO :: (KnownNat n, KnownNat (n - 1))
  => (1 <= n - 1, n - 1 <= n, (n - (n - 1)) ~ 1)

  => L (n - 1) n                -- ^ Transition matrix without transitions to
                                -- the sink.

  -> FilePath                   -- ^ Filepath where prior successes are/will be
                                -- saved.

  -> FilePath                   -- ^ Filepath where prior failures are/will be
                                -- saved.

  -> (L (n - 1) n, L (n - 1) n) -- ^ Observed successes and failures.
  -> IO Double
singleUseReliabilityIO q fps fpf (s, f) = do
  mprior <- load2StaticMatrices fps fpf
  case mprior of
    Nothing -> do
      -- We need the elementwise @max 1@ below because otherwise we get division
      -- by zero in 'expectedArcReliability'. For details see the 2017 paper by by L. Lin,
      -- Y. Xue and F. Song in the README.
      saveStaticMatrix fps "%.1f" (dmmap (max 1) s)
      saveStaticMatrix fpf "%.1f" (dmmap (max 1) f)
    Just (ps, pf) -> do
      saveStaticMatrix fps "%.1f" (ps + s)
      saveStaticMatrix fpf "%.1f" (pf + f)
  return (singleUseReliability q mprior (s, f))

loadStaticMatrix :: (KnownNat m, KnownNat n) => FilePath -> IO (Maybe (L m n))
loadStaticMatrix = fmap (join . fmap create) . D.loadMatrix'

load2StaticMatrices :: (KnownNat m, KnownNat n, KnownNat o, KnownNat p)
                    => FilePath -> FilePath -> IO (Maybe (L m n, L o p))
load2StaticMatrices fp1 fp2 = liftMA2 (loadStaticMatrix fp1) (loadStaticMatrix fp2)
  where
    liftMA2 :: (Monad m, Applicative f) => m (f a) -> m (f b) -> m (f (a, b))
    liftMA2 = liftM2 (liftA2 (,))

saveStaticMatrix :: (KnownNat m, KnownNat n)
                 => FilePath
                 -> String   -- ^ "printf" format (e.g. "%.2f", "%g", etc.)
                 -> L m n -> IO ()
saveStaticMatrix fp format = D.saveMatrix fp format . unwrap
-}

------------------------------------------------------------------------

-- | Kullback-Leibler matrix discrimination.

{- | >>> :{
let
  s = fromList 4 5
        [ 1,    0,   0,    0,    0
        , 0,    0.5, 0.5,  0,    0
        , 0,    0.5, 0.25, 0.25, 0
        , 0.25, 0,   0,    0.5,  0.25
        ]
  es = Vector.fromList [4, 5, 7, 3, 4]
  et = fromList 4 5
    [ 4, 0, 0, 0, 0
    , 0, 3, 2, 0, 0
    , 0, 4, 1, 2, 0
    , 1, 0, 0, 1, 1
    ]
in kullbackLeibler p s es et
:}
3.440540391434413e-2
-}
kullbackLeibler
  :: M      -- ^ Transitions (n x n).
  -> M      -- ^ Stimulis (m x n).
  -> V      -- ^ State visitations (n).
  -> M      -- ^ Stimulus execution (m x n).
  -> Double -- ^ Discrimination.
kullbackLeibler p s es et = Vector.sum k'
  where
    m = nrows s
    n = ncols p

    pi' :: V
    pi' = Vector.take m (perron p)

    es' :: V
    es' = Vector.take m es

    es'' :: M
    es'' = let v = colVector es'
           in List.foldl' (<|>) v (take (pred n) (repeat v))

    t :: M
    t = es'' ./ et

    k :: V
    k  = Vector.fromList $ map sum $ toLists $ (1 / log 2) ^* (s .* fmap log' (s .* t))

    k' :: V
    k' = Vector.zipWith (*) pi' k

    log' :: Double -> Double
    log' x
      | x == 0    = 0
      | isNaN x   = 0
      | otherwise = log x
