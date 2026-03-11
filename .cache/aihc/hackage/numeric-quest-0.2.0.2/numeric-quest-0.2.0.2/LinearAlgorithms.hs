------------------------------------------------------------------------------
-- Haskell module:      LinearAlgorithms
-- Date:                initialized 2001-03-25, last modified 2001-04-01
-- Author:              Jan Skibinski, Numeric Quest Inc.
-- Location:            http://www.numeric-quest.com/haskell/LinearAlgorithms.hs
-- See also:            http://www.numeric-quest.com/haskell/Orthogonals.html
--
-- Description:
-- This module provides several _selected_ linear algebra algorithms,
-- supporting computation of eigenvalues and eigenvectors of dense
-- matrices of small size. This module is to be utilized by module
-- Eigensystem, which redefines the eigenproblems in terms of
-- linear operators (maps) and abstract Dirac vectors.

-- Here is a list of implemented algorithms:
--
-- + triangular         A => R          where R is upper triangular
-- + triangular2        A => (R, Q)     such that R = Q' A Q
--
-- + tridiagonal        H => T          where H is Hermitian and T is
-- + tridiagonal2       H => (T, Q)     tridiagonal, such that T = Q' H Q
--
-- + subsAnnihilator    A => Q  such that Q A has zeroed subdiagonals
-- + reflection         x => y  where y is a complex reflection of x
--
-- Other algoritms, such as solution of linear equations are, at this time,
-- imported from module Orthogonals. The latter also deals with triangulization,
-- so you can compare the results from two different approaches:
-- orthogonalization vs. Householder reduction used in this module.
-- In essence the former method is a bit faster but overflows for large
-- number of iterations since, for typing reasons - its algorithms
-- avoid the normalization of vectors.
-- For full documentation of this module, and for references and the license,
-- go to the bottom of the page.
----------------------------------------------------------------------------

module LinearAlgorithms (
        triangular,
        triangular2,
        tridiagonal,
        tridiagonal2,
        Scalar,) where

import Data.Complex
import Orthogonals hiding (Scalar)

type Scalar = Complex Double

----------------------------------------------------------------------------
-- Category: Iterative triangularization
--
--   triangular         A => R          where R is upper triangular
--   triangular2        A => (R, Q)     such that R = Q' A Q
----------------------------------------------------------------------------

mult :: [[Scalar]] -> [[Scalar]] -> [[Scalar]]
a `mult` b
    --  A matrix-product of matrices 'a' and 'b'
    --          C = A B
    --  where all matrices are represented as lists
    --  of scalar columns
        = matrix_matrix' (transposed a) b

triangular :: Int -> [[Scalar]] -> [[Scalar]]
triangular n a
    --  A (hopefully) triangular matrix R = Q' A Q obtained by
    --  'n' similarity transformations S(k) of matrix A:
    --          Q = S1 S2 S3 ....
    --
    -- If matrix A is Hermitian then the result is close
    -- to a diagonal matrix for sufficiently large n.
    | n == 0    = a
    | otherwise = triangular (n - 1) a1
    where
        a1  = (q' `mult` a ) `mult` q
        q'  = subsAnnihilator 0 a
        q   = adjoint q'


triangular2 :: Int -> [[Scalar]] -> ([[Scalar]], [[Scalar]])
triangular2 n a
    --  A pair of matrices (R, Q) obtained by 'n'
    --  similarity transformations, where R = Q' A Q
    --  is a (hopefully) triangular matrix, or diagonal
    --  if A is Hermitian. The transformation matrix Q
    --  is required for computation of eigenvectors
    --  of A.
    = triangular2' n a (unit_matrix n)
    where
        triangular2' o b p
            | o == 0    = (b, p)
            | otherwise = triangular2' (o - 1) b1 p1
            where
                b1 = (q' `mult` b ) `mult` q
                p1 = p `mult` q
                q' = subsAnnihilator 0 b
                q  = adjoint q'


----------------------------------------------------------------------------
-- Category: Tridiagonalization of a Hermitian matrix
--
-- + tridiagonal        H -> T  where H is Hermitian and T is tridiagonal
-- + tridiagonal2       H -> (T, Q)     such that T = Q' H Q
----------------------------------------------------------------------------


tridiagonal :: [[Scalar]] -> [[Scalar]]
tridiagonal h
    --  A tridiagonal matrix T = Q' H Q, obtained from Hermitian
    --  matrix H by a finite number of elementary similarity
    --  transformations (Householder reductions).
    | n < 3             = h
    | otherwise         = f (tail es) h 1
    where
        n       = length h
        es      = unit_matrix n

        f bs a k
            | length bs == 1    = a
            | otherwise         = f (tail bs)  a1 (k+1)
            where
                a1      = (q' `mult` a) `mult` q
                q'      = [r e | e <- es]
                q       = adjoint q'
                r       = reflection u (head bs)
                u       = replicate k 0 ++ drop k (a!!(k-1))


tridiagonal2 :: [[Scalar]] -> ([[Scalar]], [[Scalar]])
tridiagonal2 h
    --  A pair (T, Q) of matrices, obtained from
    --  similarity transformation of Hermitian matrix H
    --  where T = Q' H Q is a tridiagonal matrix and Q is unitary
    --  transformation made of a finite product of
    --  elementary Householder reductions.
    | n < 3             = (h, es)
    | otherwise         = f (tail es) h es 1
    where
        n       = length h
        es      = unit_matrix n

        f bs a p k
            | length bs == 1    = (a, p)
            | otherwise         = f (tail bs) a1 p1 (k+1)
            where
                a1      = (q' `mult` a) `mult` q
                q'      = [r e | e <- es]
                q       = adjoint q'
                p1      = p `mult` q
                r       = reflection u (head bs)
                u       = replicate k 0 ++ drop k (a!!(k-1))


----------------------------------------------------------------------------
-- Category: Elementary unitary transformations
--
-- + subsAnnihilator    A => Q  such that Q A has zeroed subdiagonals
-- + reflection         x => y  where y is a complex reflection of x
----------------------------------------------------------------------------

subsAnnihilator :: Int -> [[Scalar]] -> [[Scalar]]
subsAnnihilator k a
    --  A unitary matrix Q' transforming any n x n
    --  matrix A to an upper matrix B, which has
    --  zero values below its 'k'-th subdiagonal
    --  (annihilates all subdiagonals below k-th)
    --          B = Q' A
    --  where
    --      'a' is a list of columns of matrix A
    --
    --  If k=0 then B is an upper triangular matrix,
    --  if k=1 then B is an upper Hessenberg matrix.
    --  The transformation Q is built from n - k - 1
    --  elementary Householder transformations of
    --  the first n-k-1 columns of iteratively transformed
    --  matrix A.
    | n < 2 + k         = es
    | otherwise         = f (drop k es) a1 es k
    where
        n       = length a
        es      = unit_matrix n
        a1      = take (n - 1 - k) a

        f bs b p l
            | length bs == 1    = p
            | otherwise         = f (tail bs)  b1 p1 (l+1)
            where
                b1      = [r v |v <- tail b]
                p1      = q' `mult` p
                q'      = [r e | e <- es]
                r       = reflection u (head bs)
                u       = replicate k 0 ++ drop l (head b)


reflection :: [Scalar] -> [Scalar] -> [Scalar] -> [Scalar]
reflection a e x
    --  A vector resulting from unitary complex
    --  Householder-like transformation of vector 'x'.
    --
    --  The operator of such transformation is defined
    --  by mapping vector 'a' to a multiple 'p' of vector 'e'
    --          U |a > = p | e >
    --  where scalar 'p' is chosen to guarantee unitarity
    --          < a | a > = < p e | p e>.
    --
    --  This transformation is not generally Hermitian, because
    --  the scalar 'p' might become complex - unless
    --          < a | e > = < e | a >,
    --  which is the case when both vectors are real, and
    --  when this transformation becomes a simple Hermitian
    --  reflection operation.
    --  See reference [1] for details.
    --
    | d == 0    = x
    | otherwise = [xk - z * yk |(xk, yk) <- zip x y]
    where
        z = s * bra_ket y x
        s = 2/h :+ (-2 * g)/h
        h = 1 + g^(2::Int)
        g = imagPart a_b / d
        d = a_a - realPart a_b
        y = normalized [ak - bk |(ak, bk) <- zip a b]
        p = a_a / (realPart (bra_ket e e))
        b = map ((sqrt p :+ 0) * ) e
        a_a = realPart (bra_ket a a)
        a_b = bra_ket a b

----------------------------------------------------------------------------
-- Category: Test data
--
----------------------------------------------------------------------------

-- matrixA :: [[Scalar]]
-- matrixA
--     --  Test matrix A represented as list of scalar columns.
--     =   [
--                 [1, 2, 4, 1, 5]
--         ,       [2, 3, 2, 6, 4]
--         ,       [4, 2, 5, 2, 3]
--         ,       [1, 6, 2, 7, 2]
--         ,       [5, 4, 3, 2, 9]
--         ]

----------------------------------------------------------------------------
-- Module documentation
-- ====================

-- Representation of vectors, matrices and scalars:
-- ------------------------------------------------
-- We have chosen to follow the same scheme as used in module Orthogonals:
-- vectors are represented here as lists of scalars, while matrices --
-- as lists of scalar columns (vectors). But while scalars over there are
-- generic and cover a range of types, the scalars of this module are
-- implemented as Complex Double. Although all algorithms here
-- operate on complex matrices and complex vectors, they will work
-- on real matrices without modifications. If however, the performance
-- is a premium it will be a trivial exercise to customize all these
-- algorithms to real domain. Perhaps the most important change should
-- be then made to a true workhorse of this module, the function 'reflection',
-- in order to convert it to a real reflection of a vector in a hyperplane
-- whose normal is another vector.
--
-- Schur triangularization of any matrix:
-- --------------------------------------
-- The Schur theorem states that there exists a unitary matrix Q such
-- that any nonsingular matrix A can be transformed to an upper triangular
-- matrix R via similarity transformation
--      R = Q' A Q
-- which preserves the eigenvalues. Here Q' stands for a Hermitian
-- conjugate of Q (adjoint, or Q-dagger).

-- Since the eigenvalues of a triangular matrix R are its diagonal
-- elements, finding such transformation solves the first part of
-- the eigenproblem. The second part, finding the eigenvectors of A,
-- is trivial since they can be computed from eigenvectors of R:
--      | x(A) > = Q | x(R) >
--
-- In particular, when matrix A is Hermitian, then the matrix R
-- becomes diagonal, and the eigenvectors of R are its normalized
-- columns; that is, the unit vectors. It follows that the eigenvectors
-- of A are then the columns of matrix Q.
-- But when A is not Hermitian one must first find the eigenvectors
-- of a triangular matrix R before applying the above transformation.
-- Fortunately, it is easier to find eigenvectors of a triangular matrix
-- R than those of the square matrix A.
--
-- Implementation of Schur triangularization via series of QR factorizations:
-- --------------------------------------------------------------------------
-- The methods known in literature as QR factorization (decomposition)
-- methods iteratively compose such unitary matrix Q from a series of
-- elementary unitary transformations, Q(1), Q(2)..:
--      Q = Q(1) Q(2) Q(3) ...
-- The most popular method of finding those elementary unitary
-- transformations relies on a reflection transformation, so selected as
-- to zero out all components of the matrix below its main diagonal. Our
-- implementation uses a complex variety of such a 'reflection', described
-- in the reference [1]. The columnar reduction of the lower portion of
-- the matrix to zeros is also known under the name of Householder
-- reduction, or Householder transformation. This is, however, not the
-- only possible choice for elementary transformations; see for example
-- our module Orthogonals, where such transformations are perfomed via
-- Gram-Schmidt orthogonalization procedure instead.
--
-- The iterative functions 'triangular' and 'triangular2' attempt to
-- triangularize any complex matrix A by a series of similarity
-- transformation, known in literature as QR decomposition.
-- Function 'triangular' does not deliver the transformation Q but
-- only a transformed matrix A, which should be close to triangular
-- form after a sufficient number of iterations. Use this function
-- if you are interested in eigenvalues only. But when you need
-- the eigenvectors as well, then use the function 'triangular2',
-- which also delivers the transformation Q, as shown below:
--   triangular         A => R  where R is upper triangular
--   triangular2        A => (R, Q)     such that R = Q' A Q
--
-- Tridiagonalization of Hermitian matrices:
-- -----------------------------------------
-- While the above functions are iterative and require a bit of
-- experimentation with a count of iterations to figure out whether
-- the required accuracy has yet been achieved, the tridiagonalization
-- methods transform any matrix A to a tridiagonal form in a finite
-- number of elementary transformations.
--
-- However, our implementation is not generic because it performs
-- tridiagonalization only on Hermitian matrices. It uses the same
-- unitary 'reflection', as the triangularization does.
--
-- Why would you care for such tridiagonalization at all? Many world
-- class algorithms use it as a first step to precondition the original
-- matrix A for faster convergence and for better stability and accuracy.
-- Its cost is small in comparison to the overall cost incurred during
-- the iterative stage. What's more, the triangularization iteration
-- does preserve the shape of tridiagonal matrix at each step - bringing
-- it only closer to the diagonal shape. So the tridiagonalization
-- is a recommended option to be executed before the iterative
-- triangulariation.
--
-- Again, we are offering here two versions of the tridiagonalization:
--
-- + tridiagonal        H -> T  where H is Hermitian and T is tridiagonal
-- + tridiagonal2       H -> (T, Q)     such that T = Q' H Q
--
-- Elementary transformations:
-- ---------------------------
-- All the above algorithms heavily rely on the function 'reflection'
-- which defines a complex reflection transformation of a vector. One use
-- of this function is to perform a Householder reduction of a column-vector,
-- to zero out all of its components but one. For example, the unitary
-- transformation 'subsAnnihilator 0' annihilates all subdiagonals lying
-- below the main diagonal. Similarly, 'subsAnnihilator 1' would zero out
-- all matrix components below its first subdiagonal - leading to a so-called
-- upper Hessenberg matrix.
--
-- + subsAnnihilator    A => Q  such that Q A has zeroed subdiagonals
-- + reflection         x => y  where y is a complex reflection of x
--
----------------------------------------------------------------------------
-- References:
-- [1]  Xiaobai Sun, On Elementary Unitary and Phi-unitary transformations,
--      Duke University, Department Of Computer Science, 1995,
--      http://citeseer.nj.nec.com/340881.html
---------------------------------------------------------------------------
--
-- Copyright:
--
--      (C) 2001 Numeric Quest, All rights reserved
--
--      Email: jans@numeric-quest.com
--
--      http://www.numeric-quest.com
--
-- License:
--
--      GNU General Public License, GPL
--
---------------------------------------------------------------------------

