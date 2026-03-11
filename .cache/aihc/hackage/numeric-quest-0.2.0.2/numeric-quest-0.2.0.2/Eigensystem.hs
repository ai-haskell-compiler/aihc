------------------------------------------------------------------------------
-- Haskell module:      Eigensystem
-- Date:                initialized 2001-03-25, last modified 2001-03-25
-- Author:              Jan Skibinski, Numeric Quest Inc.
-- Location:            http://www.numeric-quest.com/haskell/Eigensystem.hs
-- See also:            http://www.numeric-quest.com/haskell/QuantumVector.html
-- See also:            http://www.numeric-quest.com/haskell/Orthogonals.html
--
-- Description:
--
-- This module extends the QuantumVector module by providing functions
-- to calculate eigenvalues and eigenvectors of Hermitian operators.
-- Such toolkit is of primary importance due to pervasiveness of
-- eigenproblems in Quantum Mechanics.
--
-- This module is organized in three layers:
--
-- 1. Interface to module QuantumVector, where all function signatures
--   are expressed in terms of linear operators, Dirac vectors and scalars.
--
--   Here the operators are defined directly via maps from input to
--   output vectors. In many cases it is much easier to define the operators
--   directly rather than to rely on their matrix representation.
--
-- 2.  Conversion layer between operators and their matrix representation.
--
--   Sometimes it is more convenient to start with an underlying matrix
--   representation of an operator. There are also cases where a direct
--   manipulation on operators is too difficult, while it is trivial
--   to obtain the corresponding results via matrices. One example is a
--   computation of a Hermitian conjugate of A:
--      < ei | A' | ej > = conjugate < ej | A | ej >
--     (Here ' stands for a dagger)
--   If however the operator A is made from a product or a sum of simpler
--   operators, whose Hermitian conjugates are known to us, then the
--   direct approach from the upper layer could be easier and perhaps more
--   efficient in some cases.
--
-- 3.  Implementation layer is stored in a separate module LinearAlgorithms,
--   where matrices are represented as lists of columns of scalars, and
--   vectors -- as lists of scalars.
--
--   This layer is completely independendent of the other two and can be
--   reused separately for applications other than those caring for the
--   QuantumVector module and its notation. It can also be reimplemented
--   via Haskell arrays, or perhaps by some other means, such as trees
--   of nodes relating square blocks of data to support paralleism.
--
-- See also bottom of the page for references and license.
-----------------------------------------------------------------------------

module Eigensystem (eigenvalues, adjoint) where
import Data.Complex
import QuantumVector
import LinearAlgorithms (triangular, tridiagonal, triangular2)
import Data.List (findIndex)
import Prelude2010
import Prelude ()

----------------------------------------------------------------------------
-- Category: Eigensystem for QuantumVector
----------------------------------------------------------------------------

eigenvalues :: Ord a => Bool -> Int -> [Ket a] -> (Ket a -> Ket a) -> [Scalar]
eigenvalues doTri n es a
    --  A list of eigenvalues of operator 'a'
    --  obtained after 'n' triangularizations
    --  of a matrix corresponding to operator 'a'
    --  where
    --      'es' is a list of base vectors
    --      'doTri' declares whether or not we
    --        want the initial tridiagonalization
    --        (applies to Hermitian operators only)
    | doTri == True     =  f b1
    | otherwise         =  f b
    where
        f c             = diagonals  $ operator es $ triangular n c
        diagonals us    = [toBra e <> us e | e <- es]
        b               = matrix es a
        b1              = tridiagonal b


eigenpairs :: Ord a => Int -> [Ket a] -> (Ket a -> Ket a) -> ([Scalar], [Ket a])
eigenpairs n es a
    --  A pair of lists (eigenvalues, eigenvectors) of hermitian
    --  operator 'a' obtained after 'n' triangularizations of 'a'
    --  where
    --      'es' is a list of base vectors
    --  Note: For a moment this applies only to Hermitian operators
    --  until we decide what would be the best way to compute eigenvectors
    --  of a triangular matrix: the method from module Orthogonal, power
    --  iteration, etc.
    = (ls, xs)
    where
        (t, q)  = triangular2 n b
        b       = matrix es a
        ls      = [ tk!!k | (tk, k) <- zip t [0..length t - 1] ]
        xs      = [compose qk es | qk <- q]

adjoint :: Ord a => [Ket a] -> (Ket a -> Ket a) -> (Ket a -> Ket a)
adjoint es a
    --  A Hermitian conjugate of operator a,
    --  (or a-dagger, or adjoint to a)
    --  where 'es' is a list of base vectors
    =   operator es ms
    where
        ms = [[ conjugate (toBra ei <> vj) | vj <- v] | ei <- es]
        v = [a ej | ej <- es]


----------------------------------------------------------------------------
-- Category: Conversion from operators to matrices and vice versa
----------------------------------------------------------------------------

operator :: Ord a => [Ket a] -> [[Scalar]] -> Ket a -> Ket a
operator bss ms x
    --  Definition of an operator corresponding
    --  to a matrix 'ms' given as a list of scalar
    --  columns
    --  where
    --      'bss' (basis) is a complete list of base vectors
    --      'x' is any ket vector from this space
    =   a >< x
    where
        a u = case (findIndex (u == ) bss) of
                Just k  -> compose (ms !! k) bss
                Nothing -> error "Out of bounds"


matrix :: Ord a => [Ket a] -> (Ket a -> Ket a) -> [[Scalar]]
matrix bss a
    --  List of scalar columns representing
    --  the operator 'a' in a given 'basis'
    = [[ei' <> vj | ei' <- e'] | vj <- v]
    where
        v = [a ej | ej <- bss]
        e' = [toBra ei | ei <- bss]

----------------------------------------------------------------------------
-- Category: Test data
--
----------------------------------------------------------------------------

matrixA :: [[Scalar]]
matrixA
    --  Test matrix A represented as list of scalar columns.
    =   [
                [1, 2, 4, 1, 5]
        ,       [2, 3, 2, 6, 4]
        ,       [4, 2, 5, 2, 3]
        ,       [1, 6, 2, 7, 2]
        ,       [5, 4, 3, 2, 9]
        ]

opA :: Ket Int -> Ket Int
opA     = operator basisA matrixA

basisA :: [Ket Int]
basisA  = map Ket [1..5::Int] -- or: map Ket "abcde", etc.

---------------------------------------------------------------------------
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


