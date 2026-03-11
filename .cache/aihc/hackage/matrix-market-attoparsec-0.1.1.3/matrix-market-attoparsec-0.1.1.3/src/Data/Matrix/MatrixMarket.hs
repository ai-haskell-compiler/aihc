-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Matrix.MatrixMarket
-- Copyright   :  (c) Marco Zocca 2017-2020
-- License     :  BSD2 (see the file LICENSE)
--
-- Maintainer  :  zocca marco gmail
-- Stability   :  experimental
-- Portability :  portable
--
-- @attoparsec@-based parser and serializer for the NIST MatrixMarket format [1]. The parser logic originally appeared in @accelerate-examples@ and it is reused here (courtesy of T.McDonell and the @accelerate@ developers) with some amendments.
--
--
-- References :
--
-- 1. https://math.nist.gov/MatrixMarket/
-----------------------------------------------------------------------------
module Data.Matrix.MatrixMarket
       (-- * Load
         -- ** Matrix
        readMatrix, readMatrix',
        -- ** Array
        readArray, readArray',
        -- * Save
        -- ** Matrix
        writeMatrix, writeMatrix',
        -- ** Array
        writeArray, writeArray',
        -- * Types
        Matrix(..), Array(..),
        Format (Coordinate, Array), Structure (General, Symmetric, Hermitian, Skew),
        -- * Helpers
        -- ** Matrix-related
        nnz, dim, numDat,
        -- ** Array-related
        dimArr, numDatArr,
        -- * Exceptions
        ImportError(..), ExportError(..)) where

import Data.Matrix.MatrixMarket.Internal as M
