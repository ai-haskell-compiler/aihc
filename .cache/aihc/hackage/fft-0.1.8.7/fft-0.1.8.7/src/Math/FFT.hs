-- |
-- Module      : Math.FFT
-- Copyright   : (c) 2008 Jed Brown
-- License     : BSD-style
-- 
-- Maintainer  : jed@59A2.org
-- Stability   : experimental
-- Portability : non-portable
--
-- This module exposes an interface to FFTW, the Fastest Fourier Transform in
-- the West.
--
-- These bindings present several levels of interface.  All the higher level
-- functions ('dft', 'idft', 'dftN', ...) are easily derived from the general
-- functions ('dftG', 'dftRCG', ...).  Only the general functions let you
-- specify planner flags.  The higher levels all set 'estimate' so you should
-- not have to wait through time consuming planning (see below for more).
--
-- The simplest interface is the one-dimensional transforms.  If you supply a
-- multi-dimensional array, these will only transform the first dimension.
-- These functions only take one argument, the array to be transformed.
--
-- At the next level, we have multi-dimensional transforms where you specify
-- which dimensions to transform in and the array to transform.  For instance
--
-- > b = dftRCN [0,2] a
--
-- is the real to complex transform in dimensions 0 and 2 of the array @a@ which
-- must be at least rank 3.  The array @b@ will be complex valued with the same
-- extent as @a@ in every dimension except @2@.  If @a@ had extent @n@ in
-- dimension @2@ then the @b@ will have extent @a `div` 2 + 1@ which consists of
-- all non-negative frequency components in this dimension (the negative
-- frequencies are conjugate to the positive frequencies because of symmetry
-- since @a@ is real valued).
--
-- The real to real transforms allow different transform kinds in each
-- transformed dimension.  For example,
--
-- > b = dftRRN [(0,DHT), (1,REDFT10), (2,RODFT11)] a
--
-- is a Discrete Hartley Transform in dimension 0, a discrete cosine transform
-- (DCT-2) in dimension 1, and distrete sine transform (DST-4) in dimension 2
-- where the array @a@ must have rank at least 3.
--
-- The general interface is similar to the multi-dimensional interface, takes as
-- its first argument, a bitwise '.|.' of planning 'Flag's.  (In the complex
-- version, the sign of the transform is first.)  For example,
--
-- > b = dftG DFTBackward (patient .|. destroy_input) [1,2] a
--
-- is an inverse DFT in dimensions 1 and 2 of the complex array @a@ which has
-- rank at least 3.  It will use the patient planner to generate a (near)
-- optimal transform.  If you compute the same type of transform again, it
-- should be very fast since the plan is cached.
--
-- Inverse transforms are typically normalized.  The un-normalized inverse
-- transforms are 'dftGU', 'dftCRGU' and 'dftCROGU'.  For example
--
-- > b = dftCROGU measure [0,1] a
--
-- is an un-normalized inverse DFT in dimensions 0 and 1 of the complex array
-- @a@ (representing the non-negative frequencies, where the negative
-- frequencies are conjugate) which has rank at least 2.  Here, dimension 1 is
-- logically odd so if @a@ has extent @n@ in dimension 1, then @b@ will have
-- extent @(n - 1) * 2 + 1@ in dimension 1.  It is more common that the logical
-- dimension is even, in which case we would use 'dftCRGU' in which case @b@
-- would have extent @(n - 1) * 2@ in dimension @1@.
--
--
-- The FFTW library separates transforms into two steps.  First you compute a
-- plan for a given transform, then you execute it.  Often the planning stage is
-- quite time-consuming, but subsequent transforms of the same size and type
-- will be extremely fast.  The planning phase actually computes transforms, so
-- it overwrites its input array.  For many C codes, it is reasonable to re-use
-- the same arrays to compute a given transform on different data.  This is not
-- a very useful paradigm from Haskell.  Fortunately, FFTW caches its plans so
-- if try to generate a new plan for a transform size which has already been
-- planned, the planner will return immediately.  Unfortunately, it is not
-- possible to consult the cache, so if a plan is cached, we may use more memory
-- than is strictly necessary since we must allocate a work array which we
-- expect to be overwritten during planning.  FFTW can export its cached plans
-- to a string.  This is known as wisdom.  For high performance work, it is a
-- good idea to compute plans of the sizes you are interested in, using
-- aggressive options (i.e. 'patient'), use 'exportWisdomString' to get a string
-- representing these plans, and write this to a file.  Then for production
-- runs, you can read this in, then add it to the cache with
-- 'importWisdomString'.  Now you can use the 'estimate' planner so the Haskell
-- bindings know that FFTW will not overwrite the input array, and you will
-- still get a high quality transform (because it has wisdom).

module Math.FFT (
    -- * Data types
    Sign(..),
    Kind(..),
    -- * Planner flags
    -- ** Algorithm restriction flags
    destroyInput,
    preserveInput,
    -- ** Planning rigor flags
    estimate,
    measure,
    patient,
    exhaustive,

    -- * DFT of complex data
    -- ** DFT in first dimension only
    dft,
    idft,
    -- ** Multi-dimensional transforms
    dftN,
    idftN,
    -- ** General transform
    dftG,
    -- ** Un-normalized general transform
    dftGU,

    -- * DFT of real data
    -- ** DFT in first dimension only
    dftRC,
    dftCR,
    dftCRO,
    -- ** Multi-dimensional transforms
    dftRCN,
    dftCRN,
    dftCRON,
    -- ** General transform
    dftRCG,
    dftCRG,
    dftCROG,
    -- ** Un-normalized general transform
    dftCRGU,
    dftCROGU,

    -- * Real to real transforms (all un-normalized)
    -- ** Transforms in first dimension only
    dftRH,
    dftHR,
    dht,
    dct1,
    dct2,
    dct3,
    dct4,
    dst1,
    dst2,
    dst3,
    dst4,
    -- ** Multi-dimensional transforms with the same transform type in each dimension
    dftRHN,
    dftHRN,
    dhtN,
    dct1N,
    dct2N,
    dct3N,
    dct4N,
    dst1N,
    dst2N,
    dst3N,
    dst4N,
    -- ** Multi-dimensional transforms with possibly different transforms in each dimension
    dftRRN,
    -- ** General transforms
    dftRRG,

    -- * Wisdom
    importWisdomString,
    importWisdomSystem,
    exportWisdomString,
) where

import Math.FFT.Base
