-- |
-- Module      : Data.ELF
-- Description : Parse/serialize ELF files into structured data
-- Copyright   : (c) Aleksey Makarov, 2021
-- License     : BSD 3-Clause License
-- Maintainer  : aleksey.makarov@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Parse/serialize ELF files into structured data

module Data.Elf (
    -- * Elf
      ElfListXX (..)
    , (~:)
    , ElfNodeType (..)
    , ElfSectionData (..)
    , ElfXX (..)
    , Elf (..)
    , parseElf
    , serializeElf

    -- * Misc
    , elfFindSection
    , elfFindSectionByName
    , elfFindHeader

    -- * Symbol table
    , ElfSymbolXX(..)
    , parseSymbolTable
    , serializeSymbolTable
    ) where

import Data.Internal.Elf
