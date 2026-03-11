{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module DummyLd (dummyLd) where

import Control.Monad.Catch
import Data.Bits

import Data.Elf
import Data.Elf.Constants
import Data.Elf.Headers
import Control.Exception.ChainedException

data MachineConfig a
    = MachineConfig
        { mcAddress :: WordXX a -- ^ Virtual address of the executable segment
        , mcAlign   :: WordXX a -- ^ Required alignment of the executable segment
                                --   in physical memory (depends on max page size)
        }

getMachineConfig :: (SingElfClassI a, MonadThrow m) => ElfMachine -> m (MachineConfig a)
getMachineConfig EM_AARCH64 = return $ MachineConfig 0x400000 0x10000
getMachineConfig EM_X86_64  = return $ MachineConfig 0x400000 0x1000
getMachineConfig _          = $chainedError "could not find machine config for this arch"

dummyLd' :: forall a m . (MonadThrow m, SingElfClassI a) => ElfListXX a -> m (ElfListXX a)
dummyLd' es = do

    section' <- elfFindSectionByName es ".text"

    txtSectionData <- case esData section' of
        ElfSectionData textData -> return textData
        _ -> $chainedError "could not find correct \".text\" section"

    -- FIXME: it's better to match constructor here, but there is a bug that prevents to conclude that
    -- the match is irrefutable:
    -- https://stackoverflow.com/questions/72803815/phantom-type-makes-pattern-matching-irrefutable-but-that-seemingly-does-not-wor
    -- https://gitlab.haskell.org/ghc/ghc/-/issues/15681#note_165436
    -- But if I use lazy pattern match, then some other bug comes up that prevents type inference
    -- on GHC 9.0.2
    header' <- elfFindHeader es

    MachineConfig { .. } <- getMachineConfig (ehMachine header')

    return $
        case header' of
            ElfHeader { .. } ->
                ElfSegment
                    { epType       = PT_LOAD
                    , epFlags      = PF_X .|. PF_R
                    , epVirtAddr   = mcAddress
                    , epPhysAddr   = mcAddress
                    , epAddMemSize = 0
                    , epAlign      = mcAlign
                    , epData       =
                        ElfHeader
                            { ehType  = ET_EXEC
                            , ehEntry = mcAddress + headerSize (fromSingElfClass $ singElfClass @a)
                            , ..
                            }
                        ~: ElfRawData
                            { edData = txtSectionData
                            }
                        ~: ElfListNull
                    }
                ~: ElfSegmentTable
                ~: ElfListNull

-- | @dummyLd@ places the content of ".text" section of the input ELF
-- into the loadable segment of the resulting ELF.
-- This could work if there are no relocations or references to external symbols.
dummyLd :: MonadThrow m => Elf -> m Elf
dummyLd (Elf c l) = Elf c <$> withSingElfClassI c dummyLd' l
