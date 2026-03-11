{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module AsmAArch64
    ( CodeState
    , Register
    , Label
    , adr
    , b
    , mov
    , ldr
    , svc
    , ascii
    , label
    , exportSymbol
    , assemble
    , x0, x1, x2, x8
    , w0, w1
    ) where

import Prelude as P

import Control.Exception.ChainedException
import Control.Monad.Catch
import Control.Monad.State as MS
import Data.Bits
import Data.ByteString.Builder
import Data.ByteString.Lazy as BSL
import Data.ByteString.Lazy.Char8 as BSLC
import Data.Int
import Data.Kind
import Data.Word

import Data.Elf
import Data.Elf.Constants
import Data.Elf.Headers

data RegisterWidth = X | W

data SingRegisterWidth :: RegisterWidth -> Type where
    SX :: SingRegisterWidth 'X
    SW :: SingRegisterWidth 'W

class SingRegisterWidthI (c :: RegisterWidth) where
    singRegisterWidth :: SingRegisterWidth c

instance SingRegisterWidthI 'X where
    singRegisterWidth = SX

instance SingRegisterWidthI 'W where
    singRegisterWidth = SW

newtype Register (c :: RegisterWidth) = R Word32

newtype CodeOffset  = CodeOffset  { getCodeOffset  :: Int64 }  deriving (Eq, Show, Ord, Num, Enum, Real, Integral, Bits, FiniteBits)
newtype Instruction = Instruction { getInstruction :: Word32 } deriving (Eq, Show, Ord, Num, Enum, Real, Integral, Bits, FiniteBits)

data Label = CodeRef !CodeOffset
           | PoolRef !CodeOffset

-- Args:
-- Offset of the instruction
-- Offset of the pool
type InstructionGen = CodeOffset -> CodeOffset -> Either String Instruction

data CodeState = CodeState
    { offsetInPool    :: CodeOffset
    , poolReversed    :: [Builder]
    , codeReversed    :: [InstructionGen]
    , symbolsRefersed :: [(String, Label)]
    }

emit' :: MonadState CodeState m => InstructionGen -> m ()
emit' g = modify f where
    f CodeState {..} = CodeState { codeReversed = g : codeReversed
                                 , ..
                                 }

emit :: MonadState CodeState m => Instruction -> m ()
emit i = emit' $ \ _ _ -> Right i

emitPool :: MonadState CodeState m => Word -> ByteString -> m Label
emitPool a bs = state f where
    f CodeState {..} =
        let
            offsetInPool' = align a offsetInPool
            o = builderRepeatZero $ fromIntegral $ offsetInPool' - offsetInPool
        in
            ( PoolRef offsetInPool'
            , CodeState { offsetInPool = fromIntegral (BSL.length bs) + offsetInPool'
                        , poolReversed = lazyByteString bs : o : poolReversed
                        , ..
                        }
            )

label :: MonadState CodeState m => m Label
label = gets (CodeRef . (* instructionSize) . fromIntegral . P.length . codeReversed)

x0, x1, x2, x8 :: Register 'X
x0 = R 0
x1 = R 1
x2 = R 2
x8 = R 8

w0, w1 :: Register 'W
w0 = R 0
w1 = R 1

isPower2 :: (Bits i, Num i) => i -> Bool
isPower2 n = n .&. (n - 1) == 0

align :: Word -> CodeOffset -> CodeOffset
align a _ | not (isPower2 a) = error "align is not a power of 2"
align 0 n = n
align a n = (n + a' - 1) .&. complement (a' - 1)
    where a' = fromIntegral a

builderRepeatZero :: Int -> Builder
builderRepeatZero n = mconcat $ P.replicate n (word8 0)

b64 :: forall w . SingRegisterWidthI w => Register w -> Word32
b64 _ = case singRegisterWidth @w of
    SX -> 1
    SW -> 0

-- | C6.2.10 ADR
adr :: MonadState CodeState m => Register 'X -> Label -> m ()
adr (R n) rr =  emit' f where

    offsetToImm :: CodeOffset -> Either String Word32
    offsetToImm (CodeOffset o) =
        if not $ isBitN 19 o
                then Left "offset is too big"
                else
                    let
                        immlo = o .&. 3
                        immhi = (o `shiftR` 2)  .&. 0x7ffff
                    in
                        Right $ fromIntegral $ (immhi `shift` 5) .|. (immlo `shift` 29)

    f :: InstructionGen
    f instrAddr poolOffset = do
        imm <- offsetToImm $ findOffset poolOffset rr - instrAddr
        return $ Instruction $  0x10000000
                            .|. imm
                            .|. n

-- | C6.2.26 B
b :: MonadState CodeState m => Label -> m ()
b rr = emit' f where

    offsetToImm26 :: CodeOffset -> Either String Word32
    offsetToImm26 (CodeOffset o)
      | o .&. 0x3 /= 0    = Left $ "offset is not aligned: " ++ show o
      | not $ isBitN 28 o = Left "offset is too big"
      | otherwise         = Right $ fromIntegral $ o `shiftR` 2

    f :: InstructionGen
    f instrAddr poolOffset = do
        imm26 <- offsetToImm26 $ findOffset poolOffset rr - instrAddr
        return $ Instruction $  0x14000000 .|. imm26

-- | C6.2.187 MOV (wide immediate)
mov :: (MonadState CodeState m, SingRegisterWidthI w) => Register w -> Word16 -> m ()
mov r@(R n) imm = emit $ Instruction $ (b64 r `shift` 31)
                                    .|. 0x52800000
                                    .|. (fromIntegral imm `shift` 5)
                                    .|. n

-- | The number can be represented with bitN bits
isBitN ::(Num b, Bits b, Ord b) => Int -> b -> Bool
isBitN bitN w =
    let
        m = complement $ (1 `shift` bitN) - 1
        h = w .&. m
    in if w >= 0 then h == 0 else h == m

findOffset :: CodeOffset -> Label -> CodeOffset
findOffset _poolOffset (CodeRef codeOffset)   = codeOffset
findOffset  poolOffset (PoolRef offsetInPool) = poolOffset + offsetInPool

-- | C6.2.132 LDR (literal)
ldr :: (MonadState CodeState m, SingRegisterWidthI w) => Register w -> Label -> m ()
ldr r@(R n) rr = emit' f where

    offsetToImm19 :: CodeOffset -> Either String Word32
    offsetToImm19 (CodeOffset o)
      | o .&. 0x3 /= 0    = Left "offset is not aligned"
      | not $ isBitN 21 o = Left "offset is too big"
      | otherwise         = Right $ fromIntegral $ o `shiftR` 2

    f :: InstructionGen
    f instrAddr poolOffset = do
        imm19 <- offsetToImm19 $ findOffset poolOffset rr - instrAddr
        return $ Instruction $ (b64 r `shift` 30)
                            .|. 0x18000000
                            .|. (imm19 `shift` 5)
                            .|. n

-- | C6.2.317 SVC
svc :: MonadState CodeState m => Word16 -> m ()
svc imm = emit $ 0xd4000001 .|. (fromIntegral imm `shift` 5)

ascii :: MonadState CodeState m => String -> m Label
ascii s = emitPool 1 $ BSLC.pack s

exportSymbol :: MonadState CodeState m => String -> Label -> m ()
exportSymbol s r = modify f where
    f (CodeState {..}) = CodeState { symbolsRefersed = (s, r) : symbolsRefersed
                                   , ..
                                   }

instructionSize :: Num b => b
instructionSize = 4

zeroIndexStringItem :: ElfSymbolXX 'ELFCLASS64
zeroIndexStringItem = ElfSymbolXX "" 0 0 0 0 0

textSecN, shstrtabSecN, strtabSecN, symtabSecN :: ElfSectionIndex
textSecN     = 1
shstrtabSecN = 2
strtabSecN   = 3
symtabSecN   = 4

assemble :: MonadCatch m => StateT CodeState m () -> m Elf
assemble m = do

    CodeState {..} <- execStateT m (CodeState 0 [] [] [])

    -- resolve txt

    let
        poolOffset = instructionSize * fromIntegral (P.length codeReversed)
        poolOffsetAligned = align 8 poolOffset

        f :: (InstructionGen, CodeOffset) -> Either String Instruction
        f (ff, n) = ff n poolOffsetAligned

    code <- $eitherAddContext' $ mapM f $ P.zip (P.reverse codeReversed) (fmap (instructionSize *) [CodeOffset 0 .. ])

    let
        codeBuilder = mconcat $ fmap (word32LE . getInstruction) code
        txt = toLazyByteString $ codeBuilder
                              <> builderRepeatZero (fromIntegral $ poolOffsetAligned - poolOffset)
                              <> mconcat (P.reverse poolReversed)

    -- resolve symbolTable

    let
        ff :: (String, Label) -> ElfSymbolXX 'ELFCLASS64
        ff (s, r) =
            let
                steName  = s
                steBind  = STB_Global
                steType  = STT_NoType
                steShNdx = textSecN
                steValue = fromIntegral $ findOffset poolOffset r
                steSize  = 0 :: Word64
            in
                ElfSymbolXX{..}

        symbolTable = ff <$> P.reverse symbolsRefersed

    (symbolTableData, stringTableData) <- serializeSymbolTable ELFDATA2LSB (zeroIndexStringItem : symbolTable)

    return $ Elf SELFCLASS64 $
        ElfHeader
            { ehData       = ELFDATA2LSB
            , ehOSABI      = ELFOSABI_SYSV
            , ehABIVersion = 0
            , ehType       = ET_REL
            , ehMachine    = EM_AARCH64
            , ehEntry      = 0
            , ehFlags      = 0
            }
        ~: ElfSection
            { esName      = ".text"
            , esType      = SHT_PROGBITS
            , esFlags     = SHF_EXECINSTR .|. SHF_ALLOC
            , esAddr      = 0
            , esAddrAlign = 8
            , esEntSize   = 0
            , esN         = textSecN
            , esLink      = 0
            , esInfo      = 0
            , esData      = ElfSectionData txt
            }
        ~: ElfSection
            { esName      = ".shstrtab"
            , esType      = SHT_STRTAB
            , esFlags     = 0
            , esAddr      = 0
            , esAddrAlign = 1
            , esEntSize   = 0
            , esN         = shstrtabSecN
            , esLink      = 0
            , esInfo      = 0
            , esData      = ElfSectionDataStringTable
            }
        ~: ElfSection
            { esName      = ".symtab"
            , esType      = SHT_SYMTAB
            , esFlags     = 0
            , esAddr      = 0
            , esAddrAlign = 8
            , esEntSize   = symbolTableEntrySize ELFCLASS64
            , esN         = symtabSecN
            , esLink      = fromIntegral strtabSecN
            , esInfo      = 1
            , esData      = ElfSectionData symbolTableData
            }
        ~: ElfSection
            { esName      = ".strtab"
            , esType      = SHT_STRTAB
            , esFlags     = 0
            , esAddr      = 0
            , esAddrAlign = 1
            , esEntSize   = 0
            , esN         = strtabSecN
            , esLink      = 0
            , esInfo      = 0
            , esData      = ElfSectionData stringTableData
            }
        ~: ElfSectionTable
        ~: ElfListNull
