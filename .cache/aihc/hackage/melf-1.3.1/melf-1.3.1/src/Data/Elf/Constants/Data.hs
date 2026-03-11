{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Elf.Constants.Data where

import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Bits

import Data.Elf.Constants.TH
import Data.Endian

-- | Operating system and ABI for which the object is prepared
$(mkDeclarations BaseWord8 "ElfOSABI" "ELFOSABI"
    [ ("_SYSV",         0, "No extensions or unspecified")
    , ("_HPUX",         1, "Hewlett-Packard HP-UX")
    , ("_NETBSD",       2, "NetBSD")
    , ("_LINUX",        3, "Linux")
    , ("_SOLARIS",      6, "Sun Solaris")
    , ("_AIX",          7, "AIX")
    , ("_IRIX",         8, "IRIX")
    , ("_FREEBSD",      9, "FreeBSD")
    , ("_TRU64",       10, "Compaq TRU64 UNIX")
    , ("_MODESTO",     11, "Novell Modesto")
    , ("_OPENBSD",     12, "Open BSD")
    , ("_OPENVMS",     13, "Open VMS")
    , ("_NSK",         14, "Hewlett-Packard Non-Stop Kernel")
    , ("_AROS",        15, "Amiga Research OS")
    , ("_ARM",         97, "ARM")
    , ("_STANDALONE", 255, "Standalone (embedded) application")
    ])

-- | Object file type
$(mkDeclarations BaseWord16 "ElfType" "ET"
    [ ("_NONE", 0, "Unspecified type")
    , ("_REL",  1, "Relocatable object file")
    , ("_EXEC", 2, "Executable object file")
    , ("_DYN",  3, "Shared object file")
    , ("_CORE", 4, "Core dump object file")
    ])

-- | Target architecture
$(mkDeclarations BaseWord16 "ElfMachine" "EM"
    [ ("_NONE",         0, "No machine")
    , ("_M32",          1, "AT&T WE 32100")
    , ("_SPARC",        2, "SPARC")
    , ("_386",          3, "Intel 80386")
    , ("_68K",          4, "Motorola 68000")
    , ("_88K",          5, "Motorola 88000")
    , ("_486",          6, "Intel i486 (DO NOT USE THIS ONE)")
    , ("_860",          7, "Intel 80860")
    , ("_MIPS",         8, "MIPS I Architecture")
    , ("_S370",         9, "IBM System/370 Processor")
    , ("_MIPS_RS3_LE", 10, "MIPS RS3000 Little-endian")
    , ("_SPARC64",     11, "SPARC 64-bit")
    , ("_PARISC",      15, "Hewlett-Packard PA-RISC")
    , ("_VPP500",      17, "Fujitsu VPP500")
    , ("_SPARC32PLUS", 18, "Enhanced instruction set SPARC")
    , ("_960",         19, "Intel 80960")
    , ("_PPC",         20, "PowerPC")
    , ("_PPC64",       21, "64-bit PowerPC")
    , ("_S390",        22, "IBM System/390 Processor")
    , ("_SPU",         23, "Cell SPU")
    , ("_V800",        36, "NEC V800")
    , ("_FR20",        37, "Fujitsu FR20")
    , ("_RH32",        38, "TRW RH-32")
    , ("_RCE",         39, "Motorola RCE")
    , ("_ARM",         40, "Advanced RISC Machines ARM")
    , ("_ALPHA",       41, "Digital Alpha")
    , ("_SH",          42, "Hitachi SH")
    , ("_SPARCV9",     43, "SPARC Version 9")
    , ("_TRICORE",     44, "Siemens TriCore embedded processor")
    , ("_ARC",         45, "Argonaut RISC Core, Argonaut Technologies Inc.")
    , ("_H8_300",      46, "Hitachi H8/300")
    , ("_H8_300H",     47, "Hitachi H8/300H")
    , ("_H8S",         48, "Hitachi H8S")
    , ("_H8_500",      49, "Hitachi H8/500")
    , ("_IA_64",       50, "Intel IA-64 processor architecture")
    , ("_MIPS_X",      51, "Stanford MIPS-X")
    , ("_COLDFIRE",    52, "Motorola ColdFire")
    , ("_68HC12",      53, "Motorola M68HC12")
    , ("_MMA",         54, "Fujitsu MMA Multimedia Accelerator")
    , ("_PCP",         55, "Siemens PCP")
    , ("_NCPU",        56, "Sony nCPU embedded RISC processor")
    , ("_NDR1",        57, "Denso NDR1 microprocessor")
    , ("_STARCORE",    58, "Motorola Star*Core processor")
    , ("_ME16",        59, "Toyota ME16 processor")
    , ("_ST100",       60, "STMicroelectronics ST100 processor")
    , ("_TINYJ",       61, "Advanced Logic Corp. TinyJ embedded processor family")
    , ("_X86_64",      62, "AMD x86-64 architecture")
    , ("_PDSP",        63, "Sony DSP Processor")
    , ("_FX66",        66, "Siemens FX66 microcontroller")
    , ("_ST9PLUS",     67, "STMicroelectronics ST9+ 8/16 bit microcontroller")
    , ("_ST7",         68, "STMicroelectronics ST7 8-bit microcontroller")
    , ("_68HC16",      69, "Motorola MC68HC16 Microcontroller")
    , ("_68HC11",      70, "Motorola MC68HC11 Microcontroller")
    , ("_68HC08",      71, "Motorola MC68HC08 Microcontroller")
    , ("_68HC05",      72, "Motorola MC68HC05 Microcontroller")
    , ("_SVX",         73, "Silicon Graphics SVx")
    , ("_ST19",        74, "STMicroelectronics ST19 8-bit microcontroller")
    , ("_VAX",         75, "Digital VAX")
    , ("_CRIS",        76, "Axis Communications 32-bit embedded processor")
    , ("_JAVELIN",     77, "Infineon Technologies 32-bit embedded processor")
    , ("_FIREPATH",    78, "Element 14 64-bit DSP Processor")
    , ("_ZSP",         79, "LSI Logic 16-bit DSP Processor")
    , ("_MMIX",        80, "Donald Knuth's educational 64-bit processor")
    , ("_HUANY",       81, "Harvard University machine-independent object files")
    , ("_PRISM",       82, "SiTera Prism")
    , ("_AVR",         83, "Atmel AVR 8-bit microcontroller")
    , ("_FR30",        84, "Fujitsu FR30")
    , ("_D10V",        85, "Mitsubishi D10V")
    , ("_D30V",        86, "Mitsubishi D30V")
    , ("_V850",        87, "NEC v850")
    , ("_M32R",        88, "Mitsubishi M32R")
    , ("_MN10300",     89, "Matsushita MN10300")
    , ("_MN10200",     90, "Matsushita MN10200")
    , ("_PJ",          91, "picoJava")
    , ("_OPENRISC",    92, "OpenRISC 32-bit embedded processor")
    , ("_ARC_A5",      93, "ARC Cores Tangent-A5")
    , ("_XTENSA",      94, "Tensilica Xtensa Architecture")
    , ("_VIDEOCORE",   95, "Alphamosaic VideoCore processor")
    , ("_TMM_GPP",     96, "Thompson Multimedia General Purpose Processor")
    , ("_NS32K",       97, "National Semiconductor 32000 series")
    , ("_TPC",         98, "Tenor Network TPC processor")
    , ("_SNP1K",       99, "Trebia SNP 1000 processor")
    , ("_ST200",      100, "STMicroelectronics (www.st.com) ST200 microcontroller")
    , ("_IP2K",       101, "Ubicom IP2xxx microcontroller family")
    , ("_MAX",        102, "MAX Processor")
    , ("_CR",         103, "National Semiconductor CompactRISC microprocessor")
    , ("_F2MC16",     104, "Fujitsu F2MC16")
    , ("_MSP430",     105, "Texas Instruments embedded microcontroller msp430")
    , ("_BLACKFIN",   106, "Analog Devices Blackfin (DSP) processor")
    , ("_SE_C33",     107, "S1C33 Family of Seiko Epson processors")
    , ("_SEP",        108, "Sharp embedded microprocessor")
    , ("_ARCA",       109, "Arca RISC Microprocessor")
    , ("_UNICORE",    110, "Microprocessor series from PKU-Unity Ltd. and MPRC of Peking University")
    , ("_AARCH64",    183, "ELF for the Arm 64-bit Architecture (AArch64)")
    ])

-- | Section type
$(mkDeclarations BaseWord32 "ElfSectionType" "SHT"
    [ ("_NULL",     0, "Identifies an empty section header.")
    , ("_PROGBITS", 1, "Contains information defined by the program")
    , ("_SYMTAB",   2, "Contains a linker symbol table")
    , ("_STRTAB",   3, "Contains a string table")
    , ("_RELA",     4, "Contains \"Rela\" type relocation entries")
    , ("_HASH",     5, "Contains a symbol hash table")
    , ("_DYNAMIC",  6, "Contains dynamic linking tables")
    , ("_NOTE",     7, "Contains note information")
    , ("_NOBITS",   8, "Contains uninitialized space; does not occupy any space in the file")
    , ("_REL",      9, "Contains \"Rel\" type relocation entries")
    , ("_SHLIB",   10, "Reserved")
    , ("_DYNSYM",  11, "Contains a dynamic loader symbol table")
    ])

-- | Segment type
$(mkDeclarations BaseWord32 "ElfSegmentType" "PT"
    [ ("_NULL",    0, "Unused entry")
    , ("_LOAD",    1, "Loadable segment")
    , ("_DYNAMIC", 2, "Dynamic linking tables")
    , ("_INTERP",  3, "Program interpreter path name")
    , ("_NOTE",    4, "Note section")
    , ("_SHLIB",   5, "Reserved")
    , ("_PHDR",    6, "Program header table")
    ])

-- | Attributes of the section
$(mkDeclarations BaseWord64 "ElfSectionFlag" "SHF"
    [ ("_WRITE",     1 `shiftL` 0, "Section contains writable data")
    , ("_ALLOC",     1 `shiftL` 1, "Section is allocated in memory image of program")
    , ("_EXECINSTR", 1 `shiftL` 2, "Section contains executable instructions")
    ])

-- | Attributes of the segment
$(mkDeclarations BaseWord32 "ElfSegmentFlag" "PF"
    [ ("_X", 1 `shiftL` 0, "Execute permission")
    , ("_W", 1 `shiftL` 1, "Write permission")
    , ("_R", 1 `shiftL` 2, "Read permission")
    ])

-- | Symbol type
$(mkDeclarations BaseWord8 "ElfSymbolType" "STT"
    [ ("_NoType",  0, "NoType")
    , ("_Object",  1, "Object")
    , ("_Func",    2, "Func")
    , ("_Section", 3, "Section")
    , ("_File",    4, "File")
    , ("_Common",  5, "Common")
    , ("_TLS",     6, "TLS")
    , ("_LoOS",   10, "LoOS")
    , ("_HiOS",   12, "HiOS")
    , ("_LoProc", 13, "LoProc")
    , ("_HiProc", 15, "HiProc")
    ])

-- | Symbol binding
$(mkDeclarations BaseWord8 "ElfSymbolBinding" "STB"
    [ ("_Local",   0, "")
    , ("_Global",  1, "")
    , ("_Weak",    2, "")
    , ("_LoOS",   10, "")
    , ("_HiOS",   12, "")
    , ("_LoProc", 13, "")
    , ("_HiProc", 15, "")
    ])

-- | Section index
$(mkDeclarations BaseWord16 "ElfSectionIndex" "SHN"
    [ ("_Undef",       0, "")
    , ("_LoProc", 0xFF00, "")
    , ("_HiProc", 0xFF1F, "")
    , ("_LoOS",   0xFF20, "")
    , ("_HiOS",   0xFF3F, "")
    , ("_Abs",    0xFFF1, "")
    , ("_Common", 0xFFF2, "")
    ])

-- | AARCH64 relocation type
$(mkDeclarations BaseWord32 "ElfRelocationType_AARCH64" "R_AARCH64"

    -- Null relocation codes

    [ ("_NONE",  0,   "None")
    , ("_NONE_", 256, "None")

    -- Data relocations

    , ("_ABS64",  257, "S + A     | No overflow check")
    , ("_ABS32",  258, "S + A     | Check that -2^31 <= X < 2^32")
    , ("_ABS16",  259, "S + A     | Check that -2^15 <= X < 2^16")
    , ("_PREL64", 260, "S + A - P | No overflow check")
    , ("_PREL32", 261, "S + A - P | Check that -2^31 <= X < 2^32")
    , ("_PREL16", 262, "S + A - P | Check that -2^15 <= X < 2^16")
    , ("_PLT32",  314, "S + A - P | Check that -2^31 <= X < 2^31 see call and jump relocations")

    -- Group relocations to create a 16-, 32-, 48-, or 64-bit unsigned data value or address inline

    , ("_MOVW_UABS_G0",    263, "S + A | Set a MOV[KZ] immediate field to bits [15: 0] of X; check that 0 <= X < 2^16")
    , ("_MOVW_UABS_G0_NC", 264, "S + A | Set a MOV[KZ] immediate field to bits [15: 0] of X. No overflow check")
    , ("_MOVW_UABS_G1",    265, "S + A | Set a MOV[KZ] immediate field to bits [31:16] of X; check that 0 <= X < 2^32")
    , ("_MOVW_UABS_G1_NC", 266, "S + A | Set a MOV[KZ] immediate field to bits [31:16] of X. No overflow check")
    , ("_MOVW_UABS_G2",    267, "S + A | Set a MOV[KZ] immediate field to bits [47:32] of X; check that 0 <= X < 2^48")
    , ("_MOVW_UABS_G2_NC", 268, "S + A | Set a MOV[KZ] immediate field to bits [47:32] of X. No overflow check")
    , ("_MOVW_UABS_G3",    269, "S + A | Set a MOV[KZ] immediate field to bits [63:48] of X (no overflow check needed)")

    -- Group relocations to create a 16, 32, 48, or 64 bit signed data or offset value inline

    , ("_MOVW_SABS_G0", 270, "S + A | Set a MOV[NZ] immediate field using bits [15: 0] of X; check -2^16 <= X < 2^16")
    , ("_MOVW_SABS_G1", 271, "S + A | Set a MOV[NZ] immediate field using bits [31:16] of X; check -2^32 <= X < 2^32")
    , ("_MOVW_SABS_G2", 272, "S + A | Set a MOV[NZ] immediate field using bits [47:32] of X; check -2^48 <= X < 2^48")

    -- Relocations to generate 19, 21 and 33 bit PC-relative addresses

    , ("_LD_PREL_LO19",        273, "S + A - P             | Set a load-literal immediate value to bits [20:2] of X; check that -2^20 <= X < 2^20")
    , ("_ADR_PREL_LO21",       274, "S + A - P             | Set an ADR immediate value to bits [20:0] of X; check that -2^20 <= X < 2^20")
    , ("_ADR_PREL_PG_HI21",    275, "Page(S + A) - Page(P) | Set an ADRP immediate value to bits [32:12] of the X; check that -2^32 <= X < 2^32")
    , ("_ADR_PREL_PG_HI21_NC", 276, "Page(S + A) - Page(P) | Set an ADRP immediate value to bits [32:12] of the X. No overflow check")
    , ("_ADD_ABS_LO12_NC",     277, "S + A                 | Set an ADD immediate value to bits [11:0] of X. No overflow check. Used with relocations ADR_PREL_PG_HI21 and ADR_PREL_PG_HI21_NC")
    , ("_LDST8_ABS_LO12_NC",   278, "S + A                 | Set an LD/ST immediate value to bits [11:0] of X. No overflow check. Used with relocations ADR_PREL_PG_HI21 and ADR_PREL_PG_HI21_NC")
    , ("_LDST16_ABS_LO12_NC",  284, "S + A                 | Set an LD/ST immediate value to bits [11:1] of X. No overflow check")
    , ("_LDST32_ABS_LO12_NC",  285, "S + A                 | Set the LD/ST immediate value to bits [11:2] of X. No overflow check")
    , ("_LDST64_ABS_LO12_NC",  286, "S + A                 | Set the LD/ST immediate value to bits [11:3] of X. No overflow check")
    , ("_LDST128_ABS_LO12_NC", 299, "S + A                 | Set the LD/ST immediate value to bits [11:4] of X. No overflow check")

    -- Relocations for control-flow instructions - all offsets are a multiple of 4

    , ("_TSTBR14",  279, "S + A - P | Set the immediate field of a TBZ/TBNZ instruction to bits [15:2] of X; check -2^15 <= X < 2^15")
    , ("_CONDBR19", 280, "S + A - P | Set the immediate field of a conditional branch instruction to bits [20:2] of X; check -2^20 <= X< 2^20")
    , ("_JUMP26",   282, "S + A - P | Set a B immediate field to bits [27:2] of X; check that -2^27 <= X < 2^27")
    , ("_CALL26",   283, "S + A - P | Set a CALL immediate field to bits [27:2] of X; check that -2^27 <= X < 2^27")

    -- Group relocations to create a 16, 32, 48, or 64 bit PC-relative offset inline

    , ("_MOVW_PREL_G0",    287, "S + A - P | Set a MOV[NZ]immediate field to bits [15:0] of X")
    , ("_MOVW_PREL_G0_NC", 288, "S + A - P | Set a MOVK immediate field to bits [15:0] of X. No overflow check")
    , ("_MOVW_PREL_G1",    289, "S + A - P | Set a MOV[NZ]immediate field to bits [31:16] of X")
    , ("_MOVW_PREL_G1_NC", 290, "S + A - P | Set a MOVK immediate field to bits [31:16] of X. No overflow check")
    , ("_MOVW_PREL_G2",    291, "S + A - P | Set a MOV[NZ]immediate value to bits [47:32] of X")
    , ("_MOVW_PREL_G2_NC", 292, "S + A - P | Set a MOVK immediate field to bits [47:32] of X. No overflow check")
    , ("_MOVW_PREL_G3",    293, "S + A - P | Set a MOV[NZ]immediate value to bits [63:48] of X")

    -- Group relocations to create a 16, 32, 48, or 64 bit GOT-relative offsets inline

    , ("_MOVW_GOTOFF_G0",    300, "G(GDAT(S + A)) - GOT | Set a MOV[NZ] immediate field to bits [15:0] of X")
    , ("_MOVW_GOTOFF_G0_NC", 301, "G(GDAT(S + A)) - GOT | Set a MOVK immediate field to bits [15:0] of X. No overflow check")
    , ("_MOVW_GOTOFF_G1",    302, "G(GDAT(S + A)) - GOT | Set a MOV[NZ] immediate value to bits [31:16] of X")
    , ("_MOVW_GOTOFF_G1_NC", 303, "G(GDAT(S + A)) - GOT | Set a MOVK immediate value to bits [31:16] of X. No overflow check")
    , ("_MOVW_GOTOFF_G2",    304, "G(GDAT(S + A)) - GOT | Set a MOV[NZ] immediate value to bits [47:32] of X")
    , ("_MOVW_GOTOFF_G2_NC", 305, "G(GDAT(S + A)) - GOT | Set a MOVK immediate value to bits [47:32] of X. No overflow check")
    , ("_MOVW_GOTOFF_G3",    306, "G(GDAT(S + A)) - GOT | Set a MOV[NZ] immediate value to bits [63:48] of X")

    -- GOT-relative data relocations

    , ("_GOTREL64", 307, "S + A - GOT | Set the data to a 64-bit offset relative to the GOT")
    , ("_GOTREL32", 308, "S + A - GOT | Set the data to a 32-bit offset relative to GOT, treated as signed; check that -2^31 <= X < 2^31")

    -- GOT-relative instruction relocations

    , ("_GOT_LD_PREL19",     309, "G(GDAT(S + A))- P             | Set a load-literal immediate field to bits [20:2] of X; check –2^20 <= X < 2^20")
    , ("_LD64_GOTOFF_LO15",  310, "G(GDAT(S + A))- GOT           | Set a LD/ST immediate field to bits [14:3] of X; check that 0 <= X < 2^15 , X&7 = 0")
    , ("_ADR_GOT_PAGE",      311, "Page(G(GDAT(S + A)))- Page(P) | Set the immediate value of an ADRP to bits [32:12] of X; check that –2^32 <= X < 2^32")
    , ("_LD64_GOT_LO12_NC",  312, "G(GDAT(S + A))                | Set the LD/ST immediate field to bits [11:3] of X. No overflow check; check that X&7 = 0")
    , ("_LD64_GOTPAGE_LO15", 313, "G(GDAT(S + A))- Page(GOT)     | Set the LD/ST immediate field to bits [14:3] of X; check that 0 <= X < 2^15, X&7 = 0")

    -- Local Dynamic TLS relocations

    , ("_TLSLD_ADR_PREL21",             517, "G(GLDM(S))) - P            | Set an ADR immediate field to  bits [20:0] of X; check –2^20 <= X < 2^20")
    , ("_TLSLD_ADR_PAGE21",             518, "Page(G(GLDM(S))) - Page(P) | Set an ADRP immediate field to bits [32:12] of X; check –2^32 <= X < 2^32")
    , ("_TLSLD_ADD_LO12_NC",            519, "G(GLDM(S))                 | Set an ADD immediate field to bits [11:0] of X. No overflow check")
    , ("_TLSLD_MOVW_G1",                520, "G(GLDM(S)) - GOT           | Set a MOV[NZ] immediate field to bits [31:16] of X")
    , ("_TLSLD_MOVW_G0_NC",             521, "G(GLDM(S)) - GOT           | Set a MOVK immediate field to bits [15:0] of X. No overflow check")
    , ("_TLSLD_LD_PREL19",              522, "G(GLDM(S)) - P             | Set a load-literal immediate field to bits [20:2] of X; check –2^20 <= X < 2^20")
    , ("_TLSLD_MOVW_DTPREL_G2",         523, "DTPREL(S+A)                | Set a MOV[NZ] immediate field to bits [47:32] of X")
    , ("_TLSLD_MOVW_DTPREL_G1",         524, "DTPREL(S+A)                | Set a MOV[NZ] immediate field to bits [31:16] of X")
    , ("_TLSLD_MOVW_DTPREL_G1_NC",      525, "DTPREL(S+A)                | Set a MOVK immediate field to bits [31:16] of X. No overflow check")
    , ("_TLSLD_MOVW_DTPREL_G0",         526, "DTPREL(S+A)                | Set a MOV[NZ] immediate field to bits [15:0] of X")
    , ("_TLSLD_MOVW_DTPREL_G0_NC",      527, "DTPREL(S+A)                | Set a MOVK immediate field to bits [15:0] of X. No overflow check")
    , ("_TLSLD_ADD_DTPREL_HI12",        528, "DTPREL(S+A)                | Set an ADD immediate field to bits [23:12] of X; check 0 <= X < 2^24")
    , ("_TLSLD_ADD_DTPREL_LO12",        529, "DTPREL(S+A)                | Set an ADD immediate field to bits [11:0] of X; check 0 <= X < 2^12")
    , ("_TLSLD_ADD_DTPREL_LO12_NC",     530, "DTPREL(S+A)                | Set an ADD immediate field to bits [11:0] of X. No overflow check")
    , ("_TLSLD_LDST8_DTPREL_LO12",      531, "DTPREL(S+A)                | Set a LD/ST offset field to bits [11:0] of X; check 0 <= X < 2^12")
    , ("_TLSLD_LDST8_DTPREL_LO12_NC",   532, "DTPREL(S+A)                | Set a LD/ST offset field to bits [11:0] of X. No overflow check")
    , ("_TLSLD_LDST16_DTPREL_LO12",     533, "DTPREL(S+A)                | Set a LD/ST offset field to bits [11:1] of X; check 0 <= X < 2^12")
    , ("_TLSLD_LDST16_DTPREL_LO12_NC",  534, "DTPREL(S+A)                | Set a LD/ST offset field to bits [11:1] of X. No overflow check")
    , ("_TLSLD_LDST32_DTPREL_LO12",     535, "DTPREL(S+A)                | Set a LD/ST offset field to bits [11:2] of X; check 0 <= X < 2^12")
    , ("_TLSLD_LDST32_DTPREL_LO12_NC",  536, "DTPREL(S+A)                | Set a LD/ST offset field to bits [11:2] of X. No overflow check")
    , ("_TLSLD_LDST64_DTPREL_LO12",     537, "DTPREL(S+A)                | Set a LD/ST offset field to bits [11:3] of X; check 0 <= X < 2^12")
    , ("_TLSLD_LDST64_DTPREL_LO12_NC",  538, "DTPREL(S+A)                | Set a LD/ST offset field to bits [11:3] of X. No overflow check")
    , ("_TLSLD_LDST128_DTPREL_LO12",    572, "DTPREL(S+A)                | Set a LD/ST offset field to bits [11:4] of X; check 0 <= X < 2^12")
    , ("_TLSLD_LDST128_DTPREL_LO12_NC", 573, "DTPREL(S+A)                | Set a LD/ST offset field to bits [11:4] of X. No overflow check")

    -- Initial Exec TLS relocations

    , ("_TLSIE_MOVW_GOTTPREL_G1",      539, "G(GTPREL(S+A)) -               | GOT Set a MOV[NZ] immediate field to bits [31:16] of X")
    , ("_TLSIE_MOVW_GOTTPREL_G0_NC",   540, "G(GTPREL(S+A)) -               | GOT Set MOVK immediate to bits [15:0] of X. No overflow check")
    , ("_TLSIE_ADR_GOTTPREL_PAGE21",   541, "Page(G(GTPREL(S+A))) - Page(P) | Set an ADRP immediate field to bits [32:12] of X; check –2^32 <= X < 2^32")
    , ("_TLSIE_LD64_GOTTPREL_LO12_NC", 542, "G(GTPREL(S+A))                 | Set an LD offset field to bits [11:3] of X. No overflow check; check that X&7=0")
    , ("_TLSIE_LD_GOTTPREL_PREL19",    543, "G(GTPREL(S+A)) - P             | Set a load-literal immediate to bits [20:2] of X; check –2^20 <= X < 2^20")

    -- Local Exec TLS relocations

    , ("_TLSLE_MOVW_TPREL_G2",         544, "TPREL(S+A) | Set a MOV[NZ] immediate field to bits [47:32] of X")
    , ("_TLSLE_MOVW_TPREL_G1",         545, "TPREL(S+A) | Set a MOV[NZ] immediate field to bits [31:16] of X")
    , ("_TLSLE_MOVW_TPREL_G1_NC",      546, "TPREL(S+A) | Set a MOVK immediate field to bits [31:16] of X. No overflow check")
    , ("_TLSLE_MOVW_TPREL_G0",         547, "TPREL(S+A) | Set a MOV[NZ] immediate field to bits [15:0] of X")
    , ("_TLSLE_MOVW_TPREL_G0_NC",      548, "TPREL(S+A) | Set a MOVK immediate field to bits [15:0] of X. No overflow check")
    , ("_TLSLE_ADD_TPREL_HI12",        549, "TPREL(S+A) | Set an ADD immediate field to bits [23:12] of X; check 0 <= X < 2^24")
    , ("_TLSLE_ADD_TPREL_LO12",        550, "TPREL(S+A) | Set an ADD immediate field to bits [11:0] of X; check 0 <= X < 2^12")
    , ("_TLSLE_ADD_TPREL_LO12_NC",     551, "TPREL(S+A) | Set an ADD immediate field to bits [11:0] of X. No overflow check")
    , ("_TLSLE_LDST8_TPREL_LO12",      552, "TPREL(S+A) | Set a LD/ST offset field to bits [11:0] of X; check 0 <= X < 2^12")
    , ("_TLSLE_LDST8_TPREL_LO12_NC",   553, "TPREL(S+A) | Set a LD/ST offset field to bits [11:0] of X. No overflow check")
    , ("_TLSLE_LDST16_TPREL_LO12",     554, "TPREL(S+A) | Set a LD/ST offset field to bits [11:1] of X; check 0 <= X < 2^12")
    , ("_TLSLE_LDST16_TPREL_LO12_NC",  555, "TPREL(S+A) | Set a LD/ST offset field to bits [11:1] of X. No overflow check")
    , ("_TLSLE_LDST32_TPREL_LO12",     556, "TPREL(S+A) | Set a LD/ST offset field to bits [11:2] of X; check 0 <= X < 2^12")
    , ("_TLSLE_LDST32_TPREL_LO12_NC",  557, "TPREL(S+A) | Set a LD/ST offset field to bits [11:2] of X. No overflow check")
    , ("_TLSLE_LDST64_TPREL_LO12",     558, "TPREL(S+A) | Set a LD/ST offset field to bits [11:3] of X; check 0 <= X < 2^12")
    , ("_TLSLE_LDST64_TPREL_LO12_NC",  559, "TPREL(S+A) | Set a LD/ST offset field to bits [11:3] of X. No overflow check")
    , ("_TLSLE_LDST128_TPREL_LO12",    570, "TPREL(S+A) | Set a LD/ST offset field to bits [11:4] of X; check 0 <= X < 2^12")
    , ("_TLSLE_LDST128_TPREL_LO12_NC", 571, "TPREL(S+A) | Set a LD/ST offset field to bits [11:4] of X. No overflow check")

    -- TLS descriptor relocations

    , ("_TLSDESC_LD_PREL19",  560, "G(GTLSDESC(S+A)) - P             | Set a load-literal immediate to bits [20:2]; check -2^20 <= X < 2^20 ; check X & 3 = 0")
    , ("_TLSDESC_ADR_PREL21", 561, "G(GTLSDESC(S+A)) - P             | Set an ADR immediate field to bits [20:0]; check -2^20 <= X < 2^20")
    , ("_TLSDESC_ADR_PAGE21", 562, "Page(G(GTLSDESC(S+A))) - Page(P) | Set an ADRP immediate field to bits [32:12] of X; check -2^32 <= X < 2^32")
    , ("_TLSDESC_LD64_LO12",  563, "G(GTLSDESC(S+A))                 | Set an LD offset field to bits [11:3] of X. No overflow check; check X & 7 = 0")
    , ("_TLSDESC_ADD_LO12",   564, "G(GTLSDESC(S+A))                 | Set an ADD immediate field to bits [11:0] of X. No overflow check")
    , ("_TLSDESC_OFF_G1",     565, "G(GTLSDESC(S+A)) - GOT           | Set a MOV[NZ] immediate field to bits [31:16] of X; check -2^32 <= X < 2^32")
    , ("_TLSDESC_OFF_G0_NC",  566, "G(GTLSDESC(S+A)) - GOT           | Set a MOVK immediate field to bits [15:0] of X. No overflow check")
    , ("_TLSDESC_LDR",        567, "None                             | For relaxation only. Must be used to identify an LDR instruction which loads the TLS descriptor function pointer for S + A if it has no other relocation")
    , ("_TLSDESC_ADD",        568, "None                             | For relaxation only. Must be used to identify an ADD instruction which computes the address of the TLS Descriptor for S + A if it has no other relocation")
    , ("_TLSDESC_CALL",       569, "None                             | For relaxation only. Must be used to identify a BLR instruction which performs an indirect call to the TLS descriptor function for S + A")

    -- Dynamic relocations

    , ("_COPY",       1024,  "")
    , ("_GLOB_DAT",   1025,  "S + A")
    , ("_JUMP_SLOT",  1026,  "S + A")
    , ("_RELATIVE",   1027,  "Delta(S + A)")
    , ("_TLS_DTPMOD", 1028,  "DTPREL(S + A)")
    , ("_TLS_DTPREL", 1029,  "LDM(S)")
    , ("_TLS_TPREL",  1030,  "TPREL(S + A)")
    , ("_TLSDESC",    1031,  "TLSDESC(S + A)")
    , ("_IRELATIVE",  1032,  "Indirect(Delta(S) + A)")

    ])
