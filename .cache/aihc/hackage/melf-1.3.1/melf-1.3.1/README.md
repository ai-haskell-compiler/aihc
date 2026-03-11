# melf

> A [Haskell](https://www.haskell.org/) library to parse/serialize
> Executable and Linkable Format ([ELF](https://en.wikipedia.org/wiki/Executable_and_Linkable_Format))
> files.

## Parsing the header and table entries

Module
[`Data.Elf.Headers`](https://hackage.haskell.org/package/melf-1.3.0/docs/Data-Elf-Headers.html)
implements parsing and serialization of the ELF file header and the entries of section and segment tables.

ELF files come in two flavors: 64-bit and 32-bit.
To differentiate between them type
[`ElfClass`](https://hackage.haskell.org/package/melf-1.3.0/docs/Data-Elf-Headers.html#t:ElfClass)
is defined:

``` Haskell
data ElfClass
    = ELFCLASS32 -- ^ 32-bit ELF format
    | ELFCLASS64 -- ^ 64-bit ELF format
    deriving (Eq, Show)
```

[Singleton](https://blog.jle.im/entry/introduction-to-singletons-1.html)
types for `ElfClass` are also defined:

``` Haskell
-- | Singletons for ElfClass
data SingElfClass :: ElfClass -> Type where
    SELFCLASS32 :: SingElfClass 'ELFCLASS32  -- ^ Singleton for `ELFCLASS32`
    SELFCLASS64 :: SingElfClass 'ELFCLASS64  -- ^ Singleton for `ELFCLASS64`
```

Some fields of the header and table entries have different bitwidth for 64-bit and 32-bit files.
So the type
[`WordXX a`](https://hackage.haskell.org/package/melf-1.3.0/docs/Data-Elf-Headers.html#t:WordXX)
was borrowed from the `data-elf` package:

``` Haskell
-- | @SingElfClassI a@ is defined for each constructor of `ElfClass`.
--   It defines @WordXX a@, which is `Word32` for `ELFCLASS32` and `Word64` for `ELFCLASS64`.
--   Also it defines singletons for each of the `ElfClass` type.
class ( Typeable c
      , Typeable (WordXX c)
      , Data (WordXX c)
      , Show (WordXX c)
      , Read (WordXX c)
      , Eq (WordXX c)
      , Ord (WordXX c)
      , Bounded (WordXX c)
      , Enum (WordXX c)
      , Num (WordXX c)
      , Integral (WordXX c)
      , Real (WordXX c)
      , Bits (WordXX c)
      , FiniteBits (WordXX c)
      , Binary (Be (WordXX c))
      , Binary (Le (WordXX c))
      ) => SingElfClassI (c :: ElfClass) where
    type WordXX c = r | r -> c
    singElfClass :: SingElfClass c

instance SingElfClassI 'ELFCLASS32 where
    type WordXX 'ELFCLASS32 = Word32
    singElfClass = SELFCLASS32

instance SingElfClassI 'ELFCLASS64 where
    type WordXX 'ELFCLASS64 = Word64
    singElfClass = SELFCLASS64
```

The header of the ELF file is represented with the type
[`HeaderXX a`](https://hackage.haskell.org/package/melf-1.3.0/docs/Data-Elf-Headers.html#t:HeaderXX):

``` Haskell
-- | Parsed ELF header
data HeaderXX c =
    HeaderXX
        { hData       :: ElfData    -- ^ Data encoding (big- or little-endian)
        , hOSABI      :: ElfOSABI   -- ^ OS/ABI identification
        , hABIVersion :: Word8      -- ^ ABI version
        , hType       :: ElfType    -- ^ Object file type
        , hMachine    :: ElfMachine -- ^ Machine type
        , hEntry      :: WordXX c   -- ^ Entry point address
        , hPhOff      :: WordXX c   -- ^ Program header offset
        , hShOff      :: WordXX c   -- ^ Section header offset
        , hFlags      :: Word32     -- ^ Processor-specific flags
        , hPhEntSize  :: Word16     -- ^ Size of program header entry
        , hPhNum      :: Word16     -- ^ Number of program header entries
        , hShEntSize  :: Word16     -- ^ Size of section header entry
        , hShNum      :: Word16     -- ^ Number of section header entries
        , hShStrNdx   :: ElfSectionIndex -- ^ Section name string table index
        }
```

So we have two types `HeaderXX 'ELFCLASS64` and `HeaderXX 'ELFCLASS32`.
To be able to work with headers uniformly the type
[`Header`](https://hackage.haskell.org/package/melf-1.3.0/docs/Data-Elf-Headers.html#t:Header)
was introduced:

``` Haskell
-- | Header is a sigma type where the first entry defines the type of the second one
data Header = forall a . Header (SingElfClass a) (HeaderXX a)
```

`Header` is a pair.
The first element is an object of the type `SingElfClass` defining the width of the word.
The second element is `HeaderXX` parametrized with the first element (i. e. Σ-type from
the languages with dependent types).

`Header` is an instance of the
[`Binary`](https://hackage.haskell.org/package/binary-0.10.0.0/docs/Data-Binary.html#t:Binary)
class.

So given a lazy bytestring containing large enough initial part of ELF file one can get the header of
that file with a function like this:

``` Haskell
withHeader ::                        BSL.ByteString ->
    (forall a . SingElfClassI a => HeaderXX a -> b) -> Either String b
withHeader bs f =
    case decodeOrFail bs of
        Left (_, _, err) -> Left err
        Right (_, _, (Header sing hxx)) -> Right $ withSingElfClassI sing f hxx
```

The function
[`decodeOrFail`](https://hackage.haskell.org/package/binary-0.10.0.0/docs/Data-Binary.html#v:decodeOrFail)
is defined in the package
[`binary`](https://hackage.haskell.org/package/binary).
The function
[`withSingElfClassI`](https://hackage.haskell.org/package/melf-1.3.0/docs/Data-Elf-Headers.html#v:withSingElfClassI)
creates a context with an implicit word width available and looks like
[`withSingI`](https://hackage.haskell.org/package/singletons-3.0.1/docs/Data-Singletons.html#v:withSingI):


``` Haskell
-- | Convenience function for creating a context with an implicit singleton available.
withSingElfClassI :: SingElfClass c -> (SingElfClassI c => r) -> r
withSingElfClassI SELFCLASS64 x = x
withSingElfClassI SELFCLASS32 x = x
```

The module `Data.Elf.Headers` also defines the types
[`SectionXX`](https://hackage.haskell.org/package/melf-1.3.0/docs/Data-Elf-Headers.html#t:SectionXX),
[`SegmentXX`](https://hackage.haskell.org/package/melf-1.3.0/docs/Data-Elf-Headers.html#t:SegmentXX) and
[`SymbolXX`](https://hackage.haskell.org/package/melf-1.3.0/docs/Data-Elf-Headers.html#t:SymbolXX)
for the elements of section, segment and symbol tables.

## Parsing the whole ELF file

The module
[`Data.Elf`](https://hackage.haskell.org/package/melf-1.3.0/docs/Data-Elf.html)
implements parsing and serialization of the whole ELF files.
To parse ELF file it reads ELF header, section table and segment table and uses that data to create
a list of type
[`ElfListXX`](https://hackage.haskell.org/package/melf-1.3.0/docs/Data-Elf.html#t:ElfListXX)
of elements of the type
[`ElfXX`](https://hackage.haskell.org/package/melf-1.3.0/docs/Data-Elf.html#t:ElfXX)
representing the recursive structure of the ELF file.
It also restores section names from the the string table indexes.
That results in creating an object of type
[`Elf`](https://hackage.haskell.org/package/melf-1.3.0/docs/Data-Elf.html#t:Elf):

``` Haskell
-- | `Elf` is a forrest of trees of type `ElfXX`.
-- Trees are composed of `ElfXX` nodes, `ElfSegment` can contain subtrees
data ElfNodeType = Header | SectionTable | SegmentTable | Section | Segment | RawData | RawAlign

-- | List of ELF nodes.
data ElfListXX c where
    ElfListCons :: ElfXX t c -> ElfListXX c -> ElfListXX c
    ElfListNull :: ElfListXX c

-- | Elf is a sigma type where the first entry defines the type of the second one
data Elf = forall a . Elf (SingElfClass a) (ElfListXX a)

-- | Section data may contain a string table.
-- If a section contains a string table with section names, the data
-- for such a section is generated and `esData` should contain `ElfSectionDataStringTable`
data ElfSectionData c
    = ElfSectionData                -- ^ Regular section data
        { esdData :: BSL.ByteString -- ^ The content of the section
        }
    | ElfSectionDataStringTable     -- ^ Section data will be generated from section names
    | ElfSectionDataNoBits          -- ^ SHT_NOBITS uninitialized section data: section has size but no content
        { esdSize :: WordXX c       -- ^ Size of the section
        }

-- | The type of node that defines Elf structure.
data ElfXX t c where
    ElfHeader ::
        { ehData       :: ElfData    -- ^ Data encoding (big- or little-endian)
        , ehOSABI      :: ElfOSABI   -- ^ OS/ABI identification
        , ehABIVersion :: Word8      -- ^ ABI version
        , ehType       :: ElfType    -- ^ Object file type
        , ehMachine    :: ElfMachine -- ^ Machine type
        , ehEntry      :: WordXX c   -- ^ Entry point address
        , ehFlags      :: Word32     -- ^ Processor-specific flags
        } -> ElfXX 'Header c
    ElfSectionTable :: ElfXX 'SectionTable c
    ElfSegmentTable :: ElfXX 'SegmentTable c
    ElfSection ::
        { esName      :: String         -- ^ Section name (NB: string, not offset in the string table)
        , esType      :: ElfSectionType -- ^ Section type
        , esFlags     :: ElfSectionFlag -- ^ Section attributes
        , esAddr      :: WordXX c       -- ^ Virtual address in memory
        , esAddrAlign :: WordXX c       -- ^ Address alignment boundary
        , esEntSize   :: WordXX c       -- ^ Size of entries, if section has table
        , esN         :: ElfSectionIndex -- ^ Section number
        , esInfo      :: Word32         -- ^ Miscellaneous information
        , esLink      :: Word32         -- ^ Link to other section
        , esData      :: ElfSectionData c -- ^ The content of the section
        } -> ElfXX 'Section c
    ElfSegment ::
        { epType       :: ElfSegmentType -- ^ Type of segment
        , epFlags      :: ElfSegmentFlag -- ^ Segment attributes
        , epVirtAddr   :: WordXX c       -- ^ Virtual address in memory
        , epPhysAddr   :: WordXX c       -- ^ Physical address
        , epAddMemSize :: WordXX c       -- ^ Add this amount of memory after the section when the section is loaded to memory by execution system.
                                         --   Or, in other words this is how much `pMemSize` is bigger than `pFileSize`
        , epAlign      :: WordXX c       -- ^ Alignment of segment
        , epData       :: ElfListXX c    -- ^ Content of the segment
        } -> ElfXX 'Segment c
    -- | Some ELF files (some executables) don't bother to define
    -- sections for linking and have just raw data in segments.
    ElfRawData ::
        { edData :: BSL.ByteString -- ^ Raw data in ELF file
        } -> ElfXX 'RawData c
    -- | Align the next data in the ELF file.
    -- The offset of the next data in the ELF file
    -- will be the minimal @x@ such that
    -- @x mod eaAlign == eaOffset mod eaAlign @
    ElfRawAlign ::
        { eaOffset :: WordXX c -- ^ Align value
        , eaAlign  :: WordXX c -- ^ Align module
        } -> ElfXX 'RawAlign c

infixr 9 ~:

-- | Helper for `ElfListCons`
(~:) :: ElfXX t a -> ElfListXX a -> ElfListXX a
(~:) = ElfListCons
```

Not each object of that type can be serialized.

  * Constructor `ElfSection` still has a section number.
    It is required as the symbol table and some other structures
    refer to the sections by theirs indexes.
    So the section indexes should be consecutive integers starting from 1.
    Section with index 0 is always empty and is created by the library.

  * There should be a single `ElfHeader`.  It should be the first nonempty node of the tree.

  * If there exists at least one node `ElfSection` then there should exist exactly one
    node `ElfSectionTable` and exactly one section that has `ElfSectionDataStringTable` as the value
    of its `esData` field (the string table for the names of sections).

  * If there exists at least one node `ElfSegment` then there should exist exactly one
    node `ElfSegmentTable`.

Correctly composed ELF object can be serialized with the function
[`serializeElf`](https://hackage.haskell.org/package/melf-1.3.0/docs/Data-Elf.html#v:serializeElf)
and parsed with the function
[`parseElf`](https://hackage.haskell.org/package/melf-1.3.0/docs/Data-Elf.html#v:parseElf):

``` Haskell
serializeElf :: MonadThrow m => Elf -> m ByteString
parseElf :: MonadCatch m => ByteString -> m Elf
```

`ELF` is not an instance of the class `Binary` because
[`PutM`](https://hackage.haskell.org/package/binary-0.10.0.0/docs/Data-Binary-Put.html#t:PutM)
is not an instance of the class `MonadFail`.

## Generation of object files

To create machine code that is used in the examples a pair of modules were created.
The module
[`AsmAArch64`](https://github.com/aleksey-makarov/melf/blob/v1.2.0/examples/AsmAArch64.hs)
provides a DSL embedded in Haskell.
This DSL is a kind of assembler language for the AArch64 platform.
It exports some primitives to generate machine instructions and organize machine code.
It also exports function `assemble` that consumes the monad composed of those primitives and
produces an object of the type `Elf`:

``` Haskell
assemble :: MonadCatch m => StateT CodeState m () -> m Elf
```

The idea was inspired by the article
["Monads to Machine Code"](https://www.stephendiehl.com/posts/monads_machine_code.html)
by Stephen Diehl.
Detailed description of this module is available in russian:
[README_ru.md](https://github.com/aleksey-makarov/melf/blob/v1.2.0/examples/README_ru.md) (outdated).

The module
[`HelloWorld`](https://github.com/aleksey-makarov/melf/blob/v1.2.0/examples/HelloWorld.hs)
uses primitives from `AsmAArch64` to compose relocatable executable code that uses system calls
to output a "Hello World!" message into standard output and exit:

``` Haskell
helloWorld :: MonadCatch m => StateT CodeState m ()
```

Function `assemble` uses the `melf` library to generate an object file:

``` Haskell
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
```

It runs the `State` monad that was passed as an argument.
As a result the final state of `CodeState` includes all the data neсessary to produce ELF file, in
particular:

  * `txt` refers to the content of the `.text` section,
  * `symbolTableData` refers to the content of the symbol table section,
  * `stringTableData` refers to the content of the string table section linked to the symbol table.

Names with `SecN` suffixes (`textSecN`, `shstrtabSecN`, `symtabSecN`, `strtabSecN`)
are predefined section numbers that conform to the conditions stated above.

For the sake of simplicity external symbol resolution and data section allocation were not implemented.
It requires implementation of relocation tables.  On the other hand, the resulting code
is position-independent.

Use this module to produce object file and try to link it:

```
[nix-shell:examples]$ ghci 
GHCi, version 8.10.7: https://www.haskell.org/ghc/  :? for help
Prelude> :l AsmAArch64.hs HelloWorld.hs 
[1 of 2] Compiling AsmAArch64       ( AsmAArch64.hs, interpreted )
[2 of 2] Compiling HelloWorld       ( HelloWorld.hs, interpreted )
Ok, two modules loaded.
*AsmAArch64> import HelloWorld
*AsmAArch64 HelloWorld> elf <- assemble helloWorld
*AsmAArch64 HelloWorld> bs <- serializeElf elf
*AsmAArch64 HelloWorld> BSL.writeFile "helloWorld.o" bs
*AsmAArch64 HelloWorld> 
Leaving GHCi.

[nix-shell:examples]$ aarch64-unknown-linux-gnu-gcc -nostdlib helloWorld.o -o helloWorld

[nix-shell:examples]$ 
```

The linker accepted the object file.  Try to run the result:

```
[nix-shell:examples]$ qemu-aarch64 helloWorld
Hello World!

[nix-shell:examples]$ 
```

It works.

## Generation of executable files

The module
[`DummyLd`](https://github.com/aleksey-makarov/melf/blob/v1.2.0/examples/DummyLd.hs)
uses the section `.text` of object file to create an executable file.
Code relocation and symbol resolution is not implemented so that procedure works only
for position-independent code that does not refer to external translation units,
for example, it works with the code described above.

Function `dummyLd` consumes an object of the type `Elf` and finds a section `.text`
(using [`elfFindSectionByName`](https://hackage.haskell.org/package/melf-1.3.0/docs/Data-Elf.html#v:elfFindSectionByName))
and header
(using [`elfFindHeader`](https://hackage.haskell.org/package/melf-1.3.0/docs/Data-Elf.html#v:elfFindHeader))
in it.
Then the header type is changed to `ET_EXEC`, the address of the first executable instruction is specified and
a loadable segment containing the header and the content of `.text` is formed:

``` Haskell
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
                            , ehEntry = mcAddress + headerSize (fromSing $ sing @a)
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
```

Try to use this code to produce executable file without GNU linker:

```
[nix-shell:examples]$ ghci
GHCi, version 8.10.7: https://www.haskell.org/ghc/  :? for help
Prelude> :l DummyLd.hs
[1 of 1] Compiling DummyLd          ( DummyLd.hs, interpreted )
Ok, one module loaded.
*DummyLd> import Data.ByteString.Lazy as BSL
*DummyLd BSL> i <- BSL.readFile "helloWorld.o"
*DummyLd BSL> elf <- parseElf i
*DummyLd BSL> elf' <- dummyLd elf
*DummyLd BSL> o <- serializeElf elf'
*DummyLd BSL> BSL.writeFile "helloWorld2" o
*DummyLd BSL> 
Leaving GHCi.

[nix-shell:examples]$ chmod +x helloWorld2

[nix-shell:examples]$ qemu-aarch64 helloWorld2
Hello World!

[nix-shell:examples]$ 
```

It works.

## Related work

- [elf](https://github.com/wangbj/elf)
- [data-elf](https://github.com/mvv/data-elf)

These just parse/serialize ELF header and table entries but not the whole ELF files.

## History

For the early history look at the branch "[amakarov](https://github.com/aleksey-makarov/elf/tree/amakarov)" of
the my copy of the [elf](https://github.com/aleksey-makarov/elf) repo.

## Tests

Test data is committed with [git-lfs](https://git-lfs.github.com/).
Only testdata/orig/* tests are included to hackage distributive to keep the tarball size small.

## License

BSD 3-Clause License (c) Aleksey Makarov
