# Revision history for file-embed-lzma

## 0.1

- The default interface uses TTH (`Code`),
  the previous untyped TH interface is in the `FileEmbedLzma.Untyped` module.
- Support GHC-9.0.2...9.10.1

## 0.0.1

- Use `bytesPrimL` when available
- Drop GHC-7.x support.
  We no more depend on `th-lift-instances`,
  and use `TemplateHaskellQuotes` instead of `TemplateHaskell` for the lib
  itself.
- `embedRecursiveDir` sorts files, so the generated code is more deterministic

## 0

- First version. Released on an unsuspecting world.
