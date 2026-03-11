# NanoID generator, library and CLI ![CI](https://github.com/MichelBoucey/NanoID/actions/workflows/haskell-ci.yml/badge.svg) [![Hackage](https://img.shields.io/hackage/v/NanoID.svg)](https://hackage.haskell.org/package/NanoID)

```
[user@box ~] $ nanoid -h
nanoid v3.4.1, (c) Michel Boucey 2022-2025

Usage: nanoid [-a|--alphabet ARG] [-l|--length ARG] [-p|--password] 
              [-q|--quantity ARG] [-n|--newline] [-v|--version]

  NanoID generator

Available options:
  -a,--alphabet ARG        Use an alternative alphabet (ascii chars only)
  -l,--length ARG          Get a shorter NanoID (Default length is 21 chars)
  -p,--password            Special password generation
  -q,--quantity ARG        Quantity of NanoID to generate
  -n,--newline             Do not output the trailing newline
  -v,--version             Show version
  -h,--help                Show this help text
```

