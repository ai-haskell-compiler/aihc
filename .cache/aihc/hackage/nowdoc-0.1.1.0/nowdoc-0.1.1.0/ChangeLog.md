# Changelog for nowdoc

## Unreleased changes

* add QuasiQuoter txtfile
* add QuasiQuoter binfile

### txtfile

QuasiQuoter txtfile incerts file contents as string without transformation.
It read file as text file (with default encoding on your system).

```hs
main :: IO ()
main = putStr [txtfile|foo.txt|]
```

### binfile

QuasiQuoter txtfile incerts file contents as string without transformation.
It read file as binary file.

```hs
main :: IO ()
main = putStr [binfile|foo.dat|]
```
