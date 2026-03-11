# Haskell client library for Moss

![GitHub](https://img.shields.io/github/license/mbg/moss)
![CI](https://github.com/mbg/moss/workflows/CI/badge.svg)
![stackage-nightly](https://github.com/mbg/moss/workflows/stackage-nightly/badge.svg)
[![Hackage](https://img.shields.io/hackage/v/moss)](https://hackage.haskell.org/package/moss)

Haskell client library for [Moss](https://theory.stanford.edu/~aiken/moss/) which is an online service for checking for code similarity, primarily aimed at detecting plagiarism. 

## Example

In order to use Moss, you need to register on the [Moss website](https://theory.stanford.edu/~aiken/moss/). Once you have your access token, using the library is fairly easy:

```haskell
import Stanford.Moss

cfg :: MossCfg
cfg = defaultMossCfg{
    mossLanguage = Haskell,
    mossUser = "[YOUR ACCESS TOKEN HERE]"
}

main :: IO ()
main = do
    url <- withMoss cfg $ do
        addBaseFile "Skeleton" "Skeleton.hs"
        addFile "StudentA" "StudentA.hs"
        addFile "StudentB" "StudentB.hs"
        query "Test"
    print url
```

This example establishes a connection with Moss using Haskell as the selected programming language. We assume that some skeleton code (`Skeleton.hs`) has been made available to students and relevant parts from that file should be ignored for plagiarism checking. The example then uploads two students' submissions (`StudentA.hs` and `StudentB.hs`) before telling Moss to run the plagiarism check with `query` which eventually returns a URL to the results.
