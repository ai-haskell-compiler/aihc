# markup-parse

[![img](https://img.shields.io/hackage/v/markup-parse.svg)](https://hackage.haskell.org/package/markup-parse)
[![img](https://github.com/tonyday567/markup-parse/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/tonyday567/markup-parse/actions/workflows/haskell-ci.yml)

`markup-parse` parses and prints a subset of common XML & HTML structured data, from and to strict bytestrings


# Development

    :r
    :set -Wno-type-defaults
    :set -Wno-name-shadowing
    :set -XOverloadedStrings
    :set -XTemplateHaskell
    :set -XQuasiQuotes
    import Control.Monad
    import MarkupParse
    import MarkupParse.Internal.FlatParse
    import Data.ByteString qualified as B
    import Data.ByteString.Char8 qualified as C
    import Data.Map.Strict qualified as Map
    import Data.Function
    import FlatParse.Basic hiding (take)
    import Data.String.Interpolate
    import Control.Monad
    bs <- B.readFile "other/line.svg"
    C.length bs

    [1 of 2] Compiling MarkupParse.Internal.FlatParse ( src/MarkupParse/Internal/FlatParse.hs, interpreted ) [Flags changed]
    Ok, two modules reloaded.
    7554


# Main Pipeline types

    :t tokenize Html
    :t gather Html
    :t normalize
    :t degather
    :t detokenize Html
    :t tokenize Html >=> gather Html >=> (normalize >>> pure) >=> degather Html >=> (fmap (detokenize Html) >>> pure)

    tokenize Html :: ByteString -> Warn [Token]
    gather Html :: [Token] -> Warn Markup
    normalize :: Markup -> Markup
    degather :: Standard -> Markup -> Warn [Token]
    detokenize Html :: Token -> ByteString
    tokenize Html >=> gather Html >=> (normalize >>> pure) >=> degather Html >=> (fmap (detokenize Html) >>> pure)
      :: ByteString -> These [MarkupWarning] [ByteString]


## tests


### Round trip equality

    m = markup_ Xml bs
    m == (markup_ Xml $ markdown_ Compact Xml m)

    True


### wiki diff test debug

    bs <- B.readFile "other/Parsing - Wikipedia.html"
    m = markup_ Html bs
    m == (markup_ Html $ markdown_ Compact Html m)

    True


# Reference

[HTML Standard](https://html.spec.whatwg.org/#toc-syntax)

[Are (non-void) self-closing tags valid in HTML5? - Stack Overflow](https://stackoverflow.com/questions/3558119/are-non-void-self-closing-tags-valid-in-html5)

[Extensible Markup Language (XML) 1.0 (Fifth Edition)](https://www.w3.org/TR/xml/)


## Prior Art

[html-parse](https://hackage.haskell.org/package/html-parse) is an attoparsec-based parser for HTML. The HTML parsing here has referenced this parsing logic for HTML elements.

[blaze-markup](https://hackage.haskell.org/package/blaze-markup) & [lucid](https://hackage.haskell.org/package/lucid) are HTML DSLs and printers, but not parsers.

[xeno](https://hackage.haskell.org/package/xeno) is an &ldquo;event-based&rdquo; XML parser with a more complicated base markup type.

[XMLParser](https://hackage.haskell.org/package/XMLParser) & [hexml](https://hackage.haskell.org/package/hexml) are parsec-based parsers.


# Performance


## perf

The [perf](https://hackage.haskell.org/package/perf) library has been used to measure performance of the library.

    Running 1 benchmarks...
    Benchmark markup-parse-speed: RUNNING...
    label1          label2          old result      new result      change
    
    gather          time            9.33e4          7.73e4          improvement
    html-parse tokenstime            1.21e6          1.17e6
    html-parse tree time            6.61e4          7.59e4          slightly-degraded
    markdown        time            3.75e5          3.92e5
    markup          time            4.72e5          5.20e5          slightly-degraded
    normalize       time            2.80e5          3.04e5          slightly-degraded
    tokenize        time            8.59e5          8.67e5
    Benchmark markup-parse-speed: FINISH


## Profiling

Profiling is used to aid library development:

    cabal configure --enable-library-profiling --enable-executable-profiling -fprof-auto -fprof --write-ghc-environment-files=always --enable-benchmarks -O2

cabal.project.local

    write-ghc-environment-files: always
    ignore-project: False
    flags: +prof +prof-auto
    library-profiling: True
    executable-profiling: True

Profiling slow the main functions significantly:

    ./app/speed -n 1000 --best -c +RTS -s -p -hc -l -RTS
    label1              label2              old_result          new_result          status
    
    gather              time                2.08e4              3.01e4              degraded
    html-parse tokens   time                4.70e5              1.72e6              degraded
    html-parse tree     time                2.30e4              3.85e4              degraded
    markdown            time                3.51e5              5.70e5              degraded
    markup              time                2.10e5              1.05e6              degraded
    normalize           time                8.43e4              1.90e5              degraded
    tokenize            time                1.94e5              1.02e6              degraded
       4,520,989,296 bytes allocated in the heap
       2,668,887,592 bytes copied during GC
         287,122,272 bytes maximum residency (21 sample(s))
           1,572,000 bytes maximum slop
                 560 MiB total memory in use (0 MiB lost due to fragmentation)
    
                                         Tot time (elapsed)  Avg pause  Max pause
      Gen  0      1073 colls,     0 par    0.471s   0.479s     0.0004s    0.0024s
      Gen  1        21 colls,     0 par    2.428s   2.575s     0.1226s    0.3303s
    
      INIT    time    0.007s  (  0.008s elapsed)
      MUT     time    2.142s  (  1.945s elapsed)
      GC      time    1.904s  (  2.071s elapsed)
      RP      time    0.000s  (  0.000s elapsed)
      PROF    time    0.995s  (  0.982s elapsed)
      EXIT    time    0.026s  (  0.000s elapsed)
      Total   time    5.074s  (  5.006s elapsed)
    
      %GC     time       0.0%  (0.0% elapsed)
    
      Alloc rate    2,110,654,040 bytes per MUT second
    
      Productivity  61.8% of total user, 58.5% of total elapsed

