[![Build Status](https://secure.travis-ci.org/diagrams/diagrams-postscript.png?branch=master)](http://travis-ci.org/diagrams/diagrams-postscript)

_diagrams-postscript_ is a rendering backend for [diagrams], a powerful,
flexible, declarative domain-specific language for creating vector graphics,
using the [Haskell programming language][haskell].

[diagrams]: http://projects.haskell.org/diagrams/
[haskell]: http://www.haskell.org/haskellwiki/Haskell

_diagrams-postscript_ has a simple rendering engine that outputs
[PostScript™ language][postscript] files and is an officially supported backend
for diagrams.

[postscript]: http://www.adobe.com/products/postscript/pdfs/PLRM.pdf

# Installation

```
cabal update && cabal install diagrams-postscript
```

# Basic usage

A simple example that uses _diagrams-postscript_ to draw a blue circle:

```haskell
import Diagrams.Prelude
import Diagrams.Backend.Postscript.CmdLine

d = circle 1 # fc blue

main = mainWith (pad 1.1 d)
```

Save this to file named `Circle.hs` and compile it:

```
ghc --make Circle.hs
```

This will generate an executable which, when run, outputs a blue
circle to some file. Run the executable with the `--help` option to
find out more about how to call it.

```
$ ./Circle --help
./Circle

Usage: ./Circle [-w|--width WIDTH] [-h|--height HEIGHT] [-o|--output OUTPUT]
  Command-line diagram generation.

Available options:
  -?,--help                Show this help text
  -w,--width WIDTH         Desired WIDTH of the output image
  -h,--height HEIGHT       Desired HEIGHT of the output image
  -o,--output OUTPUT       OUTPUT file
```

The output type will be automatically determined from the file
extension.  Currently EPS and PS are supported.

```
$ ./Circle -o circle.eps -w 400
```

The command above generates an EPS file with a width of 400pt.

# Advanced usage

Instead of just creating a standalone executable, the postscript backend
can also be called from within a larger program.  For more
information, see the Diagram.Backend.Postscript module.

--

PostScript™ is a trademark of Adobe.
