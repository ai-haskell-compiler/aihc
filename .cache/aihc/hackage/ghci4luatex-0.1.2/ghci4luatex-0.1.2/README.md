# ghci4luatex : a GHCi session in LuaTeX

[![Haskell](https://img.shields.io/badge/language-Haskell-orange.svg)](https://haskell.org) [![BSD3 License](https://img.shields.io/badge/license-BSD3-blue.svg)](https://github.com/AliceRixte/ghci4luatex/LICENSE) [![Hackage](https://img.shields.io/hackage/v/ghci4luatex.svg)](https://hackage.haskell.org/package/ghci4luatex) [![Nightly](https://www.stackage.org/package/ghci4luatex/badge/nightly)](https://www.stackage.org/nightly/package/ghci4luatex) [![LTS](https://www.stackage.org/package/ghci4luatex/badge/lts)](https://www.stackage.org/lts/package/ghci4luatex) 

Run a GHCi session within a LaTeX document :

* The `ghci` environment evaluates Haskell code without printing anything :

```latex
\begin{ghci}
x :: Int
x = 4

y :: Int
y = 5
\end{ghci}
```

* The `hask` command evaluates any GHCi command and prints in Haskell what GHCi printed :

```latex
The sum of $x$ and $y$ when $x = \hask{x}$ and $y = \hask{y}$ is $\hask{x + y}$.
```


## Documentation

You can find the full pdf documentation in [here](./doc/ghci-doc.pdf)

## Quick start


1. Install `haskell` and `cabal` or `stack`

2. Install `ghci4luatex`by running either

```
cabal install ghci4luatex
```

or

```
stack install ghci4luatex
```

3. Copy `ghci.sty` and `dkjson.lua` in the folder containing a `main.tex` file with the following content :

``` latex
\documentclass{article}

\usepackage{ghci}

\begin{document}

\begin{ghci}
x :: Int
x = 5

y :: Int
y = 6
\end{ghci}

The sum of $x$ and $y$ when $x = \hask{x}$ and $y = \hask{y}$ is $\hask{x + y}$.

\end{document}
```

4. Within that folder, run the `ghci4luatex` server :

```
ghci4luatex
```

5. Open another shell and compile with `luatex` :

```
latexmk -shell-escape -lualatex main.tex
```

## Use any Haskell library

### HaTeX

```latex

\begin{ghci}
:set -XOverloadedStrings
\end{ghci}

\begin{ghci}
import Text.LaTeX
import Text.LaTeX.Base.Pretty

printTex = putStrLn . prettyLaTeX
\end{ghci}

\hask{printTex (section "A section using HaTeX")}
```

### Diagrams

```latex

\usepackage{svg}

\begin{ghci}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Prelude hiding (section)
import Diagrams.Backend.SVG

myDia = circle 1 # fc green
\end{ghci}

\begin{ghci}
  renderSVG "myDia.svg" (dims2D 400 300) myDia
\end{ghci}

\begin{figure}[h]
  \centering
  \includesvg[width=0.2\textwidth]{myDia}
  \caption{A circle using Diagrams}
\end{figure}
```

## Workflow with `lhs2tex` in Visual Studio Code with LaTeX workshop

In this repository, you will find an [example](./example/README.md) that contains a [Makefile](./example/Makefile).

You can take inspiration from this to use `make` in a LateX Workshop receipe :

1. Install the [LaTeX Workshop](https://marketplace.visualstudio.com/items?itemName=James-Yu.latex-workshop) extension.
2. In `settings.json` , add the following
```json
"latex-workshop.latex.recipes": [
        {
            "name": "ghci4luatex",
            "tools": [
                "mklatex"
            ]
        }
    ],
"latex-workshop.latex.outDir": "./build/",
"latex-workshop.latex.tools": [
        {
            "name": "mklatex",
            "command": "make",
            "args": [
                "latex",
                "main=%DOCFILE%"
            ],
            "env": {}
        }
    ],
```

