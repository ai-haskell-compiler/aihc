-----------------------------------------------------------------------------
-- |
-- Module      :  Lentil.Parse.Syntaxes
-- Copyright   :  © 2015 Francesco Ariis, Tomislav
-- License     :  GPLv3 (see the LICENSE file)
--
-- Languages descriptors
-----------------------------------------------------------------------------


module Lentil.Parse.Syntaxes where

import Lentil.Parse.Source
import Lentil.Types

import Text.Megaparsec
import Prelude

import qualified System.FilePath as SF
import qualified Data.Char as C

type MaybePar = Maybe (ParSource [CommentString])

-- todo [u:1] [request] qptain_nemo fake multiline comments in C
--      (i.e. // and \ at the bottom of the line, continued into
--      next line, are valid C comments but not recognised by lentil

-- as langParser, with alias added

-- todo [u:1] [request] (Iustin Pop) export to markdown.
-- todo [u:1] [request] scanner for yesod templates
--      (hamlet/julius/casius) which are a bit difficult since they are
--      html/js/css + embedded Haskell.

-- todo [design] Consider using https://github.com/github/semantic ?

langParserAlias :: [Alias] -> String -> MaybePar
langParserAlias as fp = maybe (langParser fp) langParser (lookup ext as)
    where ext = map C.toLower (SF.takeExtension fp)

extensionList :: [Alias] -> [String]
extensionList as = concatMap fst languages ++ map fst as

-- TODO [bug] lowercase does not work (not recognised, .r vs. .R)

langParser :: String -> MaybePar
langParser fp = lookupExt languages ext
    where ext = map C.toLower (SF.takeExtension fp)

languages :: [([String], MaybePar)]
languages = [ ([".hs", ".lhs",
                ".hsc", ".chs" ],       Just haskell),
              ([".cabal"],              Just haskell), -- cabal
              ([".elm"],                Just haskell), -- Elm
              ([".purs"],               Just haskell), -- Purescript
              ([".c", ".h"],            Just c),
              ([".cpp", ".hpp"],        Just c), -- C++
              ([".scala"],              Just c), -- Scala
              ([".java"],               Just c), -- Java
              ([".glsl"],               Just c), -- GL Shader
              ([".xrl"],                Just c), -- FLEX
              ([".zig"],                Just zig),
              ([".js"],                 Just javascript),
              ([".ts"],                 Just javascript), -- TypeScript
              ([".pas", ".pp", ".inc"], Just pascal),
              ([".py"],                 Just python),
              ([".coffee"],             Just python), -- CoffeeScript
              ([".rb"],                 Just ruby),
              ([".pl", ".pm", ".t"],    Just perl),
              ([".sh"],                 Just perl), -- shell
              ([".nix"],                Just nix),
              ([".xml", ".html"],       Just xml),
              ([".erl", ".hrl",
                ".escript", ".yrl"],    Just erlang),
              ([".ml", ".mli"],         Just ocaml),
              ([".rs", ".rlib"],        Just rust),
              ([".sml"],                Just sml),
              ([".rst"],                Just rst), -- reStructuredText
              ([".org"],                Just org), -- org-mode
              ([".r"],                  Just rp), -- R language
              ([".fs", ".fth", ".4th"], Just forth), -- forth
              ([".yaml", ".yml"],       Just perl), -- YAML
              ([".tex"],                Just latex), -- LaTeX, TeX
              ([".scm", ".ss"],         Just scheme), -- Scheme
              ([".lisp", ".lsp", ".l",
                ".cl", ".fasl"],        Just scheme), -- Lisp
              ([".agda", ".lagda" ],    Just haskell), -- Agda
              ([".txt", ".md"],         Just text) ]

haskell, c, zig, javascript, pascal, python, ruby, perl, nix,
    xml, erlang, ocaml, rp, rust, sml, forth, rst, org, latex,
    scheme,
    text :: ParSource [CommentString]
haskell    = source $ StdSyntax ["--"] [("{-", "-}")]
                                ClangLike ['"'] CommonChr ['\'']
c          = source $ StdSyntax ["//"] [("/*", "*/")]
                                ClangLike ['"'] CommonChr ['\'']
zig        = source $ StdSyntax ["//"] []
                                ClangLike ['"'] CommonChr ['\'']
javascript = source $ StdSyntax ["//"] [("/*", "*/")]
                                ClangLike ['"', '\''] CommonChr []
pascal     = source $ StdSyntax ["//"] [("{",  "}" ), ("(*", "*)")]
                                SQLLike ['\''] CommonChr []
python     = source $ StdSyntax ["#"] [("\"\"\"", "\"\"\"")]
                                ClangLike ['"', '\''] CommonChr []
ruby       = source $ StdSyntax ["#"] [("=begin", "=end")]
                                ClangLike ['"', '\''] CommonChr []
perl       = source $ StdSyntax ["#"] []
                                ClangLike ['"', '\''] CommonChr []
nix        = source $ StdSyntax ["#"] [("/*", "*/")]
                                ClangLike ['"'] CommonChr ['\'']
xml        = source $ StdSyntax [] [("<!--", "-->")]
                                ClangLike ['"', '\''] CommonChr []
erlang     = source $ StdSyntax ["%"] []
                                ClangLike ['"'] ErlangChr ['$']
ocaml      = source $ StdSyntax [] [("(*", "*)")]
                                ClangLike ['"', '\''] CommonChr []
rp         = source $ StdSyntax ["#"] []
                                ClangLike ['"', '\'', '`'] CommonChr []
rust       = source $ StdSyntax ["//!", "///", "//"] []
                                ClangLike ['"', '\''] CommonChr []
sml        = source $ StdSyntax [] [("(*", "*)")]
                                ClangLike ['"'] CommonChr []
forth      = source $ StdSyntax ["\\"] [("( ", ")")]
                                ClangLike ['"'] CommonChr []
rst        = source RstSyntax
             -- todo [bug] [design] rst parser doesn't respect whitespace or
             --      paragraphs. How to implement this without breaking other
             --      parsers?
org        = source OrgModeSyntax
latex      = source $ StdSyntax ["%:" -- TeXworks tags
                                , "%" -- single-line comments
                                ]
                                [] -- Some packages provide support for multi-line comments, but there is no support for multi-line comments, even if some packages provide support for.
                                ClangLike ['"'] CommonChr []
scheme     = source $ StdSyntax [";"] []
                                ClangLike ['"'] CommonChr []
text       = (:[]) . MultiLine 1 <$> many anySingle


-- ANCILLARIES --

lookupExt :: [([String], MaybePar)] -> String -> MaybePar
lookupExt []          _ = Nothing
lookupExt ((ss,p):ds) e | elem e ss = p
                        | otherwise = lookupExt ds e

