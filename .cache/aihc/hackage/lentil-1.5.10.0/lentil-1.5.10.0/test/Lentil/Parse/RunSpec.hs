module Lentil.Parse.RunSpec where

import Test.Hspec

import Lentil.Types
import Lentil.Parse.Run

-- Parsing tests

fileParser :: [Alias] -> [FlagWord] -> FilePath -> IO [Issue]
fileParser as fws fp = issueFinder as fws [fp]


main :: IO ()
main = hspec spec



spec :: Spec
spec = do

  let spt fp = [Issue fp 1 (Just "single comment") [],
                Issue fp 3 (Just "single2") [],
                Issue fp 8 (Just "block1")  [],
                Issue fp 9 (Just "block2") [Tag "tog"]]

  describe "issueFinder - specific" $ do
    it "doesn't parse contiguous line/block as a single issue" $
        fileParser [] [] "test/test-files/specific/contiguous.c"
            `shouldReturn` [Issue "test/test-files/specific/contiguous.c" 1
                                  (Just "issue1") []]
    it "contiguous line/block is not single issue (custom flagwords)" $
        fileParser [] ["hax"] "test/test-files/specific/cont-custom.c"
            `shouldReturn` [Issue "test/test-files/specific/cont-custom.c" 1
                                  (Just "single")  [],
                            Issue "test/test-files/specific/cont-custom.c" 2
                                  (Just "single2") [Tag "hax"]]
    it "parses a .xyz file as if it were a .c file" $
        fileParser [(".xyz", ".c")] [] "test/test-files/specific/xyz.xyz"
            `shouldReturn` spt "test/test-files/specific/xyz.xyz"
    it "parses user-defined flagword" $
        let custFlw fp = [Issue fp 3 (Just "single2") [Tag "hax"]] in
        fileParser [] ["hax"] "test/test-files/specific/custom-fwords.c"
            `shouldReturn` custFlw "test/test-files/specific/custom-fwords.c"
    it "parses user-defined flagword (CI)" $
        let custFlw fp = [Issue fp 3 (Just "single2") [Tag "hax"]] in
        fileParser [] ["hAx"] "test/test-files/specific/custom-fwords.c"
            `shouldReturn` custFlw "test/test-files/specific/custom-fwords.c"
    it "does not fail with latin1 (ISO-8859) encoding" $
        fileParser [] [] "test/test-files/specific/latin1.c"
            `shouldReturn` [Issue "test/test-files/specific/latin1.c" 1
                                  (Just "single") []]

  -- TODO Abstract spt [refactor].
  describe "commentParser - languages" $ do
    it "parses a plain text source" $
        fileParser [] [] "test/test-files/lang-comm/text.txt"
            `shouldReturn` spt "test/test-files/lang-comm/text.txt"
    it "parses a haskell source" $
        fileParser [] [] "test/test-files/lang-comm/haskell.hs"
            `shouldReturn` spt "test/test-files/lang-comm/haskell.hs"
    it "parses a C source" $
        fileParser [] [] "test/test-files/lang-comm/clang.c"
            `shouldReturn` spt "test/test-files/lang-comm/clang.c"
    it "parses a Pascal source" $
        fileParser [] [] "test/test-files/lang-comm/pascal.pas"
            `shouldReturn` spt "test/test-files/lang-comm/pascal.pas"
    it "parses a javascript source" $
        fileParser [] [] "test/test-files/lang-comm/javascript.js"
            `shouldReturn` spt "test/test-files/lang-comm/javascript.js"
    it "parses a python source" $
        fileParser [] [] "test/test-files/lang-comm/python.py"
            `shouldReturn` spt "test/test-files/lang-comm/python.py"
    it "parses a ruby source" $
        fileParser [] [] "test/test-files/lang-comm/ruby.rb"
            `shouldReturn` spt "test/test-files/lang-comm/ruby.rb"
    it "parses a perl source" $
        fileParser [] [] "test/test-files/lang-comm/perl.pl"
            `shouldReturn` spt "test/test-files/lang-comm/perl.pl"
    it "parses a shell script source" $
        fileParser [] [] "test/test-files/lang-comm/shell.sh"
            `shouldReturn` spt "test/test-files/lang-comm/shell.sh"
    it "parses a Nix source" $
        fileParser [] [] "test/test-files/lang-comm/nix.nix"
            `shouldReturn` spt "test/test-files/lang-comm/nix.nix"
    it "parses an Xml source" $
        fileParser [] [] "test/test-files/lang-comm/xml.xml"
            `shouldReturn` spt "test/test-files/lang-comm/xml.xml"
    it "parses an erlang source" $
        fileParser [] [] "test/test-files/lang-comm/erlang.erl"
            `shouldReturn` spt "test/test-files/lang-comm/erlang.erl"
    it "parses an OCaml source" $
        fileParser [] [] "test/test-files/lang-comm/ocaml.ml"
            `shouldReturn` spt "test/test-files/lang-comm/ocaml.ml"
    it "parses a Rust source" $
        fileParser [] [] "test/test-files/lang-comm/rust.rs"
            `shouldReturn` spt "test/test-files/lang-comm/rust.rs"
    it "parses a Standard ML source" $
        fileParser [] [] "test/test-files/lang-comm/standard-ml.sml"
            `shouldReturn` spt "test/test-files/lang-comm/standard-ml.sml"
    it "parses an rst+sphinx document" $
        fileParser [] [] "test/test-files/lang-comm/rst.rst"
            `shouldReturn` [Issue "test/test-files/lang-comm/rst.rst" 3
                           (Just "some text, more text another paragraph, \
                                 \still belongs to todo") [Tag "hh2"]]
    it "parses a org-mode source" $
        fileParser [] [] "test/test-files/lang-comm/org-mode.org"
            `shouldReturn` spt "test/test-files/lang-comm/org-mode.org"
    it "parses an R source" $
        fileParser [] [] "test/test-files/lang-comm/r.r"
            `shouldReturn` spt "test/test-files/lang-comm/r.r"
    it "parses a Forth source" $
        fileParser [] [] "test/test-files/lang-comm/forth.fs"
            `shouldReturn` spt "test/test-files/lang-comm/forth.fs"
    it "parses a YAML source" $
        fileParser [] [] "test/test-files/lang-comm/yaml.yml"
            `shouldReturn` spt "test/test-files/lang-comm/yaml.yml"
    it "parses a LaTeX source" $
        fileParser [] [] "test/test-files/lang-comm/latex.tex"
            `shouldReturn` spt "test/test-files/lang-comm/latex.tex"
    it "parses a Zig source" $
        fileParser [] [] "test/test-files/lang-comm/zig.zig"
            `shouldReturn` spt "test/test-files/lang-comm/zig.zig"
    it "parses a Scheme/Lisp source" $
        fileParser [] [] "test/test-files/lang-comm/scheme.scm"
            `shouldReturn` spt "test/test-files/lang-comm/scheme.scm"
    it "parses an Agda source" $
        fileParser [] [] "test/test-files/lang-comm/agda.agda"
            `shouldReturn` spt "test/test-files/lang-comm/agda.agda"
