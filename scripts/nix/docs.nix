{
  projectHsPackages,
  mkHsPkgsWithHaddock,
}: let
  mkCombinedDocsFrom = mkHsPkgsBuilder: pkgs: let
    hsPkgsHaddock = mkHsPkgsBuilder pkgs;
    parserDoc = hsPkgsHaddock.aihc-parser.doc;
    cppDoc = hsPkgsHaddock.aihc-cpp.doc;
    # Use haddock from ghc package (bundled with GHC).
    haddock = (projectHsPackages pkgs).ghc;
  in
    pkgs.runCommand "aihc-docs" {
      nativeBuildInputs = [haddock];
      inherit parserDoc cppDoc;
    } ''
      mkdir -p "$out"

      # Copy individual package docs (discover html directory dynamically).
      parserHtml=$(find "$parserDoc/share/doc" -type d -name html | head -1)
      cppHtml=$(find "$cppDoc/share/doc" -type d -name html | head -1)
      if [ -z "$parserHtml" ]; then
        echo "Could not find parser HTML docs under $parserDoc/share/doc" >&2
        exit 1
      fi
      if [ -z "$cppHtml" ]; then
        echo "Could not find cpp HTML docs under $cppDoc/share/doc" >&2
        exit 1
      fi
      cp -r "$parserHtml" "$out/aihc-parser"
      cp -r "$cppHtml" "$out/aihc-cpp"

      # Generate combined index and contents.
      haddock \
        --gen-index \
        --gen-contents \
        -o "$out" \
        --read-interface=aihc-parser,"$out/aihc-parser/aihc-parser.haddock" \
        --read-interface=aihc-cpp,"$out/aihc-cpp/aihc-cpp.haddock"
    '';
in {
  mkCombinedDocs = mkCombinedDocsFrom mkHsPkgsWithHaddock;
}
