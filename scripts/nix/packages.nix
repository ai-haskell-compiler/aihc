{
  mkCombinedDocs,
  mkCoverageReport,
  mkParserWasmWasi,
}: pkgs: {
  parser-wasm-wasi = mkParserWasmWasi pkgs;
  docs = mkCombinedDocs pkgs;
  coverage = mkCoverageReport pkgs;
  default = mkCombinedDocs pkgs;
}
