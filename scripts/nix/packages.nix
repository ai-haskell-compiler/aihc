{
  mkCombinedDocs,
  mkCoverageReport,
}: pkgs: {
  docs = mkCombinedDocs pkgs;
  coverage = mkCoverageReport pkgs;
  default = mkCombinedDocs pkgs;
}
