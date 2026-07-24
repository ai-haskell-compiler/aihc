{
  mkApiDocs,
  mkCombinedDocs,
  mkCoverageReport,
  mkUserGuide,
}: pkgs: {
  api-docs = mkApiDocs pkgs;
  docs = mkCombinedDocs pkgs;
  coverage = mkCoverageReport pkgs;
  user-guide = mkUserGuide pkgs;
  default = mkCombinedDocs pkgs;
}
