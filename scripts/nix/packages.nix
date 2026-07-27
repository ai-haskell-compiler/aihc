{
  mkChecks,
  mkUserGuide,
}: pkgs: let
  checks = mkChecks pkgs;
in {
  ci-checks = pkgs.linkFarm "aihc-ci-checks" (
    pkgs.lib.mapAttrsToList (name: path: {inherit name path;}) checks
  );
  docs = mkUserGuide pkgs;
  user-guide = mkUserGuide pkgs;
  default = mkUserGuide pkgs;
}
