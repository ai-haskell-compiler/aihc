{ lib }:

let
  parseManifest = manifestPath:
    let
      lines = lib.filter (l: l != "") (lib.map lib.trim (lib.splitString "\n" (lib.removeSuffix "\n" (lib.fileContents manifestPath)));
      parseLine = line:
        let
          parts = lib.filter (s: s != "") (lib.splitString "\t" line);
        in
          if lib.length parts >= 4
          then {
            id = lib.elemAt parts 0;
            category = lib.elemAt parts 1;
            path = lib.elemAt parts 2;
            expected = lib.elemAt parts 3;
            reason = if lib.length parts > 4 then lib.elemAt parts 4 else "";
          }
          else null;
    in
      lib.filter (x: x != null) (lib.map parseLine lines);

  countTestResults = tests:
    let
      passCount = lib.length (lib.filter (t: t.expected == "pass") tests);
      xfailCount = lib.length (lib.filter (t: t.expected == "xfail") tests);
      totalCount = lib.length tests;
    in
      { inherit passCount xfailCount totalCount; };

in
{
  extensions = {

    listExtensions = fixturesDir:
      let
        dirs = lib.attrNames (lib.filterAttrs (n: type: type == "directory") (lib.readDir fixturesDir));
        extensions = lib.filter (d: d != "haskell2010") dirs;
      in
        extensions;

    getExtensionTests = fixturesDir: extension:
      let
        manifestPath = fixturesDir + "/${extension}/manifest.tsv";
        manifestExists = lib.pathExists manifestPath;
      in
        if manifestExists
        then parseManifest manifestPath
        else [];

    getExtensionStatus = fixturesDir: extension:
      let
        tests = lib.filter (t: t.expected == "pass") (builtins.getAttr "extensions" { inherit fixturesDir; }).${extension} or [];
        passCount = lib.length tests;
        totalCount = lib.length tests;
      in
        if totalCount == 0
        then "none"
        else if passCount == totalCount
        then "supported"
        else "partial";

    analyzeExtension = fixturesDir: extension:
      let
        allTests = lib.filter (t: t.expected == "pass") (builtins.getAttr "extensions" { inherit fixturesDir; }).${extension} or [];
        passCount = lib.length allTests;
        totalCount = lib.length (builtins.getAttr "extensions" { inherit fixturesDir; }).${extension} or [];
      in
        {
          name = extension;
          passCount = passCount;
          totalCount = totalCount;
          status =
            if totalCount == 0
            then "none"
            else if passCount == totalCount
            then "supported"
            else if passCount > 0
            then "partial"
            else "none";
        };

    analyzeAllExtensions = fixturesDir:
      let
        exts = lib.attrNames (lib.filterAttrs (n: type: type == "directory") (lib.readDir fixturesDir));
        extensions = lib.filter (d: d != "haskell2010") exts;
        analyzed = lib.map (analyzeExtension fixturesDir) extensions;
        haskell2010Tests = parseManifest (fixturesDir + "/haskell2010/manifest.tsv");
        h2010Counts = countTestResults haskell2010Tests;
      in
        {
          extensions = analyzed;
          haskell2010 = {
            passCount = h2010Counts.passCount;
            totalCount = h2010Counts.totalCount;
          };
        };

    generateMarkdownReport = analyzed:
      let
        exts = analyzed.extensions;
        supported = lib.filter (e: e.status == "supported") exts;
        partial = lib.filter (e: e.status == "partial") exts;
        none = lib.filter (e: e.status == "none") exts;
        totalExts = lib.length exts;
        supportedCount = lib.length supported;
        partialCount = lib.length partial;
        noneCount = lib.length none;

        formatRow = e: "| ${e.name} | ${e.status} | ${toString e.passCount}/${toString e.totalCount} | |";

        mdHeader = ''
          # Haskell Extension Support Status

          **Generated**: ${lib.currentDate}
        '';

        mdSummary = ''
          ## Summary

          - Total Extensions: ${toString totalExts}
          - Supported: ${toString supportedCount} (${toString (lib.div 100 (lib.max 1 (lib.div totalExts supportedCount)))}%)
          - Partial: ${toString partialCount}
          - No Tests: ${toString noneCount}

          ## Haskell2010 Support

          - Tests Passing: ${toString analyzed.haskell2010.passCount}/${toString analyzed.haskell2010.totalCount}
        '';

        mdTable = ''
          ## Extension Status

          | Extension | Status | Tests | Notes |
          |-----------|--------|-------|-------|
          ${lib.concatMapStringsSep "\n" formatRow exts}
        '';
      in
        mdHeader + "\n\n" + mdSummary + "\n\n" + mdTable + "\n";
  };
}
