# Read the cabal plan.json of the repository and write the library closure of
# the `aihc` executable as tab-separated "name version source dependencies"
# lines, with each package after all of its dependencies.
#
# The source is "hackage", "local:<path relative to $root>", or
# "git:<location>@<tag>". The dependencies are a comma-separated list of names
# in the closure, or "-" when there are none.
#
# Arguments: --arg root <repository root>, and --argjson standins <array of the
# boot package names that the aihc core libraries replace>. The closure leaves
# those packages out, because aihc has its own versions of them.

.["install-plan"] as $plan
| ($plan | map({key: .id, value: .}) | from_entries) as $units
| def unit_depends($id):
    $units[$id] as $unit
    | if $unit.components then ($unit.components.lib.depends // [])
      else ($unit.depends // []) end;
  def name_of($id): $units[$id]["pkg-name"];
  def source_of($unit):
    ($unit["pkg-src"] // {type: "repo-tar"}) as $src
    | if $src.type == "local" then
        "local:" + ($src.path | ltrimstr($root + "/") | ltrimstr($root))
        | if . == "local:" then "local:." else . end
      elif $src.type == "source-repo" then
        "git:" + $src["source-repo"].location + "@" + $src["source-repo"].tag
      else "hackage" end;
  ($units["aihc-0.1.0.0-inplace"].components) as $aihc
  | [ {seen: [], todo: ($aihc.lib.depends + $aihc["exe:aihc"].depends)}
      | until(.todo | length == 0;
          .todo[0] as $head
          | .todo |= .[1:]
          | if (.seen | index($head)) then .
            else .seen += [$head] | .todo += unit_depends($head) end)
      | .seen[] ] as $closure
  # One entry per package: a package can have several units (a library and
  # its sub-libraries), and the entry takes the dependencies of all of them.
  | ( [ $closure[]
        | $units[.] as $unit
        | {name: $unit["pkg-name"], version: $unit["pkg-version"],
           source: source_of($unit),
           depends: [unit_depends(.)[] | name_of(.)]} ]
      + [ {name: "aihc", version: $units["aihc-0.1.0.0-inplace"]["pkg-version"],
           source: source_of($units["aihc-0.1.0.0-inplace"]),
           depends: [($aihc.lib.depends + $aihc["exe:aihc"].depends)[] | name_of(.)]} ]
      | group_by(.name)
      | map({name: .[0].name, version: .[0].version, source: .[0].source,
             depends: ([.[].depends[]] | unique)})
      | map(select(.name as $name | $standins | index($name) | not))
      # A sub-library depends on the library of its own package.
      | map(.name as $self
            | .depends |= map(select(. != $self and (. as $name | $standins | index($name) | not))))
    ) as $packages
  # Kahn's algorithm with the name as the tie-breaker, so the order is stable.
  | {done: [], rest: $packages}
  | until(.rest | length == 0;
      .done as $done
      | ([.rest[] | select(all(.depends[]; . as $d | $done | index($d)))] | sort_by(.name)) as $ready
      | if ($ready | length) == 0 then error("dependency cycle in the plan") else . end
      | .done += [$ready[0].name]
      | .order += [$ready[0]]
      | .rest |= map(select(.name != $ready[0].name)))
  | .order[]
  | [.name, .version, .source, (if (.depends | length) == 0 then "-" else (.depends | join(",")) end)]
  | @tsv
