# 0.4.13 (2025-11-10)
- ProvReqs: handle cmake directories as used by KDE (#5)
- ProvReqs: create python3dist dynamic provides (#6)
- depsGraphDeps bugfix: include the packages within the deps graphs

# 0.4.12 (2024-08-04)
- fix ordering of rust crates and rubygems
  by mapping their dynamic BRs to package names
- drop the manpage generated from --help

# 0.4.11 (2024-02-18)
- ProvReqs: new module for rpmspec dependency functions
- ProvReqs: generate pkgconfig and cmake provides from %files (#4)
- Graph depsGraph: include package set in potential deps
- Order: add dependencyLayersRpmOpts
- more tests

# 0.4.10 (2022-10-28)
- Graph: add depsGraphDeps which takes a list of possible deps
- Graph: add topsortGraph (factored out of sortGraph)
- tests: add testcase for deps command

# 0.4.9 (2022-08-25)
- Graph rpmspecDynBuildRequires: error if no srpm generated
- Order: add depsPackages and factor out depsGraph to Graph

# 0.4.8 (2022-06-14)
- Graph createGraph4: prepend spec name to any rpmspec stderr
- Graph optimization: filter out Provides ending in (x86-64)

# 0.4.7 (2022-05-31)
- Graph rpmspecDynBuildRequires: generate .buildreqs.nosrc.rpm in a tmpdir

# 0.4.6 (2022-05-30)
- handle Dynamic BuildRequires (#3)
  with special support for golang Provs and rust & ruby BRs
- Graph: new aliases createGraph1 for createGraph',
  createGraph2 for createGraph'', createGraph3 for createGraph''',
  and createGraph4 for createGraph'''' (deprecating the primed names)
- Graph: use simple-cmd

# 0.4.5 (2021-05-29)
- 'render': do not reverse arrows
- 'render': rename -o/--output option to -g/--gv-output

# 0.4.4.1 (2021-05-19)
- 'render': friendly error message if graphviz is not installed

# 0.4.4 (2021-05-16)
- add 'render' command to display graph graphically with graphviz
  with an option to output dot format
- fix subcycle algorithm: now lists correct shortest path subcycles
- improve cycles output (#2)
- library Graph: add printGraph, renderGraph, createGraph''''

# 0.4.3.2 (2021-02-03)
- intercalate newlines between cycles
- only list subcycles with over 2 packages

# 0.4.3.1 (2020-09-24)
- 'deps' and 'rdeps': --exclude option to ignore a "broken" neighboring package

# 0.4.3 (2020-09-18)
- 'deps' and 'rdeps' can now take --ignore-BR options
  (provided through depsPackages and new createGraph''')
- 'sort' no longer outputs a leading newline

# 0.4.2.1 (2020-09-10)
- 'sort': fix arg handled which was causing reverse ordering!
- add a sort test for the installed rpmbuild-order
- drop the erroneous dependency on Cabal

# 0.4.2 (2020-08-04)
- support options for rpmspec (suggested by QuLogic)
  eg `rpmbuild-order sort --rpmopt --with=bootstrap ...`
- Graph: add createGraphRpmOpts and createGraph''
- Order: add dependencySortRpmOpts
- add a newline between cycles

# 0.4.1 (2020-08-03)
- no longer assume pkg dir = pkg name (though still optimize for it)
- output any subcycles for cyclic graph (suggested by QuLogic)

# 0.4.0 (2020-07-29)
- performance: rework just to use String and only parse spec files once
  and also use faster PatriciaTree.Gr
  On about 500 packages roughly twice as fast as 0.3.1
- sort now defaults to outputting separate dependency stacks, with options for combined, connected, and independent packages only
- new 'layers' command outputs packages in ordered dependency independent layers
- new 'chain' command outputs Fedora chain-build format
- new 'leaves' commands to list outer leaf packages
- new 'roots' commands lists lowest dependencies
- new library exposed with 2 modules: low-level Graph and high-level Order:
  - Distribution.RPM.Build.Order provides: dependencySort, dependencySortParallel,
    dependencyLayers, sortGraph output
  - Distribution.RPM.Build.Graph provides: createGraph, dependencyNodes,
    subgraph', packageLayers, etc
- graph Nodes are now only labelled by package/spec filepath
  and no longer carry redundant dependency lists
- add a basic testsuite for the library

# 0.3.1 (2020-07-04)
- fix detection of circular dependencies (bug introduced in 0.3)

# 0.3 (2019-10-10)
- check package provides instead of rpms
- use ByteString for packages (provides)
- move to simple-cmd-args (optparse-applicative)
- add --lenient for rpmspec failures

# 0.2 (2018-08-17)
- error if no spec file
- filter out hidden files from deps search
- add --version
- generate manpage with help2man

# 0.1 (2018-07-18)
- initial release with sort, deps and rdeps (reverse dependencies) commands
