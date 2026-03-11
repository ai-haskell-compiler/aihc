# Revision history for fedora-repoquery

## 0.7.3 (2025-06-03)
- fix the naming of koji dnf repos
- flip the --dl and --debug short options
- add --queryformat "n" abbreviation
- support riscv64 repoqueries in fedora.riscv.rocks

## 0.7.2 (2025-05-08)
- warn when no release specified
- add support for epel10.x minor releases
- also parse cNs (c8s, c9s, c10s)
- use eln via download.fp.o (or dl.fp.o) and drop its channels
- support short pneumonics for common queryformats (nv,nvr,nvra,envr,envra)
- improve branched pre-release logic using fedora-releases for Bodhi API
- drop head/tail usage

## 0.7.1 (2024-09-08)
- fix for branched: "frozen" state is still in development/ like "pending"
- enable updates-testing for fedora branched from Beta freeze
- help: can use -- for system repoquery
- only output "slow redirection" for debug
- refactor eitherRelease with lower case
- curl connection: timeout for Location redirect but not Last-Modified

## 0.7 (2024-07-18)
- use curl library for HTTP instead of http-directory
- rename the executable back to fedora-repoquery to avoid confusion with fedrq

## 0.6 (2024-07-16)
- defaults to no datestamp output: --time replaces --no-check
- further speed improvements by reducing http checks
- map latest pending release version to fedora-rawhide
- add --dynamic redirect to not re-use first mirror redirect
- update c8s url to Centos Vault
- drop dep on utility-ht just for spanJust

## 0.5 (2024-07-02)
- allow multiple release args
- with no release arg use system yum repos
- add --quick to skip url http checks, also done when multiple releases
- dnf5 --qf does not add "\n" so we do it if no trailing "space"
- update cache dir commands to support also dnf5 xdg cache path
- use dnf5 repoquery.cpp list of --qf conflicts

## 0.4 (2024-05-24)
- add --dnf4 option to use dnf4 (actually /usr/bin/dnf-3) instead of dnf5
- oldest and newest active releases are now determined/cached via Bodhi API
- can now query with rawhide by version (eg 41 currently)
- disable --qf for --requires
- --list now only shows active fedora releases
- for centos and eln only check compose for the BaseOS repo
- map version 11 to eln

## 0.3.2 (2024-02-19)
- enable centos 10 stream (still alpha)
- Query: disable --qf for --provides
- minor debug tweaks

## 0.3.1 (2024-02-12)
- use dnf5 when available
- add --all-archs to query across all 64bit architectures

## 0.3 (2023-11-09)
- Query: no --qf with --changelog
- small OS versions now map to centos-stream
- use hostname not full url for mirror repo name
- repo urls should end with a slash
- improve tests to fail if no results
- map failing yzyu.jp http/3 mirror to main one
- development/39 also needs os/
- showRelease: reformat Centos and ELN repos
- default to system arch and introduce sysarch and march

## 0.2 (2023-05-12)
- support fedora and epel testing repos
- fix fedora archive urls
- support querying epel6 and older archives
- support fedora-secondary urls for ppc64le and s390x including archive
- rename --test to --test-channel and --centos-devel to --devel-channel
- expand help to cover RELEASE and add readme url

## 0.1.0 (2023-05-07)
- initial release with support for fedora releases, epel, eln and centos stream
