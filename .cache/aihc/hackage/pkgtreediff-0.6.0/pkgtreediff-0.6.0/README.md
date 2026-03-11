# pkgtreediff

[![Hackage](https://img.shields.io/hackage/v/pkgtreediff.svg)](https://hackage.haskell.org/package/pkgtreediff)
[![GPL-3 license](https://img.shields.io/badge/license-GPL--3-blue.svg)](LICENSE)
[![Stackage Lts](http://stackage.org/package/pkgtreediff/badge/lts)](http://stackage.org/lts/package/pkgtreediff)
[![Stackage Nightly](http://stackage.org/package/pkgtreediff/badge/nightly)](http://stackage.org/nightly/package/pkgtreediff)

`pkgtreediff` compares the NVRs (name-version-release) of RPM packages
in OS package trees and/or installations:

- An OS tree can be referenced by an url or directory containing a tree of rpm files.
- A text file/url containing a list of rpm NVRs can also be compared.
- Commands can also be used to get installed RPMs, eg:
  - `"rpm -qa"`
  - `"ssh myhost rpm -qa"`
  - `"podman run --rm myimage rpm -qa"`
  - `"sudo dnf -q repoquery --qf '%{name}-%{version}-%{release}.%{arch}'"`
- A koji tag: koji://tag@KojiHubUrl (or koji://tag@fedora)

## Usage examples

### Help
```shellsession
$ pkgtreediff --version
0.6.0
$ pkgtreediff -h
Package tree comparison tool

Usage: pkgtreediff [--version] [-r|--recursive] [-d|--subdir SUBDIR]
                   [(-R|--ignore-release) | (-V|--ignore-version)]
                   [(-N|--new) | (-D|--deleted) | (-U|--updated) |
                     (-u|--downgraded)]
                   [(-s|--show-summary) | (-S|--no-summary)] [-R|--rst]
                   [-p|--pattern PKGPATTERN] [-t|--timeout SECONDS]
                   URL|DIR|FILE|KOJITAG|CMD1 URL|DIR|FILE|KOJITAG|CMD2
  pkgtreediff compares the packages in two OS trees or instances

Available options:
  -h,--help                Show this help text
  --version                Show version
  -r,--recursive           Recursive down into subdirectories
  -d,--subdir SUBDIR       Select specific subdir (eg x86_64 or source)
  -R,--ignore-release      Only show version changes (ignore release)
  -V,--ignore-version      Only show package changes (ignore version-release)
  -N,--new                 Show only added packages
  -D,--deleted             Show only removed packages
  -U,--updated             Show only upgraded packages
  -u,--downgraded          Show only downgraded packages
  -s,--show-summary        Show summary of changes (default when >20 changes)
  -S,--no-summary          Do not display summary
  -R,--rst                 Use ReSTructured Text format
  -p,--pattern PKGPATTERN  Limit packages to glob matches
  -t,--timeout SECONDS     Maximum seconds to wait for http response before
                           timing out (default 30)
```

### Containers
Compare the content of two rpm based containers versions,
filtering for new added packages:

```shellsession
$ pkgtreediff --new "podman run --rm fedora:36 rpm -qa" "podman run --rm fedora:37 rpm -qa"
libb2-0.98.1-7.fc37.x86_64
```

### Package trees

Package source changes between Fedora 35 and 36 Server at GA
(ignoring release bumps):

```shellsession
$ pkgtreediff --ignore-release https://download.fedoraproject.org/pub/fedora/linux/releases/{35,36}/Server/source/tree/Packages/
ModemManager.src: 1.18.2-1.fc35 -> 1.18.6-1.fc36
NetworkManager.src: 1.32.12-1.fc35 -> 1.36.4-1.fc36
PackageKit.src: 1.2.4-3.fc35 -> 1.2.5-1.fc36
abrt.src: 2.14.6-9.fc35 -> 2.15.1-1.fc36
:
:
- tree-1.8.0-7.fc35.src
+ tree-pkg-2.0.1-2.fc36.src
tzdata.src: 2021b-1.fc35 -> 2022a-1.fc36
:
xorg-x11-xauth.src: 1.1-9.fc35 -> 1.1.1-2.fc36
+ xrdb-1.2.1-3.fc36.src
xxhash.src: 0.8.0-4.fc35 -> 0.8.1-2.fc36
yelp.src: 41.1-1.fc35 -> 42.1-1.fc36
yelp-xsl.src: 41.0-1.fc35 -> 42.0-1.fc36
zchunk.src: 1.1.15-2.fc35 -> 1.2.1-1.fc36
zstd.src: 1.5.0-2.fc35 -> 1.5.2-1.fc36

Summary
Updated: 293
Downgraded: 0
Added: 36
Deleted: 21
Arch changed: 0
Total packages: 915 -> 930
```

For very large repos like Everything it should be faster
to use a repoquery command.

### Hosts
Compare the packages on local and another host:
```
pkgtreediff "rpm -qa" "ssh otherhost rpm -qa"
```

Any types of sources can be compared, together with the use of flags.

### Koji
Compare koji tags using the `koji://tag@kojihub` syntax:
```
pkgtreediff koji://dist-c8-updates-build@centos koji://dist-c8_1-updates-build@centos
```

Please avoid using koji tags to compare full releases as it is more efficient
to query URL or CMD trees.

## Builds
pkgtreediff is packaged in Fedora.

Older builds are also available in
[copr](https://copr.fedorainfracloud.org/coprs/petersen/pkgtreediff/)
for Fedora, EPEL, and OpenSuSE
([more details](https://copr.fedorainfracloud.org/coprs/petersen/pkgtreediff/monitor/detailed)).

## RPM version ordering
RPM version ordering is somewhat [complicated](https://blog.jasonantman.com/2014/07/how-yum-and-rpm-compare-versions/).
For example "1.2.1~rc1" < "1.2.1" (greater for ^).

pkgtreediff use the [rpm-nvr](https://hackage.haskell.org/package/rpm-nvr)
library implementation of the `rpmvercmp()` algorithm,
though it has not been verified to behave identically.
