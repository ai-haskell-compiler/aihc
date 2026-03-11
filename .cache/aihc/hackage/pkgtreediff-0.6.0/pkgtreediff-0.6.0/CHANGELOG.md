# Changelog

# 0.6.0 (2022-10-27)
- significant processing performance improvement: drop expensive nub
- allow multiple mode options and separate summary & rst options
- add support for package list files via http
- fix local filesystem recursion
- initial testsuite

## 0.5.0 (2021-11-03)
- use rpm-nvr library for NVRA type (API changes) and rpmVerCompare
  to order ver-rel's
- add --downgraded filter
- add --subdir
- remove --ignore-arch

## 0.4.1 (2021-04-19)
- expose a library in Distribution.RPM.PackageTreeDiff (@juhp)
- full correct rpm version comparison (ie handling ^ and ~) (@juhp)
- new koji://tag@koji.hub method (@TristanCacqueray)
- rst summary output format  (@TristanCacqueray)

## 0.4 (2020-03-13)
- support files of lists of rpms
- support commands for getting package NVRs
  - to run "rpm -qa" etc, locally or remotely
  - note commands must be quoted and include a space to be recognized
- fix sorting of packages by rpmvercmp() style ordering (#3)
  (fixes cumulative trees like CentOS for example)
- add --timeout option for http (#1)

## 0.3 (2019-06-06)
- add --pattern for href filename globbing
- use http-directory-0.1.4 for httpDirectory basic filtering

## 0.2.1 (2019-06-06)
- better recursion handling and href filtering

## 0.2 (2019-06-05)
- print summary
- add --ignore-arch

## 0.1 (2019-06-04)
- --ignore-version option

## 0.0 (2019-05-31)
- Initially created.
- --ignore-release option
