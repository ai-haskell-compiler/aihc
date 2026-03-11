# Changelog

## 2.0.1 (2025-03-11)
- 'stage' and 'test' releases also need to follow Kiwi editions

## 2.0 (2025-03-07)
- move short `-d` option from `--dl` to `--dir`
- simplify the download dir logic
  (now falls back to current directory if ~/Downloads/ doesn't exist)
- partial downloads are now staged in a `.dl-fedora-partial/` subdirectory
- promote KDE to edition
- accumulate download errors and report number of failures
- 'next' is now also accepted as a release alias
- add `--all-desktops`
- add `--dir option` to override download dir
- add `--exclude` editions switch

## 1.3 (2025-02-16)
- F42 Workstation now created with Kiwi
- KDE spin renamed to KDE Desktop
- add COSMIC for F42
- use "c{9,10}s-live" for alt live respins instead of --cs-live-respin
- support downloading multiple or all editions/spins:
  --all-editions and --all-spins are defined per release or respin
- handle Kiwi use for different versions
- rawhide can now be specified by version number (uses fedora-release)
- respin --local: now correctly lists latest respin (not oldest)
- bump QEMU mem to 3GB
- wrap http-directory functions with retry
- allow "ws" and "gnome" as aliases for Workstation
- add release aliases 'current' and 'previous'
- aliases {9,10}-live
- --alt-cs-extra-edition to allow MAX/MIN Live images

## 1.2.1 (2024-11-01)
- add MiracleWM and KDE_Mobile Live images

## 1.2 (2024-09-17)
- default centos-stream to mirror.stream.centos.org
- initial support for CS Alternative Live respins
- refactoring for new Release type
- new --list command to list all spins/editions
- checksum now only runs for downloaded (specified) image
- fix fedora.gpg url
- prompt to retry (continue) download if curl errors
- --debug: output redirects, including http -> https

## 1.1 (2024-05-23)
- add c10s and Fedora IoT edition
- print multiple matches and download latest
- '--dvd' option to select dvd image rather than boot/netinst iso
- handle empty mirror directory by falling back to dl.fp.o
- better debug output
- improve handling of empty checksum file
- filename tweaks for Container and Cloud

## 1.0 (2023-09-15)
- default to download.fp.o: replace --no-dl with --latest
- new --check (-c) command mode: checks for newer image
- --local mode now only derefs symlink to show latest local image
- help: list ostree editions last
- add Onyx (F39 Budgie ostree image)
- allow "sb" as alias for Silverblue
- offer to fix permissions of a partially downloaded run iso file

## 0.9.6 (2023-08-18)
- --no-dl option to avoid dl.fp.o even if newer
- support c8s and add options for centos-stream channels
- update eln path
- downloadFile: add showdestdir to downloading message and curl debug
- add --debug option

## 0.9.5 (2023-04-13)
- add F38 Sericea sway ostree image

## 0.9.4 (2023-03-23)
- add new F38 spins: Budgie and Sway
- change koji target to --koji mirror option
- if mirror redirect fails then fallback to primary
- run with qemu -cpu host option

## 0.9.3 (2022-06-11)
- show timestamp of image

## 0.9.2 (2021-10-01)
- fix test (beta) image selection to be the latest version

## 0.9.1 (2021-08-30)
- new Kinoite edition for F35
- initial Centos Stream "c9s" target for production boot images

## 0.9 (2021-04-22)
- edition is now an argument after release, not an option
- --local --dryrun only accesses local files now for speed
- add '--no-http-timeout' (mostly for CI)

## 0.8 (2021-04-07)
- --local option: print (or --run) current local image instead of newer download
- improve --dryrun Downloads/ handling for testsuite in CI

## 0.7.7 (2021-04-06)
- add the new F34 i3 spin
- shorten mate_compiz to mate
- convert tests to Haskell

## 0.7.6 (2021-01-21)
- improve help text for releases related for Beta and RCs (#1)
- if ~/Downloads/iso/ exists then download to it otherwise ~/Downloads/
- support ELN boot.iso

## 0.7.5 (2020-09-13)
- always print download url and already downloaded filename
- --replace deletes previous symlinked image after downloading new one
- improved checksum file handling

## 0.7.4 (2020-03-15)
- add 'koji' release target: downloads latest branched compose from kojipkgs

## 0.7.3 (2020-02-11)
- fix CHECKSUM512 detection (for respins)

## 0.7.2 (2019-10-29)
- add stage release target (useful just prior to release)

## 0.7.1 (2019-09-25)
- print datestamp beside filesize
- show directory correctly with ~/
- use a subdirectory for checksum files

## 0.7 (2019-09-12)
- add --checksum to always do checksum when possible
- add --run to run image in qemu-kvm if available
- rework algorithms
  - check local filesize earlier: don't even check mirror if already downloaded
  - for partial local file output percentage already downloaded
  - otherwise show filesize
- show dir for symlink: so one knows location
- drop the 'devel' target (use release version number instead)
- handle old dangling symlink too (after deleting iso)

## 0.6 (2019-09-02)
- major rework to correct the url logic
  - first checks on dl.fedoraproject.org (master)
  - then tries to download corresponding filepath on download.fedoraproject.org
  - or falls back to master mirror
- new --dl option to download directly from dl.fedoraproject.org
- can now find branched development release
- also compares mirror and master filesizes
- builds with lts-14

## 0.5 (2019-07-06)
- no hardcoding of devel branch, beta, and respins
- fix Spins and Cloud/Container paths
- use rawhide for devel if not branched
- don't checksum if no file downloaded or new --no-checksum
- more informative error messages
- add simple dryrun test script

## 0.4 (2019-06-03)
- drop version 30 special case
- support gpg verification of checksum file

## 0.3 (2019-04-16)
- run sha256sum check
- support fedora Spins

## 0.2
- fix and improve symlink naming
- use new http-directory library to check exact filesize

## 0.1
* initial release
