<!--
SPDX-FileCopyrightText: 2022 Frank Doepper

SPDX-License-Identifier: GPL-3.0-only
-->

# Revision history for haskell-amqp-utils

## 0.6.7.3  -- 2025-11-02

* relax time upper bound for compatibility with stackage nightly

## 0.6.7.2  -- 2025-07-29

* add cabal bounds
* add copy-to-hotfolder simple callback script
* update flake to nixos-25.05
* update stack.yaml to lts-24.2 (ghc-9.10.2)
* fix builds.sr.ht openbsd and alpine builds
* add Dockerfile for static linux binaries (thanks mdom)
* agitprop:
  * ignore dotfiles even without -S
  * only send regular files in hotfolder mode
  * add --watchcreate (-L) option to get hardlink events
  * ignore empty files with --watchcreate (-L)

## 0.6.6.0  -- 2024-12-06

* LineBuffering
* print heartbeat parameter
* optionally delay negative acknowledgements (--delaynack/-D)

## 0.6.5.0  -- 2023-11-14

* konsum: new --stream\_offset option

## 0.6.4.0  -- 2023-07-20

* fix exception handling and avoid hang
* migrate to crypton-connection and crypton-x509-system, adjust deps
* update to ghc-9.6, unix-2.8

## 0.6.3.2  -- 2021-10-25

* update README

## 0.6.3.1  -- 2021-09-08

* avoid setCurrentDirectory which is not thread-safe
* start watch before initial scan
* put filename without directory into filename header

## 0.6.3.0  -- 2021-09-06

* watch multiple dirs in hotfolder mode
* drop hinotify < 0.3.10 compatibility
* use RawFilePath for file names
* fix handling of non-ascii filenames in agitprop hotfolder

## 0.6.2.4  -- 2021-08-30

* depend on tls >= 1.5.0 (thread safety for writes)

## 0.6.2.2  -- 2021-08-25

* agitprop: option to move sent file to another dir instead of just removing

## 0.6.1.1  -- 2021-05-11

* disable inotify on non-linux

## 0.6.1.0  -- 2020-03-11

* add remaining message properties as callback environment variables, too

## 0.6.0.0  -- 2020-03-10

* update and amend README
* add --arg as alias for --args
* konsum: remove default binding to #
* plane: use -r (--routingkey) instead of -Q (--qname) (but still accept legacy -Q)
* print server properties

## 0.5.0.0  -- 2020-03-03

* add option -f (--prefetch) for arbeite, default 1 (was unset before)
* print a line after (not) receiving rpc answer
* catch IOException instead of SomeException in callback
* remove --enable-tests from cabal in Makefile
* reformat with hindent + stylish-haskell

## 0.4.5.1  -- 2020-02-19

* unify threadDelay in mainloop (reduce system load when sleeping)

## 0.4.5.0  -- 2020-02-10

* format with stylish-haskell
* agitprop: rename variables in code
* new options -d (--dirscan) and -u (--remove) for agitprop hotfolder mode
* cwd to hotfolder directory in agitprop hotfolder mode
* change logging of boolean parameters

## 0.4.4.1  -- 2020-08-19

* change default connection timeout from 60 to 600s
* update description

## 0.4.4.0  -- 2020-02-18

* push callback options into environment variables

## 0.4.3.0  -- 2020-02-10

* specify -R YES option for callback in case of a redelivered message

## 0.4.2.0  -- 2019-12-19

* introduce --cleanup (-j) to remove temp file
* change temp file name from konsum-* to amqp-utils-*
* handle all numeric types in amqp headers same
* update to amqp-0.19
* use utf8-string for header string values

## 0.4.1.0  -- 2019-12-09

* printparam -> Flexprint
* introduce --simple / -i
* review data types
* reformat with hindent
* update doc

## 0.4.0.1  -- 2019-12-04

* fix exit codes

## 0.4.0.0  -- 2019-11-11

* agitprop: show exchange arg
* agitprop does not need -q or -Q
* allow RPC with dedicated exchange
* add missing hFlush
* stdin / stdout handling without using /dev/
* cleanup plane answer file handling
* duplicate plane body to outfile and stderr

## 0.3.7.1  -- 2019-09-10

* connect timeout

## 0.3.6.0  -- 2019-02-15

* bug-fix: avoid deadlock in arbeite
* plane: add --header option
* rpc: log to stderr, result to stdout

## 0.3.4.0  -- 2018-07-21

* bug fix: re-add exception handler
* plane + arbeite: rpc client + server

## 0.3.3.1  -- 2018-07-10

* fix debian builds
* konsum options ack and requeuenack
* fix hotfolder mode
* enable parallel build
* tls, hinotify, lts-12.0 compat

## 0.3.2.0  -- 2018-07-04

* agitprop, a publisher
* optional publisher confirms
* hotfolder mode, file magic
* several options

## 0.3.0.2  -- 2018-04-24

* use ciphersuite\_default

## 0.3.0.1  -- 2018-03-04

* don't let the thread sleep too long

## 0.3.0.0  -- 2017-11-21

*  add nix with `amqp_0_18_1`
*  multiple bindings per queue
*  remove cool smart special options

## 0.2.2.0  -- 2017-11-20

* option -Q (set queue name for temporary exclusive queue)

## 0.2.1.5  -- 2017-09-25

* travis
* debian
* repair debian jessie
* callback: pass-through timestamp, ignore non-existing sha
* reduce load
* amqp 0.17, use coName

## 0.2.1.4  -- 2017-06-01

* First version. Released on an unsuspecting world.
