# Release history for select-rpms

## 0.3.1 (2025-09-13)
- selections that do not match any RPMs no longer error
- installing debuginfo/debugsource now possible if requested
- default presets for certain package prefixes (currently only ghc*)
- add pkgMgrOpt (from koji-tool)
- add installRPMsAllowErasing which extends installRPMs
- installRPMs (installCommand): use rpm -U since package may exist
- checkSelection: now used directly by decideRPMs

## 0.3.0 (2025-06-04)
- ExistingStrategy: add ExistingError which aborts for existing installed pkg

## 0.2.0 (2024-12-01)
- extend ExistingStrategy with ExistingOnly
- use sudoLog with --debug, falling back to old sudo_
- selectRpmsOptions: renamed from selectRpmsOption
- add existingStrategyOption (for optparse-applicative)

## 0.1.0 (2024-08-14)
- initial release for fbrnch and koji-tool
- original code was taken from koji-tool Install
