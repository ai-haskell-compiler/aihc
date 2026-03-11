
# HaTeX Changelog

This is the logchange of HaTeX. It is not exhaustive.
For a full list of changes, see the commit history of the
git repository:

https://codeberg.org/daniel-casanueva/HaTeX/commits/branch/main

# Changelog by versions

## 3.23.0.1
* Metadata update.
* Drop gitlab CI.

## 3.23.0.0
* Rename `forall` as `forall_` (Patrick Ritzenfeld).
* Drop trailing zeros and decimal point from rendered Floats and Doubles (David Fox).
* Fix line break command formatting (David Fox).
* Fix typo in `twoheadrightarrow` (Daniel Casanueva).

## 3.22.4.2
* Drop support for `base < 4.14`.
* Support `hashable-1.5.0.0`.
* Remove uses of CPP.

## 3.22.4.1
* QuickCheck instance fix.

## 3.22.4.0
* Define `(<>)` instead of `mappend`.
* Use `fmap` instead of `liftM`.
* New function: `applyDOIReferenceResolves`.
* Make parser test more lenient (it was commented due to issue #144).
* Metadata update.
* Unify files `ReleaseNotes` and `CHANGELOG.md`.

## From 3.22.3.1 to 3.22.3.2
* Relax transformers dependency upper bound from `< 0.6` to `< 0.7`.

## From 3.22.3.0 to 3.22.3.1
* Allow bytestring version 0.11 (#153).
* Allow hashable 1.4 and text 2.0 (#154).

## From 3.22.2.0 to 3.22.3.0
* Add more math symbols. (leftaroundabout)

## From 3.22.1.0 to 3.22.2.0
* New render method: renderBuilder.

## From 3.22.0.0 to 3.22.1.0
* Export PlainDOI type synonym.
* Comment out QuickCheck parser test. See https://github.com/Daniel-Diaz/HaTeX/issues/144.
* New function `qed`.

## From 3.21.0.0 to 3.22.0.0
* More liftLX, commX and envX functions. (KommuSoft)
* New module: Text.LaTeX.Packages.Acronym. (KommuSoft)
* Expanded BibLaTeX module with new functionality. (leftaroundabout)
* Dropped support for GHC-7.8 (and earlier versions).

## From 3.20.0.1 to 3.21.0.0

* Replace wl-pprint-extras with prettyprinter in the latex pretty-printer.
* Now 'table' takes a list of positions. (breaking change)
* Added constructors Here and ForcePos to the Pos type.
* Make argument of 'nopagebreak' optional.

## From 3.18.0.0 to 3.19.0.0

* Fix build with GHC 8.4.1 (#113) (leftaroundabout).
* Added Multirow, Bigstrut, and Lscape modules (#107, #111) (romildo).
* More spacing and line breaking commands (#108, #110) (romildo).

## From 3.17.3.1 to 3.18.0.0

* _Warning:_ Using 3.18.0.0 is not recommended, since it fails to build with GHC 8.4.1.
  Please, upgrade to 3.19.0.0.

* New bibtex module (leftaroundabout).
* New function 'squareBraces' (NorfairKing).
* New function 'table' (leftaroundabout).

## From 3.17.1.0 to 3.17.2.0

* Semigroup instance for LaTeX.
* Data, Generic, and Typeable instances for LaTeX
* and related types.

## From 3.17.0.0 to 3.17.1.0

* New math space commands (romildo).
* New function: mapLaTeXT (ddssff).
* Some fixes for qrcode package (L3n41c).

## From 3.16.2.0 to 3.17.0.0

* New 'array' command (NorfairKing).
* Added package options for the hyperref package related to PDF metadata (dmcclean).
* QRCode module (dmcclean).
* New math symbols (leftaroundabout).
* Added 'cases' environment (NorfairKing).
* Changed the way subscripts and superscripts work.
  See [#67](https://github.com/Daniel-Diaz/HaTeX/pull/67).
  Also [#78](https://github.com/Daniel-Diaz/HaTeX/pull/78).
* LaTeX parser is now configurable.
  Currently, only configurable option is verbatim
  environments.

## From 3.16.1.1 to 3.16.2.0

* New differentiation symbols (AMSMath).
* Fix for integralFromTo.
* Extend definitions for the Num class instance of LaTeXT.
* Show and Eq instances of LaTeXT dissappear if base version
  is greater or equal to 4.5.0.0.

## From 3.16.1.0 to 3.16.1.1

* Full compatibility without warnings with GHC-7.10.

## From 3.16.0.0 to 3.16.1.0

* Pretty-printer: Use softline instead of line after commands.
* Compatibility with GHC-7.10.
* Added accent commands to AMSMath (dmcclean).
* Missing Num and Floating class methods now have a default implementation,
  using the new `operatorname` function (dmcclean).
* Added `imath` and `jmath` to AMSMath (dmcclean).
* Support for parsec-3.1.9 (snoyberg).

Thanks to Douglas McClean (dmcclean@GitHub) for the AMSMath additions.

## From 3.15.0.0 to 3.16.0.0

* New package implemented: relsize.
    Thanks José Romildo Malaquias.
* Fixed bug in autoBrackets ([#42](https://github.com/Daniel-Diaz/HaTeX/pull/42)).

## From 3.14.0.0 to 3.15.0.0

* New package implemented: AMSSymb.
* Package beamer further developed.
* Bug fix: [#35](https://github.com/Daniel-Diaz/HaTeX/issues/35).
* Added common numeric sets to AMSSymb.
* Breaking change: AMSMath functions 'pm' and 'mp' changed their
  type from `LaTeXC l => l -> l -> l` to `LaTeXC l => l`.
* Additions to the AMSMath module.

## From 3.13.1.0 to 3.14.0.0

* Fixed link in cabal file.
* Added support for arguments delimited by parenthesis (experimental).
* More tests on parsing.
* Parser now backtracks when failing in argument parsing.

## From 3.13.0.1 to 3.13.1.0

* New function ``matrixTabular`` to create tables from matrices.
* Modified LaTeX Monoid instance to make monoid laws hold.
* Some documentation improvements.
* Added this CHANGELOG!

## 3.4

* Num instance for LaTeXT.
* New re-exports: liftIO.
* Relaxed transformers package dependency to: >= 0.2.2 && < 0.4
* Changed infix rule of (=:) and (/=:).
* More documentation notes.
* New functions: hfill, vfill.
* The internal representation of LaTeXT has changed to prevent
  a set of errors.
* Changes in Text.LaTeX.
* Several minor fixes.

## 3.3

* New functions 'protectString' and 'protectText'.
* New function 'rendertexM'.
* New environments: 'figure' and 'caption'.
* New example: simple.hs.
* Updated version of transfomers to 0.3.*.
* Trees implemented: datatype and Qtree package.
* Render instances for Integer and Double.
* New docs.
* Class system implemented. Monad modules dropped.

## 3.2.0.1

* This version only fix the patch that makes HaTeX compatible with GHC 7.4.

## 3.2

### Highlights:

* Parser implemented (New dependency on parsec).
* Greek alphabet added to AMSMath.
* New LaTeX package: graphicx.
* Function 'documentclass' changed.

### Other changes:

* Dependency changed from mtl to transformers.
* New commands: par, textwidth and linewidth.
* New environment: minipage.
* Compatibility with GHC 7.4 (with CPP extension).
* Function 'lift' is now re-exported in Text.LaTeX.Base.Writer module.
* New function 'renderChars' in the Render module.
* ReadMe edited, now with ToDo list.

## 3.1.1

* Dependency relaxed from 'mtl == 2.*' to 'transformers == 0.2.*'.

## 3.1.0

### Highlights:

* Added warnings (See Text.LaTeX.Base.Warnings).
* Added an "Examples" directory.
* Num instance for LaTeX and LaTeXT.
* New package implemented "AMSThm" (See Text.LaTeX.Packages.AMSThm).

### Changes by modules:

Base modules:

* Text.LaTeX.Base.Syntax: Added Eq instance to LaTeX and TeXArg.
* Text.LaTeX.Base.Writer: Added MonadTrans instance to LaTeXT.
    New functions: execLaTeXTWarn, liftFun, liftOp.
* Text.LaTeX.Base.Types: Added Eq instance to Label.
* Text.LaTeX.Base: Added Num instance to LaTeX.
* Text.LaTeX.Base.Monad: Added Num instance to LaTeXT.
* Text.LaTeX.Base.Commands: New function: between.

Package modules:

* Text.LaTeX.Packages.AMSMath: New symbols: (=:) , (/=:) , forall
    , dagger, ddagger, in_ , ni, (<:) , (<=:), (>:) , (>=:)

## 3.0.0

* First release of the third version of HaTeX.
