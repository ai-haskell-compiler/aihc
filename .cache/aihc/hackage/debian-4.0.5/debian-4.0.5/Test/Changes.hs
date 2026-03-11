{-# LANGUAGE StandaloneDeriving, TemplateHaskell #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans #-}
module Changes where

import Debian.Changes
import Debian.Codename (Codename, parseCodename)
import Debian.Pretty (PP(..))
import Debian.Version (parseDebianVersion, parseDebianVersion')
import Debian.TH (here)
import Distribution.Pretty (pretty)
import Test.HUnit
import Text.PrettyPrint (render)

s3 = unlines
     ["name (version) dist; urgency=urgency",
      "  * details",
      " -- David Fox <dsf@seereason.com>  Wed, 21 Nov 2007 01:26:57 +0000"]

s4 = unlines
     ["haskell-regex-compat (0.92-3+seereason1~jaunty4) jaunty-seereason; urgency=low",
      "",
      "  [ Joachim Breitner ]",
      "  * Adjust priority according to override file",
      "  * Depend on hscolour (Closes: #550769)",
      "",
      "  [ Marco Túlio Gontijo e Silva ]",
      "",
      " -- David Fox <dsf@seereason.com>  Wed, 21 Nov 2007 01:26:57 +0000"]

s1 = unlines
     ["haskell-regex-compat (0.92-3+seereason1~jaunty4) jaunty-seereason; urgency=low",
      "",
      "  [ Joachim Breitner ]",
      "  * Adjust priority according to override file",
      "  * Depend on hscolour (Closes: #550769)",
      "",
      "  [ Marco Túlio Gontijo e Silva ]",
      "  * debian/control: Use more sintetic name for Vcs-Darcs.",
      "  * Built from sid apt pool",
      "  * Build dependency changes:",
      "     cpphs:                    1.9-1+seereason1~jaunty5     -> 1.9-1+seereason1~jaunty6",
      "     ghc6:                     6.10.4-1+seereason5~jaunty1  -> 6.12.1-0+seereason1~jaunty1",
      "     ghc6-doc:                 6.10.4-1+seereason5~jaunty1  -> 6.12.1-0+seereason1~jaunty1",
      "     ghc6-prof:                6.10.4-1+seereason5~jaunty1  -> 6.12.1-0+seereason1~jaunty1",
      "     haddock:                  2.4.2-3+seereason3~jaunty1   -> 6.12.1-0+seereason1~jaunty1",
      "     haskell-devscripts:       0.6.18-21+seereason1~jaunty1 -> 0.6.18-23+seereason1~jaunty1",
      "     haskell-regex-base-doc:   0.93.1-5+seereason1~jaunty1  -> 0.93.1-5++1+seereason1~jaunty1",
      "     haskell-regex-posix-doc:  0.93.2-4+seereason1~jaunty1  -> 0.93.2-4+seereason1~jaunty2",
      "     libghc6-regex-base-dev:   0.93.1-5+seereason1~jaunty1  -> 0.93.1-5++1+seereason1~jaunty1",
      "     libghc6-regex-base-prof:  0.93.1-5+seereason1~jaunty1  -> 0.93.1-5++1+seereason1~jaunty1",
      "     libghc6-regex-posix-dev:  0.93.2-4+seereason1~jaunty1  -> 0.93.2-4+seereason1~jaunty2",
      "     libghc6-regex-posix-prof: 0.93.2-4+seereason1~jaunty1  -> 0.93.2-4+seereason1~jaunty2",
      "",
      " -- SeeReason Autobuilder <autobuilder@seereason.org>  Fri, 25 Dec 2009 01:55:37 -0800",
      "",
      "haskell-regex-compat (0.92-3) unstable; urgency=low",
      "",
      "  [ Joachim Breitner ]",
      "  * Adjust priority according to override file",
      "  * Depend on hscolour (Closes: #550769)",
      "",
      "  [ Marco Túlio Gontijo e Silva ]",
      "  * debian/control: Use more sintetic name for Vcs-Darcs.",
      "",
      " -- Joachim Breitner <nomeata@debian.org>  Mon, 20 Jul 2009 13:05:35 +0200",
      "",
      "haskell-regex-compat (0.92-2) unstable; urgency=low",
      "",
      "  * Adopt package for the Debian Haskell Group",
      "  * Fix \"FTBFS with new dpkg-dev\" by adding comma to debian/control",
      "    (Closes: #536473)",
      "",
      " -- Joachim Breitner <nomeata@debian.org>  Mon, 20 Jul 2009 12:05:40 +0200",
      "",
      "haskell-regex-compat (0.92-1.1) unstable; urgency=low",
      "",
      "  * Rebuild for GHC 6.10.",
      "  * NMU with permission of the author.",
      "",
      " -- John Goerzen <jgoerzen@complete.org>  Mon, 16 Mar 2009 10:12:04 -0500",
      "",
      "haskell-regex-compat (0.92-1) unstable; urgency=low",
      "",
      "  * New upstream release",
      "  * debian/control:",
      "    - Bump Standards-Version. No changes needed.",
      "",
      " -- Arjan Oosting <arjan@debian.org>  Sun, 18 Jan 2009 00:05:02 +0100",
      "",
      "haskell-regex-compat (0.91-1) unstable; urgency=low",
      "",
      "  * Take over package from Ian, as I already maintain haskell-regex-base,",
      "    and move Ian to the Uploaders field.",
      "  * Packaging complete redone (based on my haskell-regex-base package).",
      "",
      " -- Arjan Oosting <arjan@debian.org>  Sat, 19 Jan 2008 16:48:39 +0100",
      "",
      "haskell-regex-compat (0.71.0.1-1) unstable; urgency=low",
      "",
      "  * Initial release (used to be part of ghc6).",
      "  * Using \"Generic Haskell cabal library packaging files v9\".",
      "",
      " -- Ian Lynagh (wibble) <igloo@debian.org>  Wed, 21 Nov 2007 01:26:57 +0000"]

s2 = unlines
     ["haskell-haskeline (0.6.1.6-1+seereason1~jaunty6) jaunty-seereason; urgency=low",
      "",
      "  * New upstream version.",
      "  * Remove extensible-exceptions patch, since ghc6 now ships it.",
      "  * debian/control:",
      "    - Use versioned Build-Depends.",
      "    - Use unversioned Recommends for ghc6-doc in libghc6-terminfo-doc.",
      "    - Use haskell Section.",
      "    - Use new Standards-Version: 3.8.1.",
      "    - Use DM-Upload-Allowed: yes.",
      "    - Use haskell:Recommends and haskell:Suggests.",
      "    - Don't use shlibs:Depends for -prof.",
      "    - Split dependencies in more than one line.",
      "  * Built from sid apt pool",
      "  * Build dependency changes:",
      "     ghc6:                     6.10.4-1+seereason5~jaunty1  -> 6.12.1-0+seereason1~jaunty1",
      "     ghc6-doc:                 6.10.4-1+seereason5~jaunty1  -> 6.12.1-0+seereason1~jaunty1",
      "     ghc6-prof:                6.10.4-1+seereason5~jaunty1  -> 6.12.1-0+seereason1~jaunty1",
      "     haddock:                  2.4.2-3+seereason3~jaunty1   -> 6.12.1-0+seereason1~jaunty1",
      "     haskell-devscripts:       0.6.18-21+seereason1~jaunty1 -> 0.6.18-23+seereason1~jaunty1",
      "     libghc6-mtl-dev:          1.1.0.2-7+seereason3~jaunty7 -> 1.1.0.2-7+seereason3~jaunty8",
      "     libghc6-mtl-doc:          1.1.0.2-7+seereason3~jaunty7 -> 1.1.0.2-7+seereason3~jaunty8",
      "     libghc6-mtl-prof:         1.1.0.2-7+seereason3~jaunty7 -> 1.1.0.2-7+seereason3~jaunty8",
      "     libghc6-terminfo-dev:     0.3.0.2-2+seereason1~jaunty5 -> 0.3.0.2-2+seereason1~jaunty6",
      "     libghc6-terminfo-doc:     0.3.0.2-2+seereason1~jaunty5 -> 0.3.0.2-2+seereason1~jaunty6",
      "     libghc6-terminfo-prof:    0.3.0.2-2+seereason1~jaunty5 -> 0.3.0.2-2+seereason1~jaunty6",
      "     libghc6-utf8-string-dev:  0.3.5-1+seereason3~jaunty7   -> 0.3.5-1++1+seereason1~jaunty1",
      "     libghc6-utf8-string-doc:  0.3.5-1+seereason3~jaunty7   -> 0.3.5-1++1+seereason1~jaunty1",
      "     libghc6-utf8-string-prof: 0.3.5-1+seereason3~jaunty7   -> 0.3.5-1++1+seereason1~jaunty1",
      "",
      " -- SeeReason Autobuilder <autobuilder@seereason.org>  Fri, 25 Dec 2009 13:48:18 -0800",
      "",
      "haskell-haskeline (0.6.1.6-1) unstable; urgency=low",
      "",
      "  * New upstream version.",
      "  * Remove extensible-exceptions patch, since ghc6 now ships it.",
      "  * debian/control:",
      "    - Use versioned Build-Depends.",
      "    - Use unversioned Recommends for ghc6-doc in libghc6-terminfo-doc.",
      "    - Use haskell Section.",
      "    - Use new Standards-Version: 3.8.1.",
      "    - Use DM-Upload-Allowed: yes.",
      "    - Use haskell:Recommends and haskell:Suggests.",
      "    - Don't use shlibs:Depends for -prof.",
      "    - Split dependencies in more than one line.",
      "",
      " -- Marco Túlio Gontijo e Silva <marcot@holoscopio.com>  Tue, 02 Jun 2009 10:18:27 -0300",
      "",
      "haskell-haskeline (0.6.1.3-1) unstable; urgency=low",
      "",
      "  * Initial Debian package. (Closes: #496961)",
      "",
      " -- Marco Túlio Gontijo e Silva <marcot@holoscopio.com>  Wed, 11 Mar 2009 18:58:06 -0300",
      ""]

test5 = TestCase (assertEqual "haskell-regex-compat changelog 1" s1 (render . pretty . PP . either (const (error "parse")) id . parseChangeLog $ s1))

test3 =
    TestCase (assertEqual "haskell-regex-compat changelog 2" expected (parseEntries s3))
    where expected = [Right (Entry {logPackage = "name", logVersion = parseDebianVersion' "version", logDists = [parseCodename "dist"], logUrgency = "urgency", logComments = "  * details\n", logWho = "David Fox <dsf@seereason.com>", logDate = "Wed, 21 Nov 2007 01:26:57 +0000"})]

test4 =
    TestCase (assertEqual "haskell-regex-compat changelog 3" expected (parseEntries s4))
    where expected = [Right (Entry {logPackage = "haskell-regex-compat",
                                     logVersion = parseDebianVersion' "0.92-3+seereason1~jaunty4",
                                     logDists = [parseCodename "jaunty-seereason"],
                                     logUrgency = "low",
                                     logComments = "  [ Joachim Breitner ]\n  * Adjust priority according to override file\n  * Depend on hscolour (Closes: #550769)\n\n  [ Marco T\250lio Gontijo e Silva ]\n",
                                     logWho = "David Fox <dsf@seereason.com>",
                                     logDate = "Wed, 21 Nov 2007 01:26:57 +0000"})]

test1 =
    TestCase (assertEqual "haskell-regex-compat changelog 4" expected (either (const (error "parse")) id (parseChangeLog s1)))
    where expected = ChangeLog [(Entry {logPackage = "haskell-regex-compat", logVersion = parseDebianVersion' "0.92-3+seereason1~jaunty4", logDists = [parseCodename "jaunty-seereason"], logUrgency = "low", logComments = "  [ Joachim Breitner ]\n  * Adjust priority according to override file\n  * Depend on hscolour (Closes: #550769)\n\n  [ Marco T\250lio Gontijo e Silva ]\n  * debian/control: Use more sintetic name for Vcs-Darcs.\n  * Built from sid apt pool\n  * Build dependency changes:\n     cpphs:                    1.9-1+seereason1~jaunty5     -> 1.9-1+seereason1~jaunty6\n     ghc6:                     6.10.4-1+seereason5~jaunty1  -> 6.12.1-0+seereason1~jaunty1\n     ghc6-doc:                 6.10.4-1+seereason5~jaunty1  -> 6.12.1-0+seereason1~jaunty1\n     ghc6-prof:                6.10.4-1+seereason5~jaunty1  -> 6.12.1-0+seereason1~jaunty1\n     haddock:                  2.4.2-3+seereason3~jaunty1   -> 6.12.1-0+seereason1~jaunty1\n     haskell-devscripts:       0.6.18-21+seereason1~jaunty1 -> 0.6.18-23+seereason1~jaunty1\n     haskell-regex-base-doc:   0.93.1-5+seereason1~jaunty1  -> 0.93.1-5++1+seereason1~jaunty1\n     haskell-regex-posix-doc:  0.93.2-4+seereason1~jaunty1  -> 0.93.2-4+seereason1~jaunty2\n     libghc6-regex-base-dev:   0.93.1-5+seereason1~jaunty1  -> 0.93.1-5++1+seereason1~jaunty1\n     libghc6-regex-base-prof:  0.93.1-5+seereason1~jaunty1  -> 0.93.1-5++1+seereason1~jaunty1\n     libghc6-regex-posix-dev:  0.93.2-4+seereason1~jaunty1  -> 0.93.2-4+seereason1~jaunty2\n     libghc6-regex-posix-prof: 0.93.2-4+seereason1~jaunty1  -> 0.93.2-4+seereason1~jaunty2\n", logWho = "SeeReason Autobuilder <autobuilder@seereason.org>", logDate = "Fri, 25 Dec 2009 01:55:37 -0800"}),
                      (Entry {logPackage = "haskell-regex-compat", logVersion = parseDebianVersion' "0.92-3", logDists = [parseCodename "unstable"], logUrgency = "low", logComments = "  [ Joachim Breitner ]\n  * Adjust priority according to override file\n  * Depend on hscolour (Closes: #550769)\n\n  [ Marco T\250lio Gontijo e Silva ]\n  * debian/control: Use more sintetic name for Vcs-Darcs.\n", logWho = "Joachim Breitner <nomeata@debian.org>", logDate = "Mon, 20 Jul 2009 13:05:35 +0200"}),
                      (Entry {logPackage = "haskell-regex-compat", logVersion = parseDebianVersion' "0.92-2", logDists = [parseCodename "unstable"], logUrgency = "low", logComments = "  * Adopt package for the Debian Haskell Group\n  * Fix \"FTBFS with new dpkg-dev\" by adding comma to debian/control\n    (Closes: #536473)\n", logWho = "Joachim Breitner <nomeata@debian.org>", logDate = "Mon, 20 Jul 2009 12:05:40 +0200"}),
                      (Entry {logPackage = "haskell-regex-compat", logVersion = parseDebianVersion' "0.92-1.1", logDists = [parseCodename "unstable"], logUrgency = "low", logComments = "  * Rebuild for GHC 6.10.\n  * NMU with permission of the author.\n", logWho = "John Goerzen <jgoerzen@complete.org>", logDate = "Mon, 16 Mar 2009 10:12:04 -0500"}),
                      (Entry {logPackage = "haskell-regex-compat", logVersion = parseDebianVersion' "0.92-1", logDists = [parseCodename "unstable"], logUrgency = "low", logComments = "  * New upstream release\n  * debian/control:\n    - Bump Standards-Version. No changes needed.\n", logWho = "Arjan Oosting <arjan@debian.org>", logDate = "Sun, 18 Jan 2009 00:05:02 +0100"}),
                      (Entry {logPackage = "haskell-regex-compat", logVersion = parseDebianVersion' "0.91-1", logDists = [parseCodename "unstable"], logUrgency = "low", logComments = "  * Take over package from Ian, as I already maintain haskell-regex-base,\n    and move Ian to the Uploaders field.\n  * Packaging complete redone (based on my haskell-regex-base package).\n", logWho = "Arjan Oosting <arjan@debian.org>", logDate = "Sat, 19 Jan 2008 16:48:39 +0100"}),
                      (Entry {logPackage = "haskell-regex-compat", logVersion = parseDebianVersion' "0.71.0.1-1", logDists = [parseCodename "unstable"], logUrgency = "low", logComments = "  * Initial release (used to be part of ghc6).\n  * Using \"Generic Haskell cabal library packaging files v9\".\n", logWho = "Ian Lynagh (wibble) <igloo@debian.org>", logDate = "Wed, 21 Nov 2007 01:26:57 +0000"})]

test2 =
    TestCase (assertEqual "haskell-regex-compat changelog" expected (parseEntries s2))
    where expected = [Right (Entry {logPackage = "haskell-haskeline",
                                     logVersion = parseDebianVersion' "0.6.1.6-1+seereason1~jaunty6",
                                     logDists = [parseCodename "jaunty-seereason"],
                                     logUrgency = "low",
                                     logComments = "  * New upstream version.\n  * Remove extensible-exceptions patch, since ghc6 now ships it.\n  * debian/control:\n    - Use versioned Build-Depends.\n    - Use unversioned Recommends for ghc6-doc in libghc6-terminfo-doc.\n    - Use haskell Section.\n    - Use new Standards-Version: 3.8.1.\n    - Use DM-Upload-Allowed: yes.\n    - Use haskell:Recommends and haskell:Suggests.\n    - Don't use shlibs:Depends for -prof.\n    - Split dependencies in more than one line.\n  * Built from sid apt pool\n  * Build dependency changes:\n     ghc6:                     6.10.4-1+seereason5~jaunty1  -> 6.12.1-0+seereason1~jaunty1\n     ghc6-doc:                 6.10.4-1+seereason5~jaunty1  -> 6.12.1-0+seereason1~jaunty1\n     ghc6-prof:                6.10.4-1+seereason5~jaunty1  -> 6.12.1-0+seereason1~jaunty1\n     haddock:                  2.4.2-3+seereason3~jaunty1   -> 6.12.1-0+seereason1~jaunty1\n     haskell-devscripts:       0.6.18-21+seereason1~jaunty1 -> 0.6.18-23+seereason1~jaunty1\n     libghc6-mtl-dev:          1.1.0.2-7+seereason3~jaunty7 -> 1.1.0.2-7+seereason3~jaunty8\n     libghc6-mtl-doc:          1.1.0.2-7+seereason3~jaunty7 -> 1.1.0.2-7+seereason3~jaunty8\n     libghc6-mtl-prof:         1.1.0.2-7+seereason3~jaunty7 -> 1.1.0.2-7+seereason3~jaunty8\n     libghc6-terminfo-dev:     0.3.0.2-2+seereason1~jaunty5 -> 0.3.0.2-2+seereason1~jaunty6\n     libghc6-terminfo-doc:     0.3.0.2-2+seereason1~jaunty5 -> 0.3.0.2-2+seereason1~jaunty6\n     libghc6-terminfo-prof:    0.3.0.2-2+seereason1~jaunty5 -> 0.3.0.2-2+seereason1~jaunty6\n     libghc6-utf8-string-dev:  0.3.5-1+seereason3~jaunty7   -> 0.3.5-1++1+seereason1~jaunty1\n     libghc6-utf8-string-doc:  0.3.5-1+seereason3~jaunty7   -> 0.3.5-1++1+seereason1~jaunty1\n     libghc6-utf8-string-prof: 0.3.5-1+seereason3~jaunty7   -> 0.3.5-1++1+seereason1~jaunty1\n",
                                     logWho = "SeeReason Autobuilder <autobuilder@seereason.org>",
                                     logDate = "Fri, 25 Dec 2009 13:48:18 -0800"}),
                      Right (Entry {logPackage = "haskell-haskeline",
                                     logVersion = parseDebianVersion' "0.6.1.6-1",
                                     logDists = [parseCodename "unstable"],
                                     logUrgency = "low",
                                     logComments = "  * New upstream version.\n  * Remove extensible-exceptions patch, since ghc6 now ships it.\n  * debian/control:\n    - Use versioned Build-Depends.\n    - Use unversioned Recommends for ghc6-doc in libghc6-terminfo-doc.\n    - Use haskell Section.\n    - Use new Standards-Version: 3.8.1.\n    - Use DM-Upload-Allowed: yes.\n    - Use haskell:Recommends and haskell:Suggests.\n    - Don't use shlibs:Depends for -prof.\n    - Split dependencies in more than one line.\n",
                                     logWho = "Marco T\250lio Gontijo e Silva <marcot@holoscopio.com>",
                                     logDate = "Tue, 02 Jun 2009 10:18:27 -0300"}),
                      Right (Entry {logPackage = "haskell-haskeline",
                                     logVersion = parseDebianVersion' "0.6.1.3-1",
                                     logDists = [parseCodename "unstable"],
                                     logUrgency = "low",
                                     logComments = "  * Initial Debian package. (Closes: #496961)\n",
                                     logWho = "Marco T\250lio Gontijo e Silva <marcot@holoscopio.com>",
                                     logDate = "Wed, 11 Mar 2009 18:58:06 -0300"})]

changesTests = [test3, test4, test1, test2, test5]
