{-# LANGUAGE CPP, FlexibleInstances, OverloadedStrings, StandaloneDeriving #-}
module Control where

import Test.HUnit
import Data.Monoid ((<>))
import Data.List as L (intercalate)
import Data.Text as T (Text)
import Data.Version (showVersion)
import Debian.Control
import Debian.Control.Policy
import Debian.Control.Text ({- Pretty instances -})
import Debian.Pretty (prettyShow)
import Debian.Relation
import Debian.Version (parseDebianVersion, parseDebianVersion')
import Distribution.Pretty (pretty)
import Paths_debian (version)
import Text.Parsec.Error (ParseError)
import Text.PrettyPrint.HughesPJClass (Doc, text)
import Text.Regex.TDFA ((=~), MatchResult(..))

#if !MIN_VERSION_pretty(1,1,2)
instance Eq Doc where
    a == b = show a == show b
#endif

instance Eq DebianControl where
    a == b = unDebianControl a == unDebianControl b
-- deriving instance Show (Control' Text)
-- deriving instance Show (Paragraph' Text)
-- deriving instance Show (Field' Text)

replaceStrings :: String -> String -> String -> String
replaceStrings old new x =
    case x =~ old of
      mr | null (mrMatch mr) -> x
      mr -> mrBefore mr <> new <> replaceStrings old new (mrAfter mr)

-- Additional tests of the results of parsing additional
-- inter-paragraph newlines, or missing terminating newlines, would be
-- good.
controlTests =
    [ TestCase (assertEqual "pretty1" (pretty control) (either (error "parser failed") pretty (parseControl "debian/control" sample)))
    , TestCase (assertEqual "pretty2" (text sample) (pretty control))
    , TestCase (assertEqual "pretty3" (text (head paragraphs <> "\n")) (pretty (head (unControl control))))
    -- The Pretty class instances are distinct implementations from
    -- those in Debian.Control.PrettyPrint.  Not sure why, there is a
    -- terse note about performance concerns.
    , TestCase (assertEqual "pretty4" (text sample) (pretty control))
    , TestCase (assertEqual "pretty5" (text (head paragraphs <> "\n")) (pretty (head (unControl control))))
    , TestCase (validateDebianControl control >>= \ vc -> assertEqual "policy1" (Right (unsafeDebianControl control)) vc) -- validate control file
    , TestCase (validateDebianControl control >>= \ vc -> assertEqual "policy2" (Right (Just builddeps)) (either Left (debianRelations "Build-Depends") vc)) -- parse build deps
    , TestCase (validateDebianControl control >>= \ vc -> assertEqual "policy3" (Right Nothing) (either Left (debianRelations "Foo") vc)) -- absent field
    , TestCase (parseDebianControlFromFile "Test/Control.hs" >>= \ vc ->
                assertEqual "policy4"
                            -- Exceptions have bogus Eq instances, so we need to show then compare.
                            "Left \"src/Debian/Control/Policy.hs\"(line ?, column ?): ParseControlError \"Test/Control.hs\" (line ?, column ?):\nFailed to parse Test/Control.hs"
                            (replaceStrings "[0-9]+" "?" $ show (either Left (debianRelations "Foo") vc)))
    , TestCase (parseDebianControlFromFile "nonexistant" >>= \ vc ->
                assertEqual "policy5"
                            "Left \"src/Debian/Control/Policy.hs\"(line ?, column ?): IOError nonexistant: withBinaryFile: does not exist (No such file or directory)"
                            (replaceStrings "[0-9]+" "?" . replaceStrings "openBinaryFile" "withBinaryFile" . replaceStrings "openFile" "withBinaryFile" $ show (either Left (debianRelations "Foo") vc)))

    -- Test whether embedded newlines in field values can be mistaken
    -- for field or paragraph divisions.  In cases pretty7 and pretty9
    -- the parsed output is not correct, so the buggy result is placed
    -- in the "expected" position.
    , TestCase (assertEqual "pretty6" input6 parsed6)
    , TestCase (assertEqual "pretty7" expected7 parsed7)
    , TestCase (assertEqual "pretty8" input8 parsed8)
    , TestCase (assertEqual "pretty9" expected9 parsed9)
    ]
    where
      input6 = Control {unControl = [Paragraph [Field ("Field1", " field1 begins\n  Field1a: indented text that looks like a field")]]} :: Control' String
      input7 = Control {unControl = [Paragraph [Field ("Field1", " field1 begins\nField1a: text that looks like a field")]]} :: Control' String
      -- parsed7buggy = Control {unControl = [Paragraph [Field ("Field1"," field1 begins"),Field ("Field1a"," text that looks like a field")]]} :: Control' String
      expected7 =    Control {unControl = [Paragraph [Field ("Field1"," field1 begins\n Field1a: text that looks like a field")]]}
      input8 = Control {unControl = [Paragraph [Field ("Field1", " field1 content"), Field ("Field2", " an actual second field")]]} :: Control' String
      input9 = Control {unControl = [Paragraph [Field ("Field1", " field1 content\n"), Field ("Field2", " an actual second field")]]} :: Control' String
      -- parsed9buggy = Control {unControl = [Paragraph [Field ("Field1"," field1 content")],Paragraph [Field ("Field2"," an actual second field")]]} :: Control' String
      expected9 =    Control {unControl = [Paragraph [Field ("Field1"," field1 content"),Field ("Field2"," an actual second field")]]}
      (Right parsed6) = parseControl "string" (prettyShow input6) :: Either ParseError (Control' String)
      (Right parsed7) = parseControl "string" (prettyShow input7) :: Either ParseError (Control' String)
      (Right parsed8) = parseControl "string" (prettyShow input8) :: Either ParseError (Control' String)
      (Right parsed9) = parseControl "string" (prettyShow input9) :: Either ParseError (Control' String)

-- | These paragraphs have no terminating newlines.  They are added
-- where appropriate to the expected test results.
paragraphs :: [String]
paragraphs =
    [ "Source: haskell-debian\nSection: haskell\nPriority: extra\nMaintainer: Debian Haskell Group <pkg-haskell-maintainers@lists.alioth.debian.org>\nUploaders: Joachim Breitner <nomeata@debian.org>\nBuild-Depends: debhelper (>= 7)\n  , cdbs\n  , haskell-devscripts (>= 0.7)\n  , ghc\n  , ghc-prof\n  , libghc-hunit-dev\n  , libghc-hunit-prof\n  , libghc-mtl-dev\n  , libghc-mtl-prof\n  , libghc-parsec3-dev\n  , libghc-parsec3-prof\n  , libghc-pretty-class-dev\n  , libghc-pretty-class-prof\n  , libghc-process-extras-dev (>= 0.4)\n  , libghc-process-extras-prof (>= 0.4)\n  , libghc-regex-compat-dev\n  , libghc-regex-compat-prof\n  , libghc-regex-tdfa-dev (>= 1.1.3)\n  , libghc-regex-tdfa-prof\n  , libghc-bzlib-dev (>= 0.5.0.0-4)\n  , libghc-bzlib-prof\n  , libghc-haxml-prof (>= 1:1.20)\n  , libghc-unixutils-dev (>= 1.50)\n  , libghc-unixutils-prof (>= 1.50)\n  , libghc-zlib-dev\n  , libghc-zlib-prof\n  , libghc-network-dev (>= 2.4)\n  , libghc-network-prof (>= 2.4)\n  , libghc-utf8-string-dev\n  , libghc-utf8-string-prof,\n  , libcrypto++-dev\nBuild-Depends-Indep: ghc-doc\n  , libghc-hunit-doc\n  , libghc-mtl-doc\n  , libghc-parsec3-doc\n  , libghc-pretty-class-doc\n  , libghc-process-extras-doc (>= 0.4)\n  , libghc-regex-compat-doc\n  , libghc-regex-tdfa-doc\n  , libghc-bzlib-doc\n  , libghc-haxml-doc (>= 1:1.20)\n  , libghc-unixutils-doc (>= 1.50)\n  , libghc-zlib-doc\n  , libghc-network-doc (>= 2.4)\n  , libghc-utf8-string-doc\nStandards-Version: 3.9.2\nHomepage: http://hackage.haskell.org/package/debian\nVcs-Darcs: http://darcs.debian.org/pkg-haskell/haskell-debian\nVcs-Browser: http://darcs.debian.org/cgi-bin/darcsweb.cgi?r=pkg-haskell/haskell-debian",
      "Package: libghc-debian-dev\nArchitecture: any\nDepends: ${haskell:Depends}\n  , ${shlibs:Depends}\n  , ${misc:Depends}\nRecommends: ${haskell:Recommends}\nSuggests: ${haskell:Suggests}\nProvides: ${haskell:Provides}\nDescription: Haskell library for working with the Debian package system\n This package provides a library for the Haskell programming language.\n See http://www.haskell.org/ for more information on Haskell.\n .\n This library includes modules covering almost every aspect of the Debian\n packaging system, including low level data types such as version numbers\n and dependency relations, on up to the types necessary for computing and\n installing build dependencies, building source and binary packages,\n and inserting them into a repository.\n .\n This package contains the libraries compiled for GHC 6.",
      "Package: libghc-debian-prof\nArchitecture: any\nDepends: ${haskell:Depends}\n  , ${shlibs:Depends}\n  , ${misc:Depends}\nRecommends: ${haskell:Recommends}\nSuggests: ${haskell:Suggests}\nProvides: ${haskell:Provides}\nDescription: Profiling library for working with the Debian package system\n This package provides a library for the Haskell programming language,\n compiled for profiling.\n See http://www.haskell.org/ for more information on Haskell.\n .\n This library includes modules covering almost every aspect of the Debian\n packaging system, including low level data types such as version numbers\n and dependency relations, on up to the types necessary for computing and\n installing build dependencies, building source and binary packages,\n and inserting them into a repository.\n .\n This package contains the profiling libraries compiled for GHC 6.",
      "Package: libghc-debian-doc\nSection: doc\nArchitecture: all\nDepends: ${misc:Depends}, ${haskell:Depends}\nRecommends: ${haskell:Recommends}\nSuggests: ${haskell:Suggests}\nDescription: Documentation for Debian package system library\n This package provides the documentation for a library for the Haskell\n programming language.\n See http://www.haskell.org/ for more information on Haskell.\n .\n This library includes modules covering almost every aspect of the Debian\n packaging system, including low level data types such as version numbers\n and dependency relations, on up to the types necessary for computing and\n installing build dependencies, building source and binary packages,\n and inserting them into a repository.\n .\n This package contains the library documentation.",
      "Package: haskell-debian-utils\nSection: devel\nArchitecture: any\nDepends: ghc, ${misc:Depends}, ${shlibs:Depends}\nRecommends: apt-file\nDescription: Various helpers to work with Debian packages\n This package contains tools shipped with the Haskell library \8220debian\8221:\n .\n   * fakechanges:\n     Sometimes you have the .debs, .dsc, .tar.gz, .diff.gz, etc from a package\n     build, but not the .changes file. This package lets you create a fake\n     .changes file in case you need one.\n .\n   * debian-report:\n     Analyze Debian repositories and generate reports about their contents and\n     relations. For example, a list of all packages in a distribution that are\n     trumped by another distribution.\n .\n   * cabal-debian:\n     Tool for creating debianizations of Haskell packages based on the .cabal\n     file.  If apt-file is installed it will use it to discover what is the\n     debian package name of a C library.\n .\n   * apt-get-build-depends:\n     Tool which will parse the Build-Depends{-Indep} lines from debian/control\n     and apt-get install the required packages" ]

-- The parsed build dependencies
builddeps :: Relations
builddeps = [[Rel (BinPkgName {unBinPkgName = "debhelper"}) (Just (GRE (Debian.Version.parseDebianVersion' ("7" :: String)))) Nothing],
             [Rel (BinPkgName {unBinPkgName = "cdbs"}) Nothing Nothing],
             [Rel (BinPkgName {unBinPkgName = "haskell-devscripts"}) (Just (GRE (Debian.Version.parseDebianVersion' ("0.7" :: String)))) Nothing],
             [Rel (BinPkgName {unBinPkgName = "ghc"}) Nothing Nothing],
             [Rel (BinPkgName {unBinPkgName = "ghc-prof"}) Nothing Nothing],
             [Rel (BinPkgName {unBinPkgName = "libghc-hunit-dev"}) Nothing Nothing],
             [Rel (BinPkgName {unBinPkgName = "libghc-hunit-prof"}) Nothing Nothing],
             [Rel (BinPkgName {unBinPkgName = "libghc-mtl-dev"}) Nothing Nothing],
             [Rel (BinPkgName {unBinPkgName = "libghc-mtl-prof"}) Nothing Nothing],
             [Rel (BinPkgName {unBinPkgName = "libghc-parsec3-dev"}) Nothing Nothing],
             [Rel (BinPkgName {unBinPkgName = "libghc-parsec3-prof"}) Nothing Nothing],
             [Rel (BinPkgName {unBinPkgName = "libghc-pretty-class-dev"}) Nothing Nothing],
             [Rel (BinPkgName {unBinPkgName = "libghc-pretty-class-prof"}) Nothing Nothing],
             [Rel (BinPkgName {unBinPkgName = "libghc-process-extras-dev"}) (Just (GRE (Debian.Version.parseDebianVersion' ("0.4" :: String)))) Nothing],
             [Rel (BinPkgName {unBinPkgName = "libghc-process-extras-prof"}) (Just (GRE (Debian.Version.parseDebianVersion' ("0.4" :: String)))) Nothing],
             [Rel (BinPkgName {unBinPkgName = "libghc-regex-compat-dev"}) Nothing Nothing],
             [Rel (BinPkgName {unBinPkgName = "libghc-regex-compat-prof"}) Nothing Nothing],
             [Rel (BinPkgName {unBinPkgName = "libghc-regex-tdfa-dev"}) (Just (GRE (Debian.Version.parseDebianVersion' ("1.1.3" :: String)))) Nothing],
             [Rel (BinPkgName {unBinPkgName = "libghc-regex-tdfa-prof"}) Nothing Nothing],
             [Rel (BinPkgName {unBinPkgName = "libghc-bzlib-dev"}) (Just (GRE (Debian.Version.parseDebianVersion' ("0.5.0.0-4" :: String)))) Nothing],
             [Rel (BinPkgName {unBinPkgName = "libghc-bzlib-prof"}) Nothing Nothing],
             [Rel (BinPkgName {unBinPkgName = "libghc-haxml-prof"}) (Just (GRE (Debian.Version.parseDebianVersion' ("1:1.20" :: String)))) Nothing],
             [Rel (BinPkgName {unBinPkgName = "libghc-unixutils-dev"}) (Just (GRE (Debian.Version.parseDebianVersion' ("1.50" :: String)))) Nothing],
             [Rel (BinPkgName {unBinPkgName = "libghc-unixutils-prof"}) (Just (GRE (Debian.Version.parseDebianVersion' ("1.50" :: String)))) Nothing],
             [Rel (BinPkgName {unBinPkgName = "libghc-zlib-dev"}) Nothing Nothing],
             [Rel (BinPkgName {unBinPkgName = "libghc-zlib-prof"}) Nothing Nothing],
             [Rel (BinPkgName {unBinPkgName = "libghc-network-dev"}) (Just (GRE (Debian.Version.parseDebianVersion' ("2.4" :: String)))) Nothing],
             [Rel (BinPkgName {unBinPkgName = "libghc-network-prof"}) (Just (GRE (Debian.Version.parseDebianVersion' ("2.4" :: String)))) Nothing],
             [Rel (BinPkgName {unBinPkgName = "libghc-utf8-string-dev"}) Nothing Nothing],
             [Rel (BinPkgName {unBinPkgName = "libghc-utf8-string-prof"}) Nothing Nothing],
             [Rel (BinPkgName {unBinPkgName = "libcrypto++-dev"}) Nothing Nothing]]

sample :: String
sample = intercalate "\n\n" paragraphs <> "\n"

-- | The expecte result of parsing the sample control file.
control :: Control' Text
control =
    Control
      { unControl = [Paragraph [Field ("Source"," haskell-debian")
                               ,Field ("Section"," haskell")
                               ,Field ("Priority"," extra")
                               ,Field ("Maintainer"," Debian Haskell Group <pkg-haskell-maintainers@lists.alioth.debian.org>")
                               ,Field ("Uploaders"," Joachim Breitner <nomeata@debian.org>")
                               ,Field ("Build-Depends"," debhelper (>= 7)\n  , cdbs\n  , haskell-devscripts (>= 0.7)\n  , ghc\n  , ghc-prof\n  , libghc-hunit-dev\n  , libghc-hunit-prof\n  , libghc-mtl-dev\n  , libghc-mtl-prof\n  , libghc-parsec3-dev\n  , libghc-parsec3-prof\n  , libghc-pretty-class-dev\n  , libghc-pretty-class-prof\n  , libghc-process-extras-dev (>= 0.4)\n  , libghc-process-extras-prof (>= 0.4)\n  , libghc-regex-compat-dev\n  , libghc-regex-compat-prof\n  , libghc-regex-tdfa-dev (>= 1.1.3)\n  , libghc-regex-tdfa-prof\n  , libghc-bzlib-dev (>= 0.5.0.0-4)\n  , libghc-bzlib-prof\n  , libghc-haxml-prof (>= 1:1.20)\n  , libghc-unixutils-dev (>= 1.50)\n  , libghc-unixutils-prof (>= 1.50)\n  , libghc-zlib-dev\n  , libghc-zlib-prof\n  , libghc-network-dev (>= 2.4)\n  , libghc-network-prof (>= 2.4)\n  , libghc-utf8-string-dev\n  , libghc-utf8-string-prof,\n  , libcrypto++-dev")
                               ,Field ("Build-Depends-Indep"," ghc-doc\n  , libghc-hunit-doc\n  , libghc-mtl-doc\n  , libghc-parsec3-doc\n  , libghc-pretty-class-doc\n  , libghc-process-extras-doc (>= 0.4)\n  , libghc-regex-compat-doc\n  , libghc-regex-tdfa-doc\n  , libghc-bzlib-doc\n  , libghc-haxml-doc (>= 1:1.20)\n  , libghc-unixutils-doc (>= 1.50)\n  , libghc-zlib-doc\n  , libghc-network-doc (>= 2.4)\n  , libghc-utf8-string-doc")
                               ,Field ("Standards-Version"," 3.9.2")
                               ,Field ("Homepage"," http://hackage.haskell.org/package/debian")
                               ,Field ("Vcs-Darcs"," http://darcs.debian.org/pkg-haskell/haskell-debian")
                               ,Field ("Vcs-Browser"," http://darcs.debian.org/cgi-bin/darcsweb.cgi?r=pkg-haskell/haskell-debian")]
                    ,Paragraph [Field ("Package"," libghc-debian-dev")
                               ,Field ("Architecture"," any")
                               ,Field ("Depends"," ${haskell:Depends}\n  , ${shlibs:Depends}\n  , ${misc:Depends}")
                               ,Field ("Recommends"," ${haskell:Recommends}")
                               ,Field ("Suggests"," ${haskell:Suggests}")
                               ,Field ("Provides"," ${haskell:Provides}")
                               ,Field ("Description"," Haskell library for working with the Debian package system\n This package provides a library for the Haskell programming language.\n See http://www.haskell.org/ for more information on Haskell.\n .\n This library includes modules covering almost every aspect of the Debian\n packaging system, including low level data types such as version numbers\n and dependency relations, on up to the types necessary for computing and\n installing build dependencies, building source and binary packages,\n and inserting them into a repository.\n .\n This package contains the libraries compiled for GHC 6.")]
                    ,Paragraph [Field ("Package"," libghc-debian-prof")
                               ,Field ("Architecture"," any")
                               ,Field ("Depends"," ${haskell:Depends}\n  , ${shlibs:Depends}\n  , ${misc:Depends}")
                               ,Field ("Recommends"," ${haskell:Recommends}")
                               ,Field ("Suggests"," ${haskell:Suggests}")
                               ,Field ("Provides"," ${haskell:Provides}")
                               ,Field ("Description"," Profiling library for working with the Debian package system\n This package provides a library for the Haskell programming language,\n compiled for profiling.\n See http://www.haskell.org/ for more information on Haskell.\n .\n This library includes modules covering almost every aspect of the Debian\n packaging system, including low level data types such as version numbers\n and dependency relations, on up to the types necessary for computing and\n installing build dependencies, building source and binary packages,\n and inserting them into a repository.\n .\n This package contains the profiling libraries compiled for GHC 6.")],
                     Paragraph [Field ("Package"," libghc-debian-doc")
                               ,Field ("Section"," doc")
                               ,Field ("Architecture"," all")
                               ,Field ("Depends"," ${misc:Depends}, ${haskell:Depends}")
                               ,Field ("Recommends"," ${haskell:Recommends}")
                               ,Field ("Suggests"," ${haskell:Suggests}")
                               ,Field ("Description"," Documentation for Debian package system library\n This package provides the documentation for a library for the Haskell\n programming language.\n See http://www.haskell.org/ for more information on Haskell.\n .\n This library includes modules covering almost every aspect of the Debian\n packaging system, including low level data types such as version numbers\n and dependency relations, on up to the types necessary for computing and\n installing build dependencies, building source and binary packages,\n and inserting them into a repository.\n .\n This package contains the library documentation.")],
                     Paragraph [Field ("Package"," haskell-debian-utils")
                               ,Field ("Section"," devel")
                               ,Field ("Architecture"," any")
                               ,Field ("Depends"," ghc, ${misc:Depends}, ${shlibs:Depends}")
                               ,Field ("Recommends"," apt-file")
                               ,Field ("Description"," Various helpers to work with Debian packages\n This package contains tools shipped with the Haskell library \8220debian\8221:\n .\n   * fakechanges:\n     Sometimes you have the .debs, .dsc, .tar.gz, .diff.gz, etc from a package\n     build, but not the .changes file. This package lets you create a fake\n     .changes file in case you need one.\n .\n   * debian-report:\n     Analyze Debian repositories and generate reports about their contents and\n     relations. For example, a list of all packages in a distribution that are\n     trumped by another distribution.\n .\n   * cabal-debian:\n     Tool for creating debianizations of Haskell packages based on the .cabal\n     file.  If apt-file is installed it will use it to discover what is the\n     debian package name of a C library.\n .\n   * apt-get-build-depends:\n     Tool which will parse the Build-Depends{-Indep} lines from debian/control\n     and apt-get install the required packages")]]}
