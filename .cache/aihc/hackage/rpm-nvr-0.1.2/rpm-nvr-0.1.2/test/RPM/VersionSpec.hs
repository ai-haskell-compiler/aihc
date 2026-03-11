-- taken from codec-rpm-0.2.2 and based on rpm/tests/rpmvercmp.at
-- Copyright 2017-2018 Red Hat

module RPM.VersionSpec (verspec) where

import           Test.Hspec
import           Data.Foldable(forM_)
import           Data.RPM.VerCmp

-- can also call rpm vercmp():
-- `rpm --eval '%{lua: print(rpm.vercmp("$1", "$2"))}'`

verspec :: Spec
verspec = do
    describe "Data.RPM.Version.vercmp" $ do
        let vercmpCases = [
                         ("1.0", "1.0", EQ),
                         ("1.0", "2.0", LT),
                         ("2.0", "1.0", GT),

                         ("2.0.1", "2.0.1", EQ),
                         ("2.0", "2.0.1", LT),
                         ("2.0.1", "2.0", GT),

                         ("2.0.1a", "2.0.1a", EQ),
                         ("2.0.1a", "2.0.1", GT),
                         ("2.0.1", "2.0.1a", LT),

                         ("5.5p1", "5.5p1", EQ),
                         ("5.5p1", "5.5p2", LT),
                         ("5.5p2", "5.5p1", GT),

                         ("5.5p10", "5.5p10", EQ),
                         ("5.5p1", "5.5p10", LT),
                         ("5.5p10", "5.5p1", GT),

                         ("10xyz", "10.1xyz", LT),
                         ("10.1xyz", "10xyz", GT),

                         ("xyz10", "xyz10", EQ),
                         ("xyz10", "xyz10.1", LT),
                         ("xyz10.1", "xyz10", GT),

                         ("xyz.4", "xyz.4", EQ),
                         ("xyz.4", "8", LT),
                         ("8", "xyz.4", GT),
                         ("xyz.4", "2", LT),
                         ("2", "xyz.4", GT),

                         ("5.5p2", "5.6p1", LT),
                         ("5.6p1", "5.5p2", GT),

                         ("5.6p1", "6.5p1", LT),
                         ("6.5p1", "5.6p1", GT),

                         ("6.0.rc1", "6.0", GT),
                         ("6.0", "6.0.rc1", LT),

                         ("10b2", "10a1", GT),
                         ("10a2", "10b2", LT),

                         ("1.0aa", "1.0aa", EQ),
                         ("1.0a", "1.0aa", LT),
                         ("1.0aa", "1.0a", GT),

                         ("10.0001", "10.0001", EQ),
                         ("10.0001", "10.1", EQ),
                         ("10.1", "10.0001", EQ),
                         ("10.0001", "10.0039", LT),
                         ("10.0039", "10.0001", GT),

                         ("4.999.9", "5.0", LT),
                         ("5.0", "4.999.9", GT),

                         ("20101121", "20101121", EQ),
                         ("20101121", "20101122", LT),
                         ("20101122", "20101121", GT),

                         ("2_0", "2_0", EQ),
                         ("2.0", "2_0", EQ),
                         ("2_0", "2.0", EQ),

                         -- RhBug:178798 case
                         ("a", "a", EQ),
                         ("a+", "a+", EQ),
                         ("a+", "a_", EQ),
                         ("a_", "a+", EQ),
                         ("+a", "+a", EQ),
                         ("+a", "_a", EQ),
                         ("_a", "+a", EQ),
                         ("+_", "+_", EQ),
                         ("_+", "+_", EQ),
                         ("_+", "_+", EQ),
                         ("+", "_", EQ),
                         ("_", "+", EQ),

                         -- Basic testcases for tilde sorting
                         ("1.0~rc1", "1.0~rc1", EQ),
                         ("1.0~rc1", "1.0", LT),
                         ("1.0", "1.0~rc1", GT),
                         ("1.0~rc1", "1.0~rc2", LT),
                         ("1.0~rc2", "1.0~rc1", GT),
                         ("1.0~rc1~git123", "1.0~rc1~git123", EQ),
                         ("1.0~rc1~git123", "1.0~rc1", LT),
                         ("1.0~rc1", "1.0~rc1~git123", GT),

                         -- Basic testcases for caret sorting
                         ("1.0^", "1.0^", EQ),
                         ("1.0^", "1.0", GT),
                         ("1.0", "1.0^", LT),
                         ("1.0^git1", "1.0^git1", EQ),
                         ("1.0^git1", "1.0", GT),
                         ("1.0", "1.0^git1", LT),
                         ("1.0^git1", "1.0^git2", LT),
                         ("1.0^git2", "1.0^git1", GT),
                         ("1.0^git1", "1.01", LT),
                         ("1.01", "1.0^git1", GT),
                         ("1.0^20160101", "1.0^20160101", EQ),
                         ("1.0^20160101", "1.0.1", LT),
                         ("1.0.1", "1.0^20160101", GT),
                         ("1.0^20160101^git1", "1.0^20160101^git1", EQ),
                         ("1.0^20160102", "1.0^20160101^git1", GT),
                         ("1.0^20160101^git1", "1.0^20160102", LT),

                         -- Basic testcases for tilde and caret sorting
                         ("1.0~rc1^git1", "1.0~rc1^git1", EQ),
                         ("1.0~rc1^git1", "1.0~rc1", GT),
                         ("1.0~rc1", "1.0~rc1^git1", LT),
                         ("1.0^git1~pre", "1.0^git1~pre", EQ),
                         ("1.0^git1", "1.0^git1~pre", GT),
                         ("1.0^git1~pre", "1.0^git1", LT),

                         -- local
                         ("8", "13", LT)
                         ]

        forM_ vercmpCases $ \(verA, verB, ord) ->
          it (verA ++ " " ++ show ord ++ " " ++ verB) $
          rpmVerCompare verA verB `shouldBe` ord
