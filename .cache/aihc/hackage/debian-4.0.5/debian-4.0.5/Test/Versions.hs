{-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans #-}
module Versions where

import Test.HUnit

import Debian.Version

-- * Implicit Values

implicit1 =
    TestCase (assertEqual "1.0 == 1.0-" EQ (compare (parseDebianVersion' "1.0") (parseDebianVersion' "1.0-")))

implicit2 =
    TestCase (assertEqual "1.0 == 1.0-0" EQ (compare (parseDebianVersion' "1.0") (parseDebianVersion' "1.0-0")))

implicit3 = 
    TestCase (assertEqual "1.0 == 0:1.0-0" EQ (compare (parseDebianVersion' "1.0") (parseDebianVersion' "0:1.0-0")))

implicit4 = 
    TestCase (assertEqual "1.0 == 1.0-" EQ (compare (parseDebianVersion' "1.0") (parseDebianVersion' "1.0-")))

implicit5 = 
    TestCase (assertEqual "apple = apple0" EQ (compare (parseDebianVersion' "apple") (parseDebianVersion' "apple0")))

implicit6 = 
    TestCase (assertEqual "apple = apple0-" EQ (compare (parseDebianVersion' "apple") (parseDebianVersion' "apple0-")))

implicit7 = 
    TestCase (assertEqual "apple = apple0-0" EQ (compare (parseDebianVersion' "apple") (parseDebianVersion' "apple0-0")))

-- * epoch, version, revision

epoch1 =
    TestCase (assertEqual "epoch 0:0" (Just 0) (epoch $ parseDebianVersion' "0:0"))

epoch2 =
    TestCase (assertEqual "epoch 0" Nothing(epoch $ parseDebianVersion' "0"))

epoch3 =
    TestCase (assertEqual "epoch 1:0" (Just 1) (epoch $ parseDebianVersion' "1:0"))

version1 =
    TestCase (assertEqual "version apple" "apple" (version $ parseDebianVersion' "apple"))

version2 =
    TestCase (assertEqual "version apple0" "apple0" (version $ parseDebianVersion' "apple0"))

version3 =
    TestCase (assertEqual "version apple1" "apple1" (version $ parseDebianVersion' "apple1"))

revision1 =
    TestCase (assertEqual "revision 1.0" Nothing (revision $ parseDebianVersion' "1.0"))

revision2 =
    TestCase (assertEqual "revision 1.0-" (Just "") (revision $ parseDebianVersion' "1.0-"))

revision3 =
    TestCase (assertEqual "revision 1.0-0" (Just "0") (revision $ parseDebianVersion' "1.0-0"))

revision4 =
    TestCase (assertEqual "revision 1.0-apple" (Just "apple") (revision $ parseDebianVersion' "1.0-apple"))


-- * Ordering

compareV str1 str2 = compare (parseDebianVersion' str1) (parseDebianVersion' str2)

order1 =
    TestCase (assertEqual "1:1-1 > 0:1-1" GT (compareV "1:1-1" "0:1-1"))

order2 =
    TestCase (assertEqual "1-1-1 > 1-1" GT (compareV "1-1-1" "1-1"))

-- * Dashes in upstream version

dash1 =
    TestCase (assertEqual "version of upstream-version-revision" "upstream-version" (version (parseDebianVersion' "upstream-version-revision")))

dash2 =
    TestCase (assertEqual "revision of upstream-version-revision" (Just "revision") (revision (parseDebianVersion' "upstream-version-revision")))

-- * Insignificant Zero's

zero1 =
    TestCase (assertEqual "0.09 = 0.9" EQ (compareV "0.09" "0.9"))

-- * Tests

versionTests =
    [ TestLabel "implicit1" implicit1
    , TestLabel "implicit2" implicit2
    , TestLabel "implicit3" implicit3
    , TestLabel "implicit4" implicit4
    , TestLabel "implicit5" implicit5
    , TestLabel "implicit5" implicit6
    , TestLabel "implicit5" implicit7
    , TestLabel "epoch1" epoch1
    , TestLabel "epoch2" epoch2
    , TestLabel "epoch3" epoch3
    , TestLabel "version1" version1
    , TestLabel "version2" version2
    , TestLabel "version3" version3
    , TestLabel "revision1" revision1
    , TestLabel "revision2" revision2
    , TestLabel "revision3" revision3
    , TestLabel "revision4" revision4
    , TestLabel "order1" order1
    , TestLabel "order2" order2
    , dash1
    , dash2
    , zero1
    ]
