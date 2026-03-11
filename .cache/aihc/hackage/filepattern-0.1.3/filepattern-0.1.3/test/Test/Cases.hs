
module Test.Cases(testCases) where

import Test.Util
import System.FilePath((</>))
import System.Info.Extra


testCases :: IO ()
testCases = testMatch >> testArity >> testSubstitute >> testStepNext >> testDirectory


testArity :: IO ()
testArity = do
    arity "" 0
    arity "a*b" 1
    arity "a//b" 0
    arity "a/**/b" 1
    arity "/a/b/cccc_" 0
    arity "a///b" 0
    arity "foo/**/*" 2
    arity "//*a.txt" 1
    arity "foo//a*.txt" 1
    arity "**/*a.txt" 2
    arity "foo/**/a*.txt" 2
    arity "//*a.txt" 1
    arity "foo//a*.*txt" 2
    arity "foo/**/a*.*txt" 3


testSubstitute :: IO ()
testSubstitute = do
    substitute "**/*a*.txt" ["","test","da"] "testada.txt"
    substitute "**/*a.txt" ["foo/bar","test"] "foo/bar/testa.txt"
    -- error if the number of replacements is wrong
    substituteErr "nothing" ["test"] ["substitute","nothing","expects 0","got 1","test"]
    substituteErr "*/*" ["test"] ["substitute","*/*","expects 2","got 1","test"]


testMatch :: IO ()
testMatch = do
    matchN "//*.c" "foo/bar/baz.c"
    matchY "**/*.c" "foo/bar/baz.c" ["foo/bar","baz"]
    matchY ("**" </> "*.c") ("foo/bar" </> "baz.c") ["foo/bar","baz"]
    matchY "*.c" "baz.c" ["baz"]
    matchN "//*.c" "baz.c"
    matchY "**/*.c" "baz.c" ["","baz"]
    matchY "**/*a.txt" "foo/bar/testa.txt" ["foo/bar","test"]
    matchN "**/*.c" "baz.txt"
    matchY "**/*a.txt" "testa.txt" ["","test"]
    matchY "**/a.txt" "a.txt" [""]
    matchY "a/**/b" "a/b" [""]
    matchY "a/**/b" "a/x/b" ["x"]
    matchY "a/**/b" "a/x/y/b" ["x/y"]
    matchY "a/**/**/b" "a/x/y/b" ["","x/y"]
    matchY "**/*a*.txt" "testada.txt" ["","test","da"]
    matchY "test.c" "test.c" []
    matchN "*.c" "foor/bar.c"
    matchN "*/*.c" "foo/bar/baz.c"
    matchN "foo//bar" "foobar"
    matchN "foo/**/bar" "foobar"
    matchN "foo//bar" "foobar/bar"
    matchN "foo/**/bar" "foobar/bar"
    matchN "foo//bar" "foo/foobar"
    matchN "foo/**/bar" "foo/foobar"
    matchN "foo//bar" "foo/bar"
    matchY "foo/**/bar" "foo/bar" [""]
    matchY "foo/bar" ("foo" </> "bar") []
    matchY ("foo" </> "bar") "foo/bar" []
    matchY ("foo" </> "bar") ("foo" </> "bar") []
    matchY "**/*.c" ("bar" </> "baz" </> "foo.c") ["bar/baz","foo"]
    matchY "**/*" "/bar" ["/","bar"]
    matchN "/bob//foo" "/bob/this/test/foo"
    matchY "/bob/**/foo" "/bob/this/test/foo" ["this/test"]
    matchN "/bob//foo" "bob/this/test/foo"
    matchN "/bob/**/foo" "bob/this/test/foo"
    matchN "bob//foo/" "bob/this/test/foo/"
    matchY "bob/**/foo/" "bob/this/test/foo/" ["this/test"]
    matchY "bob/**/foo/" "bob/foo/" [""]
    matchY "bob/**/foo/" "bob//foo/" ["/"]
    matchN "bob//foo/" "bob/this/test/foo"
    matchN "bob/**/foo/" "bob/this/test/foo"
    matchY ("**" </> "*a*.txt") "testada.txt" ["","test","da"]
    matchN "a//" "a"
    matchY "a/**" "a" [""]
    matchY "a/**" "a/" ["/"]
    matchN "/a//" "/a"
    matchY "a/**" "a" [""]
    matchY "/a/**" "/a" [""]
    matchN "///a//" "/a"
    matchY "**/a/**" "/a" ["/",""]
    matchN "///" ""
    matchY "/**" "/" ["/"]
    matchY "**/" "a/" ["a"]
    matchY "**/**" "" ["","/"]
    matchY "x/**/y" "x/y" [""]
    matchY "x/**/" "x/" [""]
    matchY "x/**/" "x/foo/" ["foo"]
    matchN "x///" "x"
    matchN "x/**/" "x"
    matchY "x/**/" "x/foo/bar/" ["foo/bar"]
    matchN "x///" "x/foo/bar"
    matchN "x/**/" "x/foo/bar"
    matchY "x/**/*/y" "x/z/y" ["","z"]
    matchY "" "" []
    matchN "" "y"
    matchN "" "/"

    matchY "*/*" "x/y" ["x","y"]
    matchN "*/*" "x"
    matchY "**/*" "x" ["","x"]
    matchY "**/*" "" ["",""]
    matchY "*/**" "x" ["x",""]
    matchY "*/**/*" "x/y" ["x","","y"]
    matchN "*//*" ""
    matchN "*/**/*" ""
    matchN "*//*" "x"
    matchN "*/**/*" "x"
    matchN "*//*//*" "x/y"
    matchN "*/**/*/**/*" "x/y"
    matchY "**/*/" "/" ["",""]
    matchY "*/**/**/" "/" ["","",""]
    matchN "b*b*b*//" "bb"
    matchN "b*b*b*/**" "bb"

    matchY "**" "/" ["//"] -- UGLY corner case
    matchY "**/x" "/x" ["/"]
    matchY "**" "x/" ["x/"]
    let s = if isWindows then '/' else '\\'
    matchY "**" "\\\\drive" [s:s:"drive"]
    matchY "**" "C:\\drive" ["C:"++s:"drive"]
    matchY "**" "C:drive" ["C:drive"]

    matchN "./file" "file"
    matchN "/file" "file"
    matchN "foo/./bar" "foo/bar"
    matchY "foo/./bar" "foo/./bar" []
    matchN "foo/./bar" "foo/bar"
    matchY "**z" "xyz" ["","xy"]
    matchY "**/a/b*" "a/a/a/a/bc" ["a/a/a","c"]
    matchY "**/a/b/**" "a/a/a/a/b/c" ["a/a/a","c"]


testStepNext :: IO ()
testStepNext = do
    stepNext ["*.xml"] [] StepUnknown
    stepNext ["*.xml"] ["foo"] $ StepOnly []
    stepNext ["**/*.xml"] [] StepUnknown
    stepNext ["**/*.xml"] ["foo"] StepUnknown
    stepNext ["foo/bar/*.xml"] [] $ StepOnly ["foo"]
    stepNext ["foo/bar/*.xml"] ["oof"] $ StepOnly []
    stepNext ["foo/bar/*.xml"] ["foo"] $ StepOnly ["bar"]
    stepNext ["a","b/c"] [] $ StepOnly ["a","b"]
    stepNext ["a","b/c"] ["b"] $ StepOnly ["c"]
    stepNext ["*/x"] [] StepUnknown
    stepNext ["*/x"] ["foo"] $ StepOnly ["x"]
    stepNext ["*/**"] ["bar"] StepEverything


testDirectory :: IO ()
testDirectory = do
    getDirectory ["**/*.c"] [] ["baz/test.c","baz/zoo.c","foo/bar.c","foo/foo/foo.c","zoo.c"] ["extra/test.h","foo.c/bob.h"]
    -- Currently no way to test what it can access, sadly
    -- should only look inside: foo
    getDirectory ["foo/*.c"] [] ["foo/bar.c","foo/baz.c"] ["foo.c","foo/bar/baz.c","test/foo.c"]
    -- should only look inside: . foo zoo
    getDirectory ["foo/*.c","**/*.h"] [".git/**","**/no.*"] ["foo/bar.c","foo/baz.h","zoo/test.h"] ["foo/no.c",".git/foo.h","zoo/test.c"]
