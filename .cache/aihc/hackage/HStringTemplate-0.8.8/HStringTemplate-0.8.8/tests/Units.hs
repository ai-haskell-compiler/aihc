{-# OPTIONS -O2 -fglasgow-exts #-}

module Units where
import System.IO
import qualified Data.Map as M

import Text.StringTemplate
import Text.StringTemplate.Classes
import Text.StringTemplate.Base
import Test.HUnit
import Control.Monad
import System.Environment

no_prop = toString (setAttribute "foo" "f" $ newSTMP "a$foo.bar$a")
          ~=? "aa"

one_prop = toString (setAttribute "foo" (M.singleton "bar" "baz") $ newSTMP "a$foo.bar$a")
           ~=? "abaza"

anon_tmpl = toString (setAttribute "foo" "f" $ newSTMP "a$foo:{{$foo$\\}}$a")
            ~=? "a{f}a"

setA = setAttribute "foo" ["a","b","c"]
func_first = toString (setA $ newSTMP "$first(foo)$") ~=? "a"
func_last = toString (setA $ newSTMP "$last(foo)$") ~=? "c"
func_rest = toString (setA $ newSTMP "$rest(foo)$") ~=? "bc"
func_length = toString (setA $ newSTMP "$length(foo)$") ~=? "3"
func_reverse = toString (setA $ newSTMP "$reverse(foo)$") ~=? "cba"

tests = TestList ["no_prop" ~: no_prop,
                  "one_prop" ~: one_prop,
                  "func_first" ~: func_first,
                  "func_last" ~: func_last,
                  "func_rest" ~: func_rest,
                  "func_reverse" ~: func_reverse,
                  "func_length" ~: func_length,
                  "anon_tmpl" ~: anon_tmpl]

main = do
  c <- runTestTT tests
  when (errors c > 0 || failures c > 0) $
    fail "Not all tests passed."
