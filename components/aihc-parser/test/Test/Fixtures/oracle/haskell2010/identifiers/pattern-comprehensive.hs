{- ORACLE_TEST pass -}
module Test ( pattern ) where
import Mod ( pattern )
import Mod hiding ( pattern )
fn1 pattern = ()
fn2 = \pattern -> ()
fn3 :: pattern
fn4 = () `pattern` ()
a `pattern` b = ()
infix `pattern`
