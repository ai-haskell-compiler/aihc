{- ORACLE_TEST xfail "we shouldn't wrap 'n2 `Inf` ns' in parens" -}
module M where

n = \(n1 `Inf` n2 `Inf` ns) -> ()

fn (a `App` b `App` c) = ()
