{- ORACLE_TEST xfail record pattern {x = x} pretty-prints as {x} losing explicit binding -}
module GhcEventsRecordPatternRoundtripXfail where
data Event = Event { ref :: Int }
f (Event {ref = ref}) = ref
