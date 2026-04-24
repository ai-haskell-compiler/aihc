{- ORACLE_TEST pass -}
module GhcEventsRecordPatternRoundtripXfail where
data Event = Event { ref :: Int }
f (Event {ref = ref}) = ref
