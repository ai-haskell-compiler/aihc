# agreeing

A simple data structure helping us ask questions of the following
sort: "does all this data have the same /BLANK/ and if so what is it?"

For example:

        doTheseHaveTheSameLength :: [String] -> String
        doTheseHaveTheSameLength l = case foldMap (Somebody . length) of
          Somebody n -> "They all have length " <> show n
          Nobody     -> "The lengths differ"
          Anybody    -> "You didn't give me any strings"

This can of course be done with `Maybe (Maybe x)` instead, but doing
so runs the risk of getting confused: which is `Nothing` and which is
`Just Nothing`?
