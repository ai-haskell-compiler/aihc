{-# LANGUAGE QuasiQuotes #-}

import Text.Nowdoc

main :: IO ()
main = putStr [nowdoc|
Now, fair Hippolyta, our nuptial hour
Draws on apace; four happy days brings in
Another moon: but, O, methinks, how slow
This old moon wanes! she lingers my desires,
Like to a step-dame or a dowager
Long withering out a young man revenue.
|]
