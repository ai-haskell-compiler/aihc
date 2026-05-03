{- ORACLE_TEST pass -}
module CabalDollarDoLetIn where

f neededLibWays =
  run
    $ do
      let
          neededLibWaysSet = fromList neededLibWays
          useDynamicToo = static `member` neededLibWaysSet && dynamic `member` neededLibWaysSet
          orderedBuilds
            | useDynamicToo = [buildStaticAndDynamicToo]
            | otherwise = build <$> neededLibWays
          buildStaticAndDynamicToo = run dynamic
       in sequence_ orderedBuilds
