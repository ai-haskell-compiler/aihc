{-# LANGUAGE QuasiQuotes #-}

-- | Quasi-quotes in each position. The module does not enable
-- @TemplateHaskell@: a quasi-quote is a splice only with @QuasiQuotes@.
module Quasi (quasiLines) where

import Helper (fraction)

[fraction|quarter 1/4|]

half :: [fraction|any|]
half = [fraction|1/2|]

isQuarter :: Double -> Bool
isQuarter [fraction|1/4|] = True
isQuarter _ = False

-- | The text of a fraction with no finite decimal expansion is not exact,
-- but the value of the literal is.
quasiLines :: [String]
quasiLines = [show half, show quarter, show (isQuarter quarter, isQuarter half), show ([fraction|1/3|] == (1 / 3 :: Double))]
