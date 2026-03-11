--
-- Time to manipulate time
--
-- Copyright © 2013-2018 Operational Dynamics Consulting, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

-- | Test unboxed vector usage

module CheckVectorOperations where

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Test.Hspec

import Chrono.TimeStamp

checkVectorUnboxed :: Spec
checkVectorUnboxed = do
    describe "Unbox instance for TimeStamp" $
        it "should work in an unboxed Vector" $ do
            let vs = V.fromList [minBound, 0, maxBound] :: Vector TimeStamp
            show vs `shouldBe`
                "[1677-09-21T00:12:43.145224192Z" ++
                ",1970-01-01T00:00:00.000000000Z" ++
                ",2262-04-11T23:47:16.854775807Z]"
 
