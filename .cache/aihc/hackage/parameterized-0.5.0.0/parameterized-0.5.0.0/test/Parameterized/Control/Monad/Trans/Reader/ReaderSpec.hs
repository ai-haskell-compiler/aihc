{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Parameterized.Control.Monad.Trans.Reader.ReaderSpec (main, spec) where

import Control.Applicative
import Control.Monad.Trans.Reader
import Parameterized.Control.Monad.Trans.Reader
import Parameterized.Control.Monad
import Data.Diverse
import Data.Semigroup
import Test.Hspec


-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec dicovery.
main :: IO ()
main = hspec spec

whichIntMaybeReader :: ReaderT (Which '[Int]) Maybe String
whichIntMaybeReader = do
    a <- ask
    pure . show $ obvious a

whichBoolMaybeReader :: ReaderT (Which '[Bool]) Maybe String
whichBoolMaybeReader = do
    a <- ask
    pure . show $ obvious a

whichStringMaybeReader :: ReaderT (Which '[String]) Maybe String
whichStringMaybeReader = do
    a <- ask
    pure . show $ obvious a

whichIntBoolMaybeReader :: ReaderT (Which '[Int, Bool]) Maybe String
whichIntBoolMaybeReader = do
    ay <- ask
    pure $ switch ay (CaseFunc @Show show)

whichBoolStringMaybeReader :: ReaderT (Which '[Bool, String]) Maybe String
whichBoolStringMaybeReader = do
    ay <- ask
    pure $ switch ay (CaseFunc @Show show)

whichIntStringMaybeReader :: ReaderT (Which '[Int, String]) Maybe String
whichIntStringMaybeReader = do
    ay <- ask
    pure $ switch ay (CaseFunc @Show show)


whichIntBoolStringMaybeReader :: ReaderT (Which '[Int, Bool, String]) Maybe String
whichIntBoolStringMaybeReader = do
    ay <- ask
    pure $ switch ay (CaseFunc @Show show)

--------------------------------

whichIntReader :: Reader (Which '[Int]) String
whichIntReader = do
    a <- ask
    pure . show $ obvious a

whichBoolReader :: Reader (Which '[Bool]) String
whichBoolReader = do
    a <- ask
    pure . show $ obvious a

whichStringReader :: Reader (Which '[String]) String
whichStringReader = do
    a <- ask
    pure . show $ obvious a

whichIntBoolReader :: Reader (Which '[Int, Bool]) String
whichIntBoolReader = do
    ay <- ask
    pure $ switch ay (CaseFunc @Show show)

whichIntStringReader :: Reader (Which '[Int, String]) String
whichIntStringReader = do
    ay <- ask
    pure $ switch ay (CaseFunc @Show show)


whichIntBoolStringReader :: Reader (Which '[Int, Bool, String]) String
whichIntBoolStringReader = do
    ay <- ask
    pure $ switch ay (CaseFunc @Show show)

--------------------------------

manyIntMaybeReader :: ReaderT (Many '[Int]) Maybe String
manyIntMaybeReader = do
    a <- ask
    let r = fetch @Int a
    case r of
        0 -> empty
        r' -> pure $ show r'

manyBoolMaybeReader :: ReaderT (Many '[Bool]) Maybe String
manyBoolMaybeReader = do
    a <- ask
    let r = fetch @Bool a
    case r of
        False -> empty
        r' -> pure $ show r'

--------------------------------

spec :: Spec
spec = do
    describe "Reader (Which a)" $ do

        it "is can maybe read only the types it knows about" $ do
            runReaderT whichIntMaybeReader (pick (5 :: Int)) `shouldBe` Just "5"
            runReaderT whichIntBoolMaybeReader (pick (5 :: Int)) `shouldBe` Just "5"
            runReaderT whichIntBoolStringMaybeReader (pick (5 :: Int)) `shouldBe` Just "5"
            runReaderT whichBoolMaybeReader (pick False) `shouldBe` Just "False"
            runReaderT whichIntBoolMaybeReader (pick False) `shouldBe` Just "False"
            runReaderT whichIntBoolStringMaybeReader (pick False) `shouldBe` Just "False"
            runReaderT whichStringMaybeReader (pick "foo") `shouldBe` Just "\"foo\""
            runReaderT whichIntStringMaybeReader (pick "foo") `shouldBe` Just "\"foo\""
            runReaderT whichIntBoolStringMaybeReader (pick "foo") `shouldBe` Just "\"foo\""

        it "is can read only the types it knows about" $ do
            runReader whichIntReader (pick (5 :: Int)) `shouldBe` "5"
            runReader whichIntBoolReader (pick (5 :: Int)) `shouldBe` "5"
            runReader whichIntBoolStringReader (pick (5 :: Int)) `shouldBe` "5"
            runReader whichBoolReader (pick False) `shouldBe` "False"
            runReader whichIntBoolReader (pick False) `shouldBe` "False"
            runReader whichIntBoolStringReader (pick False) `shouldBe` "False"
            runReader whichStringReader (pick "foo") `shouldBe` "\"foo\""
            runReader whichIntStringReader (pick "foo") `shouldBe` "\"foo\""
            runReader whichIntBoolStringReader (pick "foo") `shouldBe` "\"foo\""

    describe "OverlappingWhichReader" $ do
        it "pappend with empty doesn't change the type of the parameter" $ do
            let r1 = OverlappingWhichReader whichIntMaybeReader `pappend` pempty
                r2 = pempty `pappend` OverlappingWhichReader whichIntMaybeReader
                -- the "parameter" doesn't change when `pappend` with empty
                r1' = runOverlappingWhichReader r1 :: ReaderT (Which '[Int]) Maybe String
                r2' = runOverlappingWhichReader r2 :: ReaderT (Which '[Int]) Maybe String
            -- functionality unchanged
            runReaderT r1' (pick (5 :: Int)) `shouldBe` Just "5"
            runReaderT r2' (pick (5 :: Int)) `shouldBe` Just "5"

        it "it can be pappended to combine distinct readers" $ do
            let r = OverlappingWhichReader whichIntMaybeReader `pappend`
                        OverlappingWhichReader whichBoolMaybeReader
                -- the "parameter" is combined
                r' = runOverlappingWhichReader r :: ReaderT (Which '[Int, Bool]) Maybe String
            -- functionality of both readers
            runReaderT r' (pick (5 :: Int)) `shouldBe` Just "5"
            runReaderT r' (pick False) `shouldBe` Just "False"

            let r2 = r `pappend` OverlappingWhichReader whichStringMaybeReader
                -- the "parameter" is combined
                r2' = runOverlappingWhichReader r2 :: ReaderT (Which '[Int, Bool, String]) Maybe String
            -- functionality of both readers
            runReaderT r2' (pick (5 :: Int)) `shouldBe` Just "5"
            runReaderT r2' (pick False) `shouldBe` Just "False"
            runReaderT r2' (pick "foo") `shouldBe` Just "\"foo\""

        it "it can be pappended to combine overlapping readers" $ do
            let r = OverlappingWhichReader whichIntBoolMaybeReader `pappend`
                        OverlappingWhichReader whichBoolStringMaybeReader
                -- the "parameter" is combined
                r' = runOverlappingWhichReader r :: ReaderT (Which '[Int, Bool, String]) Maybe String
            -- functionality of both readers
            runReaderT r' (pick (5 :: Int)) `shouldBe` Just "5"
            runReaderT r' (pick False) `shouldBe` Just "False"
            runReaderT r' (pick "foo") `shouldBe` Just "\"foo\""

    describe "DistinctWhichReader" $ do
        it "pappend with empty doesn't change the type of the parameter" $ do
            let r1 = DistinctWhichReader whichIntMaybeReader `pappend` pempty
                r2 = pempty `pappend` DistinctWhichReader whichIntMaybeReader
                -- the "parameter" doesn't change when `pappend` with empty
                r1' = runDistinctWhichReader r1 :: ReaderT (Which '[Int]) Maybe String
                r2' = runDistinctWhichReader r2 :: ReaderT (Which '[Int]) Maybe String
            -- functionality unchanged
            runReaderT r1' (pick (5 :: Int)) `shouldBe` Just "5"
            runReaderT r2' (pick (5 :: Int)) `shouldBe` Just "5"

        it "it can be pappended to combine distinct readers" $ do
            let r = DistinctWhichReader whichIntReader `pappend` DistinctWhichReader whichBoolReader
                -- the "parameter" is combined
                r' = runDistinctWhichReader r :: Reader (Which '[Int, Bool]) String
            -- functionality of both readers
            runReader r' (pick (5 :: Int)) `shouldBe` "5"
            runReader r' (pick False) `shouldBe` "False"

            let r2 = r `pappend` DistinctWhichReader whichStringReader
                -- the "parameter" is combined
                r2' = runDistinctWhichReader r2 :: Reader (Which '[Int, Bool, String]) String
            -- functionality of both readers
            runReader r2' (pick (5 :: Int)) `shouldBe` "5"
            runReader r2' (pick False) `shouldBe` "False"
            runReader r2' (pick "foo") `shouldBe` "\"foo\""

    describe "ManyReader" $ do
        it "pappend with empty doesn't change the type of the parameter" $ do
            let r1 = ManyReader manyIntMaybeReader `pappend` pempty
                r2 = pempty `pappend` ManyReader manyIntMaybeReader
                -- the "parameter" doesn't change when `pappend` with empty
                r1' = runManyReader r1 :: ReaderT (Many '[Int]) Maybe String
                r2' = runManyReader r2 :: ReaderT (Many '[Int]) Maybe String
            -- functionality unchanged
            runReaderT r1' (single (5 :: Int)) `shouldBe` Just "5"
            runReaderT r2' (single (5 :: Int)) `shouldBe` Just "5"

        it "it can 'pappend' to combine state runners 'Alternative'ly" $ do
            let r = ManyReader manyIntMaybeReader `pappend`
                        ManyReader manyBoolMaybeReader
                -- the "parameter" is combined
                r' = runManyReader r :: ReaderT (Many '[Int, Bool]) Maybe String
            -- functionality of both readers in an Alternative fashion
            runReaderT r' ( (5 :: Int) ./ True ./ nil) `shouldBe` Just "5"
            runReaderT r' ( (5 :: Int) ./ False ./ nil) `shouldBe` Just "5"
            runReaderT r' ( (0 :: Int) ./ True ./ nil) `shouldBe` Just "True"
            runReaderT r' ( (0 :: Int) ./ False ./ nil) `shouldBe` Nothing

        it "it can 'papply' to combine state runners 'Applicative'ly" $ do
            let r = (\a b -> a <> "." <> b) <$> ManyReader manyIntMaybeReader &<*>
                        ManyReader manyBoolMaybeReader
                -- the "parameter" is combined
                r' = runManyReader r :: ReaderT (Many '[Int, Bool]) Maybe String
            -- functionality of both readers in an Applicative fashion
            runReaderT r' ( (5 :: Int) ./ True ./ nil) `shouldBe` Just "5.True"
            runReaderT r' ( (5 :: Int) ./ False ./ nil) `shouldBe` Nothing
            runReaderT r' ( (0 :: Int) ./ True ./ nil) `shouldBe` Nothing
            runReaderT r' ( (0 :: Int) ./ False ./ nil) `shouldBe` Nothing


        it "it can 'pbind' to combine reader runners 'Monad'ically" $ do
            let r = ManyReader manyIntMaybeReader &>>= \a ->
                        case a of
                            "1" -> ManyReader manyBoolMaybeReader
                            _ -> empty
                -- the "parameter" is combined
                r' = runManyReader r :: ReaderT (Many '[Int, Bool]) Maybe String
            -- functionality of both readers in an Monad fashion
            runReaderT r' ( (1 :: Int) ./ True ./ nil) `shouldBe` Just "True"
            runReaderT r' ( (1 :: Int) ./ False ./ nil) `shouldBe` Nothing
            runReaderT r' ( (5 :: Int) ./ True ./ nil) `shouldBe` Nothing
            runReaderT r' ( (5 :: Int) ./ False ./ nil) `shouldBe` Nothing
            runReaderT r' ( (0 :: Int) ./ True ./ nil) `shouldBe` Nothing
            runReaderT r' ( (0 :: Int) ./ False ./ nil) `shouldBe` Nothing
