-- Do not edit! Automatically created with doctest-extract from src/Data/NonEmptyPrivate.hs
{-# LINE 40 "src/Data/NonEmptyPrivate.hs" #-}

module Test.Data.NonEmptyPrivate where

import qualified Test.DocTest.Driver as DocTest

{-# LINE 41 "src/Data/NonEmptyPrivate.hs" #-}
import     qualified Data.NonEmpty as NonEmpty
import     qualified Data.Empty as Empty
import     qualified Data.Either.HT as EitherHT
import     qualified Control.Functor.HT as FuncHT
import     qualified Data.Ix as Ix
import     Data.NonEmpty ((!:))
import     Data.Tuple.HT (swap)
import     Data.Maybe (mapMaybe)
import     Control.Applicative (liftA2)
import     Control.Functor.HT (void)
import     qualified Test.QuickCheck as QC

forRange     :: (QC.Testable test) => ((Char,Char) -> test) -> QC.Property
forRange     =
       QC.forAll (liftA2 (,) (QC.choose ('a','h')) (QC.choose ('a','h')))

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Data.NonEmptyPrivate:604: "
{-# LINE 604 "src/Data/NonEmptyPrivate.hs" #-}
 DocTest.property
{-# LINE 604 "src/Data/NonEmptyPrivate.hs" #-}
     (let takeUntil p xs = NonEmpty.zipWith const xs $ () !: void (takeWhile (not . p) $ NonEmpty.flatten xs) in \k xs -> takeUntil (>=k) xs == NonEmpty.takeUntil (>=(k::Int)) xs)
 DocTest.printPrefix "Data.NonEmptyPrivate:748: "
{-# LINE 748 "src/Data/NonEmptyPrivate.hs" #-}
 DocTest.property
{-# LINE 748 "src/Data/NonEmptyPrivate.hs" #-}
     (\xs -> mapMaybe EitherHT.maybeLeft (NonEmpty.flatten xs) == either NonEmpty.flatten fst (NonEmpty.partitionEithersLeft (xs::NonEmpty.T[](Either Char Int))))
 DocTest.printPrefix "Data.NonEmptyPrivate:749: "
{-# LINE 749 "src/Data/NonEmptyPrivate.hs" #-}
 DocTest.property
{-# LINE 749 "src/Data/NonEmptyPrivate.hs" #-}
     (\xs -> mapMaybe EitherHT.maybeRight (NonEmpty.flatten xs) == either (const []) (NonEmpty.flatten . snd) (NonEmpty.partitionEithersLeft (xs::NonEmpty.T[](Either Char Int))))
 DocTest.printPrefix "Data.NonEmptyPrivate:750: "
{-# LINE 750 "src/Data/NonEmptyPrivate.hs" #-}
 DocTest.property
{-# LINE 750 "src/Data/NonEmptyPrivate.hs" #-}
     (\xs -> NonEmpty.partitionEithersRight (fmap EitherHT.swap xs) == EitherHT.mapLeft swap (EitherHT.swap (NonEmpty.partitionEithersLeft (xs::NonEmpty.T[](Either Char Int)))))
 DocTest.printPrefix "Data.NonEmptyPrivate:760: "
{-# LINE 760 "src/Data/NonEmptyPrivate.hs" #-}
 DocTest.property
{-# LINE 760 "src/Data/NonEmptyPrivate.hs" #-}
     (\xs -> NonEmpty.partitionEithersLeft (fmap EitherHT.swap xs) == EitherHT.mapRight swap (EitherHT.swap (NonEmpty.partitionEithersRight (xs::NonEmpty.T[](Either Char Int)))))
 DocTest.printPrefix "Data.NonEmptyPrivate:771: "
{-# LINE 771 "src/Data/NonEmptyPrivate.hs" #-}
 DocTest.property
{-# LINE 771 "src/Data/NonEmptyPrivate.hs" #-}
     (forRange $ \b0 -> forRange $ \b1 -> forRange $ \b2 -> let b = FuncHT.unzip $ b0!:b1!:b2!:Empty.Cons in map (Ix.index b) (Ix.range b) == take (Ix.rangeSize b) [0..])
