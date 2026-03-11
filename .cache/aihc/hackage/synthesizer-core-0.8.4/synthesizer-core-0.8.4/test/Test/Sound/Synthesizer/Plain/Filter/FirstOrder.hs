module Test.Sound.Synthesizer.Plain.Filter.FirstOrder (tests) where

import qualified Synthesizer.Plain.Filter.Recursive.FirstOrder as Filt1
import qualified Synthesizer.Plain.Signal as Sig
import qualified Synthesizer.Causal.Process as Causal

import Test.QuickCheck (quickCheck, )

import qualified Number.GaloisField2p32m5 as GF

import Control.Applicative ((<$), )

import NumericPrelude.Numeric
import NumericPrelude.Base
import Prelude ()



addLowHighpass :: Sig.T (Filt1.Parameter GF.T, GF.T) -> Bool
addLowHighpass pxs =
   let (ps, xs) = unzip pxs
   in  Filt1.lowpass ps xs + Filt1.highpass ps xs == xs

combineLowHighpass :: Sig.T (Filt1.Parameter GF.T) -> Sig.T GF.T -> Bool
combineLowHighpass ps xs =
   zipWith Filt1.Result (Filt1.highpass ps xs) (Filt1.lowpass ps xs)
   ==
   Causal.apply Filt1.causal (zip ps xs)


lowpassId :: Sig.T GF.T -> Bool
lowpassId xs =
   Filt1.lowpass (repeat $ Filt1.Parameter (zero::GF.T)) xs == xs

lowpassZero :: Sig.T GF.T -> Bool
lowpassZero xs =
   Filt1.lowpass (repeat $ Filt1.Parameter (one::GF.T)) xs == (zero <$ xs)
--   isZero $ Filt1.lowpass (repeat $ Filt1.Parameter (one::GF.T)) xs

highpassId :: Sig.T GF.T -> Bool
highpassId xs =
   Filt1.highpass (repeat $ Filt1.Parameter (one::GF.T)) xs == xs

highpassZero :: Sig.T GF.T -> Bool
highpassZero xs =
   Filt1.highpass (repeat $ Filt1.Parameter (zero::GF.T)) xs == (zero <$ xs)


lowpassConst :: Sig.T GF.T -> GF.T -> Bool
lowpassConst ks x =
   Filt1.lowpassInit x (map Filt1.Parameter ks) (repeat x) == (x <$ ks)

highpassConst :: Sig.T GF.T -> GF.T -> Bool
highpassConst ks x =
   Filt1.highpassInit x (map Filt1.Parameter ks) (repeat x) == (zero <$ ks)

highpassInitAlt :: GF.T -> Sig.T (Filt1.Parameter GF.T) -> Sig.T GF.T -> Bool
highpassInitAlt x0 ps xs =
   Filt1.highpassInit x0 ps xs == Filt1.highpassInitAlt x0 ps xs


tests :: [(String, IO ())]
tests =
   ("addLowHighpass", quickCheck addLowHighpass) :
   ("combineLowHighpass", quickCheck combineLowHighpass) :
   ("lowpassId", quickCheck lowpassId) :
   ("lowpassZero", quickCheck lowpassZero) :
   ("highpassId", quickCheck highpassId) :
   ("highpassZero", quickCheck highpassZero) :
   ("lowpassConst", quickCheck lowpassConst) :
   ("highpassConst", quickCheck highpassConst) :
   ("highpassInitAlt", quickCheck highpassInitAlt) :
   []
