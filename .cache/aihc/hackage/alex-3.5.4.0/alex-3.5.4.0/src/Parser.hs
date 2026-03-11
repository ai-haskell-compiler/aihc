{-# OPTIONS_GHC -w #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoStrictData #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE PartialTypeSignatures #-}
-- -----------------------------------------------------------------------------
--
-- Parser.y, part of Alex
--
-- (c) Simon Marlow 2003
--
-- -----------------------------------------------------------------------------

{-# OPTIONS_GHC -w #-}

module Parser ( parse, P ) where
import AbsSyn
import Scan
import CharSet
import ParseMonad hiding ( StartCode )

import Data.Char
--import Debug.Trace
import qualified Control.Monad as Happy_Prelude
import qualified Data.Bool as Happy_Prelude
import qualified Data.Function as Happy_Prelude
import qualified Data.Int as Happy_Prelude
import qualified Data.List as Happy_Prelude
import qualified Data.Maybe as Happy_Prelude
import qualified Data.String as Happy_Prelude
import qualified Data.Tuple as Happy_Prelude
import qualified GHC.Err as Happy_Prelude
import qualified GHC.Num as Happy_Prelude
import qualified Text.Show as Happy_Prelude
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 2.1.6

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
newtype HappyWrap5 = HappyWrap5 ((Maybe (AlexPosn,Code), [Directive], Scanner, Maybe (AlexPosn,Code)))
happyIn5 :: ((Maybe (AlexPosn,Code), [Directive], Scanner, Maybe (AlexPosn,Code))) -> (HappyAbsSyn )
happyIn5 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap5 x)
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn ) -> HappyWrap5
happyOut5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut5 #-}
newtype HappyWrap6 = HappyWrap6 (Maybe (AlexPosn,Code))
happyIn6 :: (Maybe (AlexPosn,Code)) -> (HappyAbsSyn )
happyIn6 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap6 x)
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> HappyWrap6
happyOut6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut6 #-}
newtype HappyWrap7 = HappyWrap7 ([Directive])
happyIn7 :: ([Directive]) -> (HappyAbsSyn )
happyIn7 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap7 x)
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> HappyWrap7
happyOut7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut7 #-}
newtype HappyWrap8 = HappyWrap8 (Directive)
happyIn8 :: (Directive) -> (HappyAbsSyn )
happyIn8 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap8 x)
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> HappyWrap8
happyOut8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut8 #-}
newtype HappyWrap9 = HappyWrap9 (Encoding)
happyIn9 :: (Encoding) -> (HappyAbsSyn )
happyIn9 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap9 x)
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> HappyWrap9
happyOut9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut9 #-}
newtype HappyWrap10 = HappyWrap10 (())
happyIn10 :: (()) -> (HappyAbsSyn )
happyIn10 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap10 x)
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> HappyWrap10
happyOut10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut10 #-}
newtype HappyWrap11 = HappyWrap11 (())
happyIn11 :: (()) -> (HappyAbsSyn )
happyIn11 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap11 x)
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> HappyWrap11
happyOut11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut11 #-}
newtype HappyWrap12 = HappyWrap12 (Scanner)
happyIn12 :: (Scanner) -> (HappyAbsSyn )
happyIn12 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap12 x)
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> HappyWrap12
happyOut12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut12 #-}
newtype HappyWrap13 = HappyWrap13 ([RECtx])
happyIn13 :: ([RECtx]) -> (HappyAbsSyn )
happyIn13 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap13 x)
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> HappyWrap13
happyOut13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut13 #-}
newtype HappyWrap14 = HappyWrap14 ([RECtx])
happyIn14 :: ([RECtx]) -> (HappyAbsSyn )
happyIn14 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap14 x)
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> HappyWrap14
happyOut14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut14 #-}
newtype HappyWrap15 = HappyWrap15 ((AlexPosn, RECtx))
happyIn15 :: ((AlexPosn, RECtx)) -> (HappyAbsSyn )
happyIn15 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap15 x)
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> HappyWrap15
happyOut15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut15 #-}
newtype HappyWrap16 = HappyWrap16 ([RECtx])
happyIn16 :: ([RECtx]) -> (HappyAbsSyn )
happyIn16 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap16 x)
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> HappyWrap16
happyOut16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut16 #-}
newtype HappyWrap17 = HappyWrap17 ([(String,StartCode)])
happyIn17 :: ([(String,StartCode)]) -> (HappyAbsSyn )
happyIn17 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap17 x)
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> HappyWrap17
happyOut17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut17 #-}
newtype HappyWrap18 = HappyWrap18 ([(String,StartCode)])
happyIn18 :: ([(String,StartCode)]) -> (HappyAbsSyn )
happyIn18 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap18 x)
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> HappyWrap18
happyOut18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut18 #-}
newtype HappyWrap19 = HappyWrap19 (String)
happyIn19 :: (String) -> (HappyAbsSyn )
happyIn19 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap19 x)
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> HappyWrap19
happyOut19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut19 #-}
newtype HappyWrap20 = HappyWrap20 ((AlexPosn, Maybe Code))
happyIn20 :: ((AlexPosn, Maybe Code)) -> (HappyAbsSyn )
happyIn20 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap20 x)
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> HappyWrap20
happyOut20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut20 #-}
newtype HappyWrap21 = HappyWrap21 (Maybe CharSet, RExp, RightContext RExp)
happyIn21 :: (Maybe CharSet, RExp, RightContext RExp) -> (HappyAbsSyn )
happyIn21 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap21 x)
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> HappyWrap21
happyOut21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut21 #-}
newtype HappyWrap22 = HappyWrap22 (CharSet)
happyIn22 :: (CharSet) -> (HappyAbsSyn )
happyIn22 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap22 x)
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> HappyWrap22
happyOut22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut22 #-}
newtype HappyWrap23 = HappyWrap23 (RightContext RExp)
happyIn23 :: (RightContext RExp) -> (HappyAbsSyn )
happyIn23 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap23 x)
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> HappyWrap23
happyOut23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut23 #-}
newtype HappyWrap24 = HappyWrap24 (RExp)
happyIn24 :: (RExp) -> (HappyAbsSyn )
happyIn24 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap24 x)
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> HappyWrap24
happyOut24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut24 #-}
newtype HappyWrap25 = HappyWrap25 (RExp)
happyIn25 :: (RExp) -> (HappyAbsSyn )
happyIn25 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap25 x)
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn ) -> HappyWrap25
happyOut25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut25 #-}
newtype HappyWrap26 = HappyWrap26 (RExp)
happyIn26 :: (RExp) -> (HappyAbsSyn )
happyIn26 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap26 x)
{-# INLINE happyIn26 #-}
happyOut26 :: (HappyAbsSyn ) -> HappyWrap26
happyOut26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut26 #-}
newtype HappyWrap27 = HappyWrap27 (RExp -> RExp)
happyIn27 :: (RExp -> RExp) -> (HappyAbsSyn )
happyIn27 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap27 x)
{-# INLINE happyIn27 #-}
happyOut27 :: (HappyAbsSyn ) -> HappyWrap27
happyOut27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut27 #-}
newtype HappyWrap28 = HappyWrap28 (())
happyIn28 :: (()) -> (HappyAbsSyn )
happyIn28 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap28 x)
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn ) -> HappyWrap28
happyOut28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut28 #-}
newtype HappyWrap29 = HappyWrap29 (RExp -> RExp)
happyIn29 :: (RExp -> RExp) -> (HappyAbsSyn )
happyIn29 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap29 x)
{-# INLINE happyIn29 #-}
happyOut29 :: (HappyAbsSyn ) -> HappyWrap29
happyOut29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut29 #-}
newtype HappyWrap30 = HappyWrap30 (RExp)
happyIn30 :: (RExp) -> (HappyAbsSyn )
happyIn30 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap30 x)
{-# INLINE happyIn30 #-}
happyOut30 :: (HappyAbsSyn ) -> HappyWrap30
happyOut30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut30 #-}
newtype HappyWrap31 = HappyWrap31 (CharSet)
happyIn31 :: (CharSet) -> (HappyAbsSyn )
happyIn31 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap31 x)
{-# INLINE happyIn31 #-}
happyOut31 :: (HappyAbsSyn ) -> HappyWrap31
happyOut31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut31 #-}
newtype HappyWrap32 = HappyWrap32 (CharSet)
happyIn32 :: (CharSet) -> (HappyAbsSyn )
happyIn32 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap32 x)
{-# INLINE happyIn32 #-}
happyOut32 :: (HappyAbsSyn ) -> HappyWrap32
happyOut32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut32 #-}
newtype HappyWrap33 = HappyWrap33 ([CharSet])
happyIn33 :: ([CharSet]) -> (HappyAbsSyn )
happyIn33 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap33 x)
{-# INLINE happyIn33 #-}
happyOut33 :: (HappyAbsSyn ) -> HappyWrap33
happyOut33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut33 #-}
newtype HappyWrap34 = HappyWrap34 ((AlexPosn,String))
happyIn34 :: ((AlexPosn,String)) -> (HappyAbsSyn )
happyIn34 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap34 x)
{-# INLINE happyIn34 #-}
happyOut34 :: (HappyAbsSyn ) -> HappyWrap34
happyOut34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut34 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


{-# NOINLINE happyTokenStrings #-}
happyTokenStrings = ["'.'","';'","'<'","'>'","','","'$'","'|'","'*'","'+'","'?'","'{'","'}'","'('","')'","'#'","'~'","'-'","'['","']'","'^'","'/'","ZERO","STRING","BIND","ID","CODE","CHAR","NUM","SMAC","RMAC","SMAC_DEF","RMAC_DEF","WRAPPER","ENCODING","ACTIONTYPE","TOKENTYPE","TYPECLASS","%eof"]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\xec\xff\xff\xff\xec\xff\xff\xff\xf6\x00\x00\x00\x00\x00\x00\x00\xe8\xff\xff\xff\x53\x00\x00\x00\xf6\x00\x00\x00\xfe\xff\xff\xff\x01\x00\x00\x00\x0a\x00\x00\x00\x0c\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1b\x00\x00\x00\x53\x00\x00\x00\x64\x00\x00\x00\x5e\x00\x00\x00\x00\x00\x00\x00\x1f\x00\x00\x00\x00\x00\x00\x00\xa8\x00\x00\x00\x29\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x00\x00\x64\x00\x00\x00\x52\x00\x00\x00\x00\x00\x00\x00\x2e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3c\x00\x00\x00\x00\x00\x00\x00\x2f\x00\x00\x00\xff\xff\xff\xff\x00\x00\x00\x00\xff\xff\xff\xff\x00\x00\x00\x00\x07\x00\x00\x00\x05\x00\x00\x00\x5e\x00\x00\x00\xfb\xff\xff\xff\x1a\x00\x00\x00\x2a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x64\x00\x00\x00\x33\x00\x00\x00\x40\x00\x00\x00\x43\x00\x00\x00\x64\x00\x00\x00\x00\x00\x00\x00\x5a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5e\x00\x00\x00\x00\x00\x00\x00\x45\x00\x00\x00\x00\x00\x00\x00\x59\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6d\x00\x00\x00\x75\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3a\x00\x00\x00\xfb\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4c\x00\x00\x00\x00\x00\x00\x00\x4c\x00\x00\x00\x74\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x79\x00\x00\x00\x84\x00\x00\x00\x70\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x78\x00\x00\x00\x96\x00\x00\x00\x7c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x83\x00\x00\x00\x8f\x00\x00\x00\x00\x00\x00\x00\x98\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8c\x00\x00\x00\x99\x00\x00\x00\x11\x00\x00\x00\xf1\xff\xff\xff\x00\x00\x00\x00\xfa\x00\x00\x00\x00\x00\x00\x00\x92\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd2\x00\x00\x00\x16\x00\x00\x00\xab\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9f\x00\x00\x00\x7b\x00\x00\x00\x00\x00\x00\x00\x91\x00\x00\x00\x00\x00\x00\x00\xc7\x00\x00\x00\x94\x00\x00\x00\xdd\x00\x00\x00\x95\x00\x00\x00\x00\x00\x00\x00\xae\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x03\x01\x00\x00\x00\x00\x00\x00\x07\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe8\x00\x00\x00\x00\x00\x00\x00\xa9\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf3\x00\x00\x00\x9b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa5\x00\x00\x00\x00\x00\x00\x00\xb9\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc2\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xfc\xff\xff\xff\x00\x00\x00\x00\xfa\xff\xff\xff\xfd\xff\xff\xff\x00\x00\x00\x00\xf2\xff\xff\xff\xfa\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf5\xff\xff\xff\xf6\xff\xff\xff\xf7\xff\xff\xff\xf8\xff\xff\xff\xf4\xff\xff\xff\xf9\xff\xff\xff\xfb\xff\xff\xff\x00\x00\x00\x00\xf2\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xf0\xff\xff\xff\xd6\xff\xff\xff\xd4\xff\xff\xff\xd2\xff\xff\xff\xc6\xff\xff\xff\xc3\xff\xff\xff\xc0\xff\xff\xff\xba\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xbb\xff\xff\xff\xc8\xff\xff\xff\xc2\xff\xff\xff\xb9\xff\xff\xff\xc7\xff\xff\xff\xf1\xff\xff\xff\xf3\xff\xff\xff\xfc\xff\xff\xff\xed\xff\xff\xff\xef\xff\xff\xff\xed\xff\xff\xff\xea\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd8\xff\xff\xff\xc6\xff\xff\xff\x00\x00\x00\x00\xdd\xff\xff\xff\xfe\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xbb\xff\xff\xff\x00\x00\x00\x00\xbb\xff\xff\xff\xbd\xff\xff\xff\x00\x00\x00\x00\xc9\xff\xff\xff\xd3\xff\xff\xff\x00\x00\x00\x00\xd1\xff\xff\xff\xd0\xff\xff\xff\xcf\xff\xff\xff\xd5\xff\xff\xff\x00\x00\x00\x00\xd7\xff\xff\xff\x00\x00\x00\x00\xc5\xff\xff\xff\x00\x00\x00\x00\xbf\xff\xff\xff\xbc\xff\xff\xff\xc1\xff\xff\xff\xc4\xff\xff\xff\x00\x00\x00\x00\xe4\xff\xff\xff\xe3\xff\xff\xff\xe2\xff\xff\xff\xdc\xff\xff\xff\xde\xff\xff\xff\xdb\xff\xff\xff\x00\x00\x00\x00\xd8\xff\xff\xff\xe9\xff\xff\xff\xe0\xff\xff\xff\xe1\xff\xff\xff\xec\xff\xff\xff\xe7\xff\xff\xff\xee\xff\xff\xff\xe7\xff\xff\xff\x00\x00\x00\x00\xdf\xff\xff\xff\xda\xff\xff\xff\xd9\xff\xff\xff\x00\x00\x00\x00\xe6\xff\xff\xff\xbe\xff\xff\xff\x00\x00\x00\x00\xcc\xff\xff\xff\xcb\xff\xff\xff\xce\xff\xff\xff\xe5\xff\xff\xff\xeb\xff\xff\xff\xe8\xff\xff\xff\xca\xff\xff\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\xff\xff\x02\x00\x00\x00\x07\x00\x00\x00\x04\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\x1b\x00\x00\x00\x03\x00\x00\x00\x02\x00\x00\x00\x19\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x0e\x00\x00\x00\x1d\x00\x00\x00\x27\x00\x00\x00\x11\x00\x00\x00\x16\x00\x00\x00\x13\x00\x00\x00\x0c\x00\x00\x00\x15\x00\x00\x00\x0e\x00\x00\x00\x18\x00\x00\x00\x18\x00\x00\x00\x11\x00\x00\x00\x18\x00\x00\x00\x13\x00\x00\x00\x1c\x00\x00\x00\x15\x00\x00\x00\x1e\x00\x00\x00\x1f\x00\x00\x00\x18\x00\x00\x00\x1b\x00\x00\x00\x02\x00\x00\x00\x18\x00\x00\x00\x1c\x00\x00\x00\x18\x00\x00\x00\x1e\x00\x00\x00\x1f\x00\x00\x00\x08\x00\x00\x00\x18\x00\x00\x00\x02\x00\x00\x00\x10\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x0e\x00\x00\x00\x1d\x00\x00\x00\x15\x00\x00\x00\x11\x00\x00\x00\x1b\x00\x00\x00\x13\x00\x00\x00\x1d\x00\x00\x00\x19\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x18\x00\x00\x00\x11\x00\x00\x00\x10\x00\x00\x00\x13\x00\x00\x00\x1c\x00\x00\x00\x02\x00\x00\x00\x1e\x00\x00\x00\x1f\x00\x00\x00\x18\x00\x00\x00\x12\x00\x00\x00\x17\x00\x00\x00\x02\x00\x00\x00\x1c\x00\x00\x00\x1a\x00\x00\x00\x1e\x00\x00\x00\x1f\x00\x00\x00\x1b\x00\x00\x00\x0e\x00\x00\x00\x1d\x00\x00\x00\x1b\x00\x00\x00\x11\x00\x00\x00\x10\x00\x00\x00\x13\x00\x00\x00\x02\x00\x00\x00\x1c\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x18\x00\x00\x00\x13\x00\x00\x00\x02\x00\x00\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x14\x00\x00\x00\x1e\x00\x00\x00\x1f\x00\x00\x00\x0e\x00\x00\x00\x0c\x00\x00\x00\x1c\x00\x00\x00\x11\x00\x00\x00\x1e\x00\x00\x00\x13\x00\x00\x00\x02\x00\x00\x00\x15\x00\x00\x00\x1d\x00\x00\x00\x11\x00\x00\x00\x18\x00\x00\x00\x13\x00\x00\x00\x02\x00\x00\x00\x15\x00\x00\x00\x1c\x00\x00\x00\x0f\x00\x00\x00\x1e\x00\x00\x00\x1f\x00\x00\x00\x0e\x00\x00\x00\x14\x00\x00\x00\x1c\x00\x00\x00\x11\x00\x00\x00\x1e\x00\x00\x00\x13\x00\x00\x00\x05\x00\x00\x00\x20\x00\x00\x00\x21\x00\x00\x00\x11\x00\x00\x00\x18\x00\x00\x00\x13\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x1c\x00\x00\x00\x06\x00\x00\x00\x1e\x00\x00\x00\x1f\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x1c\x00\x00\x00\x0d\x00\x00\x00\x1e\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0d\x00\x00\x00\x0c\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x06\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x1d\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x07\x00\x00\x00\x19\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x01\x00\x00\x00\x1d\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x04\x00\x00\x00\x0c\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x01\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x0f\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\x12\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x19\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x12\x00\x00\x00\x1d\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\xff\xff\xff\xff\x13\x00\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\xff\xff\xff\xff\x19\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x18\x00\x00\x00\x1d\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x1d\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\xff\xff\xff\xff\x13\x00\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x0a\x00\x00\x00\x19\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\xff\xff\xff\xff\x1d\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\xff\xff\xff\xff\x13\x00\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x19\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\xff\xff\xff\xff\x1d\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x19\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\xff\xff\xff\xff\x1d\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x19\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\xff\xff\xff\xff\x1d\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x19\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\xff\xff\xff\xff\x1d\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x19\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x15\x00\x00\x00\x1d\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\x19\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\xff\xff\xff\xff\x1d\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\x24\x00\x00\x00\x25\x00\x00\x00\x26\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x1d\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x1d\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x00\x00\x1f\x00\x00\x00\x53\x00\x00\x00\x33\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\x04\x00\x00\x00\x57\x00\x00\x00\x1f\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x20\x00\x00\x00\x1d\x00\x00\x00\xff\xff\xff\xff\x21\x00\x00\x00\x54\x00\x00\x00\x22\x00\x00\x00\x5a\x00\x00\x00\x34\x00\x00\x00\x20\x00\x00\x00\x12\x00\x00\x00\x23\x00\x00\x00\x21\x00\x00\x00\x11\x00\x00\x00\x22\x00\x00\x00\x24\x00\x00\x00\x34\x00\x00\x00\x25\x00\x00\x00\x26\x00\x00\x00\x23\x00\x00\x00\x58\x00\x00\x00\x1f\x00\x00\x00\x0f\x00\x00\x00\x24\x00\x00\x00\x0e\x00\x00\x00\x25\x00\x00\x00\x26\x00\x00\x00\x44\x00\x00\x00\x0d\x00\x00\x00\x1f\x00\x00\x00\x36\x00\x00\x00\x26\x00\x00\x00\x1c\x00\x00\x00\x20\x00\x00\x00\x1d\x00\x00\x00\x51\x00\x00\x00\x21\x00\x00\x00\x3a\x00\x00\x00\x22\x00\x00\x00\x1d\x00\x00\x00\x2a\x00\x00\x00\x20\x00\x00\x00\x3d\x00\x00\x00\x23\x00\x00\x00\x21\x00\x00\x00\x36\x00\x00\x00\x22\x00\x00\x00\x24\x00\x00\x00\x1f\x00\x00\x00\x25\x00\x00\x00\x26\x00\x00\x00\x23\x00\x00\x00\x37\x00\x00\x00\x4f\x00\x00\x00\x1f\x00\x00\x00\x24\x00\x00\x00\x50\x00\x00\x00\x25\x00\x00\x00\x26\x00\x00\x00\x4b\x00\x00\x00\x20\x00\x00\x00\x1d\x00\x00\x00\x04\x00\x00\x00\x21\x00\x00\x00\x36\x00\x00\x00\x22\x00\x00\x00\x1f\x00\x00\x00\x4b\x00\x00\x00\x36\x00\x00\x00\x21\x00\x00\x00\x23\x00\x00\x00\x22\x00\x00\x00\x1f\x00\x00\x00\x60\x00\x00\x00\x24\x00\x00\x00\x49\x00\x00\x00\x25\x00\x00\x00\x26\x00\x00\x00\x20\x00\x00\x00\x46\x00\x00\x00\x24\x00\x00\x00\x21\x00\x00\x00\x25\x00\x00\x00\x22\x00\x00\x00\x1f\x00\x00\x00\x34\x00\x00\x00\x65\x00\x00\x00\x21\x00\x00\x00\x23\x00\x00\x00\x22\x00\x00\x00\x1f\x00\x00\x00\x3a\x00\x00\x00\x24\x00\x00\x00\x47\x00\x00\x00\x25\x00\x00\x00\x26\x00\x00\x00\x20\x00\x00\x00\x63\x00\x00\x00\x24\x00\x00\x00\x21\x00\x00\x00\x25\x00\x00\x00\x22\x00\x00\x00\x62\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x21\x00\x00\x00\x23\x00\x00\x00\x22\x00\x00\x00\x04\x00\x00\x00\x02\x00\x00\x00\x24\x00\x00\x00\x61\x00\x00\x00\x25\x00\x00\x00\x26\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x24\x00\x00\x00\x69\x00\x00\x00\x25\x00\x00\x00\x2a\x00\x00\x00\x2b\x00\x00\x00\x2c\x00\x00\x00\x67\x00\x00\x00\x2d\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x66\x00\x00\x00\x2e\x00\x00\x00\x2f\x00\x00\x00\x6b\x00\x00\x00\x30\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\x12\x00\x00\x00\x06\x00\x00\x00\x28\x00\x00\x00\x1a\x00\x00\x00\x31\x00\x00\x00\x1c\x00\x00\x00\x02\x00\x00\x00\x1d\x00\x00\x00\x5a\x00\x00\x00\x2b\x00\x00\x00\x2c\x00\x00\x00\x0f\x00\x00\x00\x2d\x00\x00\x00\x27\x00\x00\x00\x14\x00\x00\x00\x34\x00\x00\x00\x2e\x00\x00\x00\x2f\x00\x00\x00\x55\x00\x00\x00\x30\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\x51\x00\x00\x00\x3d\x00\x00\x00\x3e\x00\x00\x00\x1a\x00\x00\x00\x31\x00\x00\x00\x1c\x00\x00\x00\x5d\x00\x00\x00\x1d\x00\x00\x00\x5b\x00\x00\x00\x5c\x00\x00\x00\x40\x00\x00\x00\x41\x00\x00\x00\x42\x00\x00\x00\xcd\xff\xff\xff\x2e\x00\x00\x00\x2f\x00\x00\x00\x00\x00\x00\x00\x30\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\x4c\x00\x00\x00\x4d\x00\x00\x00\x00\x00\x00\x00\x1a\x00\x00\x00\x31\x00\x00\x00\x1c\x00\x00\x00\x63\x00\x00\x00\x1d\x00\x00\x00\x5b\x00\x00\x00\x69\x00\x00\x00\x37\x00\x00\x00\x1c\x00\x00\x00\x38\x00\x00\x00\x1d\x00\x00\x00\x2e\x00\x00\x00\x2f\x00\x00\x00\x00\x00\x00\x00\x30\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\x67\x00\x00\x00\x4d\x00\x00\x00\x58\x00\x00\x00\x1a\x00\x00\x00\x31\x00\x00\x00\x1c\x00\x00\x00\x00\x00\x00\x00\x1d\x00\x00\x00\x2e\x00\x00\x00\x2f\x00\x00\x00\x00\x00\x00\x00\x30\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1a\x00\x00\x00\x31\x00\x00\x00\x1c\x00\x00\x00\x00\x00\x00\x00\x1d\x00\x00\x00\x3b\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x00\x00\x00\x00\x1d\x00\x00\x00\x54\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x00\x00\x00\x00\x1d\x00\x00\x00\x44\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x00\x00\x00\x00\x1d\x00\x00\x00\x5e\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x42\x00\x00\x00\x1d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x00\x00\x00\x00\x1d\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x37\x00\x00\x00\x1c\x00\x00\x00\x49\x00\x00\x00\x1d\x00\x00\x00\x37\x00\x00\x00\x1c\x00\x00\x00\x47\x00\x00\x00\x1d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 70) [
        (1 , happyReduce_1),
        (2 , happyReduce_2),
        (3 , happyReduce_3),
        (4 , happyReduce_4),
        (5 , happyReduce_5),
        (6 , happyReduce_6),
        (7 , happyReduce_7),
        (8 , happyReduce_8),
        (9 , happyReduce_9),
        (10 , happyReduce_10),
        (11 , happyReduce_11),
        (12 , happyReduce_12),
        (13 , happyReduce_13),
        (14 , happyReduce_14),
        (15 , happyReduce_15),
        (16 , happyReduce_16),
        (17 , happyReduce_17),
        (18 , happyReduce_18),
        (19 , happyReduce_19),
        (20 , happyReduce_20),
        (21 , happyReduce_21),
        (22 , happyReduce_22),
        (23 , happyReduce_23),
        (24 , happyReduce_24),
        (25 , happyReduce_25),
        (26 , happyReduce_26),
        (27 , happyReduce_27),
        (28 , happyReduce_28),
        (29 , happyReduce_29),
        (30 , happyReduce_30),
        (31 , happyReduce_31),
        (32 , happyReduce_32),
        (33 , happyReduce_33),
        (34 , happyReduce_34),
        (35 , happyReduce_35),
        (36 , happyReduce_36),
        (37 , happyReduce_37),
        (38 , happyReduce_38),
        (39 , happyReduce_39),
        (40 , happyReduce_40),
        (41 , happyReduce_41),
        (42 , happyReduce_42),
        (43 , happyReduce_43),
        (44 , happyReduce_44),
        (45 , happyReduce_45),
        (46 , happyReduce_46),
        (47 , happyReduce_47),
        (48 , happyReduce_48),
        (49 , happyReduce_49),
        (50 , happyReduce_50),
        (51 , happyReduce_51),
        (52 , happyReduce_52),
        (53 , happyReduce_53),
        (54 , happyReduce_54),
        (55 , happyReduce_55),
        (56 , happyReduce_56),
        (57 , happyReduce_57),
        (58 , happyReduce_58),
        (59 , happyReduce_59),
        (60 , happyReduce_60),
        (61 , happyReduce_61),
        (62 , happyReduce_62),
        (63 , happyReduce_63),
        (64 , happyReduce_64),
        (65 , happyReduce_65),
        (66 , happyReduce_66),
        (67 , happyReduce_67),
        (68 , happyReduce_68),
        (69 , happyReduce_69),
        (70 , happyReduce_70)
        ]

happyRuleArr :: HappyAddr
happyRuleArr = HappyA# "\x00\x00\x00\x00\x05\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x02\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x02\x00\x00\x00\x04\x00\x00\x00\x01\x00\x00\x00\x05\x00\x00\x00\x02\x00\x00\x00\x05\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x02\x00\x00\x00\x06\x00\x00\x00\x02\x00\x00\x00\x07\x00\x00\x00\x02\x00\x00\x00\x08\x00\x00\x00\x02\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x09\x00\x00\x00\x02\x00\x00\x00\x09\x00\x00\x00\x04\x00\x00\x00\x09\x00\x00\x00\x01\x00\x00\x00\x0a\x00\x00\x00\x02\x00\x00\x00\x0b\x00\x00\x00\x02\x00\x00\x00\x0b\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x00\x00\x03\x00\x00\x00\x0d\x00\x00\x00\x03\x00\x00\x00\x0d\x00\x00\x00\x01\x00\x00\x00\x0e\x00\x00\x00\x01\x00\x00\x00\x0e\x00\x00\x00\x01\x00\x00\x00\x0f\x00\x00\x00\x01\x00\x00\x00\x0f\x00\x00\x00\x01\x00\x00\x00\x10\x00\x00\x00\x03\x00\x00\x00\x10\x00\x00\x00\x02\x00\x00\x00\x11\x00\x00\x00\x01\x00\x00\x00\x11\x00\x00\x00\x02\x00\x00\x00\x12\x00\x00\x00\x01\x00\x00\x00\x12\x00\x00\x00\x02\x00\x00\x00\x12\x00\x00\x00\x02\x00\x00\x00\x12\x00\x00\x00\x00\x00\x00\x00\x13\x00\x00\x00\x03\x00\x00\x00\x13\x00\x00\x00\x01\x00\x00\x00\x14\x00\x00\x00\x02\x00\x00\x00\x14\x00\x00\x00\x01\x00\x00\x00\x15\x00\x00\x00\x02\x00\x00\x00\x15\x00\x00\x00\x01\x00\x00\x00\x16\x00\x00\x00\x01\x00\x00\x00\x16\x00\x00\x00\x01\x00\x00\x00\x16\x00\x00\x00\x01\x00\x00\x00\x16\x00\x00\x00\x04\x00\x00\x00\x17\x00\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00\x01\x00\x00\x00\x18\x00\x00\x00\x02\x00\x00\x00\x18\x00\x00\x00\x03\x00\x00\x00\x19\x00\x00\x00\x02\x00\x00\x00\x19\x00\x00\x00\x01\x00\x00\x00\x19\x00\x00\x00\x01\x00\x00\x00\x19\x00\x00\x00\x01\x00\x00\x00\x19\x00\x00\x00\x03\x00\x00\x00\x1a\x00\x00\x00\x03\x00\x00\x00\x1a\x00\x00\x00\x01\x00\x00\x00\x1b\x00\x00\x00\x01\x00\x00\x00\x1b\x00\x00\x00\x03\x00\x00\x00\x1b\x00\x00\x00\x01\x00\x00\x00\x1b\x00\x00\x00\x03\x00\x00\x00\x1b\x00\x00\x00\x04\x00\x00\x00\x1b\x00\x00\x00\x02\x00\x00\x00\x1c\x00\x00\x00\x02\x00\x00\x00\x1c\x00\x00\x00\x00\x00\x00\x00\x1d\x00\x00\x00\x01\x00\x00\x00\x1d\x00\x00\x00\x01\x00\x00\x00"#

happyCatchStates :: [Happy_Prelude.Int]
happyCatchStates = []

happy_n_terms = 40 :: Happy_Prelude.Int
happy_n_nonterms = 30 :: Happy_Prelude.Int

happy_n_starts = 1 :: Happy_Prelude.Int

happyReduce_1 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_1 = happyReduce 5# 0# happyReduction_1
happyReduction_1 (happy_x_5 `HappyStk`
        happy_x_4 `HappyStk`
        happy_x_3 `HappyStk`
        happy_x_2 `HappyStk`
        happy_x_1 `HappyStk`
        happyRest)
         = case happyOut6 happy_x_1 of { (HappyWrap6 happy_var_1) -> 
        case happyOut7 happy_x_2 of { (HappyWrap7 happy_var_2) -> 
        case happyOut12 happy_x_4 of { (HappyWrap12 happy_var_4) -> 
        case happyOut6 happy_x_5 of { (HappyWrap6 happy_var_5) -> 
        happyIn5
                 ((happy_var_1,happy_var_2,happy_var_4,happy_var_5)
        ) `HappyStk` happyRest}}}}

happyReduce_2 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_2 = happySpecReduce_1  1# happyReduction_2
happyReduction_2 happy_x_1
         =  case happyOutTok happy_x_1 of { happy_var_1 -> 
        happyIn6
                 (case happy_var_1 of T pos (CodeT code) ->
						Just (pos,code)
        )}

happyReduce_3 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_3 = happySpecReduce_0  1# happyReduction_3
happyReduction_3  =  happyIn6
                 (Nothing
        )

happyReduce_4 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_4 = happySpecReduce_2  2# happyReduction_4
happyReduction_4 happy_x_2
        happy_x_1
         =  case happyOut8 happy_x_1 of { (HappyWrap8 happy_var_1) -> 
        case happyOut7 happy_x_2 of { (HappyWrap7 happy_var_2) -> 
        happyIn7
                 (happy_var_1 : happy_var_2
        )}}

happyReduce_5 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_5 = happySpecReduce_0  2# happyReduction_5
happyReduction_5  =  happyIn7
                 ([]
        )

happyReduce_6 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_6 = happySpecReduce_2  3# happyReduction_6
happyReduction_6 happy_x_2
        happy_x_1
         =  case happyOutTok happy_x_2 of { (T _ (StringT happy_var_2)) -> 
        happyIn8
                 (WrapperDirective happy_var_2
        )}

happyReduce_7 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_7 = happySpecReduce_2  3# happyReduction_7
happyReduction_7 happy_x_2
        happy_x_1
         =  case happyOut9 happy_x_2 of { (HappyWrap9 happy_var_2) -> 
        happyIn8
                 (EncodingDirective happy_var_2
        )}

happyReduce_8 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_8 = happySpecReduce_2  3# happyReduction_8
happyReduction_8 happy_x_2
        happy_x_1
         =  case happyOutTok happy_x_2 of { (T _ (StringT happy_var_2)) -> 
        happyIn8
                 (ActionType happy_var_2
        )}

happyReduce_9 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_9 = happySpecReduce_2  3# happyReduction_9
happyReduction_9 happy_x_2
        happy_x_1
         =  case happyOutTok happy_x_2 of { (T _ (StringT happy_var_2)) -> 
        happyIn8
                 (TokenType happy_var_2
        )}

happyReduce_10 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_10 = happySpecReduce_2  3# happyReduction_10
happyReduction_10 happy_x_2
        happy_x_1
         =  case happyOutTok happy_x_2 of { (T _ (StringT happy_var_2)) -> 
        happyIn8
                 (TypeClass happy_var_2
        )}

happyReduce_11 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_11 = happyMonadReduce 1# 4# happyReduction_11
happyReduction_11 (happy_x_1 `HappyStk`
        happyRest) tk
         = happyThen ((case happyOutTok happy_x_1 of { (T _ (StringT happy_var_1)) -> 
        ( lookupEncoding happy_var_1)})
        ) (\r -> happyReturn (happyIn9 r))

happyReduce_12 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_12 = happySpecReduce_2  5# happyReduction_12
happyReduction_12 happy_x_2
        happy_x_1
         =  happyIn10
                 (()
        )

happyReduce_13 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_13 = happySpecReduce_0  5# happyReduction_13
happyReduction_13  =  happyIn10
                 (()
        )

happyReduce_14 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_14 = happyMonadReduce 2# 6# happyReduction_14
happyReduction_14 (happy_x_2 `HappyStk`
        happy_x_1 `HappyStk`
        happyRest) tk
         = happyThen ((case happyOutTok happy_x_1 of { (T _ (SMacDefT happy_var_1)) -> 
        case happyOut31 happy_x_2 of { (HappyWrap31 happy_var_2) -> 
        ( newSMac happy_var_1 happy_var_2)}})
        ) (\r -> happyReturn (happyIn11 r))

happyReduce_15 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_15 = happyMonadReduce 2# 6# happyReduction_15
happyReduction_15 (happy_x_2 `HappyStk`
        happy_x_1 `HappyStk`
        happyRest) tk
         = happyThen ((case happyOutTok happy_x_1 of { (T _ (RMacDefT happy_var_1)) -> 
        case happyOut24 happy_x_2 of { (HappyWrap24 happy_var_2) -> 
        ( newRMac happy_var_1 happy_var_2)}})
        ) (\r -> happyReturn (happyIn11 r))

happyReduce_16 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_16 = happySpecReduce_2  7# happyReduction_16
happyReduction_16 happy_x_2
        happy_x_1
         =  case happyOutTok happy_x_1 of { (T _ (BindT happy_var_1)) -> 
        case happyOut13 happy_x_2 of { (HappyWrap13 happy_var_2) -> 
        happyIn12
                 (Scanner happy_var_1 happy_var_2
        )}}

happyReduce_17 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_17 = happySpecReduce_2  8# happyReduction_17
happyReduction_17 happy_x_2
        happy_x_1
         =  case happyOut14 happy_x_1 of { (HappyWrap14 happy_var_1) -> 
        case happyOut13 happy_x_2 of { (HappyWrap13 happy_var_2) -> 
        happyIn13
                 (happy_var_1 ++ happy_var_2
        )}}

happyReduce_18 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_18 = happySpecReduce_0  8# happyReduction_18
happyReduction_18  =  happyIn13
                 ([]
        )

happyReduce_19 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_19 = happySpecReduce_2  9# happyReduction_19
happyReduction_19 happy_x_2
        happy_x_1
         =  case happyOut17 happy_x_1 of { (HappyWrap17 happy_var_1) -> 
        case happyOut15 happy_x_2 of { (HappyWrap15 happy_var_2) -> 
        happyIn14
                 ([ replaceCodes happy_var_1 (snd happy_var_2) ]
        )}}

happyReduce_20 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_20 = happyReduce 4# 9# happyReduction_20
happyReduction_20 (happy_x_4 `HappyStk`
        happy_x_3 `HappyStk`
        happy_x_2 `HappyStk`
        happy_x_1 `HappyStk`
        happyRest)
         = case happyOut17 happy_x_1 of { (HappyWrap17 happy_var_1) -> 
        case happyOut16 happy_x_3 of { (HappyWrap16 happy_var_3) -> 
        happyIn14
                 (map (replaceCodes happy_var_1) happy_var_3
        ) `HappyStk` happyRest}}

happyReduce_21 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_21 = happyMonadReduce 1# 9# happyReduction_21
happyReduction_21 (happy_x_1 `HappyStk`
        happyRest) tk
         = happyThen ((case happyOut15 happy_x_1 of { (HappyWrap15 happy_var_1) -> 
        ( do
                                           let (pos, res@(RECtx _ _ e _ _)) = happy_var_1
                                           warnIfNullable e pos
                                           return [ res ])})
        ) (\r -> happyReturn (happyIn14 r))

happyReduce_22 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_22 = happySpecReduce_2  10# happyReduction_22
happyReduction_22 happy_x_2
        happy_x_1
         =  case happyOut21 happy_x_1 of { (HappyWrap21 happy_var_1) -> 
        case happyOut20 happy_x_2 of { (HappyWrap20 happy_var_2) -> 
        happyIn15
                 (let
                                            (l, e, r)   = happy_var_1
                                            (pos, code) = happy_var_2
                                          in (pos, RECtx [] l e r code)
        )}}

happyReduce_23 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_23 = happySpecReduce_2  11# happyReduction_23
happyReduction_23 happy_x_2
        happy_x_1
         =  case happyOut15 happy_x_1 of { (HappyWrap15 happy_var_1) -> 
        case happyOut16 happy_x_2 of { (HappyWrap16 happy_var_2) -> 
        happyIn16
                 (snd happy_var_1 : happy_var_2
        )}}

happyReduce_24 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_24 = happySpecReduce_0  11# happyReduction_24
happyReduction_24  =  happyIn16
                 ([]
        )

happyReduce_25 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_25 = happySpecReduce_3  12# happyReduction_25
happyReduction_25 happy_x_3
        happy_x_2
        happy_x_1
         =  case happyOut18 happy_x_2 of { (HappyWrap18 happy_var_2) -> 
        happyIn17
                 (happy_var_2
        )}

happyReduce_26 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_26 = happySpecReduce_3  13# happyReduction_26
happyReduction_26 happy_x_3
        happy_x_2
        happy_x_1
         =  case happyOut19 happy_x_1 of { (HappyWrap19 happy_var_1) -> 
        case happyOut18 happy_x_3 of { (HappyWrap18 happy_var_3) -> 
        happyIn18
                 ((happy_var_1,0) : happy_var_3
        )}}

happyReduce_27 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_27 = happySpecReduce_1  13# happyReduction_27
happyReduction_27 happy_x_1
         =  case happyOut19 happy_x_1 of { (HappyWrap19 happy_var_1) -> 
        happyIn18
                 ([(happy_var_1,0)]
        )}

happyReduce_28 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_28 = happySpecReduce_1  14# happyReduction_28
happyReduction_28 happy_x_1
         =  happyIn19
                 ("0"
        )

happyReduce_29 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_29 = happySpecReduce_1  14# happyReduction_29
happyReduction_29 happy_x_1
         =  case happyOutTok happy_x_1 of { (T _ (IdT happy_var_1)) -> 
        happyIn19
                 (happy_var_1
        )}

happyReduce_30 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_30 = happySpecReduce_1  15# happyReduction_30
happyReduction_30 happy_x_1
         =  case happyOutTok happy_x_1 of { happy_var_1 -> 
        happyIn20
                 (case happy_var_1 of T pos (CodeT code) -> (pos, Just code)
        )}

happyReduce_31 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_31 = happySpecReduce_1  15# happyReduction_31
happyReduction_31 happy_x_1
         =  case happyOutTok happy_x_1 of { happy_var_1 -> 
        happyIn20
                 ((tokPosn happy_var_1, Nothing)
        )}

happyReduce_32 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_32 = happySpecReduce_3  16# happyReduction_32
happyReduction_32 happy_x_3
        happy_x_2
        happy_x_1
         =  case happyOut22 happy_x_1 of { (HappyWrap22 happy_var_1) -> 
        case happyOut24 happy_x_2 of { (HappyWrap24 happy_var_2) -> 
        case happyOut23 happy_x_3 of { (HappyWrap23 happy_var_3) -> 
        happyIn21
                 ((Just happy_var_1,happy_var_2,happy_var_3)
        )}}}

happyReduce_33 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_33 = happySpecReduce_2  16# happyReduction_33
happyReduction_33 happy_x_2
        happy_x_1
         =  case happyOut24 happy_x_1 of { (HappyWrap24 happy_var_1) -> 
        case happyOut23 happy_x_2 of { (HappyWrap23 happy_var_2) -> 
        happyIn21
                 ((Nothing,happy_var_1,happy_var_2)
        )}}

happyReduce_34 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_34 = happySpecReduce_1  17# happyReduction_34
happyReduction_34 happy_x_1
         =  happyIn22
                 (charSetSingleton '\n'
        )

happyReduce_35 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_35 = happySpecReduce_2  17# happyReduction_35
happyReduction_35 happy_x_2
        happy_x_1
         =  case happyOut31 happy_x_1 of { (HappyWrap31 happy_var_1) -> 
        happyIn22
                 (happy_var_1
        )}

happyReduce_36 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_36 = happySpecReduce_1  18# happyReduction_36
happyReduction_36 happy_x_1
         =  happyIn23
                 (RightContextRExp (Ch (charSetSingleton '\n'))
        )

happyReduce_37 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_37 = happySpecReduce_2  18# happyReduction_37
happyReduction_37 happy_x_2
        happy_x_1
         =  case happyOut24 happy_x_2 of { (HappyWrap24 happy_var_2) -> 
        happyIn23
                 (RightContextRExp happy_var_2
        )}

happyReduce_38 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_38 = happySpecReduce_2  18# happyReduction_38
happyReduction_38 happy_x_2
        happy_x_1
         =  case happyOutTok happy_x_2 of { happy_var_2 -> 
        happyIn23
                 (RightContextCode (case happy_var_2 of
						T _ (CodeT code) -> code)
        )}

happyReduce_39 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_39 = happySpecReduce_0  18# happyReduction_39
happyReduction_39  =  happyIn23
                 (NoRightContext
        )

happyReduce_40 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_40 = happySpecReduce_3  19# happyReduction_40
happyReduction_40 happy_x_3
        happy_x_2
        happy_x_1
         =  case happyOut25 happy_x_1 of { (HappyWrap25 happy_var_1) -> 
        case happyOut24 happy_x_3 of { (HappyWrap24 happy_var_3) -> 
        happyIn24
                 (happy_var_1 :|| happy_var_3
        )}}

happyReduce_41 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_41 = happySpecReduce_1  19# happyReduction_41
happyReduction_41 happy_x_1
         =  case happyOut25 happy_x_1 of { (HappyWrap25 happy_var_1) -> 
        happyIn24
                 (happy_var_1
        )}

happyReduce_42 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_42 = happySpecReduce_2  20# happyReduction_42
happyReduction_42 happy_x_2
        happy_x_1
         =  case happyOut25 happy_x_1 of { (HappyWrap25 happy_var_1) -> 
        case happyOut26 happy_x_2 of { (HappyWrap26 happy_var_2) -> 
        happyIn25
                 (happy_var_1 :%% happy_var_2
        )}}

happyReduce_43 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_43 = happySpecReduce_1  20# happyReduction_43
happyReduction_43 happy_x_1
         =  case happyOut26 happy_x_1 of { (HappyWrap26 happy_var_1) -> 
        happyIn25
                 (happy_var_1
        )}

happyReduce_44 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_44 = happySpecReduce_2  21# happyReduction_44
happyReduction_44 happy_x_2
        happy_x_1
         =  case happyOut30 happy_x_1 of { (HappyWrap30 happy_var_1) -> 
        case happyOut27 happy_x_2 of { (HappyWrap27 happy_var_2) -> 
        happyIn26
                 (happy_var_2 happy_var_1
        )}}

happyReduce_45 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_45 = happySpecReduce_1  21# happyReduction_45
happyReduction_45 happy_x_1
         =  case happyOut30 happy_x_1 of { (HappyWrap30 happy_var_1) -> 
        happyIn26
                 (happy_var_1
        )}

happyReduce_46 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_46 = happySpecReduce_1  22# happyReduction_46
happyReduction_46 happy_x_1
         =  happyIn27
                 (Star
        )

happyReduce_47 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_47 = happySpecReduce_1  22# happyReduction_47
happyReduction_47 happy_x_1
         =  happyIn27
                 (Plus
        )

happyReduce_48 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_48 = happySpecReduce_1  22# happyReduction_48
happyReduction_48 happy_x_1
         =  happyIn27
                 (Ques
        )

happyReduce_49 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_49 = happyReduce 4# 22# happyReduction_49
happyReduction_49 (happy_x_4 `HappyStk`
        happy_x_3 `HappyStk`
        happy_x_2 `HappyStk`
        happy_x_1 `HappyStk`
        happyRest)
         = case happyOut29 happy_x_3 of { (HappyWrap29 happy_var_3) -> 
        happyIn27
                 (happy_var_3
        ) `HappyStk` happyRest}

happyReduce_50 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_50 = happyMonadReduce 0# 23# happyReduction_50
happyReduction_50 (happyRest) tk
         = happyThen ((( setStartCode multiplicity))
        ) (\r -> happyReturn (happyIn28 r))

happyReduce_51 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_51 = happySpecReduce_1  24# happyReduction_51
happyReduction_51 happy_x_1
         =  case happyOutTok happy_x_1 of { (T _ (NumT happy_var_1)) -> 
        happyIn29
                 (repeat_rng happy_var_1 Nothing
        )}

happyReduce_52 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_52 = happySpecReduce_2  24# happyReduction_52
happyReduction_52 happy_x_2
        happy_x_1
         =  case happyOutTok happy_x_1 of { (T _ (NumT happy_var_1)) -> 
        happyIn29
                 (repeat_rng happy_var_1 (Just Nothing)
        )}

happyReduce_53 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_53 = happySpecReduce_3  24# happyReduction_53
happyReduction_53 happy_x_3
        happy_x_2
        happy_x_1
         =  case happyOutTok happy_x_1 of { (T _ (NumT happy_var_1)) -> 
        case happyOutTok happy_x_3 of { (T _ (NumT happy_var_3)) -> 
        happyIn29
                 (repeat_rng happy_var_1 (Just (Just happy_var_3))
        )}}

happyReduce_54 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_54 = happySpecReduce_2  25# happyReduction_54
happyReduction_54 happy_x_2
        happy_x_1
         =  happyIn30
                 (Eps
        )

happyReduce_55 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_55 = happySpecReduce_1  25# happyReduction_55
happyReduction_55 happy_x_1
         =  case happyOutTok happy_x_1 of { (T _ (StringT happy_var_1)) -> 
        happyIn30
                 (foldr (:%%) Eps
					    (map (Ch . charSetSingleton) happy_var_1)
        )}

happyReduce_56 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_56 = happyMonadReduce 1# 25# happyReduction_56
happyReduction_56 (happy_x_1 `HappyStk`
        happyRest) tk
         = happyThen ((case happyOutTok happy_x_1 of { (T _ (RMacT happy_var_1)) -> 
        ( lookupRMac happy_var_1)})
        ) (\r -> happyReturn (happyIn30 r))

happyReduce_57 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_57 = happySpecReduce_1  25# happyReduction_57
happyReduction_57 happy_x_1
         =  case happyOut31 happy_x_1 of { (HappyWrap31 happy_var_1) -> 
        happyIn30
                 (Ch happy_var_1
        )}

happyReduce_58 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_58 = happySpecReduce_3  25# happyReduction_58
happyReduction_58 happy_x_3
        happy_x_2
        happy_x_1
         =  case happyOut24 happy_x_2 of { (HappyWrap24 happy_var_2) -> 
        happyIn30
                 (happy_var_2
        )}

happyReduce_59 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_59 = happySpecReduce_3  26# happyReduction_59
happyReduction_59 happy_x_3
        happy_x_2
        happy_x_1
         =  case happyOut31 happy_x_1 of { (HappyWrap31 happy_var_1) -> 
        case happyOut32 happy_x_3 of { (HappyWrap32 happy_var_3) -> 
        happyIn31
                 (happy_var_1 `charSetMinus` happy_var_3
        )}}

happyReduce_60 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_60 = happySpecReduce_1  26# happyReduction_60
happyReduction_60 happy_x_1
         =  case happyOut32 happy_x_1 of { (HappyWrap32 happy_var_1) -> 
        happyIn31
                 (happy_var_1
        )}

happyReduce_61 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_61 = happySpecReduce_1  27# happyReduction_61
happyReduction_61 happy_x_1
         =  case happyOutTok happy_x_1 of { (T _ (CharT happy_var_1)) -> 
        happyIn32
                 (charSetSingleton happy_var_1
        )}

happyReduce_62 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_62 = happySpecReduce_3  27# happyReduction_62
happyReduction_62 happy_x_3
        happy_x_2
        happy_x_1
         =  case happyOutTok happy_x_1 of { (T _ (CharT happy_var_1)) -> 
        case happyOutTok happy_x_3 of { (T _ (CharT happy_var_3)) -> 
        happyIn32
                 (charSetRange happy_var_1 happy_var_3
        )}}

happyReduce_63 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_63 = happyMonadReduce 1# 27# happyReduction_63
happyReduction_63 (happy_x_1 `HappyStk`
        happyRest) tk
         = happyThen ((case happyOut34 happy_x_1 of { (HappyWrap34 happy_var_1) -> 
        ( lookupSMac happy_var_1)})
        ) (\r -> happyReturn (happyIn32 r))

happyReduce_64 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_64 = happySpecReduce_3  27# happyReduction_64
happyReduction_64 happy_x_3
        happy_x_2
        happy_x_1
         =  case happyOut33 happy_x_2 of { (HappyWrap33 happy_var_2) -> 
        happyIn32
                 (foldr charSetUnion emptyCharSet happy_var_2
        )}

happyReduce_65 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_65 = happyMonadReduce 4# 27# happyReduction_65
happyReduction_65 (happy_x_4 `HappyStk`
        happy_x_3 `HappyStk`
        happy_x_2 `HappyStk`
        happy_x_1 `HappyStk`
        happyRest) tk
         = happyThen ((case happyOutTok happy_x_1 of { happy_var_1 -> 
        case happyOut33 happy_x_3 of { (HappyWrap33 happy_var_3) -> 
        ( do { dot <- lookupSMac (tokPosn happy_var_1, ".");
		      	        return (dot `charSetMinus`
			      		  foldr charSetUnion emptyCharSet happy_var_3) })}})
        ) (\r -> happyReturn (happyIn32 r))

happyReduce_66 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_66 = happyMonadReduce 2# 27# happyReduction_66
happyReduction_66 (happy_x_2 `HappyStk`
        happy_x_1 `HappyStk`
        happyRest) tk
         = happyThen ((case happyOutTok happy_x_1 of { happy_var_1 -> 
        case happyOut32 happy_x_2 of { (HappyWrap32 happy_var_2) -> 
        ( do { dot <- lookupSMac (tokPosn happy_var_1, ".");
		      	        return (dot `charSetMinus` happy_var_2) })}})
        ) (\r -> happyReturn (happyIn32 r))

happyReduce_67 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_67 = happySpecReduce_2  28# happyReduction_67
happyReduction_67 happy_x_2
        happy_x_1
         =  case happyOut31 happy_x_1 of { (HappyWrap31 happy_var_1) -> 
        case happyOut33 happy_x_2 of { (HappyWrap33 happy_var_2) -> 
        happyIn33
                 (happy_var_1 : happy_var_2
        )}}

happyReduce_68 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_68 = happySpecReduce_0  28# happyReduction_68
happyReduction_68  =  happyIn33
                 ([]
        )

happyReduce_69 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_69 = happySpecReduce_1  29# happyReduction_69
happyReduction_69 happy_x_1
         =  case happyOutTok happy_x_1 of { happy_var_1 -> 
        happyIn34
                 ((tokPosn happy_var_1, ".")
        )}

happyReduce_70 :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> P (HappyAbsSyn )
happyReduce_70 = happySpecReduce_1  29# happyReduction_70
happyReduction_70 happy_x_1
         =  case happyOutTok happy_x_1 of { happy_var_1 -> 
        happyIn34
                 (case happy_var_1 of T p (SMacT s) -> (p, s)
        )}

happyTerminalToTok term = case term of {
        T _ EOFT -> 39#;
        T _ (SpecialT '.') -> 2#;
        T _ (SpecialT ';') -> 3#;
        T _ (SpecialT '<') -> 4#;
        T _ (SpecialT '>') -> 5#;
        T _ (SpecialT ',') -> 6#;
        T _ (SpecialT '$') -> 7#;
        T _ (SpecialT '|') -> 8#;
        T _ (SpecialT '*') -> 9#;
        T _ (SpecialT '+') -> 10#;
        T _ (SpecialT '?') -> 11#;
        T _ (SpecialT '{') -> 12#;
        T _ (SpecialT '}') -> 13#;
        T _ (SpecialT '(') -> 14#;
        T _ (SpecialT ')') -> 15#;
        T _ (SpecialT '#') -> 16#;
        T _ (SpecialT '~') -> 17#;
        T _ (SpecialT '-') -> 18#;
        T _ (SpecialT '[') -> 19#;
        T _ (SpecialT ']') -> 20#;
        T _ (SpecialT '^') -> 21#;
        T _ (SpecialT '/') -> 22#;
        T _ ZeroT -> 23#;
        T _ (StringT happy_dollar_dollar) -> 24#;
        T _ (BindT happy_dollar_dollar) -> 25#;
        T _ (IdT happy_dollar_dollar) -> 26#;
        T _ (CodeT _) -> 27#;
        T _ (CharT happy_dollar_dollar) -> 28#;
        T _ (NumT happy_dollar_dollar) -> 29#;
        T _ (SMacT _) -> 30#;
        T _ (RMacT happy_dollar_dollar) -> 31#;
        T _ (SMacDefT happy_dollar_dollar) -> 32#;
        T _ (RMacDefT happy_dollar_dollar) -> 33#;
        T _ WrapperT -> 34#;
        T _ EncodingT -> 35#;
        T _ ActionTypeT -> 36#;
        T _ TokenTypeT -> 37#;
        T _ TypeClassT -> 38#;
        _ -> -1#;
        }
{-# NOINLINE happyTerminalToTok #-}

happyLex kend kmore = lexer (\tk -> case tk of {
        T _ EOFT -> kend tk;
        _ -> kmore (happyTerminalToTok tk) tk })
{-# INLINE happyLex #-}

happyNewToken action sts stk = happyLex (\tk -> happyDoAction 39# tk action sts stk) (\i tk -> happyDoAction i tk action sts stk)

happyReport 39# = happyReport'
happyReport _ = happyReport'


happyThen :: () => (P a) -> (a -> (P b)) -> (P b)
happyThen = ((>>=))
happyReturn :: () => a -> (P a)
happyReturn = (return)
happyParse :: () => Happy_GHC_Exts.Int# -> P (HappyAbsSyn )

happyNewToken :: () => Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> (P (HappyAbsSyn ))

happyDoAction :: () => Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> (P (HappyAbsSyn ))

happyReduceArr :: () => Happy_Data_Array.Array Happy_Prelude.Int (Happy_GHC_Exts.Int# -> Token -> Happy_GHC_Exts.Int# -> Happy_IntList -> HappyStk (HappyAbsSyn ) -> (P (HappyAbsSyn )))

happyThen1 :: () => P a -> (a -> P b) -> P b
happyThen1 = happyThen
happyFmap1 f m = happyThen m (\a -> happyReturn (f a))
happyReturn1 :: () => a -> (P a)
happyReturn1 = happyReturn
happyReport' :: () => (Token) -> [Happy_Prelude.String] -> (P a) -> (P a)
happyReport' = (\tokens expected resume -> happyError)

happyAbort :: () => (P a)
happyAbort = Happy_Prelude.error "Called abort handler in non-resumptive parser"

parse = happySomeParser where
 happySomeParser = happyThen (happyParse 0#) (\x -> happyReturn (let {(HappyWrap5 x') = happyOut5 x} in x'))

happySeq = happyDontSeq


happyError :: P a
happyError = failP "parse error"

-- -----------------------------------------------------------------------------
-- Utils

digit c = ord c - ord '0'

repeat_rng :: Int -> Maybe (Maybe Int) -> (RExp->RExp)
repeat_rng n (Nothing) re = foldr (:%%) Eps (replicate n re)
repeat_rng n (Just Nothing) re = foldr (:%%) (Star re) (replicate n re)
repeat_rng n (Just (Just m)) re = intl :%% rst
	where
	intl = repeat_rng n Nothing re
	rst = foldr (\re re'->Ques(re :%% re')) Eps (replicate (m-n) re)

replaceCodes codes rectx = rectx{ reCtxStartCodes = codes }

lookupEncoding :: String -> P Encoding
lookupEncoding s = case map toLower s of
  "iso-8859-1" -> return Latin1
  "latin1"     -> return Latin1
  "utf-8"      -> return UTF8
  "utf8"       -> return UTF8
  _            -> failP ("encoding " ++ show s ++ " not supported")
#define HAPPY_COERCE 1
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $

#if !defined(__GLASGOW_HASKELL__)
#  error This code isn't being built with GHC.
#endif

-- Get WORDS_BIGENDIAN (if defined)
#include "MachDeps.h"

-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Happy_Prelude.Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Happy_Prelude.Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Happy_Prelude.Bool)
#define PLUS(n,m) (n Happy_GHC_Exts.+# m)
#define MINUS(n,m) (n Happy_GHC_Exts.-# m)
#define TIMES(n,m) (n Happy_GHC_Exts.*# m)
#define NEGATE(n) (Happy_GHC_Exts.negateInt# (n))

type Happy_Int = Happy_GHC_Exts.Int#
data Happy_IntList = HappyCons Happy_Int Happy_IntList

#define INVALID_TOK -1#
#define ERROR_TOK 0#
#define CATCH_TOK 1#

#if defined(HAPPY_COERCE)
#  define GET_ERROR_TOKEN(x)  (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# i) -> i })
#  define MK_ERROR_TOKEN(i)   (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# i))
#  define MK_TOKEN(x)         (happyInTok (x))
#else
#  define GET_ERROR_TOKEN(x)  (case x of { HappyErrorToken (Happy_GHC_Exts.I# i) -> i })
#  define MK_ERROR_TOKEN(i)   (HappyErrorToken (Happy_GHC_Exts.I# i))
#  define MK_TOKEN(x)         (HappyTerminal (x))
#endif

#if defined(HAPPY_DEBUG)
#  define DEBUG_TRACE(s)    (happyTrace (s)) Happy_Prelude.$
happyTrace string expr = Happy_System_IO_Unsafe.unsafePerformIO Happy_Prelude.$ do
    Happy_System_IO.hPutStr Happy_System_IO.stderr string
    Happy_Prelude.return expr
#else
#  define DEBUG_TRACE(s)    {- nothing -}
#endif

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept ERROR_TOK tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) =
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

happyDoAction i tk st =
  DEBUG_TRACE("state: " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# st) Happy_Prelude.++
              ",\ttoken: " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# i) Happy_Prelude.++
              ",\taction: ")
  case happyDecodeAction (happyNextAction i st) of
    HappyFail             -> DEBUG_TRACE("failing.\n")
                             happyFail i tk st
    HappyAccept           -> DEBUG_TRACE("accept.\n")
                             happyAccept i tk st
    HappyReduce rule      -> DEBUG_TRACE("reduce (rule " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# rule) Happy_Prelude.++ ")")
                             (happyReduceArr Happy_Data_Array.! (Happy_GHC_Exts.I# rule)) i tk st
    HappyShift  new_state -> DEBUG_TRACE("shift, enter state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# new_state) Happy_Prelude.++ "\n")
                             happyShift new_state i tk st

{-# INLINE happyNextAction #-}
happyNextAction i st = case happyIndexActionTable i st of
  Happy_Prelude.Just (Happy_GHC_Exts.I# act) -> act
  Happy_Prelude.Nothing                      -> happyIndexOffAddr happyDefActions st

{-# INLINE happyIndexActionTable #-}
happyIndexActionTable i st
  | GTE(i, 0#), GTE(off, 0#), EQ(happyIndexOffAddr happyCheck off, i)
  -- i >= 0:   Guard against INVALID_TOK (do the default action, which ultimately errors)
  -- off >= 0: Otherwise it's a default action
  -- equality check: Ensure that the entry in the compressed array is owned by st
  = Happy_Prelude.Just (Happy_GHC_Exts.I# (happyIndexOffAddr happyTable off))
  | Happy_Prelude.otherwise
  = Happy_Prelude.Nothing
  where
    off = PLUS(happyIndexOffAddr happyActOffsets st, i)

data HappyAction
  = HappyFail
  | HappyAccept
  | HappyReduce Happy_Int -- rule number
  | HappyShift Happy_Int  -- new state
  deriving Happy_Prelude.Show

{-# INLINE happyDecodeAction #-}
happyDecodeAction :: Happy_Int -> HappyAction
happyDecodeAction  0#                        = HappyFail
happyDecodeAction -1#                        = HappyAccept
happyDecodeAction action | LT(action, 0#)    = HappyReduce NEGATE(PLUS(action, 1#))
                         | Happy_Prelude.otherwise = HappyShift MINUS(action, 1#)

{-# INLINE happyIndexGotoTable #-}
happyIndexGotoTable nt st = happyIndexOffAddr happyTable off
  where
    off = PLUS(happyIndexOffAddr happyGotoOffsets st, nt)

{-# INLINE happyIndexOffAddr #-}
happyIndexOffAddr :: HappyAddr -> Happy_Int -> Happy_Int
happyIndexOffAddr (HappyA# arr) off =
#if __GLASGOW_HASKELL__ >= 901
  Happy_GHC_Exts.int32ToInt# -- qualified import because it doesn't exist on older GHC's
#endif
#ifdef WORDS_BIGENDIAN
  -- The CI of `alex` tests this code path
  (Happy_GHC_Exts.word32ToInt32# (Happy_GHC_Exts.wordToWord32# (Happy_GHC_Exts.byteSwap32# (Happy_GHC_Exts.word32ToWord# (Happy_GHC_Exts.int32ToWord32#
#endif
  (Happy_GHC_Exts.indexInt32OffAddr# arr off)
#ifdef WORDS_BIGENDIAN
  )))))
#endif

happyIndexRuleArr :: Happy_Int -> (# Happy_Int, Happy_Int #)
happyIndexRuleArr r = (# nt, len #)
  where
    !(Happy_GHC_Exts.I# n_starts) = happy_n_starts
    offs = TIMES(MINUS(r,n_starts),2#)
    nt = happyIndexOffAddr happyRuleArr offs
    len = happyIndexOffAddr happyRuleArr PLUS(offs,1#)

data HappyAddr = HappyA# Happy_GHC_Exts.Addr#

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state ERROR_TOK tk st sts stk@(x `HappyStk` _) =
     -- See "Error Fixup" below
     let i = GET_ERROR_TOKEN(x) in
     DEBUG_TRACE("shifting the error token")
     happyDoAction i tk new_state (HappyCons st sts) stk

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons st sts) (MK_TOKEN(tk) `HappyStk` stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 nt fn j tk st sts stk
     = happySeq fn (happyGoto nt j tk st (HappyCons st sts) (fn `HappyStk` stk))

happySpecReduce_1 nt fn j tk old_st sts@(HappyCons st _) (v1 `HappyStk` stk')
     = let r = fn v1 in
       happyTcHack old_st (happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk')))

happySpecReduce_2 nt fn j tk old_st
  (HappyCons _ sts@(HappyCons st _))
  (v1 `HappyStk` v2 `HappyStk` stk')
     = let r = fn v1 v2 in
       happyTcHack old_st (happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk')))

happySpecReduce_3 nt fn j tk old_st
  (HappyCons _ (HappyCons _ sts@(HappyCons st _)))
  (v1 `HappyStk` v2 `HappyStk` v3 `HappyStk` stk')
     = let r = fn v1 v2 v3 in
       happyTcHack old_st (happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk')))

happyReduce k nt fn j tk st sts stk
     = case happyDrop MINUS(k,(1# :: Happy_Int)) sts of
         sts1@(HappyCons st1 _) ->
                let r = fn stk in -- it doesn't hurt to always seq here...
                st `happyTcHack` happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons st sts) of
        sts1@(HappyCons st1 _) ->
          let drop_stk = happyDropStk k stk in
          j `happyTcHack` happyThen1 (fn stk tk)
                                     (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons st sts) of
        sts1@(HappyCons st1 _) ->
          let drop_stk = happyDropStk k stk
              off = happyIndexOffAddr happyGotoOffsets st1
              off_i = PLUS(off, nt)
              new_state = happyIndexOffAddr happyTable off_i
          in
            j `happyTcHack` happyThen1 (fn stk tk)
                                       (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l               = l
happyDrop n  (HappyCons _ t) = happyDrop MINUS(n,(1# :: Happy_Int)) t

happyDropStk 0# l                 = l
happyDropStk n  (x `HappyStk` xs) = happyDropStk MINUS(n,(1#::Happy_Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

happyGoto nt j tk st =
   DEBUG_TRACE(", goto state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# new_state) Happy_Prelude.++ "\n")
   happyDoAction j tk new_state
  where new_state = happyIndexGotoTable nt st

{- Note [Error recovery]
~~~~~~~~~~~~~~~~~~~~~~~~
When there is no applicable action for the current lookahead token `tk`,
happy enters error recovery mode. Depending on whether the grammar file
declares the two action form `%error { abort } { report }` for
    Resumptive Error Handling,
it works in one (not resumptive) or two phases (resumptive):

 1. Fixup mode:
    Try to see if there is an action for the error token ERROR_TOK. If there
    is, do *not* emit an error and pretend instead that an `error` token was
    inserted.
    When there is no ERROR_TOK action, report an error.

    In non-resumptive error handling, calling the single error handler
    (e.g. `happyError`) will throw an exception and abort the parser.
    However, in resumptive error handling we enter *error resumption mode*.

 2. Error resumption mode:
    After reporting the error (with `report`), happy will attempt to find
    a good state stack to resume parsing in.
    For each candidate stack, it discards input until one of the candidates
    resumes (i.e. shifts the current input).
    If no candidate resumes before the end of input, resumption failed and
    calls the `abort` function, to much the same effect as in non-resumptive
    error handling.

    Candidate stacks are declared by the grammar author using the special
    `catch` terminal and called "catch frames".
    This mechanism is described in detail in Note [happyResume].

The `catch` resumption mechanism (2) is what usually is associated with
`error` in `bison` or `menhir`. Since `error` is used for the Fixup mechanism
(1) above, we call the corresponding token `catch`.
Furthermore, in constrast to `bison`, our implementation of `catch`
non-deterministically considers multiple catch frames on the stack for
resumption (See Note [Multiple catch frames]).

Note [happyResume]
~~~~~~~~~~~~~~~~~~
`happyResume` implements the resumption mechanism from Note [Error recovery].
It is best understood by example. Consider

Exp :: { String }
Exp : '1'                { "1" }
    | catch              { "catch" }
    | Exp '+' Exp %shift { $1 Happy_Prelude.++ " + " Happy_Prelude.++ $3 } -- %shift: associate 1 + 1 + 1 to the right
    | '(' Exp ')'        { "(" Happy_Prelude.++ $2 Happy_Prelude.++ ")" }

The idea of the use of `catch` here is that upon encountering a parse error
during expression parsing, we can gracefully degrade using the `catch` rule,
still producing a partial syntax tree and keep on parsing to find further
syntax errors.

Let's trace the parser state for input 11+1, which will error out after shifting 1.
After shifting, we have the following item stack (growing downwards and omitting
transitive closure items):

  State 0: %start_parseExp -> . Exp
  State 5: Exp -> '1' .

(Stack as a list of state numbers: [5,0].)
As Note [Error recovery] describes, we will first try Fixup mode.
That fails because no production can shift the `error` token.
Next we try Error resumption mode. This works as follows:

  1. Pop off the item stack until we find an item that can shift the `catch`
     token. (Implemented in `pop_items`.)
       * State 5 cannot shift catch. Pop.
       * State 0 can shift catch, which would transition into
          State 4: Exp -> catch .
     So record the *stack* `[4,0]` after doing the shift transition.
     We call this a *catch frame*, where the top is a *catch state*,
     corresponding to an item in which we just shifted a `catch` token.
     There can be multiple such catch stacks, see Note [Multiple catch frames].

  2. Discard tokens from the input until the lookahead can be shifted in one
     of the catch stacks. (Implemented in `discard_input_until_exp` and
     `some_catch_state_shifts`.)
       * We cannot shift the current lookahead '1' in state 4, so we discard
       * We *can* shift the next lookahead '+' in state 4, but only after
         reducing, which pops State 4 and goes to State 3:
           State 3: %start_parseExp -> Exp .
                    Exp -> Exp . '+' Exp
         Here we can shift '+'.
     As you can see, to implement this machinery we need to simulate
     the operation of the LALR automaton, especially reduction
     (`happySimulateReduce`).

Note [Multiple catch frames]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For fewer spurious error messages, it can be beneficial to trace multiple catch
items. Consider

Exp : '1'
    | catch
    | Exp '+' Exp %shift
    | '(' Exp ')'

Let's trace the parser state for input (;+1, which will error out after shifting (.
After shifting, we have the following item stack (growing downwards):

  State 0: %start_parseExp -> . Exp
  State 6: Exp -> '(' . Exp ')'

Upon error, we want to find items in the stack which can shift a catch token.
Note that both State 0 and State 6 can shift a catch token, transitioning into
  State 4: Exp -> catch .
Hence we record the catch frames `[4,6,0]` and `[4,0]` for possible resumption.

Which catch frame do we pick for resumption?
Note that resuming catch frame `[4,0]` will parse as "catch+1", whereas
resuming the innermost frame `[4,6,0]` corresponds to parsing "(catch+1".
The latter would keep discarding input until the closing ')' is found.
So we will discard + and 1, leading to a spurious syntax error at the end of
input, aborting the parse and never producing a partial syntax tree. Bad!

It is far preferable to resume with catch frame `[4,0]`, where we can resume
successfully on input +, so that is what we do.

In general, we pick the catch frame for resumption that discards the least
amount of input for a successful shift, preferring the topmost such catch frame.
-}

-- happyFail :: Happy_Int -> Token -> Happy_Int -> _
-- This function triggers Note [Error recovery].
-- If the current token is ERROR_TOK, phase (1) has failed and we might try
-- phase (2).
happyFail ERROR_TOK = happyFixupFailed
happyFail i         = happyTryFixup i

-- Enter Error Fixup (see Note [Error recovery]):
-- generate an error token, save the old token and carry on.
-- When a `happyShift` accepts the error token, we will pop off the error token
-- to resume parsing with the current lookahead `i`.
happyTryFixup i tk action sts stk =
  DEBUG_TRACE("entering `error` fixup.\n")
  happyDoAction ERROR_TOK tk action sts (MK_ERROR_TOKEN(i) `HappyStk` stk)
  -- NB: `happyShift` will simply pop the error token and carry on with
  --     `tk`. Hence we don't change `tk` in the call here

-- See Note [Error recovery], phase (2).
-- Enter resumption mode after reporting the error by calling `happyResume`.
happyFixupFailed tk st sts (x `HappyStk` stk) =
  let i = GET_ERROR_TOKEN(x) in
  DEBUG_TRACE("`error` fixup failed.\n")
  let resume   = happyResume i tk st sts stk
      expected = happyExpectedTokens st sts in
  happyReport i tk expected resume

-- happyResume :: Happy_Int -> Token -> Happy_Int -> _
-- See Note [happyResume]
happyResume i tk st sts stk = pop_items [] st sts stk
  where
    !(Happy_GHC_Exts.I# n_starts) = happy_n_starts   -- this is to test whether we have a start token
    !(Happy_GHC_Exts.I# eof_i) = happy_n_terms Happy_Prelude.- 1   -- this is the token number of the EOF token
    happy_list_to_list :: Happy_IntList -> [Happy_Prelude.Int]
    happy_list_to_list (HappyCons st sts)
      | LT(st, n_starts)
      = [(Happy_GHC_Exts.I# st)]
      | Happy_Prelude.otherwise
      = (Happy_GHC_Exts.I# st) : happy_list_to_list sts

    -- See (1) of Note [happyResume]
    pop_items catch_frames st sts stk
      | LT(st, n_starts)
      = DEBUG_TRACE("reached start state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# st) Happy_Prelude.++ ", ")
        if Happy_Prelude.null catch_frames_new
          then DEBUG_TRACE("no resumption.\n")
               happyAbort
          else DEBUG_TRACE("now discard input, trying to anchor in states " Happy_Prelude.++ Happy_Prelude.show (Happy_Prelude.map (happy_list_to_list . Happy_Prelude.fst) (Happy_Prelude.reverse catch_frames_new)) Happy_Prelude.++ ".\n")
               discard_input_until_exp i tk (Happy_Prelude.reverse catch_frames_new)
      | (HappyCons st1 sts1) <- sts, _ `HappyStk` stk1 <- stk
      = pop_items catch_frames_new st1 sts1 stk1
      where
        !catch_frames_new
          | HappyShift new_state <- happyDecodeAction (happyNextAction CATCH_TOK st)
          , DEBUG_TRACE("can shift catch token in state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# st) Happy_Prelude.++ ", into state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# new_state) Happy_Prelude.++ "\n")
            Happy_Prelude.null (Happy_Prelude.filter (\(HappyCons _ (HappyCons h _),_) -> EQ(st,h)) catch_frames)
          = (HappyCons new_state (HappyCons st sts), MK_ERROR_TOKEN(i) `HappyStk` stk):catch_frames -- MK_ERROR_TOKEN(i) is just some dummy that should not be accessed by user code
          | Happy_Prelude.otherwise
          = DEBUG_TRACE("already shifted or can't shift catch in " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# st) Happy_Prelude.++ "\n")
            catch_frames

    -- See (2) of Note [happyResume]
    discard_input_until_exp i tk catch_frames
      | Happy_Prelude.Just (HappyCons st (HappyCons catch_st sts), catch_frame) <- some_catch_state_shifts i catch_frames
      = DEBUG_TRACE("found expected token in state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# st) Happy_Prelude.++ " after shifting from " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# catch_st) Happy_Prelude.++ ": " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# i) Happy_Prelude.++ "\n")
        happyDoAction i tk st (HappyCons catch_st sts) catch_frame
      | EQ(i,eof_i) -- is i EOF?
      = DEBUG_TRACE("reached EOF, cannot resume. abort parse :(\n")
        happyAbort
      | Happy_Prelude.otherwise
      = DEBUG_TRACE("discard token " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# i) Happy_Prelude.++ "\n")
        happyLex (\eof_tk -> discard_input_until_exp eof_i eof_tk catch_frames) -- eof
                 (\i tk   -> discard_input_until_exp i tk catch_frames)         -- not eof

    some_catch_state_shifts _ [] = DEBUG_TRACE("no catch state could shift.\n") Happy_Prelude.Nothing
    some_catch_state_shifts i catch_frames@(((HappyCons st sts),_):_) = try_head i st sts catch_frames
      where
        try_head i st sts catch_frames = -- PRECONDITION: head catch_frames = (HappyCons st sts)
          DEBUG_TRACE("trying token " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# i) Happy_Prelude.++ " in state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# st) Happy_Prelude.++ ": ")
          case happyDecodeAction (happyNextAction i st) of
            HappyFail     -> DEBUG_TRACE("fail.\n")   some_catch_state_shifts i (Happy_Prelude.tail catch_frames)
            HappyAccept   -> DEBUG_TRACE("accept.\n") Happy_Prelude.Just (Happy_Prelude.head catch_frames)
            HappyShift _  -> DEBUG_TRACE("shift.\n")  Happy_Prelude.Just (Happy_Prelude.head catch_frames)
            HappyReduce r -> case happySimulateReduce r st sts of
              (HappyCons st1 sts1) -> try_head i st1 sts1 catch_frames

happySimulateReduce r st sts =
  DEBUG_TRACE("simulate reduction of rule " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# r) Happy_Prelude.++ ", ")
  let (# nt, len #) = happyIndexRuleArr r in
  DEBUG_TRACE("nt " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# nt) Happy_Prelude.++ ", len: " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# len) Happy_Prelude.++ ", new_st ")
  let !(sts1@(HappyCons st1 _)) = happyDrop len (HappyCons st sts)
      new_st = happyIndexGotoTable nt st1 in
  DEBUG_TRACE(Happy_Prelude.show (Happy_GHC_Exts.I# new_st) Happy_Prelude.++ ".\n")
  (HappyCons new_st sts1)

happyTokenToString :: Happy_Prelude.Int -> Happy_Prelude.String
happyTokenToString i = happyTokenStrings Happy_Prelude.!! (i Happy_Prelude.- 2) -- 2: errorTok, catchTok

happyExpectedTokens :: Happy_Int -> Happy_IntList -> [Happy_Prelude.String]
-- Upon a parse error, we want to suggest tokens that are expected in that
-- situation. This function computes such tokens.
-- It works by examining the top of the state stack.
-- For every token number that does a shift transition, record that token number.
-- For every token number that does a reduce transition, simulate that reduction
-- on the state state stack and repeat.
-- The recorded token numbers are then formatted with 'happyTokenToString' and
-- returned.
happyExpectedTokens st sts =
  DEBUG_TRACE("constructing expected tokens.\n")
  Happy_Prelude.map happyTokenToString (search_shifts st sts [])
  where
    search_shifts st sts shifts = Happy_Prelude.foldr (add_action st sts) shifts (distinct_actions st)
    add_action st sts (Happy_GHC_Exts.I# i, Happy_GHC_Exts.I# act) shifts =
      DEBUG_TRACE("found action in state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# st) Happy_Prelude.++ ", input " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# i) Happy_Prelude.++ ", " Happy_Prelude.++ Happy_Prelude.show (happyDecodeAction act) Happy_Prelude.++ "\n")
      case happyDecodeAction act of
        HappyFail     -> shifts
        HappyAccept   -> shifts -- This would always be %eof or error... Not helpful
        HappyShift _  -> Happy_Prelude.insert (Happy_GHC_Exts.I# i) shifts
        HappyReduce r -> case happySimulateReduce r st sts of
          (HappyCons st1 sts1) -> search_shifts st1 sts1 shifts
    distinct_actions st
      -- The (token number, action) pairs of all actions in the given state
      = ((-1), (Happy_GHC_Exts.I# (happyIndexOffAddr happyDefActions st)))
      : [ (i, act) | i <- [begin_i..happy_n_terms], act <- get_act row_off i ]
      where
        row_off = happyIndexOffAddr happyActOffsets st
        begin_i = 2 -- +2: errorTok,catchTok
    get_act off (Happy_GHC_Exts.I# i) -- happyIndexActionTable with cached row offset
      | let off_i = PLUS(off,i)
      , GTE(off_i,0#)
      , EQ(happyIndexOffAddr happyCheck off_i,i)
      = [(Happy_GHC_Exts.I# (happyIndexOffAddr happyTable off_i))]
      | Happy_Prelude.otherwise
      = []

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Happy_Prelude.error "Internal Happy parser panic. This is not supposed to happen! Please open a bug report at https://github.com/haskell/happy/issues.\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions

happyTcHack :: Happy_Int -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}

-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Happy_GHC_Exts.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
