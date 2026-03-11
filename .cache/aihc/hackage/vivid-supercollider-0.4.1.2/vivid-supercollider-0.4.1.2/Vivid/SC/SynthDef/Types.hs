module Vivid.SC.SynthDef.Types (
     BinaryOp(..)
   , CalculationRate(..)
   , UnaryOp(..)
   ) where

-- The order of these is important for the enum instance:
-- | The rate that a UGen computes at
data CalculationRate
   = IR -- ^ constant value
   | KR -- ^ control rate
   | AR -- ^ audio rate
   | DR -- ^ demand rate
 deriving (Show, Read, Eq, Enum, Ord)

-- instance Hashable CalculationRate



-- | Binary signal operations. For the simple ones (like 'Add', 'Mul', etc.),
--   there are functions (like 'Vivid.UGens.~+', 'Vivid.UGens.~*', etc.)
--   that wrap them up so you
--   don't have to make a ugen for them yourself.
-- 
--   In the future these may not be exported -- we'll just have functions for
--   all of them.
data BinaryOp
   = Add | Sub | Mul 
   | IDiv -- ^ Integer division
   | FDiv -- ^ Float division
   | Mod | Eq | Ne | Lt | Gt | Le | Ge
   | Min | Max | BitAnd | BitOr | BitXor | Lcm | Gcd | Round | RoundUp | Trunc
   | Atan2 | Hypot | Hypotx | Pow | ShiftLeft | ShiftRight | UnsignedShift | Fill
   -- comments come from SC source:
   | Ring1 -- ^ a * (b + 1) == a * b + a
   | Ring2 -- ^ a * b + a + b
   | Ring3 -- ^ a * a * b
   | Ring4 -- ^ a * a * b - a * b * b
   | DifSqr -- ^ a * a - b * b
   | SumSqr -- ^ a * a + b * b
   | SqrSum -- ^ (a + b) ^ 2
   | SqrDif -- ^ (a - b) ^ 2
   | AbsDif -- ^ abs(a - b)
   | Thresh
   | AMClip
   | ScaleNeg
   | Clip2 -- Like 'Vivid.UGens.Maths.clip' but the min value is always the negative of the max value
   | Excess
   | Fold2 | Wrap2 | FirstArg
   | RandRange | ExpRandRange | NumBinarySelectors
 deriving (Show, Eq, Ord, Enum)

-- instance Hashable BinaryOp

-- These seem to only be in the SuperCollider source:
--   sc/server/plugins/(Bi|U)naryOpUgens.cpp

-- | Unary signal operations. Many of these have functions so you don't need to
--   use this internal representation (e.g. 'Neg' has 'neg', etc).
-- 
--   This type might not be exposed in the future.
data UnaryOp
   = Neg | Not | IsNil | NotNil
   | BitNot -- ^ There's a bug in some SC versions where .bitNot isn't implemented
            --   correctly. Vivid backfills it with a fix, so you can use BitNot with
            --   any SC version
   | Abs | AsFloat | AsInt | Ciel | Floor
   | Frac | Sign | Squared | Cubed | Sqrt | Exp | Recip | MIDICPS | CPSMIDI
   | MIDIRatio | RatioMIDI
    -- dbamp and ampdb: converts betw db and "amp" so that e.g. -inf db == 0 amp
    -- dunno how the other scaling works
   | DbAmp | AmpDb
   | OctCPS | CPSOct | Log | Log2 | Log10
   | Sin | Cos | Tan | ArcSin | ArcCos | ArcTan | SinH | CosH | TanH
   | Rand | Rand2 | LinRand | BiLinRand | Sum3Rand
   | Distort | SoftClip | Coin | DigitValue
   | Silence | Thru | RectWindow | HanWindow | WelchWindow | TriWindow | Ramp
   | SCurve | NumUnarySelectors
 deriving (Show, Eq, Ord, Enum)

-- instance Hashable UnaryOp


