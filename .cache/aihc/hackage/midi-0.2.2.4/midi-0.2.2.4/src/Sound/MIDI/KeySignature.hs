module Sound.MIDI.KeySignature (
   T(..),
   Accidentals(..), Mode(..), keyName,

   cfMajor, gfMajor, dfMajor, afMajor, efMajor,
   bfMajor, fMajor, cMajor, gMajor, dMajor, aMajor,
   eMajor, bMajor, fsMajor, csMajor,
   afMinor, efMinor, bfMinor, fMinor, cMinor,
   gMinor, dMinor, aMinor, eMinor, bMinor, fsMinor,
   csMinor, gsMinor, dsMinor, asMinor,

   get, toBytes, ) where

import Sound.MIDI.Parser.Primitive (getByte, getEnum, makeEnum, )
import qualified Sound.MIDI.Parser.Class as Parser

import Control.Monad (liftM2, )

import Data.Ix (Ix, inRange, )
import Sound.MIDI.Utility
         (enumRandomR, boundedEnumRandom, chooseEnum, checkRange, )

import Test.QuickCheck (Arbitrary(arbitrary), )
import System.Random (Random(random, randomR), )

import Data.Int (Int8, )

import Prelude hiding (putStr, )



data T = Cons Mode Accidentals
   deriving (Eq, Ord)

instance Show T where
   showsPrec p (Cons mode accs) =
      if inRange (minBound, maxBound) accs
        then showString "KeySig." .
             showString (keyName mode accs) . shows mode
        else showParen (p>10) $
             showString "KeySig.Cons " . shows mode .
             showString " " . showsPrec 11 accs

instance Arbitrary T where
   arbitrary = liftM2 Cons arbitrary arbitrary

{- |
The Key Signature specifies a mode, either major or minor.
-}
data Mode = Major | Minor
            deriving (Show, Eq, Ord, Ix, Enum, Bounded)


instance Random Mode where
   random  = boundedEnumRandom
   randomR = enumRandomR

instance Arbitrary Mode where
   arbitrary = chooseEnum





keyName :: Mode -> Accidentals -> String

keyName Major (Accidentals (-7)) = "cf"
keyName Major (Accidentals (-6)) = "gf"
keyName Major (Accidentals (-5)) = "df"
keyName Major (Accidentals (-4)) = "af"
keyName Major (Accidentals (-3)) = "ef"
keyName Major (Accidentals (-2)) = "bf"
keyName Major (Accidentals (-1)) = "f"
keyName Major (Accidentals   0)  = "c"
keyName Major (Accidentals   1)  = "g"
keyName Major (Accidentals   2)  = "d"
keyName Major (Accidentals   3)  = "a"
keyName Major (Accidentals   4)  = "e"
keyName Major (Accidentals   5)  = "b"
keyName Major (Accidentals   6)  = "fs"
keyName Major (Accidentals   7)  = "cs"

keyName Minor (Accidentals (-7)) = "af"
keyName Minor (Accidentals (-6)) = "ef"
keyName Minor (Accidentals (-5)) = "bf"
keyName Minor (Accidentals (-4)) = "f"
keyName Minor (Accidentals (-3)) = "c"
keyName Minor (Accidentals (-2)) = "g"
keyName Minor (Accidentals (-1)) = "d"
keyName Minor (Accidentals   0)  = "a"
keyName Minor (Accidentals   1)  = "e"
keyName Minor (Accidentals   2)  = "b"
keyName Minor (Accidentals   3)  = "fs"
keyName Minor (Accidentals   4)  = "cs"
keyName Minor (Accidentals   5)  = "gs"
keyName Minor (Accidentals   6)  = "ds"
keyName Minor (Accidentals   7)  = "as"

keyName _ (Accidentals n) =
   if n<0
     then show (-n) ++ " flats"
     else show n ++ " sharps"


{- |
Accidentals as used in key signature.
-}
newtype Accidentals = Accidentals Int
           deriving (Show, Eq, Ord, Ix)

instance Bounded Accidentals where
   minBound = Accidentals (-7)
   maxBound = Accidentals 7

instance Enum Accidentals where
   fromEnum (Accidentals n) = fromIntegral n
   toEnum = checkRange "Accidentals" (Accidentals . fromIntegral)

instance Random Accidentals where
   random  = boundedEnumRandom
   randomR = enumRandomR

instance Arbitrary Accidentals where
   arbitrary = chooseEnum




major, minor :: Accidentals -> T
major = Cons Major
minor = Cons Minor

cfMajor, gfMajor, dfMajor, afMajor, efMajor,
  bfMajor, fMajor, cMajor, gMajor, dMajor, aMajor,
  eMajor, bMajor, fsMajor, csMajor :: T

afMinor, efMinor, bfMinor, fMinor, cMinor,
  gMinor, dMinor, aMinor, eMinor, bMinor, fsMinor,
  csMinor, gsMinor, dsMinor, asMinor :: T

cfMajor = major (Accidentals (-7))
gfMajor = major (Accidentals (-6))
dfMajor = major (Accidentals (-5))
afMajor = major (Accidentals (-4))
efMajor = major (Accidentals (-3))
bfMajor = major (Accidentals (-2))
fMajor  = major (Accidentals (-1))
cMajor  = major (Accidentals   0)
gMajor  = major (Accidentals   1)
dMajor  = major (Accidentals   2)
aMajor  = major (Accidentals   3)
eMajor  = major (Accidentals   4)
bMajor  = major (Accidentals   5)
fsMajor = major (Accidentals   6)
csMajor = major (Accidentals   7)

afMinor = minor (Accidentals (-7))
efMinor = minor (Accidentals (-6))
bfMinor = minor (Accidentals (-5))
fMinor  = minor (Accidentals (-4))
cMinor  = minor (Accidentals (-3))
gMinor  = minor (Accidentals (-2))
dMinor  = minor (Accidentals (-1))
aMinor  = minor (Accidentals   0)
eMinor  = minor (Accidentals   1)
bMinor  = minor (Accidentals   2)
fsMinor = minor (Accidentals   3)
csMinor = minor (Accidentals   4)
gsMinor = minor (Accidentals   5)
dsMinor = minor (Accidentals   6)
asMinor = minor (Accidentals   7)


get :: (Parser.C parser) => Parser.Fragile parser T
get = liftM2 (flip Cons) getAccidentals getEnum

getAccidentals :: (Parser.C parser) => Parser.Fragile parser Accidentals
getAccidentals =
   makeEnum . fromIntegral . (id :: Int8 -> Int8) . fromIntegral =<< getByte

toBytes :: T -> [Int]
toBytes (Cons mi sf) = [fromEnum sf, fromEnum mi]
