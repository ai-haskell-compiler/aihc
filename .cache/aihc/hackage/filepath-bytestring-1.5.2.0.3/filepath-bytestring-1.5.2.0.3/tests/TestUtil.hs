{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module TestUtil(
    (==>), QFilePath(..), QFilePathValidW(..), QFilePathValidP(..),
    toRawFilePath,
    equiv_0, equiv_1, equiv_2, equiv_3,
    module Test.QuickCheck,
    module Data.Maybe
    ) where

import Test.QuickCheck hiding ((==>))
import Data.Maybe
import Data.Word
import Data.Char
import Control.Monad
import qualified System.FilePath.Windows as W
import qualified System.FilePath.Posix as P
import System.FilePath.ByteString (RawFilePath, encodeFilePath)

infixr 0 ==>

(==>) :: Bool -> Bool -> Bool
a ==> b = not a || b

class ToRawFilePath t where
        toRawFilePath :: t -> RawFilePath

instance ToRawFilePath [Char] where
        toRawFilePath = encodeFilePath

newtype QFilePathValidW = QFilePathValidW FilePath deriving Show

instance ToRawFilePath QFilePathValidW where
    toRawFilePath (QFilePathValidW p) = toRawFilePath p

instance Arbitrary QFilePathValidW where
    arbitrary = fmap (QFilePathValidW . W.makeValid) arbitraryFilePath
    shrink (QFilePathValidW x) = shrinkValid QFilePathValidW W.makeValid x

newtype QFilePathValidP = QFilePathValidP FilePath deriving Show

instance ToRawFilePath QFilePathValidP where
    toRawFilePath (QFilePathValidP p) = toRawFilePath p

instance Arbitrary QFilePathValidP where
    arbitrary = fmap (QFilePathValidP . P.makeValid) arbitraryFilePath
    shrink (QFilePathValidP x) = shrinkValid QFilePathValidP P.makeValid x

newtype QFilePath = QFilePath FilePath deriving Show

instance ToRawFilePath QFilePath where
    toRawFilePath (QFilePath p) = toRawFilePath p

instance Arbitrary QFilePath where
    arbitrary = fmap QFilePath arbitraryFilePath
    shrink (QFilePath x) = shrinkValid QFilePath id x


-- | Generate an arbitrary FilePath use a few special (interesting) characters.
arbitraryFilePath :: Gen FilePath
arbitraryFilePath = sized $ \n -> do
    k <- choose (0,n)
    replicateM k $ elements "?./:\\a ;_"

-- | Shrink, but also apply a validity function. Try and make shorter, or use more
--   @a@ (since @a@ is pretty dull), but make sure you terminate even after valid.
shrinkValid :: (FilePath -> a) -> (FilePath -> FilePath) -> FilePath -> [a]
shrinkValid wrap valid o =
    [ wrap y
    | y <- map valid $ shrinkList (\x -> ['a' | x /= 'a']) o
    , length y < length o || (length y == length o && countA y > countA o)]
    where countA = length . filter (== 'a')

class EquivResult t1 t2 where
        equivresult :: t1 -> t2 -> Bool

instance Eq t => EquivResult t t where
        equivresult a b = a == b

instance (EquivResult a c, EquivResult b d) => EquivResult (a, b) (c, d) where
        equivresult (a, b) (c, d) = equivresult a c && equivresult b d

instance (EquivResult a b) => EquivResult (Maybe a) (Maybe b) where
        equivresult Nothing Nothing = True
        equivresult (Just a) (Just b) = equivresult a b
	equivresult _ _ = False

instance (EquivResult a b) => EquivResult [a] [b] where
        equivresult a b = and (map (uncurry equivresult) (zip a b))

instance EquivResult FilePath RawFilePath where
        equivresult a b = toRawFilePath a == b

instance EquivResult RawFilePath FilePath where
        equivresult a b = toRawFilePath b == a

equiv_0
        :: EquivResult a b
        => (Word8 -> a)
        -> (Char -> b)
        -> Property
equiv_0 our their = property $ \w ->
        our w `equivresult` their (chr (fromIntegral w))

equiv_1
        :: EquivResult a b
        => (RawFilePath -> a)
        -> (FilePath -> b)
        -> Property
equiv_1 our their = property $ \(QFilePath f) ->
        our (toRawFilePath f) `equivresult` their f

equiv_2
        :: EquivResult a b
        => (RawFilePath -> RawFilePath -> a)
        -> (FilePath -> FilePath -> b)
        -> Property
equiv_2 our their = property $ \(QFilePath a) (QFilePath b) ->
        our (toRawFilePath a) (toRawFilePath b) `equivresult` their a b

equiv_3
        :: EquivResult a b
        => ([RawFilePath] -> a)
        -> ([FilePath] -> b)
        -> Property
equiv_3 our their = property $ \l ->
        our (map (\(QFilePath f) -> toRawFilePath f) l)
                `equivresult`
        their (map (\(QFilePath f) -> f) l)

