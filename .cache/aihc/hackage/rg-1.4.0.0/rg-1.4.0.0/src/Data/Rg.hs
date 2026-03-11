{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Data.Rg
  ( Rg(..)
  , RgCoreMethods(..)
  , rgCoreMethodsBE
  , RgText(..)
  , BE(..)
  , Range
  , newStartOfRangeFromList
  , newStartOfRangeFromVector
  , extractRange
  ) where

import           Data.Array
import           Data.Coerce
import qualified Data.HashMap.Strict  as HM
import           Data.Maybe
import           Data.Possibly
import qualified Data.Text            as T
import qualified Data.Vector          as V
import           Fmt


-- | Rg acts a bit like a Bounded Enum, but the size of the enumeration
-- can be dynamically determined from each value in the type (see 'sizeRg')
class Rg rg where
  -- | the number of values in the enumeration; sizeRg sz > 0
  sizeRg        :: rg -> Int
  sizeRg = _rcm_sizeRg rgCoreMethods

  -- | the nth item in the enumeration (first is 0)
  toRg          :: rg -> Int -> Maybe rg
  toRg   = _rcm_toRg   rgCoreMethods

  -- | place in the enumation (first is 0)
  fromRg        :: rg -> Int
  fromRg = _rcm_fromRg rgCoreMethods

  -- | an alternative way of specifying sizeRg, toRg and fromRg
  rgCoreMethods :: RgCoreMethods rg
  rgCoreMethods =
    RgCoreMethods
      { _rcm_sizeRg = sizeRg
      , _rcm_toRg   = toRg
      , _rcm_fromRg = fromRg
      }

  {-# MINIMAL sizeRg, toRg, fromRg | rgCoreMethods #-}

  -- | first item in the enumeration
  minRg         :: rg -> rg
  minRg     = fromMaybe oops . flip toRg 0
    where
      oops  = error "minRg: no minimum value in range"

  -- | last item in the enumeration
  maxRg         :: rg -> rg
  maxRg rg  = fromMaybe oops $ toRg rg n
    where
      n     = sizeRg rg - 1
      oops  = error "maxRg: no maximum value in range"

  -- | next item in the enumeration (Nothing if already last)
  succRg        :: rg -> Maybe rg
  succRg rg = toRg rg $ fromRg rg + 1

  -- | previous item in the enumeration (Nothing if already first)
  predRg        :: rg -> Maybe rg
  predRg rg = toRg rg $ fromRg rg - 1

  -- | list given items in the enumeration
  allListRg     :: rg -> [rg]
  allListRg rg = listRg rg [0..]

  -- | list given items in the enumeration, stopping as soon as an index is
  -- out of range
  listRg        :: rg -> [Int] -> [rg]
  listRg rg is = catMaybes $ takeWhile isJust [ toRg rg i | i<-is ]

  -- | list given items in the enumeration as a 'V.Vector'
  allVectorRg   :: rg -> [rg]
  allVectorRg rg = listRg rg [0..]

  -- | list the items in the enumeration as a 'V.Vector', stopping as soon as an
  -- index is out of range
  vectorRg    :: rg -> [Int] -> V.Vector rg
  vectorRg rg is = V.fromList $ listRg rg is



-------------------------------------------------------------------------------
-- RgCoreMethods
-------------------------------------------------------------------------------

-- | dynamically encapsulates the core 'Rg' methods
data RgCoreMethods rg =
  RgCoreMethods
    { _rcm_sizeRg :: rg -> Int
    , _rcm_toRg   :: rg -> Int -> Maybe rg
    , _rcm_fromRg :: rg -> Int
    }

-- | if you want to create an 'Rg' from a 'Bounded' 'Enum' you can bind
-- 'rgCoreMethods' to this function
rgCoreMethodsBE :: forall rg . (Bounded rg, Enum rg) => RgCoreMethods rg
rgCoreMethodsBE =
    RgCoreMethods
      { _rcm_sizeRg = coerce (sizeRg :: BE rg -> Int)
      , _rcm_toRg   = coerce (toRg   :: BE rg -> Int -> Maybe (BE rg))
      , _rcm_fromRg = coerce (fromRg :: BE rg -> Int)
      }


-------------------------------------------------------------------------------
-- class RgText
-------------------------------------------------------------------------------

-- | a class in which we can build things and parse them from 'T.Text'
class (Rg e, Buildable e, Eq e, Ord e, Show e) => RgText e where
  parseRgText :: e -> T.Text -> Possibly e
  parseRgText e txt = maybe (Left msg) Right $ HM.lookup txt $ hashmap_t e
    where
      msg = "parseRgText: enumeration not recognised: "++show txt


-------------------------------------------------------------------------------
-- newtype BE
-------------------------------------------------------------------------------

-- | a @newtype@ wrapper used for deriving 'Rg' instances from 'Bounded' 'Enum'
newtype BE a = BE { _BE :: a }
  deriving  (Eq,Ord,Bounded,Enum,Show)

instance (Bounded i,Enum i) => Rg (BE i) where
  sizeRg be = (1 +) $ fromEnum $ maxBound `asTypeOf` _BE be

  toRg be i = case 0 <= i && i < sizeRg be of
    True  -> Just $ BE $ toEnum i
    False -> Nothing

  fromRg = fromEnum . _BE


-------------------------------------------------------------------------------
-- data Range
-------------------------------------------------------------------------------

-- | used to generate 'Rg' values from lists of things
data Range a =
  Range
    { _rg_size  :: Int          -- ^ number of items in enumeration (derivable from Array)
    , _rg_elem  :: Int          -- ^ position in the enumeration of this element
    , _rg_array :: Array Int a  -- ^ all of the elements of the enumeration
    }
  deriving (Show)

instance Rg (Range a) where
  sizeRg      = _rg_size
  toRg   rg i = case 0 <= i && i < _rg_size rg of
    False -> Nothing
    True  -> Just rg { _rg_elem = i }
  fromRg      = _rg_elem

-- | generating a 'Range' from a list
newStartOfRangeFromList :: [a] -> Range a
newStartOfRangeFromList xs =
  Range
    { _rg_size  = sz
    , _rg_elem  = 0
    , _rg_array = listArray (0,sz-1) xs
    }
  where
    sz = length xs

-- | generating a 'Range' from a 'V.Vector'
newStartOfRangeFromVector :: V.Vector a -> Range a
newStartOfRangeFromVector v =
  Range
    { _rg_size  = sz
    , _rg_elem  = 0
    , _rg_array = listArray (0,sz-1) $ V.toList v
    }
  where
    sz = V.length v

-- | extracting the thing
extractRange :: Range a -> a
extractRange Range{..} = _rg_array ! _rg_elem


-------------------------------------------------------------------------------
-- hashmap_t
-------------------------------------------------------------------------------

hashmap_t :: RgText e => e -> HM.HashMap T.Text e
hashmap_t x = HM.fromList
    [ (fmt $ build c,c)
      | c <- allListRg x
      ]
