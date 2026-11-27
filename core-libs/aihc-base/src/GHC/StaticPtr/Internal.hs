{-# LANGUAGE MagicHash #-}

-- | The representation of static pointers, and the function that the
-- compiler applies for each @static@ form.
--
-- The compiler changes @static e@ into @'staticForm' site e@. The site is a
-- string literal with four lines: the unit, the module, the line, and the
-- column of the @static@ form. The key of the static pointer is the
-- fingerprint of the site. Thus each process that runs the same program
-- gives the same key to the same @static@ form.
module GHC.StaticPtr.Internal
  ( StaticPtr (..),
    StaticKey,
    StaticPtrInfo (..),
    IsStatic (..),
    staticForm,
  )
where

import Data.Typeable (Typeable)
import GHC.Exts (Addr#, unpackCString#)
import GHC.Fingerprint (Fingerprint, fingerprintString)
import Prelude

-- | A reference to a value that has the same key in each process that runs
-- the same program.
data StaticPtr a = StaticPtr StaticKey StaticPtrInfo a

-- | The key of a static pointer. The key is the same in each process that
-- runs the same program.
type StaticKey = Fingerprint

-- | Where the @static@ form of a static pointer is in the program.
data StaticPtrInfo = StaticPtrInfo
  { -- | The unit of the module.
    spInfoUnitId :: String,
    -- | The name of the module.
    spInfoModuleName :: String,
    -- | The line and the column of the @static@ form.
    spInfoSrcLoc :: (Int, Int)
  }
  deriving (Show)

-- | The types that a static pointer can become. In GHC, the @static@ form
-- has the type @p a@ for each instance @p@ of this class.
class IsStatic p where
  fromStaticPtr :: (Typeable a) => StaticPtr a -> p a

instance IsStatic StaticPtr where
  fromStaticPtr = id

-- | The value of the @static@ form at the given site.
staticForm :: (IsStatic p, Typeable a) => Addr# -> a -> p a
staticForm site value =
  fromStaticPtr (StaticPtr (fingerprintString siteText) (siteInfo siteText) value)
  where
    siteText = unpackCString# site

-- | The location of a @static@ form, from the four lines of its site.
siteInfo :: String -> StaticPtrInfo
siteInfo siteText =
  case lines siteText of
    [unit, modu, line, column] -> StaticPtrInfo unit modu (number line, number column)
    _ -> StaticPtrInfo "" "" (0, 0)
  where
    number = foldl (\total digit -> total * 10 + (fromEnum digit - fromEnum '0')) 0
