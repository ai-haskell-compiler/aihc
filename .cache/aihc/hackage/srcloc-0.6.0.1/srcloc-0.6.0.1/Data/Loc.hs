{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Safe #-}

-- |
-- Module      :  Data.Loc
-- Copyright   :  (c) Harvard University 2006-2011
--                (c) Geoffrey Mainland 2011-2015
-- License     :  BSD-style
-- Maintainer  :  Geoffrey Mainland <mainland@cs.drexel.edu>

module Data.Loc (
    Pos(..),
    posFile,
    posLine,
    posCol,
    posCoff,
    startPos,
    linePos,
    advancePos,
    displayPos,
    displaySPos,

    Loc(..),
    locStart,
    locEnd,

    (<-->),

    displayLoc,
    displaySLoc,

    SrcLoc(..),
    srclocOf,
    srcspan,

    IsLocation(..),
    noLoc,

    Located(..),

    Relocatable(..),

    L(..),
    unLoc
  ) where

import Data.Data (Data(..))
import Data.Typeable (Typeable(..))
import Data.List (foldl')
import Data.Monoid (Monoid(..))
#if MIN_VERSION_base(4,9,0) && !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup(..))
#endif

-- | Position type.
data Pos = -- | Source file name, line, column, and character offset.
           --
           -- Line numbering starts at 1, column offset starts at 1, and
           -- character offset starts at 0.
           Pos !FilePath
               {-# UNPACK #-} !Int
               {-# UNPACK #-} !Int
               {-# UNPACK #-} !Int
  deriving (Eq, Read, Show, Data, Typeable)

instance Ord Pos where
    compare (Pos f1 l1 c1 _) (Pos f2 l2 c2 _) =
        compare (f1, l1, c1) (f2, l2, c2)

-- | Position file.
posFile :: Pos -> FilePath
posFile (Pos f _ _ _) = f

-- | Position line.
posLine :: Pos -> Int
posLine (Pos _ l _ _) = l

-- | Position column.
posCol :: Pos -> Int
posCol (Pos _ _ c _) = c

-- | Position character offset.
posCoff :: Pos -> Int
posCoff (Pos _ _ _ coff) = coff

-- | Starting position for given file.
startPos :: FilePath -> Pos
startPos f = Pos f startLine startCol startCoff

startLine :: Int
startLine = 1

startCol :: Int
startCol = 1

startCoff :: Int
startCoff = 0

-- | Position corresponding to given file and line.
--
-- Note that the associated character offset is set to 0.
linePos :: FilePath -> Int -> Pos
linePos f l = Pos f l startCol startCoff

-- | Advance a position by a single character. Newlines increment the line
-- number, tabs increase the position column following a tab stop width of 8,
-- and all other characters increase the position column by one. All characters,
-- including newlines and tabs, increase the character offset by 1.
--
-- Note that 'advancePos' assumes UNIX-style newlines.
advancePos :: Pos -> Char -> Pos
advancePos (Pos f l _ coff) '\n' = Pos f (l+1) startCol     (coff + 1)
advancePos (Pos f l c coff) '\t' = Pos f l     nextTabStop  (coff + 1)
  where nextTabStop = ((c+7) `div` 8) * 8 + 1
advancePos (Pos f l c coff) _    = Pos f l     (c + 1)      (coff + 1)

-- | Location type, consisting of a beginning position and an end position.
data Loc =  NoLoc
         |  -- | Beginning and end positions
            Loc  {-# UNPACK #-} !Pos
                 {-# UNPACK #-} !Pos
  deriving (Eq, Ord, Read, Show, Data, Typeable)

-- | Starting position of the location.
locStart :: Loc -> Loc
locStart  NoLoc      = NoLoc
locStart  (Loc p _)  = Loc p p

-- | Ending position of the location.
locEnd :: Loc -> Loc
locEnd  NoLoc      = NoLoc
locEnd  (Loc _ p)  = Loc p p

-- | Append two locations.
locAppend :: Loc -> Loc -> Loc
locAppend NoLoc       l           = l
locAppend l           NoLoc       = l
locAppend (Loc b1 e1) (Loc b2 e2) = Loc (min b1 b2) (max e1 e2)

#if MIN_VERSION_base(4,9,0)
instance Semigroup Loc where
    (<>) = locAppend
#endif

instance Monoid Loc where
    mempty = NoLoc
#if !(MIN_VERSION_base(4,11,0))
    mappend = locAppend
#endif

-- | Merge the locations of two 'Located' values.
(<-->) :: (Located a, Located b) => a -> b -> Loc
x <--> y = locOf x `mappend` locOf y

infixl 6 <-->

-- | Source location type. Source location are all equal, which allows AST nodes
-- to be compared modulo location information.
newtype SrcLoc = SrcLoc Loc
  deriving (Data, Typeable)

instance Monoid SrcLoc where
    mempty = SrcLoc mempty
#if !(MIN_VERSION_base(4,11,0))
    mappend (SrcLoc l1) (SrcLoc l2) = SrcLoc (l1 `mappend` l2)
#endif

#if MIN_VERSION_base(4,9,0)
instance Semigroup SrcLoc where
  SrcLoc l1 <> SrcLoc l2 = SrcLoc (l1 <> l2)
#endif

instance Eq SrcLoc where
    _ == _ = True

instance Ord SrcLoc where
    compare _ _ = EQ

instance Show SrcLoc where
    showsPrec _ _ = showString "noLoc"

instance Read SrcLoc where
    readsPrec p s =
        readParen False
          (\s -> [(SrcLoc NoLoc, s') |
                  ("noLoc", s') <- lex s])
          s
        ++
        readParen (p > app_prec)
          (\s -> [(SrcLoc l, s'') |
                  ("SrcLoc", s') <- lex s,
                  (l, s'') <- readsPrec (app_prec+1) s'])
          s
      where
        app_prec = 10

-- | The 'SrcLoc' of a 'Located' value.
srclocOf :: Located a => a -> SrcLoc
srclocOf = fromLoc . locOf

-- | A 'SrcLoc' with (minimal) span that includes two 'Located' values.
srcspan :: (Located a, Located b) => a -> b -> SrcLoc
x `srcspan` y = SrcLoc (locOf x `mappend` locOf y)

infixl 6 `srcspan`

-- | Locations
class IsLocation a where
    fromLoc :: Loc -> a
    fromPos :: Pos -> a
    fromPos p = fromLoc (Loc p p)

instance IsLocation Loc where
    fromLoc = id

instance IsLocation SrcLoc where
    fromLoc = SrcLoc

-- | No location.
noLoc :: IsLocation a => a
noLoc = fromLoc NoLoc

-- | Located values have a location.
class Located a where
    locOf :: a -> Loc

    locOfList :: [a] -> Loc
    locOfList xs = mconcat (map locOf xs)

instance Located a => Located [a] where
    locOf = locOfList

instance Located a => Located (Maybe a) where
    locOf Nothing   = NoLoc
    locOf (Just x)  = locOf x

instance Located Pos where
    locOf p = Loc p p

instance Located Loc where
    locOf = id

instance Located SrcLoc where
    locOf (SrcLoc loc) = loc

-- | Values that can be relocated
class Relocatable a where
    reloc :: Loc -> a -> a

-- | A value of type @L a@ is a value of type @a@ with an associated 'Loc', but
-- this location is ignored when performing comparisons.
data L a = L Loc a
  deriving (Functor, Data, Typeable)

unLoc :: L a -> a
unLoc (L _ a) = a

instance Eq x => Eq (L x) where
    (L _ x) == (L _ y) = x == y

instance Ord x => Ord (L x) where
    compare (L _ x) (L _ y) = compare x y

instance Show x => Show (L x) where
    showsPrec d (L _ x) = showsPrec d x

instance Located (L a) where
    locOf (L loc _) = loc

instance Relocatable (L a) where
    reloc loc (L _ x) = L loc x

-- | Format a position in a human-readable way, returning an ordinary
-- 'String'.
displayPos :: Pos -> String
displayPos p = displayLoc (Loc p p)

-- | Format a position in a human-readable way.
displaySPos :: Pos -> ShowS
displaySPos p = displaySLoc (Loc p p)

-- | Format a location in a human-readable way, returning an ordinary
-- 'String'.
displayLoc :: Loc -> String
displayLoc loc = displaySLoc loc ""

-- | Format a location in a human-readable way.
displaySLoc :: Loc -> ShowS
displaySLoc NoLoc =
    showString "<no location>"

displaySLoc (Loc p1@(Pos src line1 col1 _) (Pos _ line2 col2 _))
  | (line1, col1) == (line2, col2) =
      -- filename.txt:2:3
      showString src . colon . shows line1 . colon . shows col1
  | line1 == line2 =
      -- filename.txt:2:3-5
      showString src .
      colon . shows line1 .
      colon . shows col1 .
      dash  . shows col2
  | otherwise =
      -- filename.txt:2:3-4:5
      showString src .
      colon . shows line1 .
      colon . shows col1 .
      dash  . shows line2 .
      colon . shows col2
  where
    colon = (':' :)
    dash  = ('-' :)
