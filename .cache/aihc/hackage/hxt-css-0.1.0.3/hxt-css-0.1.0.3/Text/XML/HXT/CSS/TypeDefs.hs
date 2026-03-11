{- |
Module      : Text.XML.HXT.CSS.TypeDefs

Stability   : provisional

Data types for the abstract syntax tree of CSS selectors. We (mostly)
follow the naming conventions of the CSS Level 3 specification document
(<http://www.w3.org/TR/css3-selectors/>). The type hierarchy tries to
strike a balance between correctness and complexity. As a result, it is
possible to construct values that correspond to invalid selectors.
For example,

@
'Negation' ('Negation' 'UniversalSelector')
@

is not valid according to the spec, as double negation is not allowed.
Note that 'Text.XML.HXT.CSS.Parser.parseCSS' never produces invalid
selectors.
-}

module Text.XML.HXT.CSS.TypeDefs where

import Data.Char

-- | The top-level selector type.
newtype SelectorsGroup = SelectorsGroup [Selector] -- ^ @E, F@
  deriving (Show, Eq)

data Selector
    = Selector SimpleSelectorSeq            -- ^ @E@
    | Descendant SimpleSelectorSeq Selector -- ^ @E F@
    | Child SimpleSelectorSeq Selector      -- ^ @E > F@
    | AdjSibling SimpleSelectorSeq Selector -- ^ @E + F@
    | FolSibling SimpleSelectorSeq Selector -- ^ @E ~ F@
  deriving (Show, Eq)

newtype SimpleSelectorSeq =
    SimpleSelectorSeq [SimpleSelector] -- ^ @tag#id.class:pseudo@
  deriving (Show, Eq)

data SimpleSelector
    = UniversalSelector            -- ^ @*@
    | TypeSelector String          -- ^ @tag@
    | IdSelector String            -- ^ @#id@
    | ClassSelector String         -- ^ @.class@
    | AttrSelector String AttrTest -- ^ @[..]@
    | Pseudo PseudoClass           -- ^ @:pseudo@
    | PseudoNth PseudoNthClass     -- ^ @:pseudo(2)@
    | Negation SimpleSelector      -- ^ @:not(..)@
  deriving (Show, Eq)

data AttrTest
    = AttrExists            -- ^ @[attr]@
    | AttrEq String         -- ^ @[attr=var]@
    | AttrContainsSp String -- ^ @[attr~=var]@
    | AttrBeginHy String    -- ^ @[attr|=var]@
    | AttrPrefix String     -- ^ @[attr^=var]@
    | AttrSuffix String     -- ^ @[attr$=var]@
    | AttrSubstr String     -- ^ @[attr*=var]@
  deriving (Show, Eq)

-- | Pseudo classes.
data PseudoClass
    = PseudoFirstChild  -- ^ @:first-child@
    | PseudoLastChild   -- ^ @:last-child@
    | PseudoOnlyChild   -- ^ @:only-child@
    | PseudoFirstOfType -- ^ @:first-of-type@
    | PseudoLastOfType  -- ^ @:last-of-type@
    | PseudoOnlyOfType  -- ^ @:only-of-type@
    | PseudoEmpty       -- ^ @:empty@
    | PseudoRoot        -- ^ @:root@
  deriving (Show, Eq)

-- | Pseudo classes that expect a argument of type 'Nth'.
data PseudoNthClass
    = PseudoNthChild      Nth -- ^ @:nth-child(..)@
    | PseudoNthLastChild  Nth -- ^ @:nth-last-child(..)@
    | PseudoNthOfType     Nth -- ^ @:nth-of-type(..)@
    | PseudoNthLastOfType Nth -- ^ @:nth-last-of-type(..)@
  deriving (Show, Eq)

-- | Type of the argument of the @:nth-child@ ('PseudoNthClass')
-- family of pseudo classes. @'Nth' a b@ matches with all integers that can
-- be written in the form @an+b@ for some nonnegative integer @n@.
data Nth
    = Nth Int Int -- ^ @an+b@
    | Odd         -- ^ @odd@
    | Even        -- ^ @even@
  deriving (Show, Eq)

-- | Find a 'PseudoClass' given its name (without the colon).
findPseudoClass :: String -> Maybe PseudoClass
findPseudoClass = flip lookup h . map toLower
  where
    h = [ ("first-child",      PseudoFirstChild)
        , ("last-child",       PseudoLastChild)
        , ("only-child",       PseudoOnlyChild)
        , ("first-of-type",    PseudoFirstOfType)
        , ("last-of-type",     PseudoLastOfType)
        , ("only-of-type",     PseudoOnlyOfType)
        , ("empty",            PseudoEmpty)
        , ("root",             PseudoRoot)
        ]

-- | Find a 'PseudoNthClass' given its name (without the colon).
findPseudoNthClass :: String -> Maybe (Nth -> PseudoNthClass)
findPseudoNthClass = flip lookup h . map toLower
  where
    h = [ ("nth-child",        PseudoNthChild)
        , ("nth-last-child",   PseudoNthLastChild)
        , ("nth-of-type",      PseudoNthOfType)
        , ("nth-last-of-type", PseudoNthLastOfType)
        ]

-- | Check whether an integer satisfies a \"Diophantine\" constraint
-- given in form of a value of type 'Nth'.
testNth :: Nth -> Int -> Bool
testNth (Nth 0 b) k = k == b
testNth (Nth a b) k = r == 0 && n >= 0
  where
    (n, r) = (k - b) `quotRem` a
testNth Odd k = odd k
testNth Even k = even k
