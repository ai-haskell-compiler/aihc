{-# LANGUAGE ExistentialQuantification, FlexibleInstances, StandaloneDeriving, GeneralizedNewtypeDeriving, TypeSynonymInstances #-}
{-# OPTIONS_HADDOCK not-home #-}
module Text.StringTemplate.Classes
    (SElem(..), StringTemplateShows(..), ToSElem(..), SMap, STShow(..),
     StFirst(..), Stringable(..), stShowsToSE
    ) where
import qualified Data.Map as M
import Data.List
import Data.Monoid
import qualified Blaze.ByteString.Builder as BB
import qualified Blaze.ByteString.Builder.Char.Utf8 as BB
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
--import qualified Data.ByteString.Lazy.Builder as DBB
import qualified Data.Semigroup as SG
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Encoding as LT
import qualified Text.PrettyPrint.HughesPJ as PP

newtype StFirst a = StFirst { stGetFirst :: Maybe a }
        deriving (Eq, Ord, Read, Show)
instance SG.Semigroup (StFirst a) where
        r@(StFirst (Just _)) <> _ = r
        StFirst Nothing      <> r = r
instance Monoid (StFirst a) where
        mempty  = StFirst Nothing
        mappend = (SG.<>)

instance Functor StFirst where
    fmap f = StFirst . fmap f . stGetFirst

type SMap a = M.Map String (SElem a)

data SElem a = STR  String
             | BS   LB.ByteString
             | TXT  LT.Text
             | STSH STShow
             | SM (SMap a)
             | LI [SElem a]
             | SBLE a
             | SNAT a
             | SNull

-- | The ToSElem class should be instantiated for all types that can be
-- inserted as attributes into a StringTemplate.
class ToSElem a where
    toSElem :: Stringable b => a -> SElem b
    toSElemList :: Stringable b => [a] -> SElem b
    toSElemList = LI . map toSElem

-- | The StringTemplateShows class should be instantiated for all types that are
-- directly displayed in a StringTemplate, but take an optional format string. Each such type must have an appropriate ToSElem method defined as well.
class (Show a) => StringTemplateShows a where
    -- | Defaults to 'show'.
    stringTemplateShow :: a -> String
    stringTemplateShow = show
    -- | Defaults to  @ \ _ a -> stringTemplateShow a @
    stringTemplateFormattedShow :: String -> a -> String
    stringTemplateFormattedShow = flip $ const . stringTemplateShow

-- | This method should be used to create ToSElem instances for
-- types defining a custom formatted show function.
stShowsToSE :: (StringTemplateShows a, Stringable b) => a -> SElem b
stShowsToSE = STSH . STShow

data STShow = forall a.(StringTemplateShows a) => STShow a

-- | The Stringable class should be instantiated with care.
-- Generally, the provided instances should be enough for anything.
class Monoid a => Stringable a where
    stFromString :: String -> a
    stFromByteString :: LB.ByteString -> a
    stFromByteString = stFromText . LT.decodeUtf8
    stFromText :: LT.Text -> a
    stFromText = stFromString . LT.unpack
    stToString :: a -> String
    -- | Defaults to  @ mconcatMap m k = foldr (mappend . k) mempty m @
    mconcatMap :: [b] -> (b -> a) -> a
    mconcatMap m k = foldr (mappend . k) mempty m
    -- | Defaults to  @ (mconcat .) . intersperse @
    mintercalate :: a -> [a] -> a
    mintercalate = (mconcat .) . intersperse
    -- | Defaults to  @  mlabel x y = mconcat [x, stFromString "[", y, stFromString "]"] @
    mlabel :: a -> a -> a
    mlabel x y = mconcat [x, stFromString "[", y, stFromString "]"]

instance Stringable String where
    stFromString = id
    stToString = id

instance Stringable PP.Doc where
    stFromString = PP.text
    stToString = PP.render
    mconcatMap m k = PP.fcat . map k $ m
    mintercalate = (PP.fcat .) . PP.punctuate
    mlabel x y = x PP.$$ PP.nest 1 y

instance Stringable B.ByteString where
    stFromString = B.pack
    stFromByteString = B.concat . LB.toChunks
    stToString = B.unpack

instance Stringable LB.ByteString where
    stFromString = LB.pack
    stFromByteString = id
    stToString = LB.unpack

instance Stringable T.Text where
    stFromString = T.pack
    stFromByteString = T.decodeUtf8 . B.concat . LB.toChunks
    stFromText = LT.toStrict
    stToString = T.unpack

instance Stringable LT.Text where
    stFromString = LT.pack
    stFromByteString = LT.decodeUtf8
    stFromText = id
    stToString = LT.unpack

instance Stringable BB.Builder where
    stFromString = BB.fromString
    stFromByteString = BB.fromLazyByteString
    stToString = LB.unpack . BB.toLazyByteString

{-
instance Stringable LBB.Builder where
    stFromString = stringUtf8
    stFromByteString = LBB.lazyByteString
    stToString = LB.unpack . LBB.toLazyByteString
-}

instance Stringable TB.Builder where
    stFromString = TB.fromString
    stFromText = TB.fromLazyText
    stToString = LT.unpack . TB.toLazyText


--add dlist instance
instance Stringable (Endo String) where
    stFromString = Endo . (++)
    stToString = ($ []) . appEndo
