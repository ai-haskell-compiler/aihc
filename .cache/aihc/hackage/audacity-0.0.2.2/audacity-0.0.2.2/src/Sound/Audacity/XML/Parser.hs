module Sound.Audacity.XML.Parser where

import qualified Text.HTML.Tagchup.Tag as Tag
import qualified Text.XML.Basic.Attribute as Attr
import qualified Text.XML.Basic.Name.MixedCase as Name

import Text.Printf (printf)

import qualified Control.Monad.Trans.State as MS
import qualified Control.Monad.Trans.Maybe as MM
import qualified Control.Monad.Trans.Class as MT
import qualified Control.Monad.Exception.Synchronous as ME
import Control.Monad (MonadPlus, void, when, guard, mzero)
import Control.Applicative (many)

import qualified Data.List.HT as ListHT
import Data.String.HT (trim)
import Data.Char (isSpace)


type T = MS.StateT [Tag.T Name.T String] (MM.MaybeT (ME.Exceptional Message))
type Message = String


tag :: T (Tag.T Name.T String)
tag = MS.StateT $ MM.MaybeT . return . ListHT.viewL

fromMaybeGen :: (MonadPlus m) => Maybe a -> m a
fromMaybeGen = maybe mzero return

fromMaybe :: Maybe a -> T a
fromMaybe = MT.lift . MM.MaybeT . return

tagOpen :: Tag.Name Name.T -> T [Attr.T Name.T String]
tagOpen name = do
   x <- tag
   (foundName, attrs) <- fromMaybe $ Tag.maybeOpen x
   guard $ foundName == name
   return attrs

tagClose :: Tag.Name Name.T -> T ()
tagClose name = do
   x <- tag
   foundName <- fromMaybe $ Tag.maybeClose x
   guard $ foundName == name

lookupAttr :: String -> [Attr.T Name.T String] -> T String
lookupAttr name attrs =
   MT.lift $ MT.lift $
      ME.fromMaybe
         (printf "did not find attribute %s in%s"
            name (Attr.formatListBlankHead attrs "")) $
      Attr.lookupLit name attrs

lookupAttrRead :: (Read a) => String -> [Attr.T Name.T String] -> T a
lookupAttrRead name attrs = do
   str <- lookupAttr name attrs
   case reads str of
      [(x, "")] -> return x
      _ ->
         MT.lift $ MT.lift $ ME.throw $
            "could not parse attribute value " ++ str

lookupAttrBool :: String -> [Attr.T Name.T String] -> T Bool
lookupAttrBool name attrs = do
   str <- lookupAttr name attrs
   case str of
      "0" -> return False
      "1" -> return True
      _ -> MT.lift $ MT.lift $ ME.throw $ "not a bool value: " ++ str

skipSpace :: T ()
skipSpace = do
   x <- tag
   text <- fromMaybe $ Tag.maybeText x
   when (not $ all isSpace text) $ MT.lift $ MT.lift $
      ME.throw $ "expected spaces, but found: " ++ show (trim text)

skipSpaces :: T ()
skipSpaces = void $ many skipSpace
