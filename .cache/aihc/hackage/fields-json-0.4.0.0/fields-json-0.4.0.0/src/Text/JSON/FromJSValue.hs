-----------------------------------------------------------------------------
-- |
-- Module      :  Text.JSON.ToJSON
-- Copyright   :  (c) Scrive 2011
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  mariusz@scrive.com
-- Stability   :  development
-- Portability :  portable
--
-- Interface for extracting data from JSValue.
--


module Text.JSON.FromJSValue
 (
   -- * Basic Parsing
   FromJSValue(..)
 , FromJSValueWithUpdate(..)
 , MatchWithJSValue(..)
 -- * Data Extraction
 , jsValueField
 , fromJSValueField
 , fromJSValueFieldBase64
 , fromJSValueFieldCustom
 , fromJSValueCustomMany
 , fromJSValueCustomList
 , fromJSValueManyWithUpdate
 -- * Running
 , withJSValue
)
where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Reader
import Data.Int
import Data.List
import Data.Word
import Text.JSON
import qualified Data.ByteString.Base64 as BASE64
import qualified Data.ByteString.UTF8 as BS

-- | Structures that can be 'parsed' from JSON. Instances must declare
-- either 'fromJSValue' (parse directly from 'JSValue') or
-- 'fromJSValueM' (uses 'MonadReader').
--
-- Example implementation:
--
-- > data D = D String Int
-- >
-- > instance FromJSValue D where
-- >   fromJSValue = do
-- >     s <- fromJSValueField "string_key"
-- >     i <- fromJSValueField "int_key"
-- >     return (D <$> s <*> i)
--
-- Note that we make use of 'MonadReader' instance for "(->)" and of
-- 'Control.Applicative' programming style with 'Control.Applicative.<$>' and 'Control.Applicative.<*>'.
--
-- Note: 'fromJSValueM' is deprecated, in future 'fromJSValue' will be
-- generalized to work in any 'Control.Monad.Reader.MonadReader' 'Text.JSON.JSValue'.
class FromJSValue a where
    fromJSValue :: JSValue -> Maybe a
    fromJSValue j = runIdentity $ withJSValue j $ liftM fromJSValueM ask
    fromJSValueM :: (MonadReader JSValue m) => m (Maybe a)
    fromJSValueM = liftM fromJSValue ask


-- | Structures that can be 'parsed' from JSON, fields absent in the
-- JSON will be filled in using (optional) original structure.
--
-- By convention JSON null should be treated as a request to reset
-- structure element to default value.
class FromJSValueWithUpdate a where
    fromJSValueWithUpdate :: Maybe a -> JSValue -> Maybe a
    fromJSValueWithUpdate ma j = runIdentity $ withJSValue j $ liftM (fromJSValueWithUpdateM ma) ask
    fromJSValueWithUpdateM :: (MonadReader JSValue m) =>  Maybe a  -> m (Maybe a)
    fromJSValueWithUpdateM ma = liftM (fromJSValueWithUpdate ma) ask

-- | Structures that can be matched with JSValue
class MatchWithJSValue a where
    matchesWithJSValue :: a -> JSValue -> Bool
    matchesWithJSValue a j = runIdentity $ withJSValue j $ liftM (matchesWithJSValueM a) ask
    matchesWithJSValueM :: (MonadReader JSValue m) =>  a  -> m Bool
    matchesWithJSValueM a = liftM (matchesWithJSValue a) ask


-- ---------------------------------------------------------------------------
-- Instances for basic datatypes
--

instance FromJSValue JSValue where
    fromJSValue = Just

instance {-# OVERLAPPING #-} FromJSValue String where
    fromJSValue (JSString string) = Just $ fromJSString string
    fromJSValue _ = Nothing

instance FromJSValue BS.ByteString where
    fromJSValue s = fmap BS.fromString (fromJSValue s)

instance FromJSValue Integer where
    fromJSValue (JSRational _ r) = Just $ round r
    fromJSValue _ = Nothing

instance FromJSValue Int where
    fromJSValue j = fromInteger <$> fromJSValue j

instance FromJSValue Int16 where
    fromJSValue j = fromInteger <$> fromJSValue j

instance FromJSValue Int32 where
    fromJSValue j = fromInteger <$> fromJSValue j

instance FromJSValue Int64 where
    fromJSValue j = fromInteger <$> fromJSValue j

instance FromJSValue Word where
    fromJSValue j = fromInteger <$> fromJSValue j

instance FromJSValue Word16 where
    fromJSValue j = fromInteger <$> fromJSValue j

instance FromJSValue Word32 where
    fromJSValue j = fromInteger <$> fromJSValue j

instance FromJSValue Word64 where
    fromJSValue j = fromInteger <$> fromJSValue j

instance FromJSValue Bool where
    fromJSValue (JSBool v) = Just $ v
    fromJSValue _ = Nothing

instance FromJSValue Double where
    fromJSValue (JSRational _ r) = Just $ fromRational r
    fromJSValue _ = Nothing

instance FromJSValue Float where
    fromJSValue (JSRational _ r) = Just $ fromRational r
    fromJSValue _ = Nothing

instance FromJSValue a => FromJSValue [a] where
    fromJSValue (JSArray list) = mapM fromJSValue list
    fromJSValue _ = Nothing

-- | Parsing any Maybe always returns Just
instance (FromJSValue a) => FromJSValue (Maybe a) where
    fromJSValue = Just . fromJSValue

instance (FromJSValue a, FromJSValue b) => FromJSValue (a,b) where
    fromJSValue (JSArray [a,b]) = do
      a' <- fromJSValue a
      b' <- fromJSValue b
      return (a',b')
    fromJSValue _ = Nothing

instance (FromJSValue a, FromJSValue b, FromJSValue c) => FromJSValue (a,b,c) where
    fromJSValue (JSArray [a,b,c]) = do
      a' <- fromJSValue a
      b' <- fromJSValue b
      c' <- fromJSValue c
      return (a',b',c')
    fromJSValue _ = Nothing

instance (FromJSValue a, FromJSValue b, FromJSValue c, FromJSValue d) => FromJSValue (a,b,c,d) where
    fromJSValue (JSArray [a,b,c,d]) = do
      a' <- fromJSValue a
      b' <- fromJSValue b
      c' <- fromJSValue c
      d' <- fromJSValue d
      return (a',b',c',d')
    fromJSValue _ = Nothing

instance (FromJSValue a, FromJSValue b, FromJSValue c,
          FromJSValue d, FromJSValue e) => FromJSValue (a,b,c,d,e) where
    fromJSValue (JSArray [a,b,c,d,e]) = do
      a' <- fromJSValue a
      b' <- fromJSValue b
      c' <- fromJSValue c
      d' <- fromJSValue d
      e' <- fromJSValue e
      return (a',b',c',d',e')
    fromJSValue _ = Nothing

instance (FromJSValue a, FromJSValue b, FromJSValue c,
          FromJSValue d, FromJSValue e, FromJSValue f) => FromJSValue (a,b,c,d,e,f) where
    fromJSValue (JSArray [a,b,c,d,e,f]) = do
      a' <- fromJSValue a
      b' <- fromJSValue b
      c' <- fromJSValue c
      d' <- fromJSValue d
      e' <- fromJSValue e
      f' <- fromJSValue f
      return (a',b',c',d',e',f')
    fromJSValue _ = Nothing

-- ----------------------------------------------------------------


-- | Reading the value that is on some field. Returns 'Nothing' if
-- JSON is not an object or field is present but cannot be parsed,
-- 'Just Nothing' if absent, and 'Just (Just a)' otherwise
jsValueField ::  (MonadReader JSValue m, FromJSValue a) => String -> m (Maybe (Maybe a))
jsValueField s = ask >>= fromObject
    where
      fromObject (JSObject object) =
        case lookup s (fromJSObject object) of
          Nothing -> return (Just Nothing)
          Just a  -> return (Just `fmap` fromJSValue a)
      fromObject _ = return Nothing

-- | Reading the value that is on a field. Semantics are a bit
-- involved, example GHCi session should clarify:
--
-- > Prelude> :set -XNoMonomorphismRestriction
-- > Prelude> let x = withJSValue (JSObject (toJSObject [("key",JSString $ toJSString "value")]))
-- > Prelude> x (fromJSValueField "key") :: IO (Maybe Int)
-- > Nothing
-- > Prelude> x (fromJSValueField "key") :: IO (Maybe (Maybe Int))
-- > Just Nothing
-- > Prelude> x (fromJSValueField "key") :: IO (Maybe (Maybe (Maybe Int)))
-- > Just (Just Nothing)
-- > Prelude> x (fromJSValueField "key") :: IO (Maybe String)
-- > Just "value"
-- > Prelude> x (fromJSValueField "key") :: IO (Maybe (Maybe String))
-- > Just (Just "value")
-- > Prelude> x (fromJSValueField "key") :: IO (Maybe (Maybe (Maybe String)))
-- > Just (Just (Just "value"))
-- > Prelude> let x = withJSValue (JSArray [])
-- > Prelude> x (fromJSValueField "key") :: IO (Maybe String)
-- > Nothing
-- > Prelude> x (fromJSValueField "key") :: IO (Maybe (Maybe String))
-- > Nothing
-- > Prelude> x (fromJSValueField "key") :: IO (Maybe (Maybe (Maybe String)))
-- > Nothing
--
fromJSValueField :: (MonadReader JSValue m, FromJSValue a) => String -> m (Maybe a)
fromJSValueField s = liftM fromObject ask
    where
      fromObject (JSObject object) = join (fmap fromJSValue (lookup s $ fromJSObject object))
      fromObject _ = Nothing

-- | Version of 'fromJSValueField' for Base64 encoded data to avoid
-- memory leak.
fromJSValueFieldBase64 :: (MonadReader JSValue m) => String -> m (Maybe BS.ByteString)
fromJSValueFieldBase64 s =  liftM dc (fromJSValueField s)
    where dc s' = case fmap BASE64.decode s' of
                    Just (Right r) -> Just r
                    _ -> Nothing

-- | Generalization of 'fromJSValueField'. Does not use 'FromJSValue'
-- instances.
fromJSValueFieldCustom :: (MonadReader JSValue m) => String -> m (Maybe a) -> m (Maybe a)
fromJSValueFieldCustom s digger = do
    mobj <- fromJSValueField s
    case mobj of
      Just obj -> local (const obj) (digger)
      Nothing -> return Nothing

-- | Runs parser on each element of underlaying json. Returns Just iff
-- JSON is array.
fromJSValueCustomMany :: (MonadReader JSValue m) => m (Maybe a) -> m (Maybe [a])
fromJSValueCustomMany digger = fromJSValueCustomList (repeat digger)

-- | Generalization of 'fromJSValueCustomMany', where each element of
-- array can have different parser.
fromJSValueCustomList :: (MonadReader JSValue m) => [m (Maybe a)] -> m (Maybe [a])
fromJSValueCustomList diggers = do
    mlist <- fromJSValueM
    case mlist of
         Nothing -> return Nothing
         Just list -> runDiggers list diggers
    where
         runDiggers (j:js) (d:ds) = do
             mres <- local (const j) d
             case mres of
                 Just res -> do
                     mress <- runDiggers js ds
                     case mress of
                         Just ress -> return $ Just (res:ress)
                         _ -> return Nothing
                 _ -> return Nothing
         runDiggers _ _ = return $ Just []


-- | Runs parser on each element of underlying json. Returns 'Just' iff
-- JSON is an array.
--
-- Note: This method has quadratic complexity. It is better to write
-- less general matching algorithms that use Maps.
fromJSValueManyWithUpdate :: (MonadReader JSValue m, FromJSValueWithUpdate a, MatchWithJSValue a) => [a] -> m (Maybe [a])
fromJSValueManyWithUpdate values = do
    mjs <- fromJSValueM
    case mjs of
         Nothing -> return Nothing
         Just js -> runFromJSValueAndUpdate js
    where
         runFromJSValueAndUpdate (j:js) =  do
             mres <- local (const j) (fromJSValueWithUpdateM (find (\v -> matchesWithJSValue v j) values))
             case mres of
                 Just res -> do
                     mress <- runFromJSValueAndUpdate js
                     case mress of
                         Just ress -> return $ Just (res:ress)
                         _ -> return Nothing
                 _ -> return Nothing
         runFromJSValueAndUpdate [] = return $ Just []

-- | Simple runner. Example:
--
-- > let (v :: MyStruct) = runIdentity $ withJSValue js (fromJSValueM)
--
-- or inline:
--
-- > let z = runIdentity $ withJSValue js $ do
-- >             a <- fromJSValueField "a"
-- >             b <- fromJSValueField "b"
-- >             c <- fromJSValueField "c"
-- >             return ((,,) <$> a <*> b <*> c)
--
-- or using the monad transformer:
--
-- > z <- withJSValue js $ do
-- >             a <- fromJSValueField "a"
-- >             b <- fromJSValueField "b"
-- >             c <- fromJSValueField "c"
-- >             return ((,,) <$> a <*> b <*> c)
--
withJSValue :: (Monad m) => JSValue -> ReaderT JSValue m a -> m a
withJSValue j a = runReaderT a j
