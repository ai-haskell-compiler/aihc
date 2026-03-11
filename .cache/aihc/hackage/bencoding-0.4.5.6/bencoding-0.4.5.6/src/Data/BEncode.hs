-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  stable
--   Portability :  portable
--
--   This module provides convinient and fast way to serialize,
--   deserealize and construct/destructure Bencoded values with
--   optional fields.
--
--   It supports four different types of values:
--
--     * byte strings — represented as 'ByteString';
--
--     * integers     — represented as 'Integer';
--
--     * lists        - represented as ordinary lists;
--
--     * dictionaries — represented as 'BDictMap';
--
--    To serialize any other types we need to make conversion.  To
--    make conversion more convenient there is type class for it:
--    'BEncode'. Any textual strings are considered as UTF8 encoded
--    'Text'.
--
--    The complete Augmented BNF syntax for bencoding format is:
--
--
--    > <BE>    ::= <DICT> | <LIST> | <INT> | <STR>
--    >
--    > <DICT>  ::= "d" 1 * (<STR> <BE>) "e"
--    > <LIST>  ::= "l" 1 * <BE>         "e"
--    > <INT>   ::= "i"     <SNUM>       "e"
--    > <STR>   ::= <NUM> ":" n * <CHAR>; where n equals the <NUM>
--    >
--    > <SNUM>  ::= "-" <NUM> / <NUM>
--    > <NUM>   ::= 1 * <DIGIT>
--    > <CHAR>  ::= %
--    > <DIGIT> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
--
--
--    This module is considered to be imported qualified, for example:
--
--    > import Data.BEncode as BE
--
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Trustworthy       #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
#endif

module Data.BEncode
       ( BValue (..)
       , BEncode (..)
       , encode
       , decode

         -- * Helpers
         -- ** Building
       , Assoc
       , (.=!)
       , (.=?)
       , (.:)
       , endDict
       , toDict

         -- ** Extraction
       , Get
       , Result
       , decodingError
       , fromDict
       , lookAhead

       , next
       , req
       , opt
       , field
       , match

       , (<$>!)
       , (<$>?)
       , (<*>!)
       , (<*>?)
       ) where


import Control.Applicative
import Control.Monad
import Control.Monad.State
#if MIN_VERSION_mtl(2, 2, 0)
import Control.Monad.Except
#else
import Control.Monad.Error
#endif
import Data.Int
import Data.List as L
#if __GLASGOW_HASKELL__ < 808
import Data.Semigroup ((<>))
#endif
import Data.Word          (Word8, Word16, Word32, Word64)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as Lazy
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import           Data.Typeable
import           Data.Version
import qualified Text.ParserCombinators.ReadP as ReadP

#if __GLASGOW_HASKELL__ < 710
import Data.Monoid (Monoid (mempty))
import Data.Word (Word)
#endif

#if __GLASGOW_HASKELL__ >= 702
import GHC.Generics
#endif

import Data.BEncode.BDict as BD
import Data.BEncode.Internal
import Data.BEncode.Types


-- | Result used in decoding operations.
type Result = Either String

-- | This class is used to define new datatypes that could be easily
-- serialized using bencode format.
--
--   By default 'BEncode' have a generic implementation; suppose
--   the following datatype:
--
-- > data List a = C { _head  :: a
-- >                 , __tail :: List a }
-- >             | N
-- >               deriving Generic
--
--   If we don't need to obey any particular specification or
--   standard, the default instance could be derived automatically
--   from the 'Generic' instance:
--
-- > instance BEncode a => BEncode (List a)
--
--   Example of derived 'toBEncode' result:
--
-- > > toBEncode (C 123 $ C 1 N)
-- > BDict (fromList [("head",BInteger 123),("tail",BList [])])
--
--  Note that prefixed underscore characters are omitted since they
--  are usually used for lens.
--
class BEncode a where
  -- | See an example of implementation here 'Assoc'
  toBEncode   :: a -> BValue

#if __GLASGOW_HASKELL__ >= 702
  default toBEncode
    :: Generic a
    => GBEncodable (Rep a) BValue
    => a -> BValue

  toBEncode = gto . from
#endif

  -- | See an example of implementation here 'Get'.
  fromBEncode :: BValue -> Result a

#if __GLASGOW_HASKELL__ >= 702
  default fromBEncode
    :: Generic a
    => GBEncodable (Rep a) BValue
    => BValue -> Result a

  fromBEncode x = to <$> gfrom x
#endif

-- | Typically used to throw an decoding error in fromBEncode; when
-- BEncode value to match expected value.
decodingError :: String -> Result a
decodingError s = Left ("fromBEncode: unable to decode " ++ s)
{-# INLINE decodingError #-}

{--------------------------------------------------------------------
  Generics
--------------------------------------------------------------------}

{- NOTE: SELECTORS FOLDING/UNFOLDING
Both List and Map are monoids:

* if fields are named, we fold record to the map;
* otherwise we collect fields using list;

and then unify them using BDict and BList constrs.
-}

#if __GLASGOW_HASKELL__ >= 702

class GBEncodable f e where
  gto   :: f a -> e
  gfrom :: e -> Result (f a)

instance BEncode f
      => GBEncodable (K1 R f) BValue where
  {-# INLINE gto #-}
  gto = toBEncode . unK1

  {-# INLINE gfrom #-}
  gfrom x = K1 <$> fromBEncode x

instance (Eq e, Monoid e)
      => GBEncodable U1 e where
  {-# INLINE gto #-}
  gto U1 = mempty

  {-# INLINE gfrom #-}
  gfrom x
    | x == mempty = pure U1
    |   otherwise = decodingError "U1"

instance (GBEncodable a BList, GBEncodable b BList)
      => GBEncodable (a :*: b) BList where
  {-# INLINE gto #-}
  gto (a :*: b) = gto a ++ gto b

  {-# INLINE gfrom #-}
  gfrom (x : xs) = (:*:) <$> gfrom [x] <*> gfrom xs
  gfrom []       = decodingError "generic: not enough fields"

instance (GBEncodable a BDict, GBEncodable b BDict)
      => GBEncodable (a :*: b) BDict where
  {-# INLINE gto #-}
  gto (a :*: b) = gto a <> gto b

  {-# INLINE gfrom #-}
  -- Just look at this! >.<
  gfrom dict = (:*:) <$> gfrom dict <*> gfrom dict


instance (GBEncodable a e, GBEncodable b e)
      =>  GBEncodable (a :+: b) e where
  {-# INLINE gto #-}
  gto (L1 x) = gto x
  gto (R1 x) = gto x

  {-# INLINE gfrom #-}
  gfrom x = case gfrom x of
    Right lv -> return (L1 lv)
    Left  le -> do
      case gfrom x of
        Right rv -> return (R1 rv)
        Left  re -> decodingError $ "generic: both" ++ le ++ " " ++ re

selRename :: String -> String
selRename = dropWhile ('_'==)

gfromM1S :: forall c f i p. Selector c
         => GBEncodable f BValue
         => BDict -> Result (M1 i c f p)
gfromM1S dict
  | Just va <- BD.lookup (BC.pack (selRename name)) dict = M1 <$> gfrom va
  | otherwise = decodingError $ "generic: Selector not found " ++ show name
  where
    name = selName (error "gfromM1S: impossible" :: M1 i c f p)

instance (Selector s, GBEncodable f BValue)
       => GBEncodable (M1 S s f) BDict where
  {-# INLINE gto #-}
  gto s@(M1 x) = BC.pack (selRename (selName s)) `BD.singleton` gto x

  {-# INLINE gfrom #-}
  gfrom = gfromM1S

-- TODO DList
instance GBEncodable f BValue
      => GBEncodable (M1 S s f) BList where
  {-# INLINE gto #-}
  gto (M1 x) = [gto x]

  gfrom [x] = M1 <$> gfrom x
  gfrom _   = decodingError "generic: empty selector"
  {-# INLINE gfrom #-}

instance (Constructor c, GBEncodable f BDict, GBEncodable f BList)
       => GBEncodable (M1 C c f) BValue where
  {-# INLINE gto #-}
  gto con@(M1 x)
      | conIsRecord con = BDict (gto x)
      |    otherwise    = BList (gto x)

  {-# INLINE gfrom #-}
  gfrom (BDict a) = M1 <$> gfrom a
  gfrom (BList a) = M1 <$> gfrom a
  gfrom _         = decodingError "generic: Constr"

instance GBEncodable f e
      => GBEncodable (M1 D d f) e where
  {-# INLINE gto #-}
  gto (M1 x) = gto x

  {-# INLINE gfrom #-}
  gfrom x = M1 <$> gfrom x

#endif

{--------------------------------------------------------------------
--  Native instances
--------------------------------------------------------------------}

instance BEncode BValue where
  toBEncode = id
  {-# INLINE toBEncode #-}

  fromBEncode = pure
  {-# INLINE fromBEncode #-}

instance BEncode BInteger where
  toBEncode = BInteger
  {-# INLINE toBEncode #-}

  fromBEncode (BInteger i) = pure i
  fromBEncode _            = decodingError "BInteger"
  {-# INLINE fromBEncode #-}

instance BEncode BString where
  toBEncode = BString
  {-# INLINE toBEncode #-}

  fromBEncode (BString s) = pure s
  fromBEncode _           = decodingError "BString"
  {-# INLINE fromBEncode #-}

{- NOTE: those overlap with instance BEncodable a => BEncodable [a]

instance BEncodable BList where
  toBEncode = BList
  {-# INLINE toBEncode #-}

  fromBEncode (BList xs) = pure xs
  fromBEncode _          = decodingError "BList"
  {-# INLINE fromBEncode #-}

-}

instance BEncode BDict where
  toBEncode   = BDict
  {-# INLINE toBEncode #-}

  fromBEncode (BDict d) = pure d
  fromBEncode _         = decodingError "BDict"
  {-# INLINE fromBEncode #-}

{--------------------------------------------------------------------
--  Integral instances
--------------------------------------------------------------------}

{- NOTE: instance Integral a => BEncodable a
   requires -XUndecidableInstances, so we avoid it
-}

toBEncodeIntegral :: Integral a => a -> BValue
toBEncodeIntegral = BInteger . fromIntegral
{-# INLINE toBEncodeIntegral #-}

fromBEncodeIntegral :: forall a. Typeable a => Integral a => BValue -> Result a
fromBEncodeIntegral (BInteger i) = pure (fromIntegral i)
fromBEncodeIntegral _
  = decodingError $ show $ typeOf (error "fromBEncodeIntegral: imposible" :: a)
{-# INLINE fromBEncodeIntegral #-}


instance BEncode Word8 where
  toBEncode = toBEncodeIntegral
  {-# INLINE toBEncode #-}

  fromBEncode = fromBEncodeIntegral
  {-# INLINE fromBEncode #-}

instance BEncode Word16 where
  toBEncode = toBEncodeIntegral
  {-# INLINE toBEncode #-}

  fromBEncode = fromBEncodeIntegral
  {-# INLINE fromBEncode #-}

instance BEncode Word32 where
  toBEncode = toBEncodeIntegral
  {-# INLINE toBEncode #-}

  fromBEncode = fromBEncodeIntegral
  {-# INLINE fromBEncode #-}

instance BEncode Word64 where
  toBEncode = toBEncodeIntegral
  {-# INLINE toBEncode #-}

  fromBEncode = fromBEncodeIntegral
  {-# INLINE fromBEncode #-}

instance BEncode Word where
  toBEncode = toBEncodeIntegral
  {-# INLINE toBEncode #-}

  fromBEncode = fromBEncodeIntegral
  {-# INLINE fromBEncode #-}

instance BEncode Int8 where
  toBEncode = toBEncodeIntegral
  {-# INLINE toBEncode #-}

  fromBEncode = fromBEncodeIntegral
  {-# INLINE fromBEncode #-}

instance BEncode Int16 where
  toBEncode = toBEncodeIntegral
  {-# INLINE toBEncode #-}

  fromBEncode = fromBEncodeIntegral
  {-# INLINE fromBEncode #-}

instance BEncode Int32 where
  toBEncode = toBEncodeIntegral
  {-# INLINE toBEncode #-}

  fromBEncode = fromBEncodeIntegral
  {-# INLINE fromBEncode #-}

instance BEncode Int64 where
  toBEncode = toBEncodeIntegral
  {-# INLINE toBEncode #-}

  fromBEncode = fromBEncodeIntegral
  {-# INLINE fromBEncode #-}

instance BEncode Int where
  toBEncode = toBEncodeIntegral
  {-# INLINE toBEncode #-}

  fromBEncode = fromBEncodeIntegral
  {-# INLINE fromBEncode #-}

{--------------------------------------------------------------------
--  Derived instances
--------------------------------------------------------------------}

instance BEncode Bool where
  toBEncode = toBEncode . fromEnum
  {-# INLINE toBEncode #-}

  fromBEncode b = do
    i <- fromBEncode b
    case i :: Int of
      0 -> return False
      1 -> return True
      _ -> decodingError "Bool"
  {-# INLINE fromBEncode #-}

instance BEncode Text where
  toBEncode = toBEncode . T.encodeUtf8
  {-# INLINE toBEncode #-}

  fromBEncode b = T.decodeUtf8 <$> fromBEncode b
  {-# INLINE fromBEncode #-}

instance BEncode a => BEncode [a] where
  {-# SPECIALIZE instance BEncode BList #-}
  toBEncode = BList . L.map toBEncode
  {-# INLINE toBEncode #-}

  fromBEncode (BList xs) = mapM fromBEncode xs
  fromBEncode _          = decodingError "list"
  {-# INLINE fromBEncode #-}

{-
instance BEncode a => BEncode (Map BKey a) where
  {-# SPECIALIZE instance BEncode (Map BKey BValue) #-}
  toBEncode = BDict .  -- BD.map toBEncode
  {-# INLINE toBEncode #-}

  fromBEncode (BDict d) = traverse fromBEncode d
  fromBEncode _         = decodingError "dictionary"
  {-# INLINE fromBEncode #-}

instance (Eq a, BEncode a) => BEncode (Set a) where
  {-# SPECIALIZE instance BEncode (Set BValue)  #-}
  toBEncode = BList . map toBEncode . S.toAscList
  {-# INLINE toBEncode #-}

  fromBEncode (BList xs) = S.fromAscList <$> traverse fromBEncode xs
  fromBEncode _          = decodingError "Data.Set"
  {-# INLINE fromBEncode #-}
-}
instance BEncode Version where
  toBEncode = toBEncode . BC.pack . showVersion
  {-# INLINE toBEncode #-}

  fromBEncode (BString bs)
    | [(v, _)] <- ReadP.readP_to_S parseVersion (BC.unpack bs)
    = return v
  fromBEncode _ = decodingError "Data.Version"
  {-# INLINE fromBEncode #-}

{--------------------------------------------------------------------
--  Tuple instances
--------------------------------------------------------------------}

instance BEncode () where
  toBEncode () = BList []
  {-# INLINE toBEncode #-}

  fromBEncode (BList []) = Right ()
  fromBEncode _          = decodingError "Unable to decode unit value"
  {-# INLINE fromBEncode #-}

instance (BEncode a, BEncode b) => BEncode (a, b) where
  {-# SPECIALIZE instance (BEncode b) => BEncode (BValue, b) #-}
  {-# SPECIALIZE instance (BEncode a) => BEncode (a, BValue) #-}
  {-# SPECIALIZE instance BEncode (BValue, BValue) #-}
  toBEncode (a, b) = BList [toBEncode a, toBEncode b]
  {-# INLINE toBEncode #-}

  fromBEncode (BList [a, b]) = (,) <$> fromBEncode a <*> fromBEncode b
  fromBEncode _              = decodingError "Unable to decode a pair."
  {-# INLINE fromBEncode #-}

instance (BEncode a, BEncode b, BEncode c) => BEncode (a, b, c) where
  toBEncode (a, b, c) = BList [toBEncode a, toBEncode b, toBEncode c]
  {-# INLINE toBEncode #-}

  fromBEncode (BList [a, b, c]) =
    (,,) <$> fromBEncode a <*> fromBEncode b <*> fromBEncode c
  fromBEncode _ = decodingError "Unable to decode a triple"
  {-# INLINE fromBEncode #-}

instance (BEncode a, BEncode b, BEncode c, BEncode d)
         => BEncode (a, b, c, d) where
  toBEncode (a, b, c, d) = BList [ toBEncode a, toBEncode b
                                 , toBEncode c, toBEncode d
                                 ]
  {-# INLINE toBEncode #-}

  fromBEncode (BList [a, b, c, d]) =
    (,,,) <$> fromBEncode a <*> fromBEncode b
          <*> fromBEncode c <*> fromBEncode d
  fromBEncode _ = decodingError "Unable to decode a tuple4"
  {-# INLINE fromBEncode #-}

instance (BEncode a, BEncode b, BEncode c, BEncode d, BEncode e)
      =>  BEncode (a, b, c, d, e) where
  toBEncode (a, b, c, d, e) = BList [ toBEncode a, toBEncode b
                                 , toBEncode c, toBEncode d
                                 , toBEncode e
                                 ]
  {-# INLINE toBEncode #-}

  fromBEncode (BList [a, b, c, d, e]) =
    (,,,,) <$> fromBEncode a <*> fromBEncode b
           <*> fromBEncode c <*> fromBEncode d <*> fromBEncode e
  fromBEncode _ = decodingError "Unable to decode a tuple5"
  {-# INLINE fromBEncode #-}

{--------------------------------------------------------------------
  Building dictionaries
--------------------------------------------------------------------}

-- | /Assoc/ used to easily build dictionaries with required and
-- optional keys. Suppose we have we following datatype we want to
-- serialize:
--
--  @
--   data FileInfo = FileInfo
--     { fileLength :: Integer
--     , fileMD5sum :: Maybe ByteString
--     , filePath   :: [ByteString]
--     , fileTags   :: Maybe [Text]
--     } deriving (Show, Read, Eq)
--  @
--
-- We need to make @instance 'BEncode' FileInfo@, though we don't want
-- to check the both 'Maybe's manually. The more declarative and
-- convenient way to define the 'toBEncode' method is to use
-- dictionary builders:
--
--  @
--   instance 'BEncode' FileInfo where
--     'toBEncode' FileInfo {..} = 'toDict' $
--          \"length\" '.=!' fileLength
--       '.:' \"md5sum\" '.=?' fileMD5sum
--       '.:' \"path\"   '.=!' filePath
--       '.:' \"tags\"   '.=?' fileTags
--       '.:' 'endDict'
--  @
--
--  NOTE: the list of pairs MUST be sorted lexicographically by keys,
--  like so:
--
--    \"length\" '<' \"md5sum\" '<' \"path\" '<' \"tags\"
--
data Assoc = Some !BKey BValue
           | None

-- | Make required key value pair.
(.=!) :: BEncode a => BKey -> a -> Assoc
(!k) .=! v = Some k (toBEncode v)
{-# INLINE (.=!) #-}

infix 6 .=!

-- | Like the ('.=!') operator but if the value is not present then
-- the key do not appear in resulting bencode dictionary.
--
(.=?) :: BEncode a => BKey -> Maybe a -> Assoc
_ .=? Nothing = None
k .=? Just v  = Some k (toBEncode v)
{-# INLINE (.=?) #-}

infix 6 .=?

-- | Cons a key\/value pair.
(.:) :: Assoc -> BDict -> BDict
None     .: d = d
Some k v .: d = Cons k v d
{-# INLINE (.:) #-}

infixr 5 .:

-- | Make a bencode value from dictionary description.
toDict :: BDict -> BValue
toDict = BDict
{-# INLINE toDict #-}

-- | Used to specify end of dictionary. See 'Assoc'.
endDict :: BDict
endDict = Nil
{-# INLINE endDict #-}

{--------------------------------------------------------------------
--  Dictionary extraction
--------------------------------------------------------------------}

-- | Dictionary extractor are similar to dictionary builders, but play
-- the opposite role: they are used to define 'fromBEncode' method in
-- declarative style. Using the same /FileInfo/ datatype the
-- 'fromBEncode' function instance looks like:
--
--  @
--   instance 'BEncode' FileInfo where
--     'fromBEncode' = 'fromDict' $ do
--       FileInfo '<$>!' \"length\"
--                '<*>?' \"md5sum\"
--                '<*>!' \"path\"
--                '<*>?' \"tags\"
--  @
--
--  The /reqKey/ is used to extract required key — if lookup is failed
--  then whole destructuring fail.
--
--  NOTE: the actions MUST be sorted lexicographically by keys, like so:
--
--  \"length\" '<' \"md5sum\" '<' \"path\" '<' \"tags\"
--
newtype Get a = Get { runGet :: StateT BDict Result a }
  deriving (Functor, Applicative)

-- | 'fail' is catchable from pure code.
instance Monad Get where
  Get m >>= f = Get (m >>= runGet . f)
  {-# INLINE (>>=) #-}

#if __GLASGOW_HASKELL__ < 808
  fail msg = Get (lift (Left msg))
  {-# INLINE fail #-}
#else
instance MonadFail Get where
  fail msg = Get (lift (Left msg))
  {-# INLINE fail #-}
#endif

-- | NOTE: @'Control.Applicative.empty' = 'fail' ""@.
instance Alternative Get where
  empty = fail ""
  Get (StateT m) <|> Get (StateT n) = Get $ StateT $ \s -> m s <> n s

-- | Run action, but return without consuming and key\/value pair.
-- Fails if the action fails.
lookAhead :: Get a -> Get a
lookAhead (Get m) = Get $ do
  s <- get
  r <- m
  put s
  return r

-- | Get lexicographical successor of the current key\/value pair.
next :: Get BValue
next = Get (StateT go)
  where
    go  Nil          = throwError "no next"
    go (Cons _ v xs) = pure (v, xs)

-- | Extract /required/ value from the given key.
req :: BKey -> Get BValue
req !key = Get (StateT search)
  where
    search  Nil          = Left msg
    search (Cons k v xs) =
      case compare k key of
        EQ -> pure (v, xs)
        LT -> search xs
        GT -> Left msg

    msg = "required field `" ++ BC.unpack key ++ "' not found"
{-# INLINE req #-}

-- | Extract optional value from the given key.
opt :: BKey -> Get (Maybe BValue)
opt = optional . req
{-# INLINE opt #-}

-- | Reconstruct a bencodable value from bencode value.
field :: BEncode a => Get BValue -> Get a
{-# SPECIALIZE field :: Get BValue -> Get BValue #-}
field m = Get $ do
  v <- runGet m
  either throwError pure $ fromBEncode v

-- | Match key with value.
match :: BKey -> BValue -> Get ()
match key expected = do
  actual <- req key
  if actual == expected
    then return ()
    else fail $ "key match failure(" ++ show key ++ "): " ++
                "expected = " ++ show expected ++
                "actual   = " ++ show actual

-- | Shorthand for: @f '<$>' 'field' ('req' k)@.
(<$>!) :: BEncode a => (a -> b) -> BKey -> Get b
f <$>! k = f <$> field (req k)
{-# INLINE (<$>!) #-}

infixl 4 <$>!

-- | Shorthand for: @f '<$>' 'optional' ('field' ('req' k))@.
(<$>?) :: BEncode a => (Maybe a -> b) -> BKey -> Get b
f <$>? k = f <$> optional (field (req k))
{-# INLINE (<$>?) #-}

infixl 4 <$>?

-- | Shorthand for: @f '<*>' 'field' ('req' k)@.
(<*>!) :: BEncode a => Get (a -> b) -> BKey -> Get b
f <*>! k = f <*> field (req k)
{-# INLINE (<*>!) #-}

infixl 4 <*>!

-- | Shorthand for: @f '<*>' 'optional' ('field' ('req' k))@.
(<*>?) :: BEncode a => Get (Maybe a -> b) -> BKey -> Get b
f <*>? k = f <*> optional (field (req k))
{-# INLINE (<*>?) #-}

infixl 4 <*>?

-- | Run a 'Get' monad. See 'Get' for usage.
fromDict :: forall a. Typeable a => Get a -> BValue -> Result a
fromDict m (BDict d) = evalStateT (runGet m) d
fromDict _  _        = decodingError (show (typeOf inst))
  where
    inst = error "fromDict: impossible" :: a

{--------------------------------------------------------------------
  Encoding
--------------------------------------------------------------------}

-- | Decode a value from a strict 'ByteString' using bencode format.
decode :: BEncode a => ByteString -> Result a
decode = parse >=> fromBEncode

-- | Encode a value using bencode format to a lazy 'ByteString'.
encode :: BEncode a => a -> Lazy.ByteString
encode = build . toBEncode
