
module Data.Radius.StreamGet.Monadic (
  -- * DSL to get typed attributes from packet
  TypedAttributes, takeTyped', takeTyped,

  Attributes, extractAttributes,
  tellT,

  -- * low-level definitions
  AttributeGetWT, attributeGetWT, runAttributeGetWT,

  decodeAsText, decodeAsString, decodeAsInteger, decodeAsIpV4,
  ) where

import Control.Applicative ((<$>), pure, (<*), (<|>))
import Control.Monad (liftM, MonadPlus, guard, msum)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.Writer (WriterT (..), tell)
import Data.Monoid (mempty)
import Data.DList (DList)
import qualified Data.DList as DList
import qualified Data.ByteString as BS
import Data.Functor.Identity (runIdentity)
import Data.Serialize.Get (runGet)

import Data.Radius.Scalar (AtText (..), AtString (..), AtInteger (..), AtIpV4 (..))
import Data.Radius.Attribute
  (Attribute (..), Attribute' (..),
   TypedNumber, typed, value, TypedNumberSets (..), )
import qualified Data.Radius.StreamGet.Base as Base


type AtList v at = DList (Attribute v at)
type AtWriterT v at = WriterT (AtList v at)

{-
-- May switch to simple Sum type structure
-- AIpV4 ... | AText ... | AInteger ... | AString ...
 -}

type AttributeGetWT' v m =
  AtWriterT v AtIpV4
  (AtWriterT v AtText
   (AtWriterT v AtInteger
    (AtWriterT v AtString m)))

attributeGetWT' :: m ((((a, AtList v AtIpV4), AtList v AtText), AtList v AtInteger), AtList v AtString)
                 -> AttributeGetWT' v m a
attributeGetWT' = WriterT . WriterT . WriterT . WriterT
                   {- coercible operation ^^ -}

runAttributeGetWT' :: AttributeGetWT' v m a
                    -> m ((((a, AtList v AtIpV4), AtList v AtText), AtList v AtInteger), AtList v AtString)
runAttributeGetWT' = runWriterT . runWriterT . runWriterT . runWriterT
                      {- coercible operation ^^ -}

liftAW :: Monad m => m a -> AttributeGetWT' v m a
liftAW = lift . lift . lift . lift

type AttributeGetWT v m = AttributeGetWT' v (WriterT (DList (Attribute' v)) m)


decodeAsText :: (TypedNumberSets v, Ord v)
             => Attribute' v
             -> MaybeT (Either String) (Attribute v AtText)
decodeAsText   a@(Attribute' _ bs) = typed attributeNumbersText    (runGet . Base.atText $ BS.length bs)   a

decodeAsString :: (TypedNumberSets v, Ord v)
               => Attribute' v
               -> MaybeT (Either String) (Attribute v AtString)
decodeAsString a@(Attribute' _ bs) = typed attributeNumbersString  (runGet . Base.atString $ BS.length bs) a

decodeAsInteger :: (TypedNumberSets v, Ord v)
                => Attribute' v
                -> MaybeT (Either String) (Attribute v AtInteger)
decodeAsInteger = typed attributeNumbersInteger (runGet $ Base.atInteger <* Base.eof)

decodeAsIpV4 :: (TypedNumberSets v, Ord v)
             => Attribute' v
             -> MaybeT (Either String) (Attribute v AtIpV4)
decodeAsIpV4    = typed attributeNumbersIpV4    (runGet $ Base.atIpV4    <* Base.eof)

-- | Decode untyped attribute into monadic context.
--   When typed-value decode error found, either typed context makes sense.
tellT :: (TypedNumberSets v, Ord v)
      => Attribute' v -> AttributeGetWT v (Either String) ()
tellT a =
  let emptyW = runIdentity . runAttributeGetWT' $ pure () in
  {-- Not recoverable context type,
      AttributeGetWT' v (Writer (DList Attribute')) == AttributeGetWT v --}
  attributeGetWT' . WriterT .
  (maybe (emptyW, pure a) (\x -> (x, mempty)) <$>) . runMaybeT .  {-- un-maybe with default untyped value  --}
  runAttributeGetWT' $

  {-- recoverable context type, AttributeGetWT' (MaybeT (Either String)) --}
  do ta <- liftAW $ decodeAsString  a
     ta `seq` lift . lift . lift . tell $ pure ta
  <|>
  do ta <- liftAW $ decodeAsInteger a
     ta `seq` lift . lift . tell $ pure ta
  <|>
  do ta <- liftAW $ decodeAsText    a
     ta `seq` lift . tell $ pure ta
  <|>
  do ta <- liftAW $ decodeAsIpV4    a
     ta `seq` tell $ pure ta

attributeGetWT :: m (((((a, AtList v AtIpV4), AtList v AtText), AtList v AtInteger), AtList v AtString), DList (Attribute' v))
                 -> AttributeGetWT v m a
attributeGetWT = attributeGetWT' . WriterT

runAttributeGetWT :: AttributeGetWT v m a
                    -> m (((((a, AtList v AtIpV4), AtList v AtText), AtList v AtInteger), AtList v AtString), DList (Attribute' v))
runAttributeGetWT = runWriterT . runAttributeGetWT'


-- | Type to express typed attribute set
data Attributes v =
  Attributes
  { textAttributes     :: ![Attribute v AtText]
  , stringAttributes   :: ![Attribute v AtString]
  , integerAttributes  :: ![Attribute v AtInteger]
  , ipV4Attributes     :: ![Attribute v AtIpV4]
  , untypedAttributes  :: ![Attribute' v]
  }

-- | Extract typed attributes.
--   For example, use like this: /extractAttributes . mapM tellT/
extractAttributes :: Monad m => AttributeGetWT v m a -> m (Attributes v)
extractAttributes w = do
  (((((_, ips), txts), ints), strs), utys)  <- runAttributeGetWT w
  return $
    Attributes
    { textAttributes     =  DList.toList txts
    , stringAttributes   =  DList.toList strs
    , integerAttributes  =  DList.toList ints
    , ipV4Attributes     =  DList.toList ips
    , untypedAttributes  =  DList.toList utys
    }

-- | Type class to generalize typed attribute param
class TypedAttributes a where
  typedAttributes :: Attributes v -> [Attribute v a]

instance TypedAttributes AtText where
  typedAttributes = textAttributes

instance TypedAttributes AtString where
  typedAttributes = stringAttributes

instance TypedAttributes AtInteger where
  typedAttributes = integerAttributes

instance TypedAttributes AtIpV4 where
  typedAttributes = ipV4Attributes

-- | Get typed attribute from attribute set.
{-# SPECIALIZE takeTyped' :: (TypedAttributes a, Eq v) => Attributes v -> TypedNumber v a -> Maybe (Attribute v a) #-}
{-# SPECIALIZE takeTyped' :: (TypedAttributes a, Eq v) => Attributes v -> TypedNumber v a -> [Attribute v a] #-}
takeTyped' :: (MonadPlus m, TypedAttributes a, Eq v)
           => Attributes v
           -> TypedNumber v a
           -> m (Attribute v a)
takeTyped' attrs tn0 =
    msum [ testA a | a <- typedAttributes attrs ]
  where
    testA a@(Attribute tn _) = do
      guard $ tn == tn0
      return a

-- | Get typed attribute value from attribute set.
{-# SPECIALIZE takeTyped :: (TypedAttributes a, Eq v) => Attributes v -> TypedNumber v a -> Maybe a #-}
{-# SPECIALIZE takeTyped :: (TypedAttributes a, Eq v) => Attributes v -> TypedNumber v a -> [a] #-}
takeTyped :: (MonadPlus m, TypedAttributes a, Eq v)
          => Attributes v
          -> TypedNumber v a
          -> m a
takeTyped attrs = liftM value . takeTyped' attrs
