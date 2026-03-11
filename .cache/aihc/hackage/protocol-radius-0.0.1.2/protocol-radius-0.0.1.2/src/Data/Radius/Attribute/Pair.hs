
module Data.Radius.Attribute.Pair (
  NumberAbstract (..),

  TypedNumber, unsafeTypedNumber, untypeNumber,

  Attribute' (..), Attribute (..), value,

  TypedNumberSet, typed,

  TypedNumberSets (..),
  ) where

import Control.Applicative ((<$>))
import Control.Monad (guard)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.ByteString (ByteString)
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Radius.Scalar (AtText, AtString, AtInteger, AtIpV4, )
import qualified Data.Radius.Attribute.Number as Radius


data NumberAbstract a
  = Standard Radius.Number
  | Vendors  a
  deriving (Eq, Ord, Show)


newtype TypedNumber v a =
  TypedNumber (NumberAbstract v)
  deriving (Eq, Ord, Show)

unsafeTypedNumber :: NumberAbstract v -> TypedNumber v a
unsafeTypedNumber = TypedNumber

untypeNumber :: TypedNumber v a -> NumberAbstract v
untypeNumber (TypedNumber n) = n

data Attribute' v =
  Attribute' !(NumberAbstract v) !ByteString
  deriving (Eq, Ord, Show)

data Attribute v a =
  Attribute !(TypedNumber v a) !a
  deriving (Eq, Ord, Show)

value :: Attribute v a -> a
value (Attribute _ v) = v


type TypedNumberSet v a = Set (TypedNumber v a)

{-
-- | Retryable error context with anthor attirbute value type /t/ /m/, and parse error context /m/.
typed :: (Monad m, Functor m, MonadTrans t, MonadPlus (t m))
      => TypedNumberSet vt
      -> (ByteString -> m a)
      -> Attribute'
      -> t m (Attribute a)
 -}
-- | Retryable error context with anthor attirbute value type 'MaybeT' /m/, and parse error context /m/.
typed :: (Monad m, Functor m, Ord v)
      => TypedNumberSet v a
      -> (ByteString -> m b)
      -> Attribute' v
      -> MaybeT m (Attribute v b)
typed s parse (Attribute' n d) = do
  let typedAN = unsafeTypedNumber n
  guard $ typedAN `Set.member` s
  lift $ Attribute typedAN <$> parse d


class TypedNumberSets v where
  attributeNumbersText    :: TypedNumberSet v AtText
  attributeNumbersString  :: TypedNumberSet v AtString
  attributeNumbersInteger :: TypedNumberSet v AtInteger
  attributeNumbersIpV4    :: TypedNumberSet v AtIpV4
