{-# LANGUAGE DuplicateRecordFields #-}

module Test.Data.Registry.Aeson.Examples.Deliverix where

import Protolude hiding (Product)

{-

  Data model in its second incarnation
  The initial version didn't have a coupon and the price was an Int

-}

data Delivery = Delivery
  { customer :: Customer,
    order :: Order
  }
  deriving (Eq, Show)

data Order = Order
  { price :: Price,
    coupon :: Maybe Coupon,
    items :: Map Int Product
  }
  deriving (Eq, Show)

newtype Price = Price Double deriving (Eq, Show, Num)

data Address = Address
  { number :: StreetNumber,
    street :: Text,
    city :: Text
  }
  deriving (Eq, Show)

newtype StreetNumber = StreetNumber { streetNumber :: Text } deriving (Eq, Show)

data Customer = Customer
  { customerName :: Text,
    customerAddress :: Address
  }
  deriving (Eq, Show)

data Product
  = Pizza
  | Sushis
  | Tandoori
  deriving (Eq, Show)

data Coupon
  = FivePercentOff
  | TenPercentOff
  deriving (Eq, Show)

newtype PriceV1 = PriceV1 Int

data OrderV1 = OrderV1
  { price :: Price,
    items :: Map Int Product
  }

data AddressV2 = AddressV2
  { number :: Int,
    street :: Text,
    city :: Text
  }
  deriving (Eq, Show)
