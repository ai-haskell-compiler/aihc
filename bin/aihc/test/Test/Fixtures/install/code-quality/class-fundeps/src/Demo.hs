module Demo where

data Bag = Bag

class Collection c e | c -> e where
  insert :: e -> c -> c

instance Collection Bag Bag where
  insert _ bag = bag
