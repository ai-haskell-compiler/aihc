module Example where

data Switch = Off | On

class Default a where
  def :: a

instance Default Switch where
  def = Off
