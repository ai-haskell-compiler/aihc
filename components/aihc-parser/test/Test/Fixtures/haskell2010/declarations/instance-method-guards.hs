module M where
class Class a where
  fn :: a -> ()
instance Class X where
  fn a | True = ()
