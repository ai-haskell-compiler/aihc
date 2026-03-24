module M where
data Target a = Target {
    lTarget  :: a -> Double
  , glTarget :: Maybe (a -> a)
  }
