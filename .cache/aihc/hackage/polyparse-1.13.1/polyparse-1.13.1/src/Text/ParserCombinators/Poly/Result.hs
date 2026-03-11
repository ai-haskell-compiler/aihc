module Text.ParserCombinators.Poly.Result
  ( -- * The parsing result type
    Result(..)  -- A parsing result type, with Success, Failure, and Commitment.
  , resultToEither
  ) where


-- | A return type like Either, that distinguishes not only between
--   right and wrong answers, but also has commitment, so that a failure
--   cannot be undone.  This should only be used for writing very primitive
--   parsers - really it is an internal detail of the library.
--   The z type is the remaining unconsumed input.
data Result z a = Success   z a
                | Failure   z String
                | Committed (Result z a)

instance Functor (Result z) where
    fmap f (Success z a) = Success z (f a)
    fmap f (Failure z e) = Failure z e
    fmap f (Committed r) = Committed (fmap f r)

-- | Convert a Result to an Either, paired with the remaining unconsumed input.
resultToEither :: Result z a -> (Either String a, z)
resultToEither (Success z a)  =  (Right a, z)
resultToEither (Failure z e)  =  (Left e, z)
resultToEither (Committed r)  =  resultToEither r

------------------------------------------------------------------------
