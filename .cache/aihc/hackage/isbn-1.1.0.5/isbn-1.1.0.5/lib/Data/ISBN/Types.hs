module Data.ISBN.Types where

import           Data.Text


-- | Data type for representing ISBN values. Values of this type should be
-- created safely using 'Data.ISBN.validateISBN', which will produce 'ISBN10'
-- or 'ISBN13' values after validating the input.
--
-- The 'Data.ISBN.validateISBN10' and 'Data.ISBN.validateISBN13' functions can
-- also be used to only attempt to create ISBNs of a specific type.
--
-- To create @ISBN@ values without validation, use the 'Data.ISBN.unsafeToISBN10'
-- and 'Data.ISBN.unsafeToISBN13' functions to coerce 'Data.Text.Text' values
-- into 'ISBN' values.
data ISBN
    = ISBN10 Text -- ^ An ISBN-10 value. Consists of 9 digits followed by a base-11 check digit (@0-9@ or @\'X\'@).
    | ISBN13 Text -- ^ An ISBN-13 value. Consists of 12 digits followed by a base-10 check digit (@0-9@).
    deriving (Show, Eq)

