{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}

module Parameterized.TypeLevel where

-- | Get the first type of a value level tuple
-- This is useful for defining a newtype wrapper with a single "parameterized" type,
-- around a type with many "parameterized" type variables.
type family At0 t where
    At0 (x, _) = x
    At0 (x, _, _) = x
    At0 (x, _, _, _) = x
    At0 (x, _, _, _, _) = x
    At0 (x, _, _, _, _, _) = x
    At0 (x, _, _, _, _, _, _) = x
    At0 (x, _, _, _, _, _, _, _) = x
    At0 (x, _, _, _, _, _, _, _, _) = x
    At0 (x, _, _, _, _, _, _, _, _, _) = x

-- | Get the second type of a value level tuple
type family At1 t where
    At1 (_, x) = x
    At1 (_, x, _) = x
    At1 (_, x, _, _) = x
    At1 (_, x, _, _, _) = x
    At1 (_, x, _, _, _, _) = x
    At1 (_, x, _, _, _, _, _) = x
    At1 (_, x, _, _, _, _, _, _) = x
    At1 (_, x, _, _, _, _, _, _, _) = x
    At1 (_, x, _, _, _, _, _, _, _, _) = x

-- | Get the third type of a value level tuple
type family At2 t where
    At2 (_, _, x) = x
    At2 (_, _, x, _) = x
    At2 (_, _, x, _, _) = x
    At2 (_, _, x, _, _, _) = x
    At2 (_, _, x, _, _, _, _) = x
    At2 (_, _, x, _, _, _, _, _) = x
    At2 (_, _, x, _, _, _, _, _, _) = x
    At2 (_, _, x, _, _, _, _, _, _, _) = x

-- | Get the fourth type of a value level tuple
type family At3 t where
    At3 (_, _, _, x) = x
    At3 (_, _, _, x, _) = x
    At3 (_, _, _, x, _, _) = x
    At3 (_, _, _, x, _, _, _) = x
    At3 (_, _, _, x, _, _, _, _) = x
    At3 (_, _, _, x, _, _, _, _, _) = x
    At3 (_, _, _, x, _, _, _, _, _, _) = x

-- | Get the fifth type of a value level tuple
type family At4 t where
    At4 (_, _, _, _, x) = x
    At4 (_, _, _, _, x, _) = x
    At4 (_, _, _, _, x, _, _) = x
    At4 (_, _, _, _, x, _, _, _) = x
    At4 (_, _, _, _, x, _, _, _, _) = x
    At4 (_, _, _, _, x, _, _, _, _, _) = x

-- | Get the sixth type of a value level tuple
type family At5 t where
    At5 (_, _, _, _, _, x) = x
    At5 (_, _, _, _, _, x, _) = x
    At5 (_, _, _, _, _, x, _, _) = x
    At5 (_, _, _, _, _, x, _, _, _) = x
    At5 (_, _, _, _, _, x, _, _, _, _) = x

-- | Get the seventh type of a value level tuple
type family At6 t where
    At6 (_, _, _, _, _, _, x) = x
    At6 (_, _, _, _, _, _, x, _) = x
    At6 (_, _, _, _, _, _, x, _, _) = x
    At6 (_, _, _, _, _, _, x, _, _, _) = x

-- | Get the eigth type of a value level tuple
type family At7 t where
    At7 (_, _, _, _, _, _, _, x) = x
    At7 (_, _, _, _, _, _, _, x, _) = x
    At7 (_, _, _, _, _, _, _, x, _, _) = x

-- | Get the nineth type of a value level tuple
type family At8 t where
    At8 (_, _, _, _, _, _, _, _, x) = x
    At8 (_, _, _, _, _, _, _, _, x, _) = x

-- | Get the tenth type of a value level tuple
type family At9 t where
    At9 (_, _, _, _, _, _, _, _, _, x) = x
