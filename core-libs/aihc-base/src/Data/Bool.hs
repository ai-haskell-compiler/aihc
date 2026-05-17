module Data.Bool
  ( Bool (False, True),
    (&&),
    not,
    otherwise,
    (||),
  )
where

data Bool = False | True

infixr 3 &&

(&&) :: Bool -> Bool -> Bool
False && _ = False
True && x = x

not :: Bool -> Bool
not False = True
not True = False

otherwise :: Bool
otherwise = True

infixr 2 ||

(||) :: Bool -> Bool -> Bool
False || x = x
True || _ = True
