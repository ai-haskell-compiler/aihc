module Data.Either.HT (
   mapLeft,
   mapRight,
   mapBoth,
   maybeLeft,
   maybeRight,
   swap,
   ) where


mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f = either (Left . f) Right

mapRight :: (b -> c) -> Either a b -> Either a c
mapRight f = either Left (Right . f)

mapBoth :: (a -> c) -> (b -> d) -> Either a b -> Either c d
mapBoth f g = either (Left . f) (Right . g)

maybeLeft :: Either a b -> Maybe a
maybeLeft = either Just (const Nothing)

maybeRight :: Either a b -> Maybe b
maybeRight = either (const Nothing) Just

swap :: Either a b -> Either b a
swap = either Right Left
