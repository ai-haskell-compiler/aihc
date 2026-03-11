{-|
There are two ways to use this library: the way with extra boilerplate,
or the way with a leaky abstraction. A truly good solution would fix it
all with Template Haskell; but the author is not a truly good Haskeller,
so this will have to do for now.

The boilerplate way:

@
data Hideous = Hideous { yak :: [String] }
instance ToJSON Hideous where
  toJSON x = object [ "yak" .= hairy (yak x) ]
instance FromJSON Hideous where
  parseJSON (Object o) = Hideous \<$> (shave \<$> o .:? "yak")
@
>>> encode $ Hideous ["foo","bar"]
"{\"yak\":[\"foo\",\"bar\"]}"
>>> encode $ Hideous ["baz"]
"{\"yak\":\"baz\"}"
>>> encode $ Hideous []
"{\"yak\":null}"
>>> yak <$> decode "{\"yak\":[\"foo\",\"bar\"]}"
Just ["foo", "bar"]
>>> yak <$> decode "{\"yak\":\"baz\"}"
Just ["baz"]
>>> yak <$> decode "{}"
Just []

The leaky way:

@
data Abhorrent = Abhorrent { yak :: Yak String }
$(deriveJSON defaultOptions{omitNothingFields=True} ''Abhorrent)
@
>>> encode . Abhorrent . hairy $ ["shaven","shorn","sheared"]
"{\"yak\":[\"shaven\",\"shorn\",\"sheared\"]}"
>>> shave . yak <$> decode "{}"
Just []

Which to prefer depends on how many yaks you need to deal with /vs./ how
much you hate cleaning up yak droppings in the rest of your codebase.
-}
module Data.Aeson.Yak
( Yak
, hairy
, shave)
where

import           Data.Aeson
import           Data.Foldable

-- | Data whose JSON representation may legally be an array, a single element,
--   or null\/absent. No, please, calm down. It'll be okay. Mostly.
--
--   /('Lousy' is not exposed to avoid namespace infestation./
--   /This is open for discussion if a use case can be shown.)/
type Yak a = Maybe (Lousy a)

-- | Convert a @'Foldable'@ to a 'Yak'. Should probably be specific to lists,
--   but what's life without a little adventure?
hairy :: (Foldable f) => f a -> Yak a
hairy beast = case toList beast of
  []  -> Nothing
  yak -> Just (Lousy yak)

-- | Convert a 'Yak' to a list. Relax, and allow yourself to breathe.
shave :: Yak a -> [a]
shave Nothing    = []
shave (Just nit) = pick nit

newtype Lousy a = Lousy { pick :: [a] } deriving (Eq)
instance (Show a) => Show (Lousy a) where
  show = ("Lousy " ++) . show . pick

instance (ToJSON a) => ToJSON (Lousy a) where
  toJSON nit = case pick nit of
    [x] -> toJSON x
    xs  -> toJSON xs

instance (FromJSON a) => FromJSON (Lousy a) where
  parseJSON vs@(Array _) = Lousy <$> parseJSON vs
  parseJSON v            = (\a -> Lousy [a]) <$> parseJSON v
