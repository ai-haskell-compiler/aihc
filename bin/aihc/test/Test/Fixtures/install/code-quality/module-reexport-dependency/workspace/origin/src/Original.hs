module Original (Token(..), identity) where

data Token = Token

identity :: Token -> Token
identity value = value
