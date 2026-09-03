module Dep.Types where

data Token = Token

class Mark value where
  mark :: value -> value

token = Token
