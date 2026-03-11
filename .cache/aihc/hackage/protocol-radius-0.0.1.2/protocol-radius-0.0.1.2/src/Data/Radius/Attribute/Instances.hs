{-# LANGUAGE TemplateHaskell #-}

module Data.Radius.Attribute.Instances where

import Data.Radius.Attribute.Number (Number (..))
import Data.Radius.Attribute.Pair (NumberAbstract (..))
import Data.Radius.Attribute.TH (unsafeTypedNumberSetTemplate)
import Data.Radius.Scalar(AtText, AtString, AtInteger, AtIpV4)


$(unsafeTypedNumberSetTemplate
 "numbersText"  Nothing [t|AtText|]
 [ ([| Standard |], ['ReplyMessage]) ])

$(unsafeTypedNumberSetTemplate
 "numbersString"  Nothing [t|AtString|]
 [ ([| Standard |], [ 'UserName, 'ProxyState, 'State, 'MessageAuthenticator]) ])

$(unsafeTypedNumberSetTemplate
  "numbersInteger"   Nothing [t|AtInteger|]
  [])

$(unsafeTypedNumberSetTemplate
  "numbersIpV4"   Nothing [t|AtIpV4|]
  [])
