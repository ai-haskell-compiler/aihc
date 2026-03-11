-- | Types in [MessagePack RPC](https://github.com/msgpack-rpc/msgpack-rpc/blob/master/spec.md)

module Data.MessagePack.RPC (
    MessageId
  , MethodName
  , Message(..)
  ) where

import           Data.MessagePack (MessagePack (..), Object (..))
import           Data.List        (intercalate)
import qualified Data.Text        as T
import           Data.Word        (Word64)

-- | Message ID.
type MessageId = Word64

-- | Method name.
type MethodName = T.Text

-- | Message type of MessagePack PRC.
--   Use 'toObject' and 'fromObject' for conversion.
data Message =
    -- | Request
      RequestMessage MessageId MethodName [Object]
    -- | Response. 'Left' is an error. 'Right' is a result.
    | ResponseMessage MessageId (Either Object Object)
    -- | Notification.
    | NotificationMessage MethodName [Object]
  deriving Eq

instance MessagePack Message where
  toObject (RequestMessage mid methodName args) =
    ObjectArray
      [ ObjectWord 0
      , ObjectWord mid
      , ObjectStr methodName
      , ObjectArray args
      ]

  toObject (ResponseMessage mid (Right result)) =
    ObjectArray
      [ ObjectWord 1
      , ObjectWord mid
      , ObjectNil
      , result
      ]

  toObject (ResponseMessage mid (Left err)) =
    ObjectArray
      [ ObjectWord 1
      , ObjectWord mid
      , err
      , ObjectNil
      ]

  toObject (NotificationMessage methodName params) =
    ObjectArray
      [ ObjectWord 2
      , ObjectStr methodName
      , ObjectArray params
      ]

  fromObject
    ( ObjectArray
        [ ObjectWord 0
        , ObjectWord mid
        , ObjectStr methodName
        , ObjectArray args
        ]
    ) =
      return $ RequestMessage mid methodName args

  fromObject
    ( ObjectArray
        [ ObjectWord 1
        , ObjectWord mid
        , ObjectNil
        , result
        ]
    ) =
      return $ ResponseMessage mid (Right result)
  fromObject
    ( ObjectArray
        [ ObjectWord 1
        , ObjectWord mid
        , err
        , ObjectNil
        ]
    ) =
      return $ ResponseMessage mid (Left err)

  fromObject
    ( ObjectArray
        [ ObjectWord 2
        , ObjectStr methodName
        , ObjectArray params
        ]
    ) =
      return $ NotificationMessage methodName params

  fromObject other =
    fail $ "Unexpected object:" ++ show other

instance Show Message where
  show (RequestMessage mid method objs) =
      "request(" ++ show mid ++ ") " ++ T.unpack method ++ " " ++ showObjs objs
  show (ResponseMessage mid (Left  obj)) =
      "response error(" ++ show mid ++ ") " ++ showObj obj
  show (ResponseMessage mid (Right obj)) =
      "response(" ++ show mid ++ ") " ++ showObj obj
  show (NotificationMessage method objs) =
      "notification " ++ T.unpack method ++ " " ++ showObjs objs

showObjs :: [Object] -> String
showObjs objs = "[" ++ intercalate "," (map showObj objs) ++ "]"

showObj :: Object -> String
showObj (ObjectWord w)  = "+" ++ show w
showObj (ObjectInt  n)  = show n
showObj ObjectNil       = "nil"
showObj (ObjectBool  b) = show b
showObj (ObjectStr   s) = "\"" ++ T.unpack s ++ "\""
showObj (ObjectArray v) = "[" ++ intercalate "," (map showObj v) ++ "]"
showObj (ObjectMap   m) = "{" ++ intercalate "," (map showPair m) ++ "}"
    where showPair (x, y) = "(" ++ showObj x ++ "," ++ showObj y ++ ")"
showObj (ObjectBin _   ) = error "ObjectBin"
showObj (ObjectExt _ _ ) = error "ObjectExt"
showObj (ObjectFloat  _) = error "ObjectFloat"
showObj (ObjectDouble _) = error "ObjectDouble"
