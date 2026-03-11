{-# language GADTSyntax, PatternSynonyms #-}
-- | Elements in the Protocol Buffers syntax,
--   as defined in <https://developers.google.com/protocol-buffers/docs/reference/proto3-spec>
module Language.ProtocolBuffers.Types where

import qualified Data.Text as T

type Identifier = T.Text
type FullIdentifier = [Identifier]

-- | Whole definition, in which declarations are
--   sorted out by their form.
data ProtoBuf
  = ProtoBuf { syntax   :: Maybe T.Text
             , package  :: Maybe FullIdentifier
             , imports  :: [(ImportType, T.Text)]
             , options  :: [Option]
             , types    :: [TypeDeclaration]
             , services :: [ServiceDeclaration]
             }
  deriving (Eq, Show)

declsToProtoBuf :: [Declaration] -> ProtoBuf
declsToProtoBuf things
  = ProtoBuf (safeHead [s | DSyntax s <- things])
             (safeHead [s | DPackage s <- things])
             [(i,t) | DImport i t <- things]
             [o | DOption o <- things]
             [t | DType t <- things]
             [s | DService s <- things]

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

-- | Declarations, that is, anything which may
--   appear in the top-level.
data Declaration where
  DSyntax  :: T.Text               -> Declaration
  DImport  :: ImportType -> T.Text -> Declaration
  DPackage :: FullIdentifier       -> Declaration
  DOption  :: Option               -> Declaration
  DType    :: TypeDeclaration      -> Declaration
  DService :: ServiceDeclaration   -> Declaration
  deriving (Eq, Show)

data Option where
  Option :: FullIdentifier -> Constant -> Option
  deriving (Eq, Show)

data TypeDeclaration where
  DEnum    :: Identifier -> [Option] -> [EnumField]
           -> TypeDeclaration
  DMessage :: Identifier -> [Option] -> [Reserved]
           -> [MessageField] -> [TypeDeclaration]
           -> TypeDeclaration
  deriving (Eq, Show)

data ServiceDeclaration where
  Service :: Identifier -> [Option] -> [Method] -> ServiceDeclaration
  deriving (Eq, Show)

data Method where
  Method :: Identifier
         -> Repetition -> FieldType
         -> Repetition -> FieldType
         -> [Option] -> Method
  deriving (Eq, Show)

data ImportType
  = Normal | Weak | Public
  deriving (Eq, Show)

data Constant where
  KIdentifier :: FullIdentifier       -> Constant
  KInt        :: Integer              -> Constant
  KFloat      :: Float                -> Constant
  KString     :: T.Text               -> Constant
  KBool       :: Bool                 -> Constant
  KObject     :: [(T.Text, Constant)] -> Constant
  deriving (Eq, Show)

data EnumField where
  EnumField :: FieldName -> FieldNumber -> [Option] -> EnumField
  deriving (Eq, Show)

type TypeName = FullIdentifier
data FieldType
  = TInt32 | TInt64 | TUInt32 | TUInt64 | TSInt32 | TSInt64
  | TFixed32 | TFixed64 | TSFixed32 | TSFixed64
  | TDouble | TBool | TString | TBytes | TOther TypeName
  deriving (Eq, Show)

type FieldName = Identifier
type FieldNumber = Int

data MessageField where
  NormalField :: Repetition -> FieldType
              -> FieldName -> FieldNumber
              -> [Option] -> MessageField
  OneOfField  :: FieldName -> [MessageField]
              -> MessageField
  MapField    :: FieldType -> FieldType
              -> FieldName -> FieldNumber
              -> [Option] -> MessageField
  deriving (Eq, Show)

data Repetition
  = Single | Repeated
  deriving (Eq, Show)

pattern Stream = Repeated

type Reserved = [ReservedValue]
data ReservedValue where
  RInt   :: Int -> ReservedValue
  RRange :: Int -> Int -> ReservedValue
  RName  :: Identifier -> ReservedValue
  deriving (Eq, Show)