{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-
  These examples are simply provided to show what must compile
-}
module Test.Data.Registry.SimpleExamples where

import Data.Registry
import Data.Text as T (length)
import Protolude hiding (C1)

-- | No typeclass instance is necessary for a "record of functions" to be a Registry component
data Logging = Logging
  { info :: Text -> IO (),
    debug :: Text -> IO ()
  }

logging = make @Logging (fun Logging {info = print, debug = print} +: end)

-- | Simple datatypes which can be used in a registry
newtype Text1 = Text1 Text deriving (Eq, Show)

newtype Text2 = Text2 Text deriving (Eq, Show)

newtype Int1 = Int1 Int deriving (Eq, Show)

-- | values and functions
int1 :: Int
int1 = 1

add1 :: Int -> Text
add1 i = show (i + 1)

add2 :: Int -> Text -> Text1
add2 i j = Text1 (show (i + 1) <> j)

text1 :: Text
text1 = "text1"

toText2 :: Text1 -> Text2
toText2 (Text1 t) = Text2 t

registry1 :: Registry [Text1, Text, Int] [Text2, Text1, Text, Int]
registry1 =
  normalize $
    fun toText2
      <: fun add2
      <: fun text1
      <: fun add1
      <: val int1

countSize :: Text -> Maybe Int
countSize t = Just (T.length t)

made1 :: Text
made1 = make @Text registry1

made2 :: Text1
made2 = make @Text1 registry1

made3 :: Text2
made3 = make @Text2 registry1

--
countSize1 :: Text -> Int1
countSize1 t = Int1 (T.length t)

registry2 =
  fun countSize1
    <: fun add1
    <: fun int1

made4 :: Int1
made4 = make @Int1 registry2

-- | This does *not* compile because Double in not in the
--   list of outputs for registry2

{-
wrong :: Double
wrong = make @Double registry2
-}

-- | This does *not* compile because the list of inputs
--   in registry2 is not included in the list of outputs
unknown :: Double -> Text1
unknown _ = Text1 "text1"

-- | This does not compile because we need a double
--   to make Text1 and it is not in the list of outputs

{-
registry3 :: Registry [Double, Int, Text] [Int, Text1, Text, Int1]
registry3 =
     fun countSize1
  <: fun unknown
  <: fun add1
  <: val int1
-}

-- | Example with an optional configuration
--   The ProductionComponent will only be used if the ServiceConfig says we are in production
newtype ProductionComponent = ProductionComponent {doItInProduction :: IO ()}

data ProductionConfig = ProductionConfig deriving (Eq, Show)

newtype Service = Service {service :: IO ()}

data ServiceConfig = InProd | InDev deriving (Eq, Show)

-- | Production component definition

-- Normal constructor
newProductionComponent :: ProductionConfig -> Logging -> ProductionComponent
newProductionComponent _ _ = ProductionComponent (pure ())

-- Constructor which returns a "builder function", only used if a `ProductionConfig` is available
newProductionComponentBuilder :: Logging -> Tag "builder" (ProductionConfig -> ProductionComponent)
newProductionComponentBuilder = Tag . flip newProductionComponent

-- | Service component definition
--   It uses a builder for the ProductionComponent and only uses it in production
newService :: ServiceConfig -> Tag "builder" (ProductionConfig -> ProductionComponent) -> Service
newService InDev _ = Service {service = print ("dev" :: Text)}
newService InProd f = Service {service = doItInProduction (unTag f ProductionConfig)}

registryWithOptionalComponents =
  fun newService
    <: fun newProductionComponentBuilder
    <: fun logging
    <: val InDev

makeService = make @Service registryWithOptionalComponents
