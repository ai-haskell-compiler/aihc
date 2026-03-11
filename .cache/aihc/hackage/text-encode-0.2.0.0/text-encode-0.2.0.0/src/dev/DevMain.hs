{-# LANGUAGE OverloadedStrings #-}

module DevMain where

import Text.Encode
import Text.Encode.Aeson (AesonEncoding (..))

import Data.Aeson qualified as Aeson
import Data.Functor (($>))
import Text.Convert (LazyByteString, asString)

data MyType = MyTypeFoo | MyTypeBar | MyTypeFooBar
    deriving stock (Read, Show, Eq, Ord, Bounded, Enum)
    deriving
        (TextEncode)
        via ReadShowEncoding (Cased 'Pascal 'QuietSnake (DropPrefix "MyType" ())) MyType
    deriving
        ( Aeson.FromJSON
        , Aeson.ToJSON
        )
        via ViaTextEncode MyType

instance LazyByteStringPrimitives MyType where
    lazyByteStringEncode = encodeLazyByteString
    lazyByteStringDecode = decodeLazyByteString

data YourType = YourTypeZero | YourTypeOne | YourTypeTwo
    deriving stock (Read, Show, Eq, Ord, Bounded, Enum)
    deriving TextEncode via BoundedEnumEncoding YourType
    deriving (Aeson.FromJSON, Aeson.ToJSON) via ViaTextEncode YourType

instance LazyByteStringPrimitives YourType where
    lazyByteStringEncode = encodeLazyByteString
    lazyByteStringDecode = decodeLazyByteString

newtype TheirType = TheirType String
    deriving stock (Read, Show, Eq, Ord)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via String
    deriving TextEncode via AesonEncoding TheirType

myCases :: [(MyType, LazyByteString, LazyByteString)]
myCases =
    [ let foo = "foo" in (MyTypeFoo, foo, quoted foo)
    , let bar = "bar" in (MyTypeBar, bar, quoted bar)
    , let fooBar = "foo_bar" in (MyTypeFooBar, fooBar, quoted fooBar)
    ]

yourCases :: [(YourType, LazyByteString, LazyByteString)]
yourCases =
    [ let zero = "0" in (YourTypeZero, zero, quoted zero)
    , let one = "1" in (YourTypeOne, one, quoted one)
    , let two = "2" in (YourTypeTwo, two, quoted two)
    ]

theirCases :: [(TheirType, LazyByteString, LazyByteString)]
theirCases =
    [ let hello = "Hello, Aeson!" in (TheirType $ asString hello, quoted hello, quoted hello)
    , let nums = "1234" in (TheirType $ asString nums, quoted nums, quoted nums)
    , let none = "" in (TheirType $ asString none, quoted none, quoted none)
    ]

quoted :: LazyByteString -> LazyByteString
quoted = ("\"" <>) . (<> "\"")

checkCase :: (Eq a, Show a, Aeson.FromJSON a, Aeson.ToJSON a, TextEncode a) => (a, LazyByteString, LazyByteString) -> IO Int
checkCase (dec, enc, enc') = do
    putStr $ take 55 $
        let msg = "Checking:\t" <> show dec <> " <-> " <> show enc
         in if length msg <= 55
                then msg <> repeat ' '
                else take 52 msg <> repeat '.'
    let
        encodingWorks = encodeLazyByteString dec == enc
        decodingWorks = decodeLazyByteString enc == Right dec
        encodingStringWorks = encodeString dec == asString enc
        decodingStringWorks = decodeString (asString enc) == Right dec
        jsonEncodingWorks = Aeson.encode dec == enc'
        jsonDecodingWorks = Aeson.decode enc' == Just dec
        everythingWorks =
            and
                [ jsonEncodingWorks
                , jsonDecodingWorks
                , encodingStringWorks
                , decodingStringWorks
                , encodingWorks
                , decodingWorks
                ]
    if everythingWorks
        then putStrLn "\tPassed." $> 0
        else putStrLn "\tFAILED!" $> 1

main :: IO ()
main = do
    myFailures <- traverse checkCase myCases
    yourFailures <- traverse checkCase yourCases
    theirFailures <- traverse checkCase theirCases
    let failures = sum (myFailures <> yourFailures <> theirFailures)
    if failures == 0
        then putStrLn "All tests passed."
        else do
            putStrLn $ "There were " <> show failures <> " failures."
            error "Test suite failed!"
