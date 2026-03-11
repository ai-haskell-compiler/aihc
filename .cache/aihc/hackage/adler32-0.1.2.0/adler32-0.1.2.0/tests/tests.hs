{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import qualified Data.ByteString as B
import Data.Word (Word32)
import Data.Foldable (forM_)
import Data.Monoid ((<>))
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mempty)
#endif

import Data.Digest.Adler32

main :: IO ()
main = hspec $ do
    describe "adler32" $
        forString $ \c s ->
            adler32' s `shouldBe` c

    describe "mempty :: Adler32" $
        it "mempty is the checksum of the empty string" $
            mempty `shouldBe` makeAdler32 1 (0 :: Int)

    describe "<>" $
        forStringPair $ \c1 s1 c2 s2 ->
            (c1 <> c2) `shouldBe` adler32' (s1 <> s2)

    describe "adler32SlideL" $
        forString $ \_ s ->
            forM_ (B.unsnoc s) $ \(s', d2) ->
                forM_ (B.uncons s) $ \(d1, s'') ->
                    adler32SlideL d1 (adler32' s') d2
                        `shouldBe` adler32' s''

    describe "adler32SlideR" $
        forString $ \_ s ->
            forM_ (B.uncons s) $ \(d1, s') ->
                forM_ (B.unsnoc s) $ \(s'', d2) ->
                    adler32SlideR d1 (adler32' s') d2
                        `shouldBe` adler32' s''

    describe "adler32AppendByte" $
        forString $ \c s ->
            forM_ (B.unsnoc s) $ \(s', d) ->
                (adler32' s' `adler32AppendByte` d) `shouldBe` c

    describe "adler32UnAppendByte" $
        forString $ \c s ->
            forM_ (B.unsnoc s) $ \(s', d) ->
                (c `adler32UnAppendByte` d) `shouldBe` adler32' s'

    describe "adler32PrependByte" $
        forString $ \c s ->
            forM_ (B.uncons s) $ \(d, s') ->
                (d `adler32PrependByte` adler32' s') `shouldBe` c

    describe "adler32UnPrependByte" $
        forString $ \c s ->
            forM_ (B.uncons s) $ \(d, s') ->
                (d `adler32UnPrependByte` c) `shouldBe` adler32' s'

    describe "adler32UnAppend" $
        forStringPair $ \c1 s1 c2 s2 ->
            ((adler32' (s1 <> s2)) `adler32UnAppend` c2) `shouldBe` c1

    describe "adler32UnPrepend" $
        forStringPair $ \c1 s1 c2 s2 ->
            (c1 `adler32UnPrepend` (adler32' (s1 <> s2))) `shouldBe` c2

forStringPair
    :: (Adler32 -> B.ByteString -> Adler32 -> B.ByteString -> Expectation)
    -> SpecWith ()
forStringPair action =
    forString $ \c1 s1 ->
        forM_ testStrings $ \(_, (c2, s2)) ->
            action c1 s1 (makeAdler32 c2 (B.length s2)) s2

forString
    :: (Adler32 -> B.ByteString -> Expectation)
    -> SpecWith ()
forString action =
    forM_ testStrings $ \(n, (c, s)) ->
        it ("string " ++ show n) $
            action (makeAdler32 c (B.length s)) s

testStrings :: [(Int, (Word32, B.ByteString))]
testStrings = zip [0..] $
    [ (0x00000001, "")
    , (0x11e60398, "Wikipedia")
    , (0x045d01c1, "test")
    , (0x90860b20, "abcdefghijklmnopqrstuvwxyz")
    , (0x492f072f, "sinless in my crime")
    , (0x0bb80001, B.replicate 3000 0)
    , (0x7732635e, B.replicate 6000 37)
    , (0xe6620ec4, "\223\138p\201\248\NAK\226:\DELt\152)\241_\SYN&\188:\142\169g\192ccfD\214\246(")
    , (0x10591cf2, "?N\ESC\189Z\206a\ETX\139\165y&\173kWPV\235\164\218(\155\240\247KH\165\129\136\SI\218\160]l\176K\230\168\149\161n\135\226\198'\155\253\233\&0d;'\212\153\175`")
    , (0x4c001df4, "\255\246;\224\DLE\169\&3\200J1\155\137\188\171;qI]\GSY\215\202\247\136c\DC4N\185\128\169E\162\181pT\136O\181\GS\169\f\172o\v!\209n\150j>0\204\r\f2^\134\SO(\249$Y\180\"I\167{")
    , (0x45752d45, "Wu\186\CAN\181\174P\227<\210\155\185\202\132\132\221\153\US\193\129\EOT\171\153 zE;\153\ACK\191\227Gc\EMf*\253G\232w\178KV\178\253E\143s\239T-N\185\197\194\211\246!\243\228(\223\NAK\143A\229gh.\155\GS\ETB\149\DC2\SUB\190N-^\140f\NAK\152\SUB\219\STX\138\242\189\161\255")
    ]
