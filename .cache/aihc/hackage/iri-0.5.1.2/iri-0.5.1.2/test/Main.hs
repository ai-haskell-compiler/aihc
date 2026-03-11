module Main where

import qualified Data.Text as T
import qualified Iri.Parsing.ByteString as D
import qualified Iri.Parsing.Text as F
import Iri.QuasiQuoter
import qualified Iri.Rendering.ByteString as E
import qualified Iri.Rendering.Text as C
import Test.Tasty
import Test.Tasty.HUnit
import Prelude

main :: IO ()
main =
  defaultMain
    $ testGroup "All tests"
    $ [ testCase "No path"
          $ assertEqual
            ""
            (Right "https://ru.wikipedia.org?query")
            ( fmap
                C.iri
                (D.uri "https://ru.wikipedia.org?query")
            ),
        testCase "Cyrillic path" $ do
          assertEqual
            ""
            (Right "https://ru.wikipedia.org/wiki/Баренцбург")
            ( fmap
                C.iri
                (D.uri "https://ru.wikipedia.org/wiki/%D0%91%D0%B0%D1%80%D0%B5%D0%BD%D1%86%D0%B1%D1%83%D1%80%D0%B3")
            ),
        testCase "Complex query" $ do
          assertEqual
            ""
            (Right "https://news.yandex.ru/yandsearch?cl4url=iz.ru/669426/2017-11-10/smi-inoagenty-obiazhut-soobshchat-ob-etom-v-sotcsetiakh&lang=ru&from=main_portal&stid=5bdMOjSopWu34pdT-391&lr=213&msid=1510328052.50316.22874.7658&mlid=1510327228.glob_225.904af5bb")
            ( fmap
                C.iri
                (D.uri "https://news.yandex.ru/yandsearch?cl4url=iz.ru/669426/2017-11-10/smi-inoagenty-obiazhut-soobshchat-ob-etom-v-sotcsetiakh&lang=ru&from=main_portal&stid=5bdMOjSopWu34pdT-391&lr=213&msid=1510328052.50316.22874.7658&mlid=1510327228.glob_225.904af5bb")
            ),
        testCase "Rendering"
          $ let inputs =
                  [ "https://ru.wikipedia.org/wiki/%D0%91%D0%B0%D1%80%D0%B5%D0%BD%D1%86%D0%B1%D1%83%D1%80%D0%B3",
                    "https://news.yandex.ru/yandsearch?cl4url=iz.ru/669426/2017-11-10/smi-inoagenty-obiazhut-soobshchat-ob-etom-v-sotcsetiakh&lang=ru&from=main_portal&stid=5bdMOjSopWu34pdT-391&lr=213&msid=1510328052.50316.22874.7658&mlid=1510327228.glob_225.904af5bb"
                  ]
             in forM_ inputs $ \input ->
                  assertEqual "" (Right input) (fmap E.uri (D.uri input)),
        testCase "about:blank" $ assertEqual "" "about:blank" (E.uri [uri|about:blank|]),
        testCase "Empty hierarchy" $ assertEqual "" "about:" (E.uri [uri|about:|]),
        testCase "HTTP URI Text Rendering"
          $ assertEqual
            ""
            "https://ru.wikipedia.org/wiki/Баренцбург"
            (C.httpIri [httpUri|https://ru.wikipedia.org/wiki/%D0%91%D0%B0%D1%80%D0%B5%D0%BD%D1%86%D0%B1%D1%83%D1%80%D0%B3|]),
        testCase "HTTP URI Show"
          $ assertEqual
            ""
            "\"https://ru.wikipedia.org/wiki/Баренцбург\""
            (show [httpUri|https://ru.wikipedia.org/wiki/%D0%91%D0%B0%D1%80%D0%B5%D0%BD%D1%86%D0%B1%D1%83%D1%80%D0%B3|]),
        testCase "Messed up URIs"
          $ let testUri uri =
                  assertEqual "Original and parsed URIs are not equal" (Right uri) (fmap E.uri (D.uri uri))
             in traverse_
                  testUri
                  [ "http://m.ru.hellomagazine.com/krasota-i-zdorove/ukhod-za-kozhey-i-volosami/11373-5-prichesok-kotorye-sdelayut-vas-molozhe.html?utm_medium=teaser&utm_campaign=gnezdo1&utm_term=15376&utm_content=[TID",
                    "https://m.tsargrad.tv/news/komediju_113156?%90\"%20"
                  ],
        testCase "User Info"
          $ assertEqual "" (Right "http://user:password@localhost:993") (fmap E.uri (D.uri "http://user:password@localhost:993")),
        testGroup "IRI parser vs. URI parser"
          $ let testUriIri iri uri =
                  testCase (iri <> " vs. " <> uri)
                    $ assertEqual "" (F.iri (fromString iri)) (D.uri (fromString uri))
             in [ testUriIri
                    "https://ru.wikipedia.org/wiki/Баренцбург"
                    "https://ru.wikipedia.org/wiki/%D0%91%D0%B0%D1%80%D0%B5%D0%BD%D1%86%D0%B1%D1%83%D1%80%D0%B3",
                  testUriIri
                    "https://www.w3.org/International/articles/idn-and-iri/JP納豆/引き割り納豆.html"
                    "https://www.w3.org/International/articles/idn-and-iri/JP%E7%B4%8D%E8%B1%86/%E5%BC%95%E3%81%8D%E5%89%B2%E3%82%8A%E7%B4%8D%E8%B1%86.html",
                  testUriIri
                    "http://点心和烤鸭.w3.mag.keio.ac.jp"
                    "http://xn--0trv4xfvn8el34t.w3.mag.keio.ac.jp/"
                ],
        testCase "Hex digit parsing bug reproduction - lowercase hex digits"
          $ do
            -- Bug: lowercase hex digits 'a'-'f' were calculated as (ascii - 97) instead of (ascii - 87)
            -- This caused %6a (should be ASCII 106 = 'j') to be parsed incorrectly
            -- %6a = 6*16 + 10 = 106 = 'j', but with bug: 6*16 + 0 = 96 = '`'
            let result = F.iri "https://example.com/test%6a" -- should be 'j'
            case result of
              Left err -> assertFailure $ "Should parse successfully: " ++ T.unpack err
              Right iri ->
                let rendered = C.iri iri -- Use text rendering to see decoded content
                 in assertBool "Should contain 'j' when correctly parsed" ('j' `T.elem` rendered),
        testCase "Lowercase hex digits a-f UTF-8 parsing"
          $ do
            -- Test that %c3%a9 (UTF-8 for é) parses correctly with lowercase hex
            -- This tests the fix for hex digits 'c' and 'a' which were affected by the bug
            let result = F.iri "https://example.com/caf%c3%a9"
            case result of
              Left _ -> assertFailure "Should parse UTF-8 encoded URL successfully"
              Right iri ->
                let rendered = C.iri iri -- Use text rendering to see the decoded UTF-8
                 in assertBool "Should contain é character when correctly decoded" ('é' `T.elem` rendered),
        testGroup "Mess"
          $ [ testCase "1"
                $ assertEqual
                  ""
                  (Right "https://www.kupivip.ru/catalog/muzhchinam/odezhda?srcid=goo-tm-open&lpu=|source:google|medium:cpc|adposition:1t1|creative:195388077752|term:%2B%D0%B2%D0%B8%D0%BF%20%2B%D0%BA%D1%83%D0%BF%D0%B8|campaignid:801742449|campaign:goo_tm_mob_s_male|other:m|ad:1|&gclid=CjwKEAjwgb3OBRDNi_2TwZ6u7D4SJADsmW8QKiRxDZZ0hRAnyspAcVW582c7zzQSA7CC0FavPR63dBoCrKPw_wcB")
                  (fmap E.uri (D.uri "https://www.kupivip.ru/catalog/muzhchinam/odezhda?srcid=goo-tm-open&lpu=|source:google|medium:cpc|adposition:1t1|creative:195388077752|term:%2B%D0%B2%D0%B8%D0%BF%20%2B%D0%BA%D1%83%D0%BF%D0%B8|campaignid:801742449|campaign:goo_tm_mob_s_male|other:m|ad:1|&gclid=CjwKEAjwgb3OBRDNi_2TwZ6u7D4SJADsmW8QKiRxDZZ0hRAnyspAcVW582c7zzQSA7CC0FavPR63dBoCrKPw_wcB")),
              testGroup "2"
                $ [ testCase "show"
                      $ assertEqual
                        ""
                        (Right "\"https://www.pleer.ru/search_%e4%eb%ff+%e1%e5%eb%fc%ff+%f1%f3%f8%e8%eb%ea%e0.html\"")
                        (fmap show (D.uri "https://www.pleer.ru/search_%e4%eb%ff+%e1%e5%eb%fc%ff+%f1%f3%f8%e8%eb%ea%e0.html")),
                    testCase "Text rendering"
                      $ assertEqual
                        ""
                        (Right "https://www.pleer.ru/search_%e4%eb%ff+%e1%e5%eb%fc%ff+%f1%f3%f8%e8%eb%ea%e0.html")
                        (fmap C.iri (D.uri "https://www.pleer.ru/search_%e4%eb%ff+%e1%e5%eb%fc%ff+%f1%f3%f8%e8%eb%ea%e0.html"))
                  ]
            ]
      ]
