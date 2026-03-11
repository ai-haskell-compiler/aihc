{-# LANGUAGE OverloadedStrings #-}

module Data.Text.ENIG.Config where


import Data.Text.ENIG.Data


import Data.Char
import Data.Text (Text)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V

-- | List of the last consonants' code (NFKDed)
--
--   받침의 문자코드 리스트
lastConsonantCodeList :: [Code]
lastConsonantCodeList = [4520..4607]
-- | List of the last consonants
--
--   받침의 문자 리스트
lastConsonantList :: [Char]
lastConsonantList = map chr lastConsonantCodeList

-- | List of the vowel consonants' code (NFKDed)
--
--   모음의 문자코드 리스트
vowelCodeList :: [Code]
vowelCodeList = [4449..4519]
-- | List of the vowel consonants
--
--   모음의 문자 리스트
vowelList :: [Char]
vowelList = map chr vowelCodeList

-- | List of Hangul components' code
--
--   한글 자소 코드 리스트
hangulComponentCodeList :: [Code]
hangulComponentCodeList = [4352..4607]
-- | List of Hangul components
--
--   한글 자소 리스트
hangulComponentList :: [Char]
hangulComponentList = map chr hangulComponentCodeList


-- TODO: Not good generation code. Need to be refactored.
-- | PPP corresponding list
--
--   조사 대응 리스트
pppidVector :: VU.Vector (Code,Code)
pppidVector = VU.fromList
  [(fromEnum Eun,fromEnum Neun)
  ,(fromEnum Eul,fromEnum Leul)
  ,(fromEnum I,fromEnum Ga)
  ,(fromEnum Gwa,fromEnum Wa)
  ,(fromEnum A,fromEnum Ya)
  ,(fromEnum Ix,fromEnum X)
  ,(fromEnum Eux,fromEnum X)
  ]

-- autoPatternList :: V.Vector Text
-- autoPatternList = V.fromList
autoPatternList :: [(Text,PPPCategory,Text)]
autoPatternList =
  [ ("은(는)",EN,"")
  , ("을(를)",EL,"")
  , ("이(가)",IG,"")
  , ("와(과)",WG,"")
  , ("아(야)",AY,"")
  , ("(이)나",IX,"나")
  , ("(이)든",IX,"든")
  , ("(이)란",IX,"란")
  , ("(이)랑",IX,"랑")
  , ("(이)면",IX,"면")
  , ("(이)여",IX,"여")
  , ("(으)로",EuX,"로")
  -- , "(이)다" - Not yet defined
  ]
