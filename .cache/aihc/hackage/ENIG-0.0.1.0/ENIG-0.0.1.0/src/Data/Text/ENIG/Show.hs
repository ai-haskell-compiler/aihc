{-# LANGUAGE OverloadedStrings #-}

module Data.Text.ENIG.Show where

import Data.Text.ENIG.Data

import Data.Text (Text)
import qualified Data.Text as T


-- | Show function for 'PPPCategory' as 'Text'
tShowPPPCa :: PPPCategory -> Text
tShowPPPCa EN  = "은(는)"
tShowPPPCa EL  = "을(를)"
tShowPPPCa IG  = "이(가)"
tShowPPPCa WG  = "와(과)"
tShowPPPCa AY  = "아(야)"
tShowPPPCa IX  = "(이)"
tShowPPPCa EuX = "(으)"

-- | Show function for 'PPPCategory' as 'String'
sShowPPPCa :: PPPCategory -> String
sShowPPPCa = T.unpack . tShowPPPCa

-- | Show function for 'PPPIdentity' as 'Text'
tShowPPPId :: PPPIdentity -> Text
tShowPPPId Eun  = "은"
tShowPPPId Neun = "는"
tShowPPPId Eul  = "을"
tShowPPPId Leul = "를"
tShowPPPId I    = "이"
tShowPPPId Ga   = "가"
tShowPPPId Wa   = "와"
tShowPPPId Gwa  = "과"
tShowPPPId A    = "아"
tShowPPPId Ya   = "야"
tShowPPPId Ix   = "이"
tShowPPPId Eux  = "으"
tShowPPPId X    = ""

-- | Show function for 'PPPIdentity' as 'String'
sShowPPPId :: PPPIdentity -> String
sShowPPPId = T.unpack . tShowPPPId
