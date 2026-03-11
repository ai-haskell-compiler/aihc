module Data.LanguageCodes where
import Prelude hiding (Ordering(..))
 
data ISO639_1 = AA
              | AB
              | AE
              | AF
              | AK
              | AM
              | AN
              | AR
              | AS
              | AV
              | AY
              | AZ
              | BA
              | BE
              | BG
              | BH
              | BI
              | BM
              | BN
              | BO
              | BR
              | BS
              | CA
              | CE
              | CH
              | CO
              | CR
              | CS
              | CU
              | CV
              | CY
              | DA
              | DE
              | DV
              | DZ
              | EE
              | EL
              | EN
              | EO
              | ES
              | ET
              | EU
              | FA
              | FF
              | FI
              | FJ
              | FO
              | FR
              | FY
              | GA
              | GD
              | GL
              | GN
              | GU
              | GV
              | HA
              | HE
              | HI
              | HO
              | HR
              | HT
              | HU
              | HY
              | HZ
              | IA
              | ID
              | IE
              | IG
              | II
              | IK
              | IO
              | IS
              | IT
              | IU
              | JA
              | JV
              | KA
              | KG
              | KI
              | KJ
              | KK
              | KL
              | KM
              | KN
              | KO
              | KR
              | KS
              | KU
              | KV
              | KW
              | KY
              | LA
              | LB
              | LG
              | LI
              | LN
              | LO
              | LT
              | LU
              | LV
              | MG
              | MH
              | MI
              | MK
              | ML
              | MN
              | MR
              | MS
              | MT
              | MY
              | NA
              | NB
              | ND
              | NE
              | NG
              | NL
              | NN
              | NO
              | NR
              | NV
              | NY
              | OC
              | OJ
              | OM
              | OR
              | OS
              | PA
              | PI
              | PL
              | PS
              | PT
              | QU
              | RM
              | RN
              | RO
              | RU
              | RW
              | SA
              | SC
              | SD
              | SE
              | SG
              | SI
              | SK
              | SL
              | SM
              | SN
              | SO
              | SQ
              | SR
              | SS
              | ST
              | SU
              | SV
              | SW
              | TA
              | TE
              | TG
              | TH
              | TI
              | TK
              | TL
              | TN
              | TO
              | TR
              | TS
              | TT
              | TW
              | TY
              | UG
              | UK
              | UR
              | UZ
              | VE
              | VI
              | VO
              | WA
              | WO
              | XH
              | YI
              | YO
              | ZA
              | ZH
              | ZU
              deriving (Show, Read, Eq, Enum, Ord)
toChars (code)
  = case (code) of
        (AA) -> ('a', 'a')
        (AB) -> ('a', 'b')
        (AE) -> ('a', 'e')
        (AF) -> ('a', 'f')
        (AK) -> ('a', 'k')
        (AM) -> ('a', 'm')
        (AN) -> ('a', 'n')
        (AR) -> ('a', 'r')
        (AS) -> ('a', 's')
        (AV) -> ('a', 'v')
        (AY) -> ('a', 'y')
        (AZ) -> ('a', 'z')
        (BA) -> ('b', 'a')
        (BE) -> ('b', 'e')
        (BG) -> ('b', 'g')
        (BH) -> ('b', 'h')
        (BI) -> ('b', 'i')
        (BM) -> ('b', 'm')
        (BN) -> ('b', 'n')
        (BO) -> ('b', 'o')
        (BR) -> ('b', 'r')
        (BS) -> ('b', 's')
        (CA) -> ('c', 'a')
        (CE) -> ('c', 'e')
        (CH) -> ('c', 'h')
        (CO) -> ('c', 'o')
        (CR) -> ('c', 'r')
        (CS) -> ('c', 's')
        (CU) -> ('c', 'u')
        (CV) -> ('c', 'v')
        (CY) -> ('c', 'y')
        (DA) -> ('d', 'a')
        (DE) -> ('d', 'e')
        (DV) -> ('d', 'v')
        (DZ) -> ('d', 'z')
        (EE) -> ('e', 'e')
        (EL) -> ('e', 'l')
        (EN) -> ('e', 'n')
        (EO) -> ('e', 'o')
        (ES) -> ('e', 's')
        (ET) -> ('e', 't')
        (EU) -> ('e', 'u')
        (FA) -> ('f', 'a')
        (FF) -> ('f', 'f')
        (FI) -> ('f', 'i')
        (FJ) -> ('f', 'j')
        (FO) -> ('f', 'o')
        (FR) -> ('f', 'r')
        (FY) -> ('f', 'y')
        (GA) -> ('g', 'a')
        (GD) -> ('g', 'd')
        (GL) -> ('g', 'l')
        (GN) -> ('g', 'n')
        (GU) -> ('g', 'u')
        (GV) -> ('g', 'v')
        (HA) -> ('h', 'a')
        (HE) -> ('h', 'e')
        (HI) -> ('h', 'i')
        (HO) -> ('h', 'o')
        (HR) -> ('h', 'r')
        (HT) -> ('h', 't')
        (HU) -> ('h', 'u')
        (HY) -> ('h', 'y')
        (HZ) -> ('h', 'z')
        (IA) -> ('i', 'a')
        (ID) -> ('i', 'd')
        (IE) -> ('i', 'e')
        (IG) -> ('i', 'g')
        (II) -> ('i', 'i')
        (IK) -> ('i', 'k')
        (IO) -> ('i', 'o')
        (IS) -> ('i', 's')
        (IT) -> ('i', 't')
        (IU) -> ('i', 'u')
        (JA) -> ('j', 'a')
        (JV) -> ('j', 'v')
        (KA) -> ('k', 'a')
        (KG) -> ('k', 'g')
        (KI) -> ('k', 'i')
        (KJ) -> ('k', 'j')
        (KK) -> ('k', 'k')
        (KL) -> ('k', 'l')
        (KM) -> ('k', 'm')
        (KN) -> ('k', 'n')
        (KO) -> ('k', 'o')
        (KR) -> ('k', 'r')
        (KS) -> ('k', 's')
        (KU) -> ('k', 'u')
        (KV) -> ('k', 'v')
        (KW) -> ('k', 'w')
        (KY) -> ('k', 'y')
        (LA) -> ('l', 'a')
        (LB) -> ('l', 'b')
        (LG) -> ('l', 'g')
        (LI) -> ('l', 'i')
        (LN) -> ('l', 'n')
        (LO) -> ('l', 'o')
        (LT) -> ('l', 't')
        (LU) -> ('l', 'u')
        (LV) -> ('l', 'v')
        (MG) -> ('m', 'g')
        (MH) -> ('m', 'h')
        (MI) -> ('m', 'i')
        (MK) -> ('m', 'k')
        (ML) -> ('m', 'l')
        (MN) -> ('m', 'n')
        (MR) -> ('m', 'r')
        (MS) -> ('m', 's')
        (MT) -> ('m', 't')
        (MY) -> ('m', 'y')
        (NA) -> ('n', 'a')
        (NB) -> ('n', 'b')
        (ND) -> ('n', 'd')
        (NE) -> ('n', 'e')
        (NG) -> ('n', 'g')
        (NL) -> ('n', 'l')
        (NN) -> ('n', 'n')
        (NO) -> ('n', 'o')
        (NR) -> ('n', 'r')
        (NV) -> ('n', 'v')
        (NY) -> ('n', 'y')
        (OC) -> ('o', 'c')
        (OJ) -> ('o', 'j')
        (OM) -> ('o', 'm')
        (OR) -> ('o', 'r')
        (OS) -> ('o', 's')
        (PA) -> ('p', 'a')
        (PI) -> ('p', 'i')
        (PL) -> ('p', 'l')
        (PS) -> ('p', 's')
        (PT) -> ('p', 't')
        (QU) -> ('q', 'u')
        (RM) -> ('r', 'm')
        (RN) -> ('r', 'n')
        (RO) -> ('r', 'o')
        (RU) -> ('r', 'u')
        (RW) -> ('r', 'w')
        (SA) -> ('s', 'a')
        (SC) -> ('s', 'c')
        (SD) -> ('s', 'd')
        (SE) -> ('s', 'e')
        (SG) -> ('s', 'g')
        (SI) -> ('s', 'i')
        (SK) -> ('s', 'k')
        (SL) -> ('s', 'l')
        (SM) -> ('s', 'm')
        (SN) -> ('s', 'n')
        (SO) -> ('s', 'o')
        (SQ) -> ('s', 'q')
        (SR) -> ('s', 'r')
        (SS) -> ('s', 's')
        (ST) -> ('s', 't')
        (SU) -> ('s', 'u')
        (SV) -> ('s', 'v')
        (SW) -> ('s', 'w')
        (TA) -> ('t', 'a')
        (TE) -> ('t', 'e')
        (TG) -> ('t', 'g')
        (TH) -> ('t', 'h')
        (TI) -> ('t', 'i')
        (TK) -> ('t', 'k')
        (TL) -> ('t', 'l')
        (TN) -> ('t', 'n')
        (TO) -> ('t', 'o')
        (TR) -> ('t', 'r')
        (TS) -> ('t', 's')
        (TT) -> ('t', 't')
        (TW) -> ('t', 'w')
        (TY) -> ('t', 'y')
        (UG) -> ('u', 'g')
        (UK) -> ('u', 'k')
        (UR) -> ('u', 'r')
        (UZ) -> ('u', 'z')
        (VE) -> ('v', 'e')
        (VI) -> ('v', 'i')
        (VO) -> ('v', 'o')
        (WA) -> ('w', 'a')
        (WO) -> ('w', 'o')
        (XH) -> ('x', 'h')
        (YI) -> ('y', 'i')
        (YO) -> ('y', 'o')
        (ZA) -> ('z', 'a')
        (ZH) -> ('z', 'h')
        (ZU) -> ('z', 'u')
fromChars (c1) (c2)
  = case ((c1), (c2)) of
        ('z', 'u') -> (Just) (ZU)
        ('z', 'h') -> (Just) (ZH)
        ('z', 'a') -> (Just) (ZA)
        ('y', 'o') -> (Just) (YO)
        ('y', 'i') -> (Just) (YI)
        ('x', 'h') -> (Just) (XH)
        ('w', 'o') -> (Just) (WO)
        ('w', 'a') -> (Just) (WA)
        ('v', 'o') -> (Just) (VO)
        ('v', 'i') -> (Just) (VI)
        ('v', 'e') -> (Just) (VE)
        ('u', 'z') -> (Just) (UZ)
        ('u', 'r') -> (Just) (UR)
        ('u', 'k') -> (Just) (UK)
        ('u', 'g') -> (Just) (UG)
        ('t', 'y') -> (Just) (TY)
        ('t', 'w') -> (Just) (TW)
        ('t', 't') -> (Just) (TT)
        ('t', 's') -> (Just) (TS)
        ('t', 'r') -> (Just) (TR)
        ('t', 'o') -> (Just) (TO)
        ('t', 'n') -> (Just) (TN)
        ('t', 'l') -> (Just) (TL)
        ('t', 'k') -> (Just) (TK)
        ('t', 'i') -> (Just) (TI)
        ('t', 'h') -> (Just) (TH)
        ('t', 'g') -> (Just) (TG)
        ('t', 'e') -> (Just) (TE)
        ('t', 'a') -> (Just) (TA)
        ('s', 'w') -> (Just) (SW)
        ('s', 'v') -> (Just) (SV)
        ('s', 'u') -> (Just) (SU)
        ('s', 't') -> (Just) (ST)
        ('s', 's') -> (Just) (SS)
        ('s', 'r') -> (Just) (SR)
        ('s', 'q') -> (Just) (SQ)
        ('s', 'o') -> (Just) (SO)
        ('s', 'n') -> (Just) (SN)
        ('s', 'm') -> (Just) (SM)
        ('s', 'l') -> (Just) (SL)
        ('s', 'k') -> (Just) (SK)
        ('s', 'i') -> (Just) (SI)
        ('s', 'g') -> (Just) (SG)
        ('s', 'e') -> (Just) (SE)
        ('s', 'd') -> (Just) (SD)
        ('s', 'c') -> (Just) (SC)
        ('s', 'a') -> (Just) (SA)
        ('r', 'w') -> (Just) (RW)
        ('r', 'u') -> (Just) (RU)
        ('r', 'o') -> (Just) (RO)
        ('r', 'n') -> (Just) (RN)
        ('r', 'm') -> (Just) (RM)
        ('q', 'u') -> (Just) (QU)
        ('p', 't') -> (Just) (PT)
        ('p', 's') -> (Just) (PS)
        ('p', 'l') -> (Just) (PL)
        ('p', 'i') -> (Just) (PI)
        ('p', 'a') -> (Just) (PA)
        ('o', 's') -> (Just) (OS)
        ('o', 'r') -> (Just) (OR)
        ('o', 'm') -> (Just) (OM)
        ('o', 'j') -> (Just) (OJ)
        ('o', 'c') -> (Just) (OC)
        ('n', 'y') -> (Just) (NY)
        ('n', 'v') -> (Just) (NV)
        ('n', 'r') -> (Just) (NR)
        ('n', 'o') -> (Just) (NO)
        ('n', 'n') -> (Just) (NN)
        ('n', 'l') -> (Just) (NL)
        ('n', 'g') -> (Just) (NG)
        ('n', 'e') -> (Just) (NE)
        ('n', 'd') -> (Just) (ND)
        ('n', 'b') -> (Just) (NB)
        ('n', 'a') -> (Just) (NA)
        ('m', 'y') -> (Just) (MY)
        ('m', 't') -> (Just) (MT)
        ('m', 's') -> (Just) (MS)
        ('m', 'r') -> (Just) (MR)
        ('m', 'n') -> (Just) (MN)
        ('m', 'l') -> (Just) (ML)
        ('m', 'k') -> (Just) (MK)
        ('m', 'i') -> (Just) (MI)
        ('m', 'h') -> (Just) (MH)
        ('m', 'g') -> (Just) (MG)
        ('l', 'v') -> (Just) (LV)
        ('l', 'u') -> (Just) (LU)
        ('l', 't') -> (Just) (LT)
        ('l', 'o') -> (Just) (LO)
        ('l', 'n') -> (Just) (LN)
        ('l', 'i') -> (Just) (LI)
        ('l', 'g') -> (Just) (LG)
        ('l', 'b') -> (Just) (LB)
        ('l', 'a') -> (Just) (LA)
        ('k', 'y') -> (Just) (KY)
        ('k', 'w') -> (Just) (KW)
        ('k', 'v') -> (Just) (KV)
        ('k', 'u') -> (Just) (KU)
        ('k', 's') -> (Just) (KS)
        ('k', 'r') -> (Just) (KR)
        ('k', 'o') -> (Just) (KO)
        ('k', 'n') -> (Just) (KN)
        ('k', 'm') -> (Just) (KM)
        ('k', 'l') -> (Just) (KL)
        ('k', 'k') -> (Just) (KK)
        ('k', 'j') -> (Just) (KJ)
        ('k', 'i') -> (Just) (KI)
        ('k', 'g') -> (Just) (KG)
        ('k', 'a') -> (Just) (KA)
        ('j', 'v') -> (Just) (JV)
        ('j', 'a') -> (Just) (JA)
        ('i', 'u') -> (Just) (IU)
        ('i', 't') -> (Just) (IT)
        ('i', 's') -> (Just) (IS)
        ('i', 'o') -> (Just) (IO)
        ('i', 'k') -> (Just) (IK)
        ('i', 'i') -> (Just) (II)
        ('i', 'g') -> (Just) (IG)
        ('i', 'e') -> (Just) (IE)
        ('i', 'd') -> (Just) (ID)
        ('i', 'a') -> (Just) (IA)
        ('h', 'z') -> (Just) (HZ)
        ('h', 'y') -> (Just) (HY)
        ('h', 'u') -> (Just) (HU)
        ('h', 't') -> (Just) (HT)
        ('h', 'r') -> (Just) (HR)
        ('h', 'o') -> (Just) (HO)
        ('h', 'i') -> (Just) (HI)
        ('h', 'e') -> (Just) (HE)
        ('h', 'a') -> (Just) (HA)
        ('g', 'v') -> (Just) (GV)
        ('g', 'u') -> (Just) (GU)
        ('g', 'n') -> (Just) (GN)
        ('g', 'l') -> (Just) (GL)
        ('g', 'd') -> (Just) (GD)
        ('g', 'a') -> (Just) (GA)
        ('f', 'y') -> (Just) (FY)
        ('f', 'r') -> (Just) (FR)
        ('f', 'o') -> (Just) (FO)
        ('f', 'j') -> (Just) (FJ)
        ('f', 'i') -> (Just) (FI)
        ('f', 'f') -> (Just) (FF)
        ('f', 'a') -> (Just) (FA)
        ('e', 'u') -> (Just) (EU)
        ('e', 't') -> (Just) (ET)
        ('e', 's') -> (Just) (ES)
        ('e', 'o') -> (Just) (EO)
        ('e', 'n') -> (Just) (EN)
        ('e', 'l') -> (Just) (EL)
        ('e', 'e') -> (Just) (EE)
        ('d', 'z') -> (Just) (DZ)
        ('d', 'v') -> (Just) (DV)
        ('d', 'e') -> (Just) (DE)
        ('d', 'a') -> (Just) (DA)
        ('c', 'y') -> (Just) (CY)
        ('c', 'v') -> (Just) (CV)
        ('c', 'u') -> (Just) (CU)
        ('c', 's') -> (Just) (CS)
        ('c', 'r') -> (Just) (CR)
        ('c', 'o') -> (Just) (CO)
        ('c', 'h') -> (Just) (CH)
        ('c', 'e') -> (Just) (CE)
        ('c', 'a') -> (Just) (CA)
        ('b', 's') -> (Just) (BS)
        ('b', 'r') -> (Just) (BR)
        ('b', 'o') -> (Just) (BO)
        ('b', 'n') -> (Just) (BN)
        ('b', 'm') -> (Just) (BM)
        ('b', 'i') -> (Just) (BI)
        ('b', 'h') -> (Just) (BH)
        ('b', 'g') -> (Just) (BG)
        ('b', 'e') -> (Just) (BE)
        ('b', 'a') -> (Just) (BA)
        ('a', 'z') -> (Just) (AZ)
        ('a', 'y') -> (Just) (AY)
        ('a', 'v') -> (Just) (AV)
        ('a', 's') -> (Just) (AS)
        ('a', 'r') -> (Just) (AR)
        ('a', 'n') -> (Just) (AN)
        ('a', 'm') -> (Just) (AM)
        ('a', 'k') -> (Just) (AK)
        ('a', 'f') -> (Just) (AF)
        ('a', 'e') -> (Just) (AE)
        ('a', 'b') -> (Just) (AB)
        ('a', 'a') -> (Just) (AA)
        _ -> (Nothing)
language (code)
  = case (code) of
        (AA) -> "Afar"
        (AB) -> "Abkhazian"
        (AE) -> "Avestan"
        (AF) -> "Afrikaans"
        (AK) -> "Akan"
        (AM) -> "Amharic"
        (AN) -> "Aragonese"
        (AR) -> "Arabic"
        (AS) -> "Assamese"
        (AV) -> "Avaric"
        (AY) -> "Aymara"
        (AZ) -> "Azerbaijani"
        (BA) -> "Bashkir"
        (BE) -> "Belarusian"
        (BG) -> "Bulgarian"
        (BH) -> "Bihari languages"
        (BI) -> "Bislama"
        (BM) -> "Bambara"
        (BN) -> "Bengali"
        (BO) -> "Tibetan"
        (BR) -> "Breton"
        (BS) -> "Bosnian"
        (CA) -> "Catalan"
        (CE) -> "Chechen"
        (CH) -> "Chamorro"
        (CO) -> "Corsican"
        (CR) -> "Cree"
        (CS) -> "Czech"
        (CU) -> "Church Slavic"
        (CV) -> "Chuvash"
        (CY) -> "Welsh"
        (DA) -> "Danish"
        (DE) -> "German"
        (DV) -> "Dhivehi"
        (DZ) -> "Dzongkha"
        (EE) -> "Ewe"
        (EL) -> "Greek, Modern (1453-)"
        (EN) -> "English"
        (EO) -> "Esperanto"
        (ES) -> "Castilian"
        (ET) -> "Estonian"
        (EU) -> "Basque"
        (FA) -> "Persian"
        (FF) -> "Fulah"
        (FI) -> "Finnish"
        (FJ) -> "Fijian"
        (FO) -> "Faroese"
        (FR) -> "French"
        (FY) -> "Western Frisian"
        (GA) -> "Irish"
        (GD) -> "Gaelic"
        (GL) -> "Galician"
        (GN) -> "Guarani"
        (GU) -> "Gujarati"
        (GV) -> "Manx"
        (HA) -> "Hausa"
        (HE) -> "Hebrew"
        (HI) -> "Hindi"
        (HO) -> "Hiri Motu"
        (HR) -> "Croatian"
        (HT) -> "Haitian"
        (HU) -> "Hungarian"
        (HY) -> "Armenian"
        (HZ) -> "Herero"
        (IA) -> "Interlingua (International Auxiliary Language Association)"
        (ID) -> "Indonesian"
        (IE) -> "Interlingue"
        (IG) -> "Igbo"
        (II) -> "Nuosu"
        (IK) -> "Inupiaq"
        (IO) -> "Ido"
        (IS) -> "Icelandic"
        (IT) -> "Italian"
        (IU) -> "Inuktitut"
        (JA) -> "Japanese"
        (JV) -> "Javanese"
        (KA) -> "Georgian"
        (KG) -> "Kongo"
        (KI) -> "Gikuyu"
        (KJ) -> "Kuanyama"
        (KK) -> "Kazakh"
        (KL) -> "Greenlandic"
        (KM) -> "Central Khmer"
        (KN) -> "Kannada"
        (KO) -> "Korean"
        (KR) -> "Kanuri"
        (KS) -> "Kashmiri"
        (KU) -> "Kurdish"
        (KV) -> "Komi"
        (KW) -> "Cornish"
        (KY) -> "Kirghiz"
        (LA) -> "Latin"
        (LB) -> "Letzeburgesch"
        (LG) -> "Ganda"
        (LI) -> "Limburgan"
        (LN) -> "Lingala"
        (LO) -> "Lao"
        (LT) -> "Lithuanian"
        (LU) -> "Luba-Katanga"
        (LV) -> "Latvian"
        (MG) -> "Malagasy"
        (MH) -> "Marshallese"
        (MI) -> "Maori"
        (MK) -> "Macedonian"
        (ML) -> "Malayalam"
        (MN) -> "Mongolian"
        (MR) -> "Marathi"
        (MS) -> "Malay"
        (MT) -> "Maltese"
        (MY) -> "Burmese"
        (NA) -> "Nauru"
        (NB) -> "Bokm\229l, Norwegian"
        (ND) -> "Ndebele, North"
        (NE) -> "Nepali"
        (NG) -> "Ndonga"
        (NL) -> "Dutch"
        (NN) -> "Norwegian Nynorsk"
        (NO) -> "Norwegian"
        (NR) -> "Ndebele, South"
        (NV) -> "Navaho"
        (NY) -> "Chewa"
        (OC) -> "Occitan (post 1500)"
        (OJ) -> "Ojibwa"
        (OM) -> "Oromo"
        (OR) -> "Oriya"
        (OS) -> "Ossetian"
        (PA) -> "Panjabi"
        (PI) -> "Pali"
        (PL) -> "Polish"
        (PS) -> "Pashto"
        (PT) -> "Portuguese"
        (QU) -> "Quechua"
        (RM) -> "Romansh"
        (RN) -> "Rundi"
        (RO) -> "Moldavian"
        (RU) -> "Russian"
        (RW) -> "Kinyarwanda"
        (SA) -> "Sanskrit"
        (SC) -> "Sardinian"
        (SD) -> "Sindhi"
        (SE) -> "Northern Sami"
        (SG) -> "Sango"
        (SI) -> "Sinhala"
        (SK) -> "Slovak"
        (SL) -> "Slovenian"
        (SM) -> "Samoan"
        (SN) -> "Shona"
        (SO) -> "Somali"
        (SQ) -> "Albanian"
        (SR) -> "Serbian"
        (SS) -> "Swati"
        (ST) -> "Sotho, Southern"
        (SU) -> "Sundanese"
        (SV) -> "Swedish"
        (SW) -> "Swahili"
        (TA) -> "Tamil"
        (TE) -> "Telugu"
        (TG) -> "Tajik"
        (TH) -> "Thai"
        (TI) -> "Tigrinya"
        (TK) -> "Turkmen"
        (TL) -> "Tagalog"
        (TN) -> "Tswana"
        (TO) -> "Tonga (Tonga Islands)"
        (TR) -> "Turkish"
        (TS) -> "Tsonga"
        (TT) -> "Tatar"
        (TW) -> "Twi"
        (TY) -> "Tahitian"
        (UG) -> "Uighur"
        (UK) -> "Ukrainian"
        (UR) -> "Urdu"
        (UZ) -> "Uzbek"
        (VE) -> "Venda"
        (VI) -> "Vietnamese"
        (VO) -> "Volap\252k"
        (WA) -> "Walloon"
        (WO) -> "Wolof"
        (XH) -> "Xhosa"
        (YI) -> "Yiddish"
        (YO) -> "Yoruba"
        (ZA) -> "Chuang"
        (ZH) -> "Chinese"
        (ZU) -> "Zulu"