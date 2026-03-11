module Graphics.Gnuplot.ColorSpecification (
   T,
   rgb,
   rgb8,
   name,
   paletteFrac,

   white,
   black,
   gray0,
   grey0,
   gray10,
   grey10,
   gray20,
   grey20,
   gray30,
   grey30,
   gray40,
   grey40,
   gray50,
   grey50,
   gray60,
   grey60,
   gray70,
   grey70,
   gray80,
   grey80,
   gray90,
   grey90,
   gray100,
   grey100,
   gray,
   grey,
   lightGray,
   lightGrey,
   darkGray,
   darkGrey,
   red,
   lightRed,
   darkRed,
   yellow,
   lightYellow,
   darkYellow,
   green,
   lightGreen,
   darkGreen,
   springGreen,
   forestGreen,
   seaGreen,
   blue,
   lightBlue,
   darkBlue,
   midnightBlue,
   navy,
   mediumBlue,
   royalblue,
   skyblue,
   cyan,
   lightCyan,
   darkCyan,
   magenta,
   lightMagenta,
   darkMagenta,
   turquoise,
   lightTurquoise,
   darkTurquoise,
   pink,
   lightPink,
   darkPink,
   coral,
   lightCoral,
   orangeRed,
   salmon,
   lightSalmon,
   darkSalmon,
   aquamarine,
   khaki,
   darkKhaki,
   goldenrod,
   lightGoldenrod,
   darkGoldenrod,
   gold,
   beige,
   brown,
   orange,
   darkOrange,
   violet,
   darkViolet,
   plum,
   purple,

   ) where

import qualified Graphics.Gnuplot.Private.ColorSpecification as Color
import Graphics.Gnuplot.Private.ColorSpecification (T, )
import Data.Ord.HT (limit, )
import Data.Word (Word8, )


{- |
Color components for Red, Green, Blue, must be in the range @[0,1]@.
-}
rgb :: Double -> Double -> Double -> T
rgb r g b =
   rgb8 (word8FromDouble r) (word8FromDouble g) (word8FromDouble b)

word8FromDouble :: Double -> Word8
word8FromDouble c =
   round (limit (0,1) c * 255)

rgb8 :: Word8 -> Word8 -> Word8 -> T
rgb8 = Color.RGB8

paletteFrac :: Double -> T
paletteFrac = Color.PaletteFrac

{- |
Specify a color by name.
You are responsible for choosing an existing name in gnuplot.
If your color is available as Haskell variable in this module,
then prefer this one.
-}
name :: String -> T
name = Color.Name


{-
This function chooses how we represent below colors in gnuplot.
If we use RGB values then we stay independent from the available color names
in the current gnuplot version.
If we use color names, then the generated gnuplot scripts are more readable.
-}
makeColor :: String -> Int -> Word8 -> Word8 -> Word8 -> T
makeColor str _ _ _ _ = name str


white,
 black,
 gray0,
 grey0,
 gray10,
 grey10,
 gray20,
 grey20,
 gray30,
 grey30,
 gray40,
 grey40,
 gray50,
 grey50,
 gray60,
 grey60,
 gray70,
 grey70,
 gray80,
 grey80,
 gray90,
 grey90,
 gray100,
 grey100,
 gray,
 grey,
 lightGray,
 lightGrey,
 darkGray,
 darkGrey,
 red,
 lightRed,
 darkRed,
 yellow,
 lightYellow,
 darkYellow,
 green,
 lightGreen,
 darkGreen,
 springGreen,
 forestGreen,
 seaGreen,
 blue,
 lightBlue,
 darkBlue,
 midnightBlue,
 navy,
 mediumBlue,
 royalblue,
 skyblue,
 cyan,
 lightCyan,
 darkCyan,
 magenta,
 lightMagenta,
 darkMagenta,
 turquoise,
 lightTurquoise,
 darkTurquoise,
 pink,
 lightPink,
 darkPink,
 coral,
 lightCoral,
 orangeRed,
 salmon,
 lightSalmon,
 darkSalmon,
 aquamarine,
 khaki,
 darkKhaki,
 goldenrod,
 lightGoldenrod,
 darkGoldenrod,
 gold,
 beige,
 brown,
 orange,
 darkOrange,
 violet,
 darkViolet,
 plum,
 purple :: T

{-
gnuplot> show colornames
-}

white             = makeColor "white"              0xffffff 255 255 255
black             = makeColor "black"              0x000000   0   0   0
gray0             = makeColor "gray0"              0x000000   0   0   0
grey0             = makeColor "grey0"              0x000000   0   0   0
gray10            = makeColor "gray10"             0x1a1a1a  26  26  26
grey10            = makeColor "grey10"             0x1a1a1a  26  26  26
gray20            = makeColor "gray20"             0x333333  51  51  51
grey20            = makeColor "grey20"             0x333333  51  51  51
gray30            = makeColor "gray30"             0x4d4d4d  77  77  77
grey30            = makeColor "grey30"             0x4d4d4d  77  77  77
gray40            = makeColor "gray40"             0x666666 102 102 102
grey40            = makeColor "grey40"             0x666666 102 102 102
gray50            = makeColor "gray50"             0x7f7f7f 127 127 127
grey50            = makeColor "grey50"             0x7f7f7f 127 127 127
gray60            = makeColor "gray60"             0x999999 153 153 153
grey60            = makeColor "grey60"             0x999999 153 153 153
gray70            = makeColor "gray70"             0xb3b3b3 179 179 179
grey70            = makeColor "grey70"             0xb3b3b3 179 179 179
gray80            = makeColor "gray80"             0xcccccc 204 204 204
grey80            = makeColor "grey80"             0xcccccc 204 204 204
gray90            = makeColor "gray90"             0xe5e5e5 229 229 229
grey90            = makeColor "grey90"             0xe5e5e5 229 229 229
gray100           = makeColor "gray100"            0xffffff 255 255 255
grey100           = makeColor "grey100"            0xffffff 255 255 255
gray              = makeColor "gray"               0xbebebe 190 190 190
grey              = makeColor "grey"               0xbebebe 190 190 190
lightGray         = makeColor "light-gray"         0xd3d3d3 211 211 211
lightGrey         = makeColor "light-grey"         0xd3d3d3 211 211 211
darkGray          = makeColor "dark-gray"          0xa9a9a9 169 169 169
darkGrey          = makeColor "dark-grey"          0xa9a9a9 169 169 169
red               = makeColor "red"                0xff0000 255   0   0
lightRed          = makeColor "light-red"          0xf03232 240  50  50
darkRed           = makeColor "dark-red"           0x8b0000 139   0   0
yellow            = makeColor "yellow"             0xffff00 255 255   0
lightYellow       = makeColor "light-yellow"       0xffffe0 255 255 224
darkYellow        = makeColor "dark-yellow"        0xc8c800 200 200   0
green             = makeColor "green"              0x00ff00   0 255   0
lightGreen        = makeColor "light-green"        0x90ee90 144 238 144
darkGreen         = makeColor "dark-green"         0x006400   0 100   0
springGreen       = makeColor "spring-green"       0x00ff7f   0 255 127
forestGreen       = makeColor "forest-green"       0x228b22  34 139  34
seaGreen          = makeColor "sea-green"          0x2e8b57  46 139  87
blue              = makeColor "blue"               0x0000ff   0   0 255
lightBlue         = makeColor "light-blue"         0xadd8e6 173 216 230
darkBlue          = makeColor "dark-blue"          0x00008b   0   0 139
midnightBlue      = makeColor "midnight-blue"      0x191970  25  25 112
navy              = makeColor "navy"               0x000080   0   0 128
mediumBlue        = makeColor "medium-blue"        0x0000cd   0   0 205
royalblue         = makeColor "royalblue"          0x4169e1  65 105 225
skyblue           = makeColor "skyblue"            0x87ceeb 135 206 235
cyan              = makeColor "cyan"               0x00ffff   0 255 255
lightCyan         = makeColor "light-cyan"         0xe0ffff 224 255 255
darkCyan          = makeColor "dark-cyan"          0x008b8b   0 139 139
magenta           = makeColor "magenta"            0xff00ff 255   0 255
lightMagenta      = makeColor "light-magenta"      0xf055f0 240  85 240
darkMagenta       = makeColor "dark-magenta"       0x8b008b 139   0 139
turquoise         = makeColor "turquoise"          0x40e0d0  64 224 208
lightTurquoise    = makeColor "light-turquoise"    0xafeeee 175 238 238
darkTurquoise     = makeColor "dark-turquoise"     0x00ced1   0 206 209
pink              = makeColor "pink"               0xffc0cb 255 192 203
lightPink         = makeColor "light-pink"         0xffb6c1 255 182 193
darkPink          = makeColor "dark-pink"          0xff1493 255  20 147
coral             = makeColor "coral"              0xff7f50 255 127  80
lightCoral        = makeColor "light-coral"        0xf08080 240 128 128
orangeRed         = makeColor "orange-red"         0xff4500 255  69   0
salmon            = makeColor "salmon"             0xfa8072 250 128 114
lightSalmon       = makeColor "light-salmon"       0xffa07a 255 160 122
darkSalmon        = makeColor "dark-salmon"        0xe9967a 233 150 122
aquamarine        = makeColor "aquamarine"         0x7fffd4 127 255 212
khaki             = makeColor "khaki"              0xf0e68c 240 230 140
darkKhaki         = makeColor "dark-khaki"         0xbdb76b 189 183 107
goldenrod         = makeColor "goldenrod"          0xdaa520 218 165  32
lightGoldenrod    = makeColor "light-goldenrod"    0xeedd82 238 221 130
darkGoldenrod     = makeColor "dark-goldenrod"     0xb8860b 184 134  11
gold              = makeColor "gold"               0xffd700 255 215   0
beige             = makeColor "beige"              0xf5f5dc 245 245 220
brown             = makeColor "brown"              0xa52a2a 165  42  42
orange            = makeColor "orange"             0xffa500 255 165   0
darkOrange        = makeColor "dark-orange"        0xff8c00 255 140   0
violet            = makeColor "violet"             0xee82ee 238 130 238
darkViolet        = makeColor "dark-violet"        0x9400d3 148   0 211
plum              = makeColor "plum"               0xdda0dd 221 160 221
purple            = makeColor "purple"             0xa020f0 160  32 240
