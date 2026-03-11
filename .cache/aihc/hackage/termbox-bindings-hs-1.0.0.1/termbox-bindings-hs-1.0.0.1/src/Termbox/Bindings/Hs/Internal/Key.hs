module Termbox.Bindings.Hs.Internal.Key
  ( Tb_key
      ( Tb_key,
        TB_KEY_ARROW_DOWN,
        TB_KEY_ARROW_LEFT,
        TB_KEY_ARROW_RIGHT,
        TB_KEY_ARROW_UP,
        TB_KEY_BACKSPACE,
        TB_KEY_BACKSPACE2,
        TB_KEY_CTRL_2,
        TB_KEY_CTRL_3,
        TB_KEY_CTRL_4,
        TB_KEY_CTRL_5,
        TB_KEY_CTRL_6,
        TB_KEY_CTRL_7,
        TB_KEY_CTRL_8,
        TB_KEY_CTRL_A,
        TB_KEY_CTRL_B,
        TB_KEY_CTRL_BACKSLASH,
        TB_KEY_CTRL_C,
        TB_KEY_CTRL_D,
        TB_KEY_CTRL_E,
        TB_KEY_CTRL_F,
        TB_KEY_CTRL_G,
        TB_KEY_CTRL_H,
        TB_KEY_CTRL_I,
        TB_KEY_CTRL_J,
        TB_KEY_CTRL_K,
        TB_KEY_CTRL_L,
        TB_KEY_CTRL_LSQ_BRACKET,
        TB_KEY_CTRL_M,
        TB_KEY_CTRL_N,
        TB_KEY_CTRL_O,
        TB_KEY_CTRL_P,
        TB_KEY_CTRL_Q,
        TB_KEY_CTRL_R,
        TB_KEY_CTRL_RSQ_BRACKET,
        TB_KEY_CTRL_S,
        TB_KEY_CTRL_SLASH,
        TB_KEY_CTRL_T,
        TB_KEY_CTRL_TILDE,
        TB_KEY_CTRL_U,
        TB_KEY_CTRL_UNDERSCORE,
        TB_KEY_CTRL_V,
        TB_KEY_CTRL_W,
        TB_KEY_CTRL_X,
        TB_KEY_CTRL_Y,
        TB_KEY_CTRL_Z,
        TB_KEY_DELETE,
        TB_KEY_END,
        TB_KEY_ENTER,
        TB_KEY_ESC,
        TB_KEY_F1,
        TB_KEY_F10,
        TB_KEY_F11,
        TB_KEY_F12,
        TB_KEY_F2,
        TB_KEY_F3,
        TB_KEY_F4,
        TB_KEY_F5,
        TB_KEY_F6,
        TB_KEY_F7,
        TB_KEY_F8,
        TB_KEY_F9,
        TB_KEY_HOME,
        TB_KEY_INSERT,
        TB_KEY_MOUSE_LEFT,
        TB_KEY_MOUSE_MIDDLE,
        TB_KEY_MOUSE_RELEASE,
        TB_KEY_MOUSE_RIGHT,
        TB_KEY_MOUSE_WHEEL_DOWN,
        TB_KEY_MOUSE_WHEEL_UP,
        TB_KEY_PGDN,
        TB_KEY_PGUP,
        TB_KEY_SPACE,
        TB_KEY_TAB
      ),
  )
where

import Data.Word (Word16)
import Termbox.Bindings.C

-- | A key.
newtype Tb_key
  = Tb_key Word16
  deriving stock (Eq, Ord)

instance Show Tb_key where
  show = \case
    TB_KEY_F1 -> "TB_KEY_F1"
    TB_KEY_F2 -> "TB_KEY_F2"
    TB_KEY_F3 -> "TB_KEY_F3"
    TB_KEY_F4 -> "TB_KEY_F4"
    TB_KEY_F5 -> "TB_KEY_F5"
    TB_KEY_F6 -> "TB_KEY_F6"
    TB_KEY_F7 -> "TB_KEY_F7"
    TB_KEY_F8 -> "TB_KEY_F8"
    TB_KEY_F9 -> "TB_KEY_F9"
    TB_KEY_F10 -> "TB_KEY_F10"
    TB_KEY_F11 -> "TB_KEY_F11"
    TB_KEY_F12 -> "TB_KEY_F12"
    TB_KEY_INSERT -> "TB_KEY_INSERT"
    TB_KEY_DELETE -> "TB_KEY_DELETE"
    TB_KEY_HOME -> "TB_KEY_HOME"
    TB_KEY_END -> "TB_KEY_END"
    TB_KEY_PGUP -> "TB_KEY_PGUP"
    TB_KEY_PGDN -> "TB_KEY_PGDN"
    TB_KEY_ARROW_UP -> "TB_KEY_ARROW_UP"
    TB_KEY_ARROW_DOWN -> "TB_KEY_ARROW_DOWN"
    TB_KEY_ARROW_LEFT -> "TB_KEY_ARROW_LEFT"
    TB_KEY_ARROW_RIGHT -> "TB_KEY_ARROW_RIGHT"
    TB_KEY_MOUSE_LEFT -> "TB_KEY_MOUSE_LEFT"
    TB_KEY_MOUSE_RIGHT -> "TB_KEY_MOUSE_RIGHT"
    TB_KEY_MOUSE_MIDDLE -> "TB_KEY_MOUSE_MIDDLE"
    TB_KEY_MOUSE_RELEASE -> "TB_KEY_MOUSE_RELEASE"
    TB_KEY_MOUSE_WHEEL_UP -> "TB_KEY_MOUSE_WHEEL_UP"
    TB_KEY_MOUSE_WHEEL_DOWN -> "TB_KEY_MOUSE_WHEEL_DOWN"
    TB_KEY_CTRL_TILDE -> "TB_KEY_CTRL_TILDE"
    TB_KEY_CTRL_A -> "TB_KEY_CTRL_A"
    TB_KEY_CTRL_B -> "TB_KEY_CTRL_B"
    TB_KEY_CTRL_C -> "TB_KEY_CTRL_C"
    TB_KEY_CTRL_D -> "TB_KEY_CTRL_D"
    TB_KEY_CTRL_E -> "TB_KEY_CTRL_E"
    TB_KEY_CTRL_F -> "TB_KEY_CTRL_F"
    TB_KEY_CTRL_G -> "TB_KEY_CTRL_G"
    TB_KEY_BACKSPACE -> "TB_KEY_BACKSPACE"
    TB_KEY_TAB -> "TB_KEY_TAB"
    TB_KEY_CTRL_J -> "TB_KEY_CTRL_J"
    TB_KEY_CTRL_K -> "TB_KEY_CTRL_K"
    TB_KEY_CTRL_L -> "TB_KEY_CTRL_L"
    TB_KEY_ENTER -> "TB_KEY_ENTER"
    TB_KEY_CTRL_N -> "TB_KEY_CTRL_N"
    TB_KEY_CTRL_O -> "TB_KEY_CTRL_O"
    TB_KEY_CTRL_P -> "TB_KEY_CTRL_P"
    TB_KEY_CTRL_Q -> "TB_KEY_CTRL_Q"
    TB_KEY_CTRL_R -> "TB_KEY_CTRL_R"
    TB_KEY_CTRL_S -> "TB_KEY_CTRL_S"
    TB_KEY_CTRL_T -> "TB_KEY_CTRL_T"
    TB_KEY_CTRL_U -> "TB_KEY_CTRL_U"
    TB_KEY_CTRL_V -> "TB_KEY_CTRL_V"
    TB_KEY_CTRL_W -> "TB_KEY_CTRL_W"
    TB_KEY_CTRL_X -> "TB_KEY_CTRL_X"
    TB_KEY_CTRL_Y -> "TB_KEY_CTRL_Y"
    TB_KEY_CTRL_Z -> "TB_KEY_CTRL_Z"
    TB_KEY_ESC -> "TB_KEY_ESC"
    TB_KEY_CTRL_4 -> "TB_KEY_CTRL_4"
    TB_KEY_CTRL_5 -> "TB_KEY_CTRL_5"
    TB_KEY_CTRL_6 -> "TB_KEY_CTRL_6"
    TB_KEY_CTRL_7 -> "TB_KEY_CTRL_7"
    TB_KEY_SPACE -> "TB_KEY_SPACE"
    TB_KEY_BACKSPACE2 -> "TB_KEY_BACKSPACE2"
    Tb_key k -> "Tb_key " ++ show k

pattern TB_KEY_ARROW_DOWN :: Tb_key
pattern TB_KEY_ARROW_DOWN <-
  ((== Tb_key _TB_KEY_ARROW_DOWN) -> True)
  where
    TB_KEY_ARROW_DOWN = Tb_key _TB_KEY_ARROW_DOWN

pattern TB_KEY_ARROW_LEFT :: Tb_key
pattern TB_KEY_ARROW_LEFT <-
  ((== Tb_key _TB_KEY_ARROW_LEFT) -> True)
  where
    TB_KEY_ARROW_LEFT = Tb_key _TB_KEY_ARROW_LEFT

pattern TB_KEY_ARROW_RIGHT :: Tb_key
pattern TB_KEY_ARROW_RIGHT <-
  ((== Tb_key _TB_KEY_ARROW_RIGHT) -> True)
  where
    TB_KEY_ARROW_RIGHT = Tb_key _TB_KEY_ARROW_RIGHT

pattern TB_KEY_ARROW_UP :: Tb_key
pattern TB_KEY_ARROW_UP <-
  ((== Tb_key _TB_KEY_ARROW_UP) -> True)
  where
    TB_KEY_ARROW_UP = Tb_key _TB_KEY_ARROW_UP

pattern TB_KEY_BACKSPACE :: Tb_key
pattern TB_KEY_BACKSPACE <-
  ((== Tb_key _TB_KEY_BACKSPACE) -> True)
  where
    TB_KEY_BACKSPACE = Tb_key _TB_KEY_BACKSPACE

pattern TB_KEY_BACKSPACE2 :: Tb_key
pattern TB_KEY_BACKSPACE2 <-
  ((== Tb_key _TB_KEY_BACKSPACE2) -> True)
  where
    TB_KEY_BACKSPACE2 = Tb_key _TB_KEY_BACKSPACE2

pattern TB_KEY_CTRL_2 :: Tb_key
pattern TB_KEY_CTRL_2 <-
  ((== Tb_key _TB_KEY_CTRL_2) -> True)
  where
    TB_KEY_CTRL_2 = Tb_key _TB_KEY_CTRL_2

pattern TB_KEY_CTRL_3 :: Tb_key
pattern TB_KEY_CTRL_3 <-
  ((== Tb_key _TB_KEY_CTRL_3) -> True)
  where
    TB_KEY_CTRL_3 = Tb_key _TB_KEY_CTRL_3

pattern TB_KEY_CTRL_4 :: Tb_key
pattern TB_KEY_CTRL_4 <-
  ((== Tb_key _TB_KEY_CTRL_4) -> True)
  where
    TB_KEY_CTRL_4 = Tb_key _TB_KEY_CTRL_4

pattern TB_KEY_CTRL_5 :: Tb_key
pattern TB_KEY_CTRL_5 <-
  ((== Tb_key _TB_KEY_CTRL_5) -> True)
  where
    TB_KEY_CTRL_5 = Tb_key _TB_KEY_CTRL_5

pattern TB_KEY_CTRL_6 :: Tb_key
pattern TB_KEY_CTRL_6 <-
  ((== Tb_key _TB_KEY_CTRL_6) -> True)
  where
    TB_KEY_CTRL_6 = Tb_key _TB_KEY_CTRL_6

pattern TB_KEY_CTRL_7 :: Tb_key
pattern TB_KEY_CTRL_7 <-
  ((== Tb_key _TB_KEY_CTRL_7) -> True)
  where
    TB_KEY_CTRL_7 = Tb_key _TB_KEY_CTRL_7

pattern TB_KEY_CTRL_8 :: Tb_key
pattern TB_KEY_CTRL_8 <-
  ((== Tb_key _TB_KEY_CTRL_8) -> True)
  where
    TB_KEY_CTRL_8 = Tb_key _TB_KEY_CTRL_8

pattern TB_KEY_CTRL_A :: Tb_key
pattern TB_KEY_CTRL_A <-
  ((== Tb_key _TB_KEY_CTRL_A) -> True)
  where
    TB_KEY_CTRL_A = Tb_key _TB_KEY_CTRL_A

pattern TB_KEY_CTRL_B :: Tb_key
pattern TB_KEY_CTRL_B <-
  ((== Tb_key _TB_KEY_CTRL_B) -> True)
  where
    TB_KEY_CTRL_B = Tb_key _TB_KEY_CTRL_B

pattern TB_KEY_CTRL_BACKSLASH :: Tb_key
pattern TB_KEY_CTRL_BACKSLASH <-
  ((== Tb_key _TB_KEY_CTRL_BACKSLASH) -> True)
  where
    TB_KEY_CTRL_BACKSLASH = Tb_key _TB_KEY_CTRL_BACKSLASH

pattern TB_KEY_CTRL_C :: Tb_key
pattern TB_KEY_CTRL_C <-
  ((== Tb_key _TB_KEY_CTRL_C) -> True)
  where
    TB_KEY_CTRL_C = Tb_key _TB_KEY_CTRL_C

pattern TB_KEY_CTRL_D :: Tb_key
pattern TB_KEY_CTRL_D <-
  ((== Tb_key _TB_KEY_CTRL_D) -> True)
  where
    TB_KEY_CTRL_D = Tb_key _TB_KEY_CTRL_D

pattern TB_KEY_CTRL_E :: Tb_key
pattern TB_KEY_CTRL_E <-
  ((== Tb_key _TB_KEY_CTRL_E) -> True)
  where
    TB_KEY_CTRL_E = Tb_key _TB_KEY_CTRL_E

pattern TB_KEY_CTRL_F :: Tb_key
pattern TB_KEY_CTRL_F <-
  ((== Tb_key _TB_KEY_CTRL_F) -> True)
  where
    TB_KEY_CTRL_F = Tb_key _TB_KEY_CTRL_F

pattern TB_KEY_CTRL_G :: Tb_key
pattern TB_KEY_CTRL_G <-
  ((== Tb_key _TB_KEY_CTRL_G) -> True)
  where
    TB_KEY_CTRL_G = Tb_key _TB_KEY_CTRL_G

pattern TB_KEY_CTRL_H :: Tb_key
pattern TB_KEY_CTRL_H <-
  ((== Tb_key _TB_KEY_CTRL_H) -> True)
  where
    TB_KEY_CTRL_H = Tb_key _TB_KEY_CTRL_H

pattern TB_KEY_CTRL_I :: Tb_key
pattern TB_KEY_CTRL_I <-
  ((== Tb_key _TB_KEY_CTRL_I) -> True)
  where
    TB_KEY_CTRL_I = Tb_key _TB_KEY_CTRL_I

pattern TB_KEY_CTRL_J :: Tb_key
pattern TB_KEY_CTRL_J <-
  ((== Tb_key _TB_KEY_CTRL_J) -> True)
  where
    TB_KEY_CTRL_J = Tb_key _TB_KEY_CTRL_J

pattern TB_KEY_CTRL_K :: Tb_key
pattern TB_KEY_CTRL_K <-
  ((== Tb_key _TB_KEY_CTRL_K) -> True)
  where
    TB_KEY_CTRL_K = Tb_key _TB_KEY_CTRL_K

pattern TB_KEY_CTRL_L :: Tb_key
pattern TB_KEY_CTRL_L <-
  ((== Tb_key _TB_KEY_CTRL_L) -> True)
  where
    TB_KEY_CTRL_L = Tb_key _TB_KEY_CTRL_L

pattern TB_KEY_CTRL_LSQ_BRACKET :: Tb_key
pattern TB_KEY_CTRL_LSQ_BRACKET <-
  ((== Tb_key _TB_KEY_CTRL_LSQ_BRACKET) -> True)
  where
    TB_KEY_CTRL_LSQ_BRACKET = Tb_key _TB_KEY_CTRL_LSQ_BRACKET

pattern TB_KEY_CTRL_M :: Tb_key
pattern TB_KEY_CTRL_M <-
  ((== Tb_key _TB_KEY_CTRL_M) -> True)
  where
    TB_KEY_CTRL_M = Tb_key _TB_KEY_CTRL_M

pattern TB_KEY_CTRL_N :: Tb_key
pattern TB_KEY_CTRL_N <-
  ((== Tb_key _TB_KEY_CTRL_N) -> True)
  where
    TB_KEY_CTRL_N = Tb_key _TB_KEY_CTRL_N

pattern TB_KEY_CTRL_O :: Tb_key
pattern TB_KEY_CTRL_O <-
  ((== Tb_key _TB_KEY_CTRL_O) -> True)
  where
    TB_KEY_CTRL_O = Tb_key _TB_KEY_CTRL_O

pattern TB_KEY_CTRL_P :: Tb_key
pattern TB_KEY_CTRL_P <-
  ((== Tb_key _TB_KEY_CTRL_P) -> True)
  where
    TB_KEY_CTRL_P = Tb_key _TB_KEY_CTRL_P

pattern TB_KEY_CTRL_Q :: Tb_key
pattern TB_KEY_CTRL_Q <-
  ((== Tb_key _TB_KEY_CTRL_Q) -> True)
  where
    TB_KEY_CTRL_Q = Tb_key _TB_KEY_CTRL_Q

pattern TB_KEY_CTRL_R :: Tb_key
pattern TB_KEY_CTRL_R <-
  ((== Tb_key _TB_KEY_CTRL_R) -> True)
  where
    TB_KEY_CTRL_R = Tb_key _TB_KEY_CTRL_R

pattern TB_KEY_CTRL_RSQ_BRACKET :: Tb_key
pattern TB_KEY_CTRL_RSQ_BRACKET <-
  ((== Tb_key _TB_KEY_CTRL_RSQ_BRACKET) -> True)
  where
    TB_KEY_CTRL_RSQ_BRACKET = Tb_key _TB_KEY_CTRL_RSQ_BRACKET

pattern TB_KEY_CTRL_S :: Tb_key
pattern TB_KEY_CTRL_S <-
  ((== Tb_key _TB_KEY_CTRL_S) -> True)
  where
    TB_KEY_CTRL_S = Tb_key _TB_KEY_CTRL_S

pattern TB_KEY_CTRL_SLASH :: Tb_key
pattern TB_KEY_CTRL_SLASH <-
  ((== Tb_key _TB_KEY_CTRL_SLASH) -> True)
  where
    TB_KEY_CTRL_SLASH = Tb_key _TB_KEY_CTRL_SLASH

pattern TB_KEY_CTRL_T :: Tb_key
pattern TB_KEY_CTRL_T <-
  ((== Tb_key _TB_KEY_CTRL_T) -> True)
  where
    TB_KEY_CTRL_T = Tb_key _TB_KEY_CTRL_T

pattern TB_KEY_CTRL_TILDE :: Tb_key
pattern TB_KEY_CTRL_TILDE <-
  ((== Tb_key _TB_KEY_CTRL_TILDE) -> True)
  where
    TB_KEY_CTRL_TILDE = Tb_key _TB_KEY_CTRL_TILDE

pattern TB_KEY_CTRL_U :: Tb_key
pattern TB_KEY_CTRL_U <-
  ((== Tb_key _TB_KEY_CTRL_U) -> True)
  where
    TB_KEY_CTRL_U = Tb_key _TB_KEY_CTRL_U

pattern TB_KEY_CTRL_UNDERSCORE :: Tb_key
pattern TB_KEY_CTRL_UNDERSCORE <-
  ((== Tb_key _TB_KEY_CTRL_UNDERSCORE) -> True)
  where
    TB_KEY_CTRL_UNDERSCORE = Tb_key _TB_KEY_CTRL_UNDERSCORE

pattern TB_KEY_CTRL_V :: Tb_key
pattern TB_KEY_CTRL_V <-
  ((== Tb_key _TB_KEY_CTRL_V) -> True)
  where
    TB_KEY_CTRL_V = Tb_key _TB_KEY_CTRL_V

pattern TB_KEY_CTRL_W :: Tb_key
pattern TB_KEY_CTRL_W <-
  ((== Tb_key _TB_KEY_CTRL_W) -> True)
  where
    TB_KEY_CTRL_W = Tb_key _TB_KEY_CTRL_W

pattern TB_KEY_CTRL_X :: Tb_key
pattern TB_KEY_CTRL_X <-
  ((== Tb_key _TB_KEY_CTRL_X) -> True)
  where
    TB_KEY_CTRL_X = Tb_key _TB_KEY_CTRL_X

pattern TB_KEY_CTRL_Y :: Tb_key
pattern TB_KEY_CTRL_Y <-
  ((== Tb_key _TB_KEY_CTRL_Y) -> True)
  where
    TB_KEY_CTRL_Y = Tb_key _TB_KEY_CTRL_Y

pattern TB_KEY_CTRL_Z :: Tb_key
pattern TB_KEY_CTRL_Z <-
  ((== Tb_key _TB_KEY_CTRL_Z) -> True)
  where
    TB_KEY_CTRL_Z = Tb_key _TB_KEY_CTRL_Z

pattern TB_KEY_DELETE :: Tb_key
pattern TB_KEY_DELETE <-
  ((== Tb_key _TB_KEY_DELETE) -> True)
  where
    TB_KEY_DELETE = Tb_key _TB_KEY_DELETE

pattern TB_KEY_END :: Tb_key
pattern TB_KEY_END <-
  ((== Tb_key _TB_KEY_END) -> True)
  where
    TB_KEY_END = Tb_key _TB_KEY_END

pattern TB_KEY_ENTER :: Tb_key
pattern TB_KEY_ENTER <-
  ((== Tb_key _TB_KEY_ENTER) -> True)
  where
    TB_KEY_ENTER = Tb_key _TB_KEY_ENTER

pattern TB_KEY_ESC :: Tb_key
pattern TB_KEY_ESC <-
  ((== Tb_key _TB_KEY_ESC) -> True)
  where
    TB_KEY_ESC = Tb_key _TB_KEY_ESC

pattern TB_KEY_F1 :: Tb_key
pattern TB_KEY_F1 <-
  ((== Tb_key _TB_KEY_F1) -> True)
  where
    TB_KEY_F1 = Tb_key _TB_KEY_F1

pattern TB_KEY_F10 :: Tb_key
pattern TB_KEY_F10 <-
  ((== Tb_key _TB_KEY_F10) -> True)
  where
    TB_KEY_F10 = Tb_key _TB_KEY_F10

pattern TB_KEY_F11 :: Tb_key
pattern TB_KEY_F11 <-
  ((== Tb_key _TB_KEY_F11) -> True)
  where
    TB_KEY_F11 = Tb_key _TB_KEY_F11

pattern TB_KEY_F12 :: Tb_key
pattern TB_KEY_F12 <-
  ((== Tb_key _TB_KEY_F12) -> True)
  where
    TB_KEY_F12 = Tb_key _TB_KEY_F12

pattern TB_KEY_F2 :: Tb_key
pattern TB_KEY_F2 <-
  ((== Tb_key _TB_KEY_F2) -> True)
  where
    TB_KEY_F2 = Tb_key _TB_KEY_F2

pattern TB_KEY_F3 :: Tb_key
pattern TB_KEY_F3 <-
  ((== Tb_key _TB_KEY_F3) -> True)
  where
    TB_KEY_F3 = Tb_key _TB_KEY_F3

pattern TB_KEY_F4 :: Tb_key
pattern TB_KEY_F4 <-
  ((== Tb_key _TB_KEY_F4) -> True)
  where
    TB_KEY_F4 = Tb_key _TB_KEY_F4

pattern TB_KEY_F5 :: Tb_key
pattern TB_KEY_F5 <-
  ((== Tb_key _TB_KEY_F5) -> True)
  where
    TB_KEY_F5 = Tb_key _TB_KEY_F5

pattern TB_KEY_F6 :: Tb_key
pattern TB_KEY_F6 <-
  ((== Tb_key _TB_KEY_F6) -> True)
  where
    TB_KEY_F6 = Tb_key _TB_KEY_F6

pattern TB_KEY_F7 :: Tb_key
pattern TB_KEY_F7 <-
  ((== Tb_key _TB_KEY_F7) -> True)
  where
    TB_KEY_F7 = Tb_key _TB_KEY_F7

pattern TB_KEY_F8 :: Tb_key
pattern TB_KEY_F8 <-
  ((== Tb_key _TB_KEY_F8) -> True)
  where
    TB_KEY_F8 = Tb_key _TB_KEY_F8

pattern TB_KEY_F9 :: Tb_key
pattern TB_KEY_F9 <-
  ((== Tb_key _TB_KEY_F9) -> True)
  where
    TB_KEY_F9 = Tb_key _TB_KEY_F9

pattern TB_KEY_HOME :: Tb_key
pattern TB_KEY_HOME <-
  ((== Tb_key _TB_KEY_HOME) -> True)
  where
    TB_KEY_HOME = Tb_key _TB_KEY_HOME

pattern TB_KEY_INSERT :: Tb_key
pattern TB_KEY_INSERT <-
  ((== Tb_key _TB_KEY_INSERT) -> True)
  where
    TB_KEY_INSERT = Tb_key _TB_KEY_INSERT

pattern TB_KEY_MOUSE_LEFT :: Tb_key
pattern TB_KEY_MOUSE_LEFT <-
  ((== Tb_key _TB_KEY_MOUSE_LEFT) -> True)
  where
    TB_KEY_MOUSE_LEFT = Tb_key _TB_KEY_MOUSE_LEFT

pattern TB_KEY_MOUSE_MIDDLE :: Tb_key
pattern TB_KEY_MOUSE_MIDDLE <-
  ((== Tb_key _TB_KEY_MOUSE_MIDDLE) -> True)
  where
    TB_KEY_MOUSE_MIDDLE = Tb_key _TB_KEY_MOUSE_MIDDLE

pattern TB_KEY_MOUSE_RELEASE :: Tb_key
pattern TB_KEY_MOUSE_RELEASE <-
  ((== Tb_key _TB_KEY_MOUSE_RELEASE) -> True)
  where
    TB_KEY_MOUSE_RELEASE = Tb_key _TB_KEY_MOUSE_RELEASE

pattern TB_KEY_MOUSE_RIGHT :: Tb_key
pattern TB_KEY_MOUSE_RIGHT <-
  ((== Tb_key _TB_KEY_MOUSE_RIGHT) -> True)
  where
    TB_KEY_MOUSE_RIGHT = Tb_key _TB_KEY_MOUSE_RIGHT

pattern TB_KEY_MOUSE_WHEEL_DOWN :: Tb_key
pattern TB_KEY_MOUSE_WHEEL_DOWN <-
  ((== Tb_key _TB_KEY_MOUSE_WHEEL_DOWN) -> True)
  where
    TB_KEY_MOUSE_WHEEL_DOWN = Tb_key _TB_KEY_MOUSE_WHEEL_DOWN

pattern TB_KEY_MOUSE_WHEEL_UP :: Tb_key
pattern TB_KEY_MOUSE_WHEEL_UP <-
  ((== Tb_key _TB_KEY_MOUSE_WHEEL_UP) -> True)
  where
    TB_KEY_MOUSE_WHEEL_UP = Tb_key _TB_KEY_MOUSE_WHEEL_UP

pattern TB_KEY_PGDN :: Tb_key
pattern TB_KEY_PGDN <-
  ((== Tb_key _TB_KEY_PGDN) -> True)
  where
    TB_KEY_PGDN = Tb_key _TB_KEY_PGDN

pattern TB_KEY_PGUP :: Tb_key
pattern TB_KEY_PGUP <-
  ((== Tb_key _TB_KEY_PGUP) -> True)
  where
    TB_KEY_PGUP = Tb_key _TB_KEY_PGUP

pattern TB_KEY_SPACE :: Tb_key
pattern TB_KEY_SPACE <-
  ((== Tb_key _TB_KEY_SPACE) -> True)
  where
    TB_KEY_SPACE = Tb_key _TB_KEY_SPACE

pattern TB_KEY_TAB :: Tb_key
pattern TB_KEY_TAB <-
  ((== Tb_key _TB_KEY_TAB) -> True)
  where
    TB_KEY_TAB = Tb_key _TB_KEY_TAB
