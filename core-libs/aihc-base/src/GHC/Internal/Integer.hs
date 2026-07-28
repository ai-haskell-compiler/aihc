{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.Internal.Integer
  ( Integer (..),
    compareInteger#,
    eqInteger#,
    integerAbs,
    integerAdd,
    integerMul,
    integerNegate,
    integerQuotRemWord#,
    integerSignum,
    integerSub,
    integerToInt#,
  )
where

import GHC.Prim
  ( ByteArray#,
    MutableByteArray#,
    RealWorld,
    State#,
    addIntC#,
    addWordC#,
    eqWord#,
    indexWordArray#,
    int2Word#,
    ltWord#,
    newByteArray#,
    plusWord#,
    quotRemWord2#,
    quotWord#,
    readWordArray#,
    realWorld#,
    shrinkMutableByteArray#,
    sizeofByteArray#,
    subIntC#,
    subWordC#,
    timesWord2#,
    unsafeFreezeByteArray#,
    word2Int#,
    writeWordArray#,
    (*#),
    (+#),
    (-#),
    (<#),
    (==#),
  )

-- Magnitudes use canonical, little-endian 64-bit limbs.  Small values stay
-- allocation-free in IS; IP and IN never contain a value representable by IS.
data Integer
  = IS Int#
  | IP ByteArray#
  | IN ByteArray#

integerAdd :: Integer -> Integer -> Integer
integerAdd left right =
  case left of
    IS leftInt ->
      case right of
        IS rightInt ->
          case addIntC# leftInt rightInt of
            (# result, overflow #) ->
              case overflow of
                0# -> IS result
                _ -> addMagnitudesWithSigns left right
        _ -> addMagnitudesWithSigns left right
    _ -> addMagnitudesWithSigns left right

integerSub :: Integer -> Integer -> Integer
integerSub left right = integerAdd left (integerNegate right)

integerMul :: Integer -> Integer -> Integer
integerMul left right =
  case left of
    IS leftInt ->
      case right of
        IS rightInt -> multiplySmall# leftInt rightInt
        _ -> multiplyLarge left right
    _ -> multiplyLarge left right

multiplySmall# :: Int# -> Int# -> Integer
multiplySmall# left right =
  case left of
    0# -> IS 0#
    _ ->
      case right of
        0# -> IS 0#
        _ ->
          case timesWord2# (absoluteIntWord# left) (absoluteIntWord# right) of
            (# high, low #) ->
              case (==#) ((<#) left 0#) ((<#) right 0#) of
                1# -> integerFromTwoWords# 1# high low
                _ -> integerFromTwoWords# ((-#) 0# 1#) high low

multiplyLarge :: Integer -> Integer -> Integer
multiplyLarge left right =
  case signInteger# left of
    0# -> IS 0#
    leftSign ->
      case signInteger# right of
        0# -> IS 0#
        rightSign -> integerFromMagnitude# ((*#) leftSign rightSign) (multiplyMagnitudes# left right)

integerNegate :: Integer -> Integer
integerNegate (IP magnitude) = IN magnitude
integerNegate (IN magnitude) = IP magnitude
integerNegate (IS value) =
  case subIntC# 0# value of
    (# result, overflow #) ->
      case overflow of
        0# -> IS result
        _ -> integerFromWord# 1# (int2Word# value)

integerAbs :: Integer -> Integer
integerAbs (IN magnitude) = IP magnitude
integerAbs value =
  case value of
    IS intValue ->
      case (<#) intValue 0# of
        0# -> value
        _ -> integerNegate value
    _ -> value

integerSignum :: Integer -> Integer
integerSignum value = IS (signInteger# value)

integerToInt# :: Integer -> Int#
integerToInt# (IS value) = value
integerToInt# (IP magnitude) = word2Int# (indexWordArray# magnitude 0#)
integerToInt# (IN magnitude) = (-#) 0# (word2Int# (indexWordArray# magnitude 0#))

integerQuotRemWord# :: Integer -> Word# -> (# Integer, Word# #)
integerQuotRemWord# value divisor =
  case signInteger# value of
    0# -> (# IS 0#, int2Word# 0# #)
    sign ->
      case magnitudeSize# value of
        wordCount ->
          case newByteArray# ((*#) wordCount 8#) realWorld# of
            (# state0, mutable #) ->
              case divideMagnitudeByWord# value divisor mutable ((-#) wordCount 1#) (int2Word# 0#) state0 of
                (# state1, remainder #) ->
                  case trimMagnitudeWords# mutable ((-#) wordCount 1#) state1 of
                    (# state2, usedWords #) ->
                      case freezeTrimmed# mutable usedWords state2 of
                        quotientMagnitude -> (# integerFromMagnitude# sign quotientMagnitude, remainder #)

divideMagnitudeByWord# :: Integer -> Word# -> MutableByteArray# RealWorld -> Int# -> Word# -> State# RealWorld -> (# State# RealWorld, Word# #)
divideMagnitudeByWord# value divisor mutable index remainder state =
  case (<#) index 0# of
    1# -> (# state, remainder #)
    _ ->
      case quotRemWord2# remainder (magnitudeWord# value index) divisor of
        (# quotientWord, nextRemainder #) ->
          case writeWordArray# mutable index quotientWord state of
            state1 -> divideMagnitudeByWord# value divisor mutable ((-#) index 1#) nextRemainder state1

compareInteger# :: Integer -> Integer -> Int#
compareInteger# left right =
  case signInteger# left of
    leftSign ->
      case signInteger# right of
        rightSign ->
          case (<#) leftSign rightSign of
            1# -> (-#) 0# 1#
            _ ->
              case (<#) rightSign leftSign of
                1# -> 1#
                _ ->
                  case leftSign of
                    0# -> 0#
                    1# -> compareMagnitudes# left right
                    _ -> (-#) 0# (compareMagnitudes# left right)

eqInteger# :: Integer -> Integer -> Int#
eqInteger# left right = (==#) (compareInteger# left right) 0#

addMagnitudesWithSigns :: Integer -> Integer -> Integer
addMagnitudesWithSigns left right =
  case signInteger# left of
    0# -> right
    leftSign ->
      case signInteger# right of
        0# -> left
        rightSign ->
          case (==#) leftSign rightSign of
            1# -> integerFromMagnitude# leftSign (addMagnitudes# left right)
            _ ->
              case compareMagnitudes# left right of
                0# -> IS 0#
                1# -> integerFromMagnitude# leftSign (subtractMagnitudes# left right)
                _ -> integerFromMagnitude# rightSign (subtractMagnitudes# right left)

signInteger# :: Integer -> Int#
signInteger# (IP _) = 1#
signInteger# (IN _) = (-#) 0# 1#
signInteger# (IS value) =
  case value of
    0# -> 0#
    _ ->
      case (<#) value 0# of
        1# -> (-#) 0# 1#
        _ -> 1#

magnitudeSize# :: Integer -> Int#
magnitudeSize# (IS value) =
  case value of
    0# -> 0#
    _ -> 1#
magnitudeSize# (IP magnitude) = wordCount# magnitude
magnitudeSize# (IN magnitude) = wordCount# magnitude

magnitudeWord# :: Integer -> Int# -> Word#
magnitudeWord# (IS value) _ = absoluteIntWord# value
magnitudeWord# (IP magnitude) index = indexWordArray# magnitude index
magnitudeWord# (IN magnitude) index = indexWordArray# magnitude index

absoluteIntWord# :: Int# -> Word#
absoluteIntWord# value =
  case (<#) value 0# of
    0# -> int2Word# value
    _ -> int2Word# ((-#) 0# value)

wordCount# :: ByteArray# -> Int#
wordCount# magnitude = word2Int# (quotWord# (int2Word# (sizeofByteArray# magnitude)) (int2Word# 8#))

compareMagnitudes# :: Integer -> Integer -> Int#
compareMagnitudes# left right =
  case magnitudeSize# left of
    leftSize ->
      case magnitudeSize# right of
        rightSize ->
          case (<#) leftSize rightSize of
            1# -> (-#) 0# 1#
            _ ->
              case (<#) rightSize leftSize of
                1# -> 1#
                _ -> compareMagnitudeWords# left right ((-#) leftSize 1#)

compareMagnitudeWords# :: Integer -> Integer -> Int# -> Int#
compareMagnitudeWords# left right index =
  case (<#) index 0# of
    1# -> 0#
    _ ->
      case magnitudeWord# left index of
        leftWord ->
          case magnitudeWord# right index of
            rightWord ->
              case ltWord# leftWord rightWord of
                1# -> (-#) 0# 1#
                _ ->
                  case ltWord# rightWord leftWord of
                    1# -> 1#
                    _ -> compareMagnitudeWords# left right ((-#) index 1#)

addMagnitudes# :: Integer -> Integer -> ByteArray#
addMagnitudes# left right =
  case maxInt# (magnitudeSize# left) (magnitudeSize# right) of
    wordCount ->
      case newByteArray# ((*#) ((+#) wordCount 1#) 8#) realWorld# of
        (# state0, mutable #) ->
          case addMagnitudeWords# left right mutable wordCount 0# 0# state0 of
            (# state1, usedWords #) -> freezeTrimmed# mutable usedWords state1

addMagnitudeWords# :: Integer -> Integer -> MutableByteArray# RealWorld -> Int# -> Int# -> Int# -> State# RealWorld -> (# State# RealWorld, Int# #)
addMagnitudeWords# left right mutable wordCount index carry state =
  case (==#) index wordCount of
    1# ->
      case carry of
        0# -> (# state, wordCount #)
        _ ->
          case writeWordArray# mutable index (int2Word# carry) state of
            state1 -> (# state1, (+#) wordCount 1# #)
    _ ->
      case addWordC# (magnitudeWordOrZero# left index) (magnitudeWordOrZero# right index) of
        (# partial, carry0 #) ->
          case addWordC# partial (int2Word# carry) of
            (# result, carry1 #) ->
              case writeWordArray# mutable index result state of
                state1 -> addMagnitudeWords# left right mutable wordCount ((+#) index 1#) ((+#) carry0 carry1) state1

magnitudeWordOrZero# :: Integer -> Int# -> Word#
magnitudeWordOrZero# value index =
  case (<#) index (magnitudeSize# value) of
    1# -> magnitudeWord# value index
    _ -> int2Word# 0#

subtractMagnitudes# :: Integer -> Integer -> ByteArray#
subtractMagnitudes# larger smaller =
  case magnitudeSize# larger of
    wordCount ->
      case newByteArray# ((*#) wordCount 8#) realWorld# of
        (# state0, mutable #) ->
          case subtractMagnitudeWords# larger smaller mutable wordCount 0# 0# state0 of
            (# state1, _ #) ->
              case trimMagnitudeWords# mutable ((-#) wordCount 1#) state1 of
                (# state2, usedWords #) -> freezeTrimmed# mutable usedWords state2

subtractMagnitudeWords# :: Integer -> Integer -> MutableByteArray# RealWorld -> Int# -> Int# -> Int# -> State# RealWorld -> (# State# RealWorld, Int# #)
subtractMagnitudeWords# larger smaller mutable wordCount index borrow state =
  case (==#) index wordCount of
    1# -> (# state, borrow #)
    _ ->
      case subWordC# (magnitudeWord# larger index) (magnitudeWordOrZero# smaller index) of
        (# partial, borrow0 #) ->
          case subWordC# partial (int2Word# borrow) of
            (# result, borrow1 #) ->
              case writeWordArray# mutable index result state of
                state1 -> subtractMagnitudeWords# larger smaller mutable wordCount ((+#) index 1#) ((+#) borrow0 borrow1) state1

multiplyMagnitudes# :: Integer -> Integer -> ByteArray#
multiplyMagnitudes# left right =
  case magnitudeSize# left of
    leftSize ->
      case magnitudeSize# right of
        rightSize ->
          case (+#) leftSize rightSize of
            resultSize ->
              case newByteArray# ((*#) resultSize 8#) realWorld# of
                (# state0, mutable #) ->
                  case zeroMagnitudeWords# mutable resultSize 0# state0 of
                    (# state1, _ #) ->
                      case multiplyOuter# left right mutable leftSize rightSize 0# state1 of
                        (# state2, _ #) ->
                          case trimMagnitudeWords# mutable ((-#) resultSize 1#) state2 of
                            (# state3, usedWords #) -> freezeTrimmed# mutable usedWords state3

zeroMagnitudeWords# :: MutableByteArray# RealWorld -> Int# -> Int# -> State# RealWorld -> (# State# RealWorld, Int# #)
zeroMagnitudeWords# mutable wordCount index state =
  case (==#) index wordCount of
    1# -> (# state, index #)
    _ ->
      case writeWordArray# mutable index (int2Word# 0#) state of
        state1 -> zeroMagnitudeWords# mutable wordCount ((+#) index 1#) state1

multiplyOuter# :: Integer -> Integer -> MutableByteArray# RealWorld -> Int# -> Int# -> Int# -> State# RealWorld -> (# State# RealWorld, Int# #)
multiplyOuter# left right mutable leftSize rightSize rightIndex state =
  case (==#) rightIndex rightSize of
    1# -> (# state, rightIndex #)
    _ ->
      case multiplyInner# left mutable leftSize (magnitudeWord# right rightIndex) rightIndex 0# (int2Word# 0#) state of
        (# state1, _ #) -> multiplyOuter# left right mutable leftSize rightSize ((+#) rightIndex 1#) state1

multiplyInner# :: Integer -> MutableByteArray# RealWorld -> Int# -> Word# -> Int# -> Int# -> Word# -> State# RealWorld -> (# State# RealWorld, Word# #)
multiplyInner# left mutable leftSize rightWord rightIndex leftIndex carry state =
  case (==#) leftIndex leftSize of
    1# ->
      case writeWordArray# mutable ((+#) leftSize rightIndex) carry state of
        state1 -> (# state1, carry #)
    _ ->
      case timesWord2# (magnitudeWord# left leftIndex) rightWord of
        (# high, low #) ->
          case readWordArray# mutable ((+#) leftIndex rightIndex) state of
            (# state1, existing #) ->
              case addWordC# low existing of
                (# partial, carry0 #) ->
                  case addWordC# partial carry of
                    (# result, carry1 #) ->
                      case writeWordArray# mutable ((+#) leftIndex rightIndex) result state1 of
                        state2 -> multiplyInner# left mutable leftSize rightWord rightIndex ((+#) leftIndex 1#) (plusWord# high (int2Word# ((+#) carry0 carry1))) state2

trimMagnitudeWords# :: MutableByteArray# RealWorld -> Int# -> State# RealWorld -> (# State# RealWorld, Int# #)
trimMagnitudeWords# mutable index state =
  case (<#) index 0# of
    1# -> (# state, 0# #)
    _ ->
      case readWordArray# mutable index state of
        (# state1, word #) ->
          case eqWord# word (int2Word# 0#) of
            1# -> trimMagnitudeWords# mutable ((-#) index 1#) state1
            _ -> (# state1, (+#) index 1# #)

freezeTrimmed# :: MutableByteArray# RealWorld -> Int# -> State# RealWorld -> ByteArray#
freezeTrimmed# mutable usedWords state =
  case shrinkMutableByteArray# mutable ((*#) usedWords 8#) state of
    state1 ->
      case unsafeFreezeByteArray# mutable state1 of
        (# _, magnitude #) -> magnitude

integerFromMagnitude# :: Int# -> ByteArray# -> Integer
integerFromMagnitude# sign magnitude =
  case wordCount# magnitude of
    0# -> IS 0#
    1# -> integerFromWord# sign (indexWordArray# magnitude 0#)
    _ ->
      case sign of
        1# -> IP magnitude
        _ -> IN magnitude

integerFromTwoWords# :: Int# -> Word# -> Word# -> Integer
integerFromTwoWords# sign high low =
  case eqWord# high (int2Word# 0#) of
    1# -> integerFromWord# sign low
    _ ->
      case newByteArray# 16# realWorld# of
        (# state0, mutable #) ->
          case writeWordArray# mutable 0# low state0 of
            state1 ->
              case writeWordArray# mutable 1# high state1 of
                state2 ->
                  case unsafeFreezeByteArray# mutable state2 of
                    (# _, magnitude #) ->
                      case sign of
                        1# -> IP magnitude
                        _ -> IN magnitude

integerFromWord# :: Int# -> Word# -> Integer
integerFromWord# sign word =
  case eqWord# word (int2Word# 0#) of
    1# -> IS 0#
    _ ->
      case word2Int# word of
        intValue ->
          case sign of
            1# ->
              case (<#) intValue 0# of
                0# -> IS intValue
                _ -> allocateWordInteger# sign word
            _ ->
              case (<#) intValue 0# of
                0# -> IS ((-#) 0# intValue)
                _ ->
                  case (==#) intValue ((-#) 0# intValue) of
                    1# -> IS intValue
                    _ -> allocateWordInteger# sign word

allocateWordInteger# :: Int# -> Word# -> Integer
allocateWordInteger# sign word =
  case newByteArray# 8# realWorld# of
    (# state0, mutable #) ->
      case writeWordArray# mutable 0# word state0 of
        state1 ->
          case unsafeFreezeByteArray# mutable state1 of
            (# _, magnitude #) ->
              case sign of
                1# -> IP magnitude
                _ -> IN magnitude

maxInt# :: Int# -> Int# -> Int#
maxInt# left right =
  case (<#) left right of
    1# -> right
    _ -> left
