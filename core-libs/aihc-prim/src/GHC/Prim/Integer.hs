{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.Prim.Integer
  ( Integer (..),
    compareInteger#,
    eqInteger#,
    integerAbs,
    integerAdd,
    integerAnd,
    integerBit#,
    integerComplement,
    integerFromMagnitude#,
    integerFromTwoWords#,
    integerFromWord#,
    integerLog2#,
    integerLogBase#,
    integerMul,
    integerNegate,
    integerQuotRem,
    integerOr,
    integerPopCount#,
    integerQuotRemWord#,
    integerShiftL#,
    integerShiftR#,
    integerSignum,
    integerSub,
    integerTestBit#,
    integerToInt#,
    integerXor,
    wordLog2#,
  )
where

import GHC.Prim
  ( ByteArray#,
    Int#,
    MutableByteArray#,
    RealWorld,
    State#,
    Word#,
    addIntC#,
    addWordC#,
    and#,
    clz#,
    copyByteArray#,
    eqWord#,
    indexWordArray#,
    int2Word#,
    ltWord#,
    minusWord#,
    newByteArray#,
    not#,
    or#,
    plusWord#,
    popCnt#,
    quotRemWord2#,
    quotWord#,
    readWordArray#,
    realWorld#,
    shrinkMutableByteArray#,
    sizeofByteArray#,
    subIntC#,
    subWordC#,
    timesWord#,
    timesWord2#,
    uncheckedIShiftRA#,
    uncheckedShiftL#,
    uncheckedShiftRL#,
    unsafeFreezeByteArray#,
    word2Int#,
    writeWordArray#,
    xor#,
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
integerAdd left@(IS leftInt) right@(IS rightInt) =
  case addIntC# leftInt rightInt of
    (# result, 0# #) -> IS result
    _ -> addMagnitudesWithSigns left right
integerAdd (IP left) (IP right) = IP (addByteArrays# left right)
integerAdd (IN left) (IN right) = IN (addByteArrays# left right)
integerAdd left right = addMagnitudesWithSigns left right

integerSub :: Integer -> Integer -> Integer
integerSub left@(IS leftInt) right@(IS rightInt) =
  case subIntC# leftInt rightInt of
    (# result, 0# #) -> IS result
    _ -> integerAdd left (integerNegate right)
integerSub left right = integerAdd left (integerNegate right)

integerMul :: Integer -> Integer -> Integer
integerMul (IS left) (IS right) = multiplySmall# left right
integerMul left right = multiplyLarge left right

multiplySmall# :: Int# -> Int# -> Integer
multiplySmall# 0# _ = IS 0#
multiplySmall# _ 0# = IS 0#
multiplySmall# left right =
  case timesWord2# (absoluteIntWord# left) (absoluteIntWord# right) of
    (# high, low #) ->
      case (left <# 0#) ==# (right <# 0#) of
        1# -> integerFromTwoWords# 1# high low
        _ -> integerFromTwoWords# (0# -# 1#) high low

multiplyLarge :: Integer -> Integer -> Integer
multiplyLarge (IS factor) value = multiplyByInt# value factor
multiplyLarge value (IS factor) = multiplyByInt# value factor
multiplyLarge left right =
  integerFromMagnitude# (signInteger# left *# signInteger# right) (multiplyMagnitudes# left right)

multiplyByInt# :: Integer -> Int# -> Integer
multiplyByInt# _ 0# = IS 0#
multiplyByInt# value 1# = value
multiplyByInt# value factor =
  case factor ==# (0# -# 1#) of
    1# -> integerNegate value
    _ ->
      case factor <# 0# of
        1# -> integerFromMagnitude# (0# -# signInteger# value) (multiplyMagnitudeByWord# (magnitudeBytes# value) (absoluteIntWord# factor))
        _ -> integerFromMagnitude# (signInteger# value) (multiplyMagnitudeByWord# (magnitudeBytes# value) (int2Word# factor))

multiplyMagnitudeByWord# :: ByteArray# -> Word# -> ByteArray#
multiplyMagnitudeByWord# magnitude factor =
  let count = wordCount# magnitude
   in case newByteArray# ((count +# 1#) *# 8#) realWorld# of
        (# state, mutable #) ->
          case multiplyWordLoop# magnitude factor mutable count 0# (int2Word# 0#) state of
            (# state1, used #) -> freezeTrimmed# mutable used state1

multiplyWordLoop# :: ByteArray# -> Word# -> MutableByteArray# RealWorld -> Int# -> Int# -> Word# -> State# RealWorld -> (# State# RealWorld, Int# #)
multiplyWordLoop# magnitude factor mutable count index carry state =
  case index ==# count of
    1# ->
      case eqWord# carry (int2Word# 0#) of
        1# -> (# state, count #)
        _ ->
          case writeWordArray# mutable index carry state of
            state1 -> (# state1, count +# 1# #)
    _ ->
      case timesWord2# (indexWordArray# magnitude index) factor of
        (# high, low #) ->
          case addWordC# low carry of
            (# result, overflow #) ->
              case writeWordArray# mutable index result state of
                state1 -> multiplyWordLoop# magnitude factor mutable count (index +# 1#) (plusWord# high (int2Word# overflow)) state1

integerNegate :: Integer -> Integer
integerNegate (IP magnitude) = IN magnitude
integerNegate (IN magnitude) = IP magnitude
integerNegate (IS value) =
  case subIntC# 0# value of
    (# result, 0# #) -> IS result
    _ -> integerFromWord# 1# (int2Word# value)

integerAbs :: Integer -> Integer
integerAbs (IN magnitude) = IP magnitude
integerAbs value@(IS small) =
  case small <# 0# of
    0# -> value
    _ -> integerNegate value
integerAbs value = value

integerSignum :: Integer -> Integer
integerSignum value = IS (signInteger# value)

integerAnd :: Integer -> Integer -> Integer
integerAnd (IS 0#) _ = IS 0#
integerAnd (IS left) (IS right) = IS (word2Int# (and# (int2Word# left) (int2Word# right)))
integerAnd left right = integerAndLarge left right

integerAndLarge :: Integer -> Integer -> Integer
integerAndLarge left right =
  case signInteger# left of
    1# ->
      case signInteger# right of
        1# -> positiveBitwise# 0# left right
        0# -> IS 0#
        _ -> positiveAndNot left (integerPredecessorMagnitude right)
    0# -> IS 0#
    _ ->
      case signInteger# right of
        1# -> positiveAndNot right (integerPredecessorMagnitude left)
        0# -> IS 0#
        _ -> negativeFromComplement (positiveBitwise# 1# (integerPredecessorMagnitude left) (integerPredecessorMagnitude right))

integerOr :: Integer -> Integer -> Integer
integerOr (IS left) (IS right) = IS (word2Int# (or# (int2Word# left) (int2Word# right)))
integerOr left right =
  case signInteger# left of
    1# ->
      case signInteger# right of
        1# -> positiveBitwise# 1# left right
        0# -> left
        _ -> negativeFromComplement (positiveAndNot (integerPredecessorMagnitude right) left)
    0# -> right
    _ ->
      case signInteger# right of
        1# -> negativeFromComplement (positiveAndNot (integerPredecessorMagnitude left) right)
        0# -> left
        _ -> negativeFromComplement (positiveBitwise# 0# (integerPredecessorMagnitude left) (integerPredecessorMagnitude right))

integerXor :: Integer -> Integer -> Integer
integerXor (IS left) (IS right) = IS (word2Int# (xor# (int2Word# left) (int2Word# right)))
integerXor left right =
  case signInteger# left of
    1# ->
      case signInteger# right of
        1# -> positiveBitwise# 2# left right
        0# -> left
        _ -> negativeFromComplement (positiveBitwise# 2# left (integerPredecessorMagnitude right))
    0# -> right
    _ ->
      case signInteger# right of
        1# -> negativeFromComplement (positiveBitwise# 2# (integerPredecessorMagnitude left) right)
        0# -> left
        _ -> positiveBitwise# 2# (integerPredecessorMagnitude left) (integerPredecessorMagnitude right)

integerComplement :: Integer -> Integer
integerComplement (IS value) = IS (word2Int# (not# (int2Word# value)))
integerComplement value = integerSub (integerNegate value) (IS 1#)

integerBit# :: Int# -> Integer
integerBit# amount =
  case amount <# 0# of
    1# -> IS 0#
    _ -> integerShiftL# (IS 1#) amount

integerTestBit# :: Integer -> Int# -> Int#
integerTestBit# value amount =
  case amount <# 0# of
    1# -> 0#
    _ ->
      case signInteger# value <# 0# of
        1# ->
          case testMagnitudeBit# (integerPredecessorMagnitude value) amount of
            0# -> 1#
            _ -> 0#
        _ -> testMagnitudeBit# value amount

integerShiftL# :: Integer -> Int# -> Integer
integerShiftL# value 0# = value
integerShiftL# value amount =
  case amount <# 0# of
    1# -> integerShiftL# value amount
    _ ->
      case signInteger# value of
        0# -> IS 0#
        sign -> integerFromMagnitude# sign (shiftMagnitudeL# value amount)

integerShiftR# :: Integer -> Int# -> Integer
integerShiftR# value 0# = value
integerShiftR# value@(IS small) amount =
  case amount <# 0# of
    1# -> integerShiftR# value amount
    _ ->
      case amount <# 64# of
        1# -> IS (uncheckedIShiftRA# small amount)
        _ ->
          case small <# 0# of
            1# -> IS (0# -# 1#)
            _ -> IS 0#
integerShiftR# value amount =
  case amount <# 0# of
    1# -> integerShiftR# value amount
    _ ->
      case signInteger# value of
        0# -> IS 0#
        1# -> integerFromMagnitude# 1# (shiftMagnitudeR# value amount)
        _ -> negativeFromComplement (integerFromMagnitude# 1# (shiftMagnitudeR# (integerPredecessorMagnitude value) amount))

integerPopCount# :: Integer -> Int#
integerPopCount# (IS small) =
  let count = word2Int# (popCnt# (absoluteIntWord# small))
   in case small <# 0# of
        1# -> 0# -# count
        _ -> count
integerPopCount# (IP magnitude) = popCountMagnitude# magnitude (wordCount# magnitude) 0# 0#
integerPopCount# (IN magnitude) = 0# -# popCountMagnitude# magnitude (wordCount# magnitude) 0# 0#

integerPredecessorMagnitude :: Integer -> Integer
integerPredecessorMagnitude value = integerSub (integerAbs value) (IS 1#)

negativeFromComplement :: Integer -> Integer
negativeFromComplement value = integerNegate (integerAdd value (IS 1#))

positiveBitwise# :: Int# -> Integer -> Integer -> Integer
positiveBitwise# operation left right =
  let leftBytes = magnitudeBytes# left
      rightBytes = magnitudeBytes# right
      leftSize = wordCount# leftBytes
      rightSize = wordCount# rightBytes
      count = bitwiseSize# operation leftSize rightSize
   in case newByteArray# (count *# 8#) realWorld# of
        (# state, mutable #) ->
          case writeBitwiseWords# operation leftBytes rightBytes leftSize rightSize mutable count 0# state of
            (# state1, _ #) ->
              case trimMagnitudeWords# mutable (count -# 1#) state1 of
                (# state2, used #) -> integerFromMagnitude# 1# (freezeTrimmed# mutable used state2)

bitwiseSize# :: Int# -> Int# -> Int# -> Int#
bitwiseSize# 0# leftSize rightSize = minInt# leftSize rightSize
bitwiseSize# _ leftSize rightSize = maxInt# leftSize rightSize

positiveAndNot :: Integer -> Integer -> Integer
positiveAndNot left right =
  let leftBytes = magnitudeBytes# left
      rightBytes = magnitudeBytes# right
      count = wordCount# leftBytes
      rightSize = wordCount# rightBytes
   in case newByteArray# (count *# 8#) realWorld# of
        (# state, mutable #) ->
          case writeAndNotWords# leftBytes rightBytes rightSize mutable count 0# state of
            (# state1, _ #) ->
              case trimMagnitudeWords# mutable (count -# 1#) state1 of
                (# state2, used #) -> integerFromMagnitude# 1# (freezeTrimmed# mutable used state2)

writeBitwiseWords# :: Int# -> ByteArray# -> ByteArray# -> Int# -> Int# -> MutableByteArray# RealWorld -> Int# -> Int# -> State# RealWorld -> (# State# RealWorld, Int# #)
writeBitwiseWords# operation left right leftSize rightSize mutable count index state =
  case index ==# count of
    1# -> (# state, index #)
    _ ->
      let result = bitwiseWord# operation (byteArrayWordOrZero# left leftSize index) (byteArrayWordOrZero# right rightSize index)
       in case writeWordArray# mutable index result state of
            state1 -> writeBitwiseWords# operation left right leftSize rightSize mutable count (index +# 1#) state1

writeAndNotWords# :: ByteArray# -> ByteArray# -> Int# -> MutableByteArray# RealWorld -> Int# -> Int# -> State# RealWorld -> (# State# RealWorld, Int# #)
writeAndNotWords# left right rightSize mutable count index state =
  case index ==# count of
    1# -> (# state, index #)
    _ ->
      let result = and# (indexWordArray# left index) (not# (byteArrayWordOrZero# right rightSize index))
       in case writeWordArray# mutable index result state of
            state1 -> writeAndNotWords# left right rightSize mutable count (index +# 1#) state1

bitwiseWord# :: Int# -> Word# -> Word# -> Word#
bitwiseWord# 0# left right = and# left right
bitwiseWord# 1# left right = or# left right
bitwiseWord# _ left right = xor# left right

splitBitIndex# :: Int# -> (# Int#, Int# #)
splitBitIndex# amount =
  (# word2Int# (uncheckedShiftRL# (int2Word# amount) 6#), word2Int# (and# (int2Word# amount) (int2Word# 63#)) #)

testMagnitudeBit# :: Integer -> Int# -> Int#
testMagnitudeBit# value amount =
  case splitBitIndex# amount of
    (# wordIndex, bitIndex #) ->
      case wordIndex <# magnitudeSize# value of
        1# ->
          case eqWord# (and# (magnitudeWord# value wordIndex) (uncheckedShiftL# (int2Word# 1#) bitIndex)) (int2Word# 0#) of
            1# -> 0#
            _ -> 1#
        _ -> 0#

shiftMagnitudeL# :: Integer -> Int# -> ByteArray#
shiftMagnitudeL# value amount =
  let magnitude = magnitudeBytes# value
      count = wordCount# magnitude
   in case splitBitIndex# amount of
        (# wordShift, bitShift #) ->
          let resultSize = ((count +# wordShift) +# 1#)
           in case newByteArray# (resultSize *# 8#) realWorld# of
                (# state, mutable #) ->
                  case zeroMagnitudeWords# mutable wordShift 0# state of
                    (# state1, _ #) ->
                      case writeShiftedLeftWords# magnitude count mutable wordShift bitShift 0# (int2Word# 0#) state1 of
                        (# state2, used #) -> freezeTrimmed# mutable used state2

writeShiftedLeftWords# :: ByteArray# -> Int# -> MutableByteArray# RealWorld -> Int# -> Int# -> Int# -> Word# -> State# RealWorld -> (# State# RealWorld, Int# #)
writeShiftedLeftWords# magnitude count mutable wordShift bitShift index carry state =
  case index ==# count of
    1# ->
      case eqWord# carry (int2Word# 0#) of
        1# -> (# state, wordShift +# index #)
        _ ->
          case writeWordArray# mutable (wordShift +# index) carry state of
            state1 -> (# state1, (wordShift +# index) +# 1# #)
    _ ->
      case shiftedLeftWord# (indexWordArray# magnitude index) bitShift carry of
        (# result, nextCarry #) ->
          case writeWordArray# mutable (wordShift +# index) result state of
            state1 -> writeShiftedLeftWords# magnitude count mutable wordShift bitShift (index +# 1#) nextCarry state1

shiftedLeftWord# :: Word# -> Int# -> Word# -> (# Word#, Word# #)
shiftedLeftWord# word 0# _ = (# word, int2Word# 0# #)
shiftedLeftWord# word bitShift carry =
  (# or# (uncheckedShiftL# word bitShift) carry, uncheckedShiftRL# word (64# -# bitShift) #)

shiftMagnitudeR# :: Integer -> Int# -> ByteArray#
shiftMagnitudeR# value amount =
  let magnitude = magnitudeBytes# value
      count = wordCount# magnitude
   in case splitBitIndex# amount of
        (# wordShift, bitShift #) ->
          case wordShift <# count of
            0# -> emptyMagnitude# 0#
            _ ->
              let resultSize = (count -# wordShift)
               in case newByteArray# (resultSize *# 8#) realWorld# of
                    (# state, mutable #) ->
                      case writeShiftedRightWords# magnitude count mutable wordShift bitShift resultSize 0# state of
                        (# state1, _ #) ->
                          case trimMagnitudeWords# mutable (resultSize -# 1#) state1 of
                            (# state2, used #) -> freezeTrimmed# mutable used state2

writeShiftedRightWords# :: ByteArray# -> Int# -> MutableByteArray# RealWorld -> Int# -> Int# -> Int# -> Int# -> State# RealWorld -> (# State# RealWorld, Int# #)
writeShiftedRightWords# magnitude count mutable wordShift bitShift resultSize index state =
  case index ==# resultSize of
    1# -> (# state, index #)
    _ ->
      let result = shiftedRightWord# magnitude count (wordShift +# index) bitShift
       in case writeWordArray# mutable index result state of
            state1 -> writeShiftedRightWords# magnitude count mutable wordShift bitShift resultSize (index +# 1#) state1

shiftedRightWord# :: ByteArray# -> Int# -> Int# -> Int# -> Word#
shiftedRightWord# magnitude _ sourceIndex 0# = indexWordArray# magnitude sourceIndex
shiftedRightWord# magnitude count sourceIndex bitShift =
  or#
    (uncheckedShiftRL# (indexWordArray# magnitude sourceIndex) bitShift)
    (uncheckedShiftL# (byteArrayWordOrZero# magnitude count (sourceIndex +# 1#)) (64# -# bitShift))

emptyMagnitude# :: Int# -> ByteArray#
emptyMagnitude# size =
  case newByteArray# size realWorld# of
    (# state, mutable #) ->
      case unsafeFreezeByteArray# mutable state of
        (# _, magnitude #) -> magnitude

popCountMagnitude# :: ByteArray# -> Int# -> Int# -> Int# -> Int#
popCountMagnitude# magnitude count index total =
  case index ==# count of
    1# -> total
    _ -> popCountMagnitude# magnitude count (index +# 1#) (total +# word2Int# (popCnt# (indexWordArray# magnitude index)))

integerToInt# :: Integer -> Int#
integerToInt# (IS value) = value
integerToInt# (IP magnitude) = word2Int# (indexWordArray# magnitude 0#)
integerToInt# (IN magnitude) = 0# -# word2Int# (indexWordArray# magnitude 0#)

-- | The base 2 logarithm of a 'Word#', rounded down.  @wordLog2# 0##@ is
-- @-1@ read as an 'Int#'.
wordLog2# :: Word# -> Word#
wordLog2# value = minusWord# (int2Word# 63#) (clz# value)

-- | The base 2 logarithm of a positive 'Integer', rounded down.  The
-- magnitude is canonical, so only its most significant limb matters.
integerLog2# :: Integer -> Word#
integerLog2# value =
  case magnitudeSize# value of
    0# -> wordLog2# (int2Word# 0#)
    wordCount ->
      let top = (wordCount -# 1#)
       in plusWord#
            (wordLog2# (magnitudeWord# value top))
            (int2Word# (top *# 64#))

-- | The logarithm of a positive 'Integer' to a base greater than one,
-- rounded down.  Other arguments give a meaningless result.
integerLogBase# :: Integer -> Integer -> Word#
integerLogBase# base value =
  case eqInteger# base (IS 2#) of
    1# -> integerLog2# value
    _ ->
      case logBaseStep# value base of
        (# _, exponent #) -> exponent

-- | @logBaseStep# m pw@ divides @m@ by the largest power of @pw@ below it,
-- returning the remaining quotient and how many times @pw@ went into @m@.
logBaseStep# :: Integer -> Integer -> (# Integer, Word# #)
logBaseStep# value power =
  case compareInteger# value power <# 0# of
    1# -> (# value, int2Word# 0# #)
    _ ->
      case logBaseStep# value (integerMul power power) of
        (# rest, exponent #) ->
          let doubled = timesWord# exponent (int2Word# 2#)
           in case compareInteger# rest power <# 0# of
                1# -> (# rest, doubled #)
                _ ->
                  case integerQuotRem rest power of
                    (quotient, _) -> (# quotient, plusWord# doubled (int2Word# 1#) #)

integerQuotRem :: Integer -> Integer -> (Integer, Integer)
integerQuotRem numerator denominator =
  case signInteger# denominator of
    0# -> integerDivisionByZero
    denominatorSign ->
      case signInteger# numerator of
        0# -> (IS 0#, IS 0#)
        numeratorSign ->
          case positiveQuotRem (integerAbs numerator) (integerAbs denominator) of
            (quotient, remainder) ->
              case numeratorSign ==# denominatorSign of
                1# -> (quotient, signedRemainder numeratorSign remainder)
                _ -> (integerNegate quotient, signedRemainder numeratorSign remainder)

signedRemainder :: Int# -> Integer -> Integer
signedRemainder 1# remainder = remainder
signedRemainder _ remainder = integerNegate remainder

positiveQuotRem :: Integer -> Integer -> (Integer, Integer)
positiveQuotRem dividend divisor =
  case magnitudeSize# divisor of
    1# ->
      case integerQuotRemWord# dividend (magnitudeWord# divisor 0#) of
        (# quotient, remainder #) -> (quotient, integerFromWord# 1# remainder)
    _ -> divideFromScale dividend divisor (IS 1#)

divideFromScale :: Integer -> Integer -> Integer -> (Integer, Integer)
divideFromScale dividend scaledDivisor quotientBit =
  case compareMagnitudes# scaledDivisor dividend of
    1# -> (IS 0#, dividend)
    _ ->
      case divideFromScale dividend (integerAdd scaledDivisor scaledDivisor) (integerAdd quotientBit quotientBit) of
        (quotient, remainder) ->
          case compareMagnitudes# scaledDivisor remainder of
            1# -> (quotient, remainder)
            _ -> (integerAdd quotient quotientBit, integerSub remainder scaledDivisor)

integerDivisionByZero :: a
integerDivisionByZero = integerDivisionByZero

integerQuotRemWord# :: Integer -> Word# -> (# Integer, Word# #)
integerQuotRemWord# (IS 0#) _ = (# IS 0#, int2Word# 0# #)
integerQuotRemWord# value@(IS small) divisor =
  case quotRemWord2# (int2Word# 0#) (absoluteIntWord# small) divisor of
    (# quotient, remainder #) -> (# integerFromWord# (signInteger# value) quotient, remainder #)
integerQuotRemWord# (IP magnitude) divisor = divideByteArrayByWord# 1# magnitude divisor
integerQuotRemWord# (IN magnitude) divisor = divideByteArrayByWord# (0# -# 1#) magnitude divisor

divideByteArrayByWord# :: Int# -> ByteArray# -> Word# -> (# Integer, Word# #)
divideByteArrayByWord# sign magnitude divisor =
  let count = wordCount# magnitude
   in case newByteArray# (count *# 8#) realWorld# of
        (# state, mutable #) ->
          case divideMagnitudeByWord# magnitude divisor mutable (count -# 1#) (int2Word# 0#) state of
            (# state1, remainder #) ->
              case trimMagnitudeWords# mutable (count -# 1#) state1 of
                (# state2, used #) -> (# integerFromMagnitude# sign (freezeTrimmed# mutable used state2), remainder #)

divideMagnitudeByWord# :: ByteArray# -> Word# -> MutableByteArray# RealWorld -> Int# -> Word# -> State# RealWorld -> (# State# RealWorld, Word# #)
divideMagnitudeByWord# magnitude divisor mutable index remainder state =
  case index <# 0# of
    1# -> (# state, remainder #)
    _ ->
      case quotRemWord2# remainder (indexWordArray# magnitude index) divisor of
        (# quotientWord, nextRemainder #) ->
          case writeWordArray# mutable index quotientWord state of
            state1 -> divideMagnitudeByWord# magnitude divisor mutable (index -# 1#) nextRemainder state1

compareInteger# :: Integer -> Integer -> Int#
compareInteger# left right =
  let leftSign = signInteger# left
      rightSign = signInteger# right
   in case leftSign <# rightSign of
        1# -> 0# -# 1#
        _ ->
          case rightSign <# leftSign of
            1# -> 1#
            _ ->
              case leftSign of
                0# -> 0#
                1# -> compareMagnitudes# left right
                _ -> 0# -# compareMagnitudes# left right

eqInteger# :: Integer -> Integer -> Int#
eqInteger# left right = compareInteger# left right ==# 0#

addMagnitudesWithSigns :: Integer -> Integer -> Integer
addMagnitudesWithSigns left right =
  case signInteger# left of
    0# -> right
    leftSign ->
      case signInteger# right of
        0# -> left
        rightSign ->
          case leftSign ==# rightSign of
            1# -> integerFromMagnitude# leftSign (addMagnitudes# left right)
            _ ->
              case compareMagnitudes# left right of
                0# -> IS 0#
                1# -> integerFromMagnitude# leftSign (subtractMagnitudes# left right)
                _ -> integerFromMagnitude# rightSign (subtractMagnitudes# right left)

signInteger# :: Integer -> Int#
signInteger# (IP _) = 1#
signInteger# (IN _) = 0# -# 1#
signInteger# (IS 0#) = 0#
signInteger# (IS value) =
  case value <# 0# of
    1# -> 0# -# 1#
    _ -> 1#

magnitudeSize# :: Integer -> Int#
magnitudeSize# (IS 0#) = 0#
magnitudeSize# (IS _) = 1#
magnitudeSize# (IP magnitude) = wordCount# magnitude
magnitudeSize# (IN magnitude) = wordCount# magnitude

magnitudeWord# :: Integer -> Int# -> Word#
magnitudeWord# (IS value) _ = absoluteIntWord# value
magnitudeWord# (IP magnitude) index = indexWordArray# magnitude index
magnitudeWord# (IN magnitude) index = indexWordArray# magnitude index

absoluteIntWord# :: Int# -> Word#
absoluteIntWord# value =
  case value <# 0# of
    0# -> int2Word# value
    _ -> int2Word# (0# -# value)

wordCount# :: ByteArray# -> Int#
wordCount# magnitude = word2Int# (quotWord# (int2Word# (sizeofByteArray# magnitude)) (int2Word# 8#))

compareMagnitudes# :: Integer -> Integer -> Int#
compareMagnitudes# left right =
  let leftSize = magnitudeSize# left
      rightSize = magnitudeSize# right
   in case leftSize <# rightSize of
        1# -> 0# -# 1#
        _ ->
          case rightSize <# leftSize of
            1# -> 1#
            _ ->
              case leftSize of
                0# -> 0#
                1# -> compareWords# (magnitudeWord# left 0#) (magnitudeWord# right 0#)
                _ -> compareMagnitudeWords# (magnitudeBytes# left) (magnitudeBytes# right) (leftSize -# 1#)

compareWords# :: Word# -> Word# -> Int#
compareWords# left right =
  case ltWord# left right of
    1# -> 0# -# 1#
    _ ->
      case ltWord# right left of
        1# -> 1#
        _ -> 0#

compareMagnitudeWords# :: ByteArray# -> ByteArray# -> Int# -> Int#
compareMagnitudeWords# left right index =
  case index <# 0# of
    1# -> 0#
    _ ->
      case compareWords# (indexWordArray# left index) (indexWordArray# right index) of
        0# -> compareMagnitudeWords# left right (index -# 1#)
        result -> result

-- Extract operands once. Word loops use arrays and unboxed sizes.
magnitudeBytes# :: Integer -> ByteArray#
magnitudeBytes# (IP magnitude) = magnitude
magnitudeBytes# (IN magnitude) = magnitude
magnitudeBytes# (IS value) =
  case newByteArray# 8# realWorld# of
    (# state, mutable #) ->
      case writeWordArray# mutable 0# (absoluteIntWord# value) state of
        state1 ->
          case unsafeFreezeByteArray# mutable state1 of
            (# _, magnitude #) -> magnitude

byteArrayWordOrZero# :: ByteArray# -> Int# -> Int# -> Word#
byteArrayWordOrZero# magnitude count index =
  case index <# count of
    1# -> indexWordArray# magnitude index
    _ -> int2Word# 0#

addMagnitudes# :: Integer -> Integer -> ByteArray#
addMagnitudes# (IS value) right = addByteArrayWord# (magnitudeBytes# right) (absoluteIntWord# value)
addMagnitudes# left (IS value) = addByteArrayWord# (magnitudeBytes# left) (absoluteIntWord# value)
addMagnitudes# left right = addByteArrays# (magnitudeBytes# left) (magnitudeBytes# right)

addByteArrayWord# :: ByteArray# -> Word# -> ByteArray#
addByteArrayWord# magnitude word =
  let count = wordCount# magnitude
   in case newByteArray# ((count +# 1#) *# 8#) realWorld# of
        (# state, mutable #) ->
          case addRemainingWords# magnitude mutable count 0# word state of
            (# state1, used #) -> freezeTrimmed# mutable used state1

addByteArrays# :: ByteArray# -> ByteArray# -> ByteArray#
addByteArrays# left right =
  let leftSize = wordCount# left
      rightSize = wordCount# right
   in case leftSize <# rightSize of
        1# -> addSizedByteArrays# right left rightSize leftSize
        _ -> addSizedByteArrays# left right leftSize rightSize

addSizedByteArrays# :: ByteArray# -> ByteArray# -> Int# -> Int# -> ByteArray#
addSizedByteArrays# larger smaller largerSize smallerSize =
  case newByteArray# ((largerSize +# 1#) *# 8#) realWorld# of
    (# state, mutable #) ->
      case addMagnitudeWords# larger smaller mutable smallerSize 0# 0# state of
        (# state1, carry #) ->
          case addRemainingWords# larger mutable largerSize smallerSize (int2Word# carry) state1 of
            (# state2, used #) -> freezeTrimmed# mutable used state2

addMagnitudeWords# :: ByteArray# -> ByteArray# -> MutableByteArray# RealWorld -> Int# -> Int# -> Int# -> State# RealWorld -> (# State# RealWorld, Int# #)
addMagnitudeWords# left right mutable count index carry state =
  case index ==# count of
    1# -> (# state, carry #)
    _ ->
      case addWordC# (indexWordArray# left index) (indexWordArray# right index) of
        (# partial, carry0 #) ->
          case addWordC# partial (int2Word# carry) of
            (# result, carry1 #) ->
              case writeWordArray# mutable index result state of
                state1 -> addMagnitudeWords# left right mutable count (index +# 1#) (carry0 +# carry1) state1

addRemainingWords# :: ByteArray# -> MutableByteArray# RealWorld -> Int# -> Int# -> Word# -> State# RealWorld -> (# State# RealWorld, Int# #)
addRemainingWords# magnitude mutable count index carry state =
  case index ==# count of
    1# ->
      case eqWord# carry (int2Word# 0#) of
        1# -> (# state, count #)
        _ ->
          case writeWordArray# mutable index carry state of
            state1 -> (# state1, count +# 1# #)
    _ ->
      case eqWord# carry (int2Word# 0#) of
        1# ->
          case copyMagnitudeTail# magnitude mutable count index state of
            state1 -> (# state1, count #)
        _ ->
          case addWordC# (indexWordArray# magnitude index) carry of
            (# result, nextCarry #) ->
              case writeWordArray# mutable index result state of
                state1 -> addRemainingWords# magnitude mutable count (index +# 1#) (int2Word# nextCarry) state1

subtractMagnitudes# :: Integer -> Integer -> ByteArray#
subtractMagnitudes# larger (IS smaller) = subtractByteArrayWord# (magnitudeBytes# larger) (absoluteIntWord# smaller)
subtractMagnitudes# larger smaller =
  let largerBytes = magnitudeBytes# larger
      smallerBytes = magnitudeBytes# smaller
      count = wordCount# largerBytes
      smallerSize = wordCount# smallerBytes
   in case newByteArray# (count *# 8#) realWorld# of
        (# state, mutable #) ->
          case subtractMagnitudeWords# largerBytes smallerBytes mutable smallerSize 0# 0# state of
            (# state1, borrow #) ->
              case subtractRemainingWords# largerBytes mutable count smallerSize (int2Word# borrow) state1 of
                (# state2, _ #) ->
                  case trimMagnitudeWords# mutable (count -# 1#) state2 of
                    (# state3, used #) -> freezeTrimmed# mutable used state3

subtractByteArrayWord# :: ByteArray# -> Word# -> ByteArray#
subtractByteArrayWord# magnitude word =
  let count = wordCount# magnitude
   in case newByteArray# (count *# 8#) realWorld# of
        (# state, mutable #) ->
          case subtractRemainingWords# magnitude mutable count 0# word state of
            (# state1, _ #) ->
              case trimMagnitudeWords# mutable (count -# 1#) state1 of
                (# state2, used #) -> freezeTrimmed# mutable used state2

subtractMagnitudeWords# :: ByteArray# -> ByteArray# -> MutableByteArray# RealWorld -> Int# -> Int# -> Int# -> State# RealWorld -> (# State# RealWorld, Int# #)
subtractMagnitudeWords# larger smaller mutable count index borrow state =
  case index ==# count of
    1# -> (# state, borrow #)
    _ ->
      case subWordC# (indexWordArray# larger index) (indexWordArray# smaller index) of
        (# partial, borrow0 #) ->
          case subWordC# partial (int2Word# borrow) of
            (# result, borrow1 #) ->
              case writeWordArray# mutable index result state of
                state1 -> subtractMagnitudeWords# larger smaller mutable count (index +# 1#) (borrow0 +# borrow1) state1

subtractRemainingWords# :: ByteArray# -> MutableByteArray# RealWorld -> Int# -> Int# -> Word# -> State# RealWorld -> (# State# RealWorld, Word# #)
subtractRemainingWords# magnitude mutable count index borrow state =
  case index ==# count of
    1# -> (# state, borrow #)
    _ ->
      case eqWord# borrow (int2Word# 0#) of
        1# ->
          case copyMagnitudeTail# magnitude mutable count index state of
            state1 -> (# state1, int2Word# 0# #)
        _ ->
          case subWordC# (indexWordArray# magnitude index) borrow of
            (# result, nextBorrow #) ->
              case writeWordArray# mutable index result state of
                state1 -> subtractRemainingWords# magnitude mutable count (index +# 1#) (int2Word# nextBorrow) state1

copyMagnitudeTail# :: ByteArray# -> MutableByteArray# RealWorld -> Int# -> Int# -> State# RealWorld -> State# RealWorld
copyMagnitudeTail# magnitude mutable count index =
  copyByteArray# magnitude (index *# 8#) mutable (index *# 8#) ((count -# index) *# 8#)

multiplyMagnitudes# :: Integer -> Integer -> ByteArray#
multiplyMagnitudes# left right =
  let leftBytes = magnitudeBytes# left
      rightBytes = magnitudeBytes# right
      leftSize = wordCount# leftBytes
      rightSize = wordCount# rightBytes
      resultSize = (leftSize +# rightSize)
   in case newByteArray# (resultSize *# 8#) realWorld# of
        (# state, mutable #) ->
          case zeroMagnitudeWords# mutable resultSize 0# state of
            (# state1, _ #) ->
              case multiplyOuter# leftBytes rightBytes mutable leftSize rightSize 0# state1 of
                (# state2, _ #) ->
                  case trimMagnitudeWords# mutable (resultSize -# 1#) state2 of
                    (# state3, used #) -> freezeTrimmed# mutable used state3

zeroMagnitudeWords# :: MutableByteArray# RealWorld -> Int# -> Int# -> State# RealWorld -> (# State# RealWorld, Int# #)
zeroMagnitudeWords# mutable wordCount index state =
  case index ==# wordCount of
    1# -> (# state, index #)
    _ ->
      case writeWordArray# mutable index (int2Word# 0#) state of
        state1 -> zeroMagnitudeWords# mutable wordCount (index +# 1#) state1

multiplyOuter# :: ByteArray# -> ByteArray# -> MutableByteArray# RealWorld -> Int# -> Int# -> Int# -> State# RealWorld -> (# State# RealWorld, Int# #)
multiplyOuter# left right mutable leftSize rightSize rightIndex state =
  case rightIndex ==# rightSize of
    1# -> (# state, rightIndex #)
    _ ->
      case multiplyInner# left mutable leftSize (indexWordArray# right rightIndex) rightIndex 0# (int2Word# 0#) state of
        (# state1, _ #) -> multiplyOuter# left right mutable leftSize rightSize (rightIndex +# 1#) state1

multiplyInner# :: ByteArray# -> MutableByteArray# RealWorld -> Int# -> Word# -> Int# -> Int# -> Word# -> State# RealWorld -> (# State# RealWorld, Word# #)
multiplyInner# left mutable leftSize rightWord rightIndex leftIndex carry state =
  case leftIndex ==# leftSize of
    1# ->
      case writeWordArray# mutable (leftSize +# rightIndex) carry state of
        state1 -> (# state1, carry #)
    _ ->
      case timesWord2# (indexWordArray# left leftIndex) rightWord of
        (# high, low #) ->
          case readWordArray# mutable (leftIndex +# rightIndex) state of
            (# state1, existing #) ->
              case addWordC# low existing of
                (# partial, carry0 #) ->
                  case addWordC# partial carry of
                    (# result, carry1 #) ->
                      case writeWordArray# mutable (leftIndex +# rightIndex) result state1 of
                        state2 -> multiplyInner# left mutable leftSize rightWord rightIndex (leftIndex +# 1#) (plusWord# high (int2Word# (carry0 +# carry1))) state2

trimMagnitudeWords# :: MutableByteArray# RealWorld -> Int# -> State# RealWorld -> (# State# RealWorld, Int# #)
trimMagnitudeWords# mutable index state =
  case index <# 0# of
    1# -> (# state, 0# #)
    _ ->
      case readWordArray# mutable index state of
        (# state1, word #) ->
          case eqWord# word (int2Word# 0#) of
            1# -> trimMagnitudeWords# mutable (index -# 1#) state1
            _ -> (# state1, index +# 1# #)

freezeTrimmed# :: MutableByteArray# RealWorld -> Int# -> State# RealWorld -> ByteArray#
freezeTrimmed# mutable usedWords state =
  case shrinkMutableByteArray# mutable (usedWords *# 8#) state of
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
      let intValue = word2Int# word
       in case sign of
            1# ->
              case intValue <# 0# of
                0# -> IS intValue
                _ -> allocateWordInteger# sign word
            _ ->
              case intValue <# 0# of
                0# -> IS (0# -# intValue)
                _ ->
                  case intValue ==# (0# -# intValue) of
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
  case left <# right of
    1# -> right
    _ -> left

minInt# :: Int# -> Int# -> Int#
minInt# left right =
  case left <# right of
    1# -> left
    _ -> right
