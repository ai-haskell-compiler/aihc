{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- wordToInteger keeps its argument: the arity pass reads the arity from the
-- body, and an eta-reduced alias is not inlined after phase 1.
{-# HLINT ignore "Eta reduce" #-}

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
    integerRem,
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
    magnitudeRemWord#,
    wordLog2#,
    wordToInteger,
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
    remWord#,
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
integerNegate (IP magnitude) = negatePositiveMagnitude# magnitude
integerNegate (IN magnitude) = IP magnitude
integerNegate (IS value) =
  case subIntC# 0# value of
    (# result, 0# #) -> IS result
    _ -> negatedMinimumInt

-- | The negation of an 'IP' magnitude. The magnitude 2^63 becomes the
-- minimum 'Int#', because the canonical form of that value is 'IS'.
negatePositiveMagnitude# :: ByteArray# -> Integer
negatePositiveMagnitude# magnitude =
  case wordCount# magnitude of
    1# ->
      let word = indexWordArray# magnitude 0#
       in case eqWord# word (uncheckedShiftL# (int2Word# 1#) 63#) of
            1# -> IS (word2Int# word)
            _ -> IN magnitude
    _ -> IN magnitude
{-# NOINLINE negatePositiveMagnitude# #-}

-- | The negation of the minimum 'Int#'. It is the only 'IS' value whose
-- negation is not an 'IS' value. A constant keeps the inlined negation small.
negatedMinimumInt :: Integer
negatedMinimumInt = allocateWordInteger# 1# (uncheckedShiftL# (int2Word# 1#) 63#)
{-# NOINLINE negatedMinimumInt #-}

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
-- GHC keeps integerToInt# as a call, thus the conversion rules can match it.
-- For example, the rule for wordToInteger removes the large Integer
-- branch from fromIntegral :: Word64 -> Word8. The rule below is GHC's
-- "Int# -> Integer -> Int#" rule. It removes a small Integer before phase 1.
{-# NOINLINE [0] integerToInt# #-}

{-# RULES
"integerToInt#/IS" forall x. integerToInt# (IS x) = x
  #-}

-- The low word of an 'Integer' that a sign and a magnitude make. A large
-- literal is such an 'Integer', thus @fromInteger@ of a large literal at a
-- word type needs no allocation.
{-# RULES
"integerToInt#/integerFromWord#" forall sign word. integerToInt# (integerFromWord# sign word) = signedWordToInt# sign word
"integerToInt#/integerFromLargeWord#" forall sign word. integerToInt# (integerFromLargeWord# sign word) = signedWordToInt# sign word
  #-}

-- | The low word of the 'Integer' with a sign and a magnitude. The sign 1
-- is positive, and each other sign is negative.
signedWordToInt# :: Int# -> Word# -> Int#
signedWordToInt# sign word =
  case sign of
    1# -> word2Int# word
    _ -> 0# -# word2Int# word

-- | The 'Integer' of a word. The small-value path is inline, and the large
-- Integer allocation is a call.
wordToInteger :: Word# -> Integer
wordToInteger value = integerFromWord# 1# value
-- The rule is GHC's "Word# -> Integer -> Int#" rule. It removes the large
-- Integer branch from a conversion such as fromIntegral :: Word64 -> Word8.
-- The rule cannot match after the inliner copies the body, thus the inliner
-- must keep the call until phase 1. GHC.Num.Natural has a rule for the
-- conversion to 'Natural' too.
{-# NOINLINE [0] wordToInteger #-}

{-# RULES
"integerToInt#/wordToInteger" forall x. integerToInt# (wordToInteger x) = word2Int# x
  #-}

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
  case compareMagnitudes# dividend divisor of
    0# -> (IS 1#, IS 0#)
    1# ->
      case magnitudeSize# divisor of
        1# ->
          case integerQuotRemWord# dividend (magnitudeWord# divisor 0#) of
            (# quotient, remainder #) -> (quotient, integerFromWord# 1# remainder)
        _ ->
          case divideByteArrays# (magnitudeBytes# dividend) (magnitudeBytes# divisor) 1# of
            (# quotient, remainder #) -> (integerFromMagnitude# 1# quotient, integerFromMagnitude# 1# remainder)
    _ -> (IS 0#, dividend)

-- | Divide magnitudes without a quotient array. The remainder has the
-- sign of the numerator.
integerRem :: Integer -> Integer -> Integer
integerRem (IP magnitude) (IS denominator) =
  case denominator of
    0# -> integerDivisionByZero
    _ -> IS (word2Int# (magnitudeRemWord# magnitude (absoluteIntWord# denominator)))
integerRem (IN magnitude) (IS denominator) =
  case denominator of
    0# -> integerDivisionByZero
    _ -> IS (0# -# word2Int# (magnitudeRemWord# magnitude (absoluteIntWord# denominator)))
integerRem numerator denominator =
  case signInteger# denominator of
    0# -> integerDivisionByZero
    _ ->
      case compareMagnitudes# numerator denominator of
        0# -> IS 0#
        1# -> signedRemainder (signInteger# numerator) (positiveRemainder numerator denominator)
        _ -> numerator

positiveRemainder :: Integer -> Integer -> Integer
positiveRemainder dividend divisor =
  case magnitudeSize# divisor of
    1# -> integerFromWord# 1# (integerRemWord# dividend (magnitudeWord# divisor 0#))
    _ ->
      case divideByteArrays# (magnitudeBytes# dividend) (magnitudeBytes# divisor) 0# of
        (# _, remainder #) -> integerFromMagnitude# 1# remainder

integerRemWord# :: Integer -> Word# -> Word#
integerRemWord# (IS small) divisor =
  case quotRemWord2# 0## (absoluteIntWord# small) divisor of
    (# _, remainder #) -> remainder
integerRemWord# (IP magnitude) divisor = remainderWords# magnitude divisor (wordCount# magnitude -# 1#) 0##
integerRemWord# (IN magnitude) divisor = remainderWords# magnitude divisor (wordCount# magnitude -# 1#) 0##

-- | The remainder of a magnitude and a nonzero divisor word. A big
-- magnitude has two or more limbs. Two limbs do not use the loop. The
-- result is less than the divisor, thus it is not more than 2^63 - 1.
magnitudeRemWord# :: ByteArray# -> Word# -> Word#
magnitudeRemWord# magnitude divisor =
  case wordCount# magnitude of
    2# ->
      case quotRemWord2# (remWord# (indexWordArray# magnitude 1#) divisor) (indexWordArray# magnitude 0#) divisor of
        (# _, remainder #) -> remainder
    size -> remainderWords# magnitude divisor (size -# 1#) 0##

remainderWords# :: ByteArray# -> Word# -> Int# -> Word# -> Word#
remainderWords# magnitude divisor index remainder =
  case index <# 0# of
    1# -> remainder
    _ ->
      case quotRemWord2# remainder (indexWordArray# magnitude index) divisor of
        (# _, next #) -> remainderWords# magnitude divisor (index -# 1#) next

-- | Normalize the divisor, then divide one limb at a time. Each step
-- changes the dividend scratch array in place. A remainder needs no
-- quotient array. The first result is unused when wantQuotient is zero.
divideByteArrays# :: ByteArray# -> ByteArray# -> Int# -> (# ByteArray#, ByteArray# #)
divideByteArrays# dividend divisor wantQuotient =
  let dividendSize = wordCount# dividend
      divisorSize = wordCount# divisor
      shift = word2Int# (clz# (indexWordArray# divisor (divisorSize -# 1#)))
   in case newByteArray# (divisorSize *# 8#) realWorld# of
        (# state, divisorScratch #) ->
          case normalizeDivisionWords# divisor divisorScratch divisorSize shift state of
            (# state1, _ #) ->
              case unsafeFreezeByteArray# divisorScratch state1 of
                (# state2, normalizedDivisor #) ->
                  case newByteArray# ((dividendSize +# 1#) *# 8#) state2 of
                    (# state3, dividendScratch #) ->
                      case normalizeDivisionWords# dividend dividendScratch dividendSize shift state3 of
                        (# state4, high #) ->
                          case writeWordArray# dividendScratch dividendSize high state4 of
                            state5 ->
                              case wantQuotient of
                                0# -> finishLimbDivision# normalizedDivisor dividendScratch dividendScratch dividendSize divisorSize shift 0# state5
                                _ ->
                                  case newByteArray# ((dividendSize -# divisorSize +# 1#) *# 8#) state5 of
                                    (# state6, quotient #) -> finishLimbDivision# normalizedDivisor dividendScratch quotient dividendSize divisorSize shift 1# state6

normalizeDivisionWords# :: ByteArray# -> MutableByteArray# RealWorld -> Int# -> Int# -> State# RealWorld -> (# State# RealWorld, Word# #)
normalizeDivisionWords# source target count shift state =
  case shift of
    0# ->
      case copyByteArray# source 0# target 0# (count *# 8#) state of
        state1 -> (# state1, 0## #)
    _ -> shiftDivisionWords# source target count shift 0# 0## state

shiftDivisionWords# :: ByteArray# -> MutableByteArray# RealWorld -> Int# -> Int# -> Int# -> Word# -> State# RealWorld -> (# State# RealWorld, Word# #)
shiftDivisionWords# source target count shift index carry state =
  case index ==# count of
    1# -> (# state, carry #)
    _ ->
      let word = indexWordArray# source index
          result = or# (uncheckedShiftL# word shift) carry
          next = uncheckedShiftRL# word (64# -# shift)
       in case writeWordArray# target index result state of
            state1 -> shiftDivisionWords# source target count shift (index +# 1#) next state1

finishLimbDivision# :: ByteArray# -> MutableByteArray# RealWorld -> MutableByteArray# RealWorld -> Int# -> Int# -> Int# -> Int# -> State# RealWorld -> (# ByteArray#, ByteArray# #)
finishLimbDivision# divisor dividend quotient dividendSize divisorSize shift wantQuotient state =
  case divideLimbLoop# divisor dividend quotient divisorSize (dividendSize -# divisorSize) wantQuotient state of
    state1 ->
      case unnormalizeRemainder# dividend divisorSize shift 0# state1 of
        state2 ->
          case trimMagnitudeWords# dividend (divisorSize -# 1#) state2 of
            (# state3, used #) ->
              case shrinkMutableByteArray# dividend (used *# 8#) state3 of
                state4 ->
                  case unsafeFreezeByteArray# dividend state4 of
                    (# state5, remainder #) ->
                      case wantQuotient of
                        0# -> (# divisor, remainder #)
                        _ ->
                          case trimMagnitudeWords# quotient (dividendSize -# divisorSize) state5 of
                            (# state6, quotientUsed #) -> (# freezeTrimmed# quotient quotientUsed state6, remainder #)

-- | Estimate a quotient limb from the top dividend limbs. Correct the
-- estimate with the next divisor limb before the multiply and subtraction.
divideLimbLoop# :: ByteArray# -> MutableByteArray# RealWorld -> MutableByteArray# RealWorld -> Int# -> Int# -> Int# -> State# RealWorld -> State# RealWorld
divideLimbLoop# divisor dividend quotient count offset wantQuotient state =
  case offset <# 0# of
    1# -> state
    _ ->
      case readWordArray# dividend (offset +# count) state of
        (# state1, high #) ->
          case readWordArray# dividend (offset +# count -# 1#) state1 of
            (# state2, low #) ->
              case readWordArray# dividend (offset +# count -# 2#) state2 of
                (# state3, next #) ->
                  let top = indexWordArray# divisor (count -# 1#)
                      second = indexWordArray# divisor (count -# 2#)
                   in case estimateQuotientLimb# high low next top second of
                        estimate ->
                          case subtractDivisorProduct# divisor dividend count offset estimate 0# 0## state3 of
                            (# state4, borrow #) ->
                              case borrow of
                                0# -> storeQuotientLimb# divisor dividend quotient count offset wantQuotient estimate state4
                                _ ->
                                  case addDivisorBack# divisor dividend count offset 0# 0## state4 of
                                    state5 -> storeQuotientLimb# divisor dividend quotient count offset wantQuotient (minusWord# estimate (int2Word# 1#)) state5

storeQuotientLimb# :: ByteArray# -> MutableByteArray# RealWorld -> MutableByteArray# RealWorld -> Int# -> Int# -> Int# -> Word# -> State# RealWorld -> State# RealWorld
storeQuotientLimb# divisor dividend quotient count offset wantQuotient word state =
  case wantQuotient of
    0# -> divideLimbLoop# divisor dividend quotient count (offset -# 1#) 0# state
    _ ->
      case writeWordArray# quotient offset word state of
        state1 -> divideLimbLoop# divisor dividend quotient count (offset -# 1#) 1# state1

estimateQuotientLimb# :: Word# -> Word# -> Word# -> Word# -> Word# -> Word#
estimateQuotientLimb# high low next top second =
  case eqWord# high top of
    1# ->
      case addWordC# low top of
        (# remainder, overflow #) ->
          case overflow of
            0# -> correctQuotientLimb# (not# 0##) remainder next top second
            _ -> not# 0##
    _ ->
      case quotRemWord2# high low top of
        (# quotient, remainder #) -> correctQuotientLimb# quotient remainder next top second

correctQuotientLimb# :: Word# -> Word# -> Word# -> Word# -> Word# -> Word#
correctQuotientLimb# quotient remainder next top second =
  case timesWord2# quotient second of
    (# high, low #) ->
      case ltWord# remainder high of
        1# -> decreaseQuotientLimb# quotient remainder next top second
        _ ->
          case eqWord# remainder high of
            1# ->
              case ltWord# next low of
                1# -> decreaseQuotientLimb# quotient remainder next top second
                _ -> quotient
            _ -> quotient

decreaseQuotientLimb# :: Word# -> Word# -> Word# -> Word# -> Word# -> Word#
decreaseQuotientLimb# quotient remainder next top second =
  let corrected = minusWord# quotient (int2Word# 1#)
   in case addWordC# remainder top of
        (# remainder1, overflow #) ->
          case overflow of
            0# -> correctQuotientLimb# corrected remainder1 next top second
            _ -> corrected

subtractDivisorProduct# :: ByteArray# -> MutableByteArray# RealWorld -> Int# -> Int# -> Word# -> Int# -> Word# -> State# RealWorld -> (# State# RealWorld, Int# #)
subtractDivisorProduct# divisor dividend count offset quotient index carry state =
  case index ==# count of
    1# ->
      case readWordArray# dividend (offset +# count) state of
        (# state1, word #) ->
          case subWordC# word carry of
            (# result, borrow #) ->
              case writeWordArray# dividend (offset +# count) result state1 of
                state2 -> (# state2, borrow #)
    _ ->
      case timesWord2# quotient (indexWordArray# divisor index) of
        (# high, low #) ->
          case addWordC# low carry of
            (# product, overflow #) ->
              case readWordArray# dividend (offset +# index) state of
                (# state1, word #) ->
                  case subWordC# word product of
                    (# result, borrow #) ->
                      case writeWordArray# dividend (offset +# index) result state1 of
                        state2 -> subtractDivisorProduct# divisor dividend count offset quotient (index +# 1#) (plusWord# high (int2Word# (overflow +# borrow))) state2

-- | If the estimate was one too large, restore one divisor in place.
addDivisorBack# :: ByteArray# -> MutableByteArray# RealWorld -> Int# -> Int# -> Int# -> Word# -> State# RealWorld -> State# RealWorld
addDivisorBack# divisor dividend count offset index carry state =
  case readWordArray# dividend (offset +# index) state of
    (# state1, word #) ->
      case index ==# count of
        1# -> writeWordArray# dividend (offset +# index) (plusWord# word carry) state1
        _ ->
          case addWordC# word (indexWordArray# divisor index) of
            (# partial, carry0 #) ->
              case addWordC# partial carry of
                (# result, carry1 #) ->
                  case writeWordArray# dividend (offset +# index) result state1 of
                    state2 -> addDivisorBack# divisor dividend count offset (index +# 1#) (int2Word# (carry0 +# carry1)) state2

unnormalizeRemainder# :: MutableByteArray# RealWorld -> Int# -> Int# -> Int# -> State# RealWorld -> State# RealWorld
unnormalizeRemainder# magnitude count shift index state =
  case shift of
    0# -> state
    _ ->
      case index ==# count of
        1# -> state
        _ ->
          case readWordArray# magnitude index state of
            (# state1, low #) ->
              case readWordArray# magnitude (index +# 1#) state1 of
                (# state2, high #) ->
                  let result = or# (uncheckedShiftRL# low shift) (uncheckedShiftL# high (64# -# shift))
                   in case writeWordArray# magnitude index result state2 of
                        state3 -> unnormalizeRemainder# magnitude count shift (index +# 1#) state3

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

-- | Equality without an ordering. The representation is canonical, thus
-- values with different constructors are not equal. A test against a small
-- literal becomes one comparison of the 'IS' field.
eqInteger# :: Integer -> Integer -> Int#
eqInteger# (IS left) (IS right) = left ==# right
eqInteger# (IP left) (IP right) = eqMagnitudes# left right
eqInteger# (IN left) (IN right) = eqMagnitudes# left right
eqInteger# _ _ = 0#

-- | Equality of two canonical magnitudes.
eqMagnitudes# :: ByteArray# -> ByteArray# -> Int#
eqMagnitudes# left right =
  let count = wordCount# left
   in case count ==# wordCount# right of
        1# -> compareMagnitudeWords# left right (count -# 1#) ==# 0#
        _ -> 0#
{-# NOINLINE eqMagnitudes# #-}

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
-- Each caller has just allocated the magnitude, thus a call costs little.
-- An inlined copy adds about 20 lines to each large Integer operation.
{-# NOINLINE integerFromMagnitude# #-}

integerFromTwoWords# :: Int# -> Word# -> Word# -> Integer
integerFromTwoWords# sign high low =
  case eqWord# high (int2Word# 0#) of
    1# -> integerFromWord# sign low
    _ -> allocateTwoWordInteger# sign high low

-- | The 'Integer' of two limbs. The high limb is not zero.
allocateTwoWordInteger# :: Int# -> Word# -> Word# -> Integer
allocateTwoWordInteger# sign high low =
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
{-# NOINLINE allocateTwoWordInteger# #-}

-- | The 'Integer' with a sign and a magnitude. The sign 1 is positive, and
-- each other sign is negative. A magnitude below 2^63 is the inlined fast
-- path. The larger magnitudes go to a call.
integerFromWord# :: Int# -> Word# -> Integer
integerFromWord# sign word =
  let intValue = word2Int# word
   in case intValue <# 0# of
        0# ->
          case sign of
            1# -> IS intValue
            _ -> IS (0# -# intValue)
        _ -> integerFromLargeWord# sign word

-- | The 'Integer' with a sign and a magnitude of 2^63 or more. Only the
-- negative magnitude 2^63 fits in 'IS'.
integerFromLargeWord# :: Int# -> Word# -> Integer
integerFromLargeWord# sign word =
  case sign of
    1# -> allocateWordInteger# sign word
    _ ->
      let intValue = word2Int# word
       in case intValue ==# (0# -# intValue) of
            1# -> IS intValue
            _ -> allocateWordInteger# sign word
{-# NOINLINE integerFromLargeWord# #-}

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
-- The allocation is the slow path of each conversion from a word. A call
-- keeps the inlined conversions small.
{-# NOINLINE allocateWordInteger# #-}

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
