{-# LANGUAGE OverloadedStrings #-}

-- | Wide division with a word fast path and a portable normalized algorithm.
module Aihc.Lir.Division (lowerWideDivision) where

import Aihc.Lir.Inline (inlineModule)
import Aihc.Lir.Syntax
import Data.Set qualified as Set
import Data.Text qualified as T

-- | Splice division before register allocation or backend control-flow analysis.
-- The hardware path retains one wide operation after its input checks.
-- The portable path computes two quotient digits in base 2^32.
lowerWideDivision :: Bool -> Function -> Function
lowerWideDivision hardware function
  | not (any isDivision instructions) = function
  | otherwise = case inlineModule (Module [ItemFunction rewritten, ItemFunction helper]) of
      Module [ItemFunction result] -> result
      _ -> error "wide division expansion did not return one function"
  where
    instructions = concatMap blockInstructions (functionBlocks function)
    isDivision (Instruction _ QuotRemWide {}) = True
    isDivision _ = False
    used = Set.fromList (functionName function : [callee | Instruction _ (Call callee _) <- instructions] <> [callee | block <- functionBlocks function, TailCall callee _ <- [blockTerminator block]])
    name = freshName 0
    freshName :: Int -> Symbol
    freshName index =
      let candidate = Symbol ("$quotrem.wide.u." <> T.pack (show index))
       in if Set.member candidate used then freshName (index + 1) else candidate
    helper = divisionFunction hardware name
    rewritten = function {functionBlocks = map rewriteBlock (functionBlocks function)}
    rewriteBlock block = block {blockInstructions = map rewrite (blockInstructions block)}
    rewrite (Instruction results (QuotRemWide _ high low divisor)) = Instruction results (Call name [high, low, divisor])
    rewrite instruction = instruction

-- | Algorithm D uses two 32-bit digits for the divisor. Each quotient
-- estimate needs at most two corrections. The caller requires high < divisor.
-- All arithmetic below wraps at 64 bits.
divisionFunction :: Bool -> Symbol -> Function
divisionFunction hardware name =
  Function
    { functionName = name,
      functionParameters = [(Var "high", I64), (Var "low", I64), (Var "divisor", I64)],
      functionResults = [I64, I64],
      functionConvention = AihcConvention,
      functionLinkage = Inline,
      functionBlocks =
        [ block "entry" [] [op "zero" (Compare Eq I64 (v "divisor") (n 0))] (branch "zero" "zero" "nonzero"),
          block "zero" [] [] (Trap "integer division by zero"),
          block "nonzero" [] [op "overflow" (Compare GeU I64 (v "high") (v "divisor"))] (branch "overflow" "overflow" "valid"),
          block "overflow" [] [] (Trap "integer division overflow"),
          block "valid" [] [op "small" (Compare Eq I64 (v "high") (n 0))] (branch "small" "small" "wide"),
          block
            "small"
            []
            [ op "small_q" (Binary DivU I64 (v "low") (v "divisor")),
              op "small_product" (Binary Mul I64 (v "small_q") (v "divisor")),
              op "small_r" (Binary Sub I64 (v "low") (v "small_product"))
            ]
            (Return [v "small_q", v "small_r"])
        ]
          <> if hardware
            then [block "wide" [] [Instruction [Var "q", Var "r"] (QuotRemWide I64 (v "high") (v "low") (v "divisor"))] (Return [v "q", v "r"])]
            else portable
    }
  where
    portable =
      [ block
          "wide"
          []
          [ op "shift" (Unary Clz I64 (v "divisor")),
            op "vn" (Binary Shl I64 (v "divisor") (v "shift")),
            op "vn1" (Binary ShrU I64 (v "vn") (n 32)),
            op "vn0" (Binary And I64 (v "vn") (n 0xffffffff)),
            op "un10" (Binary Shl I64 (v "low") (v "shift")),
            op "un1" (Binary ShrU I64 (v "un10") (n 32)),
            op "un0" (Binary And I64 (v "un10") (n 0xffffffff)),
            op "no_shift" (Compare Eq I64 (v "shift") (n 0))
          ]
          (Branch (v "no_shift") (target "first" [v "high"]) (target "shift_high" [])),
        block
          "shift_high"
          []
          [ op "shift_back" (Binary Sub I64 (n 64) (v "shift")),
            op "carry" (Binary ShrU I64 (v "low") (v "shift_back")),
            op "shifted_high" (Binary Shl I64 (v "high") (v "shift")),
            op "normalized_high" (Binary Or I64 (v "shifted_high") (v "carry"))
          ]
          (Jump (target "first" [v "normalized_high"])),
        block
          "first"
          ["un64"]
          [ op "estimate1" (Binary DivU I64 (v "un64") (v "vn1")),
            op "product1" (Binary Mul I64 (v "estimate1") (v "vn1")),
            op "remainder1" (Binary Sub I64 (v "un64") (v "product1"))
          ]
          (Jump (target "check1" [v "estimate1", v "remainder1"]))
      ]
        <> correction "1" "un1" "second"
        <> [ block
               "second"
               []
               [ op "un64_low" (Binary Shl I64 (v "un64") (n 32)),
                 op "un21_sum" (Binary Add I64 (v "un64_low") (v "un1")),
                 op "q1_product" (Binary Mul I64 (v "digit1") (v "vn")),
                 op "un21" (Binary Sub I64 (v "un21_sum") (v "q1_product")),
                 op "estimate0" (Binary DivU I64 (v "un21") (v "vn1")),
                 op "product0" (Binary Mul I64 (v "estimate0") (v "vn1")),
                 op "remainder0" (Binary Sub I64 (v "un21") (v "product0"))
               ]
               (Jump (target "check0" [v "estimate0", v "remainder0"]))
           ]
        <> correction "0" "un0" "finish"
        <> [ block
               "finish"
               []
               [ op "un21_low" (Binary Shl I64 (v "un21") (n 32)),
                 op "remainder_sum" (Binary Add I64 (v "un21_low") (v "un0")),
                 op "q0_product" (Binary Mul I64 (v "digit0") (v "vn")),
                 op "normalized_remainder" (Binary Sub I64 (v "remainder_sum") (v "q0_product")),
                 op "remainder" (Binary ShrU I64 (v "normalized_remainder") (v "shift")),
                 op "q_high" (Binary Shl I64 (v "digit1") (n 32)),
                 op "quotient" (Binary Or I64 (v "q_high") (v "digit0"))
               ]
               (Return [v "quotient", v "remainder"])
           ]
    correction suffix low next =
      let q = "q" <> suffix
          r = "r" <> suffix
          digit = "digit" <> suffix
          check = "check" <> suffix
          compareLabel = "compare" <> suffix
          decrease = "decrease" <> suffix
          done = "done" <> suffix
          large = "large" <> suffix
          productName = "product_low" <> suffix
          high = "r_high" <> suffix
          sumName = "r_sum" <> suffix
          exceeds = "exceeds" <> suffix
          smaller = "smaller" <> suffix
          larger = "larger" <> suffix
          carry = "carry" <> suffix
       in [ block check [q, r] [op large (Compare GeU I64 (v q) (n 0x100000000))] (branch large decrease compareLabel),
            block
              compareLabel
              []
              [ op productName (Binary Mul I64 (v q) (v "vn0")),
                op high (Binary Shl I64 (v r) (n 32)),
                op sumName (Binary Add I64 (v high) (v low)),
                op exceeds (Compare GtU I64 (v productName) (v sumName))
              ]
              (Branch (v exceeds) (target decrease []) (target done [v q])),
            block
              decrease
              []
              [ op smaller (Binary Sub I64 (v q) (n 1)),
                op larger (Binary Add I64 (v r) (v "vn1")),
                op carry (Compare GeU I64 (v larger) (n 0x100000000))
              ]
              (Branch (v carry) (target done [v smaller]) (target check [v smaller, v larger])),
            block done [digit] [] (Jump (target next []))
          ]
    v = OperandVar . Var
    n = OperandLiteral . LitInt
    op label operation = Instruction [Var label] operation
    target label = Target (Label label)
    branch value yes no = Branch (v value) (target yes []) (target no [])
    block label parameters instructions terminator = Block (Label label) [(Var parameter, I64) | parameter <- parameters] instructions terminator False
