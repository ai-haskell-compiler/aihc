{-# LANGUAGE OverloadedStrings #-}

module Aihc.Testing.ExceptionProgram
  ( synchronousExceptionProgram,
  )
where

import Aihc.Grin.Syntax
import Aihc.Tc.Types (Levity (..), RuntimeRep (..))

-- | Raise while evaluating a thunk, catch it, rethrow from that handler, and
-- print from an outer handler. This exercises normal, update, and nested catch
-- frames without relying on a source-language exception library.
synchronousExceptionProgram :: GrinProgram
synchronousExceptionProgram =
  GrinProgram
    { grinConstructors = [("Exception", [])],
      grinPrimitives = [],
      grinForeignCalls = [putcharCall],
      grinExternalGlobals = [],
      grinExternalFunctions = [],
      grinWhnfGlobals =
        [ (mainClosure, GrinNode (GrinClosure mainFunction [[]]) []),
          (outerActionClosure, GrinNode (GrinClosure outerActionFunction [[]]) []),
          (failingActionClosure, GrinNode (GrinClosure failingActionFunction [[]]) []),
          (rethrowHandlerClosure, GrinNode (GrinClosure rethrowHandlerFunction [[lifted]]) []),
          (outerHandlerClosure, GrinNode (GrinClosure outerHandlerFunction [[lifted]]) [])
        ],
      grinCafs = [(failingThunk, GrinNode (GrinThunk failingThunkFunction) [])],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = mainFunction,
              grinFunctionLinkName = Nothing,
              grinFunctionParameters = [],
              grinFunctionResultRep = lifted,
              grinFunctionBody = GrinCatch lifted (GrinVarValue outerActionClosure) (GrinVarValue outerHandlerClosure) []
            },
          GrinFunction
            { grinFunctionName = outerActionFunction,
              grinFunctionLinkName = Nothing,
              grinFunctionParameters = [],
              grinFunctionResultRep = lifted,
              grinFunctionBody = GrinCatch lifted (GrinVarValue failingActionClosure) (GrinVarValue rethrowHandlerClosure) []
            },
          GrinFunction
            { grinFunctionName = failingActionFunction,
              grinFunctionLinkName = Nothing,
              grinFunctionParameters = [],
              grinFunctionResultRep = lifted,
              grinFunctionBody = GrinEval lifted (GrinVarValue failingThunk)
            },
          GrinFunction
            { grinFunctionName = failingThunkFunction,
              grinFunctionLinkName = Nothing,
              grinFunctionParameters = [],
              grinFunctionResultRep = lifted,
              grinFunctionBody =
                GrinBind
                  [exception]
                  (GrinStore (GrinNode (GrinConstructor "Exception" 0) []))
                  (GrinThrow (GrinVarValue exception))
            },
          GrinFunction
            { grinFunctionName = rethrowHandlerFunction,
              grinFunctionLinkName = Nothing,
              grinFunctionParameters = [innerException],
              grinFunctionResultRep = lifted,
              grinFunctionBody = GrinThrow (GrinVarValue innerException)
            },
          GrinFunction
            { grinFunctionName = outerHandlerFunction,
              grinFunctionLinkName = Nothing,
              grinFunctionParameters = [outerException],
              grinFunctionResultRep = lifted,
              grinFunctionBody =
                GrinBind
                  [output]
                  ( GrinForeignCallExpr
                      putcharCall
                      [GrinLitValue (GrinLitInt Int32Rep (toInteger (fromEnum 'E')))]
                  )
                  (GrinConstant [GrinVarValue outerException])
            }
        ]
    }
  where
    lifted = BoxedRep Lifted
    mainFunction = FunctionName "$exception_main"
    outerActionFunction = FunctionName "$exception_outer_action"
    failingActionFunction = FunctionName "$exception_failing_action"
    failingThunkFunction = FunctionName "$exception_failing_thunk"
    rethrowHandlerFunction = FunctionName "$exception_rethrow_handler"
    outerHandlerFunction = FunctionName "$exception_outer_handler"
    mainClosure = GrinVar "main" 1 lifted
    outerActionClosure = GrinVar "exception_outer_action" 2 lifted
    failingActionClosure = GrinVar "exception_failing_action" 3 lifted
    rethrowHandlerClosure = GrinVar "exception_rethrow_handler" 4 lifted
    outerHandlerClosure = GrinVar "exception_outer_handler" 5 lifted
    failingThunk = GrinVar "exception_thunk" 6 lifted
    exception = GrinVar "exception" 7 lifted
    innerException = GrinVar "inner_exception" 8 lifted
    outerException = GrinVar "outer_exception" 9 lifted
    output = GrinVar "output" 10 Int32Rep

putcharCall :: GrinForeignCall
putcharCall =
  GrinForeignCall
    { grinForeignCallName = "$ffi$putchar",
      grinForeignCallSymbol = "putchar",
      grinForeignCallSignature =
        GrinForeignSignature
          { grinForeignArgumentTypes = [GrinForeignInt32],
            grinForeignResultType = GrinForeignInt32,
            grinForeignEffect = GrinForeignPure
          }
    }
