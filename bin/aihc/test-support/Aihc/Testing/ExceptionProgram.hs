{-# LANGUAGE OverloadedStrings #-}

module Aihc.Testing.ExceptionProgram
  ( synchronousExceptionProgram,
  )
where

import Aihc.Grin.Syntax

-- | Raise while evaluating a thunk, catch it, rethrow from that handler, and
-- print from an outer handler. This exercises normal, update, and nested catch
-- frames without relying on a source-language exception library.
synchronousExceptionProgram :: GrinProgram
synchronousExceptionProgram =
  GrinProgram
    { grinConstructors = [("Exception", [])],
      grinPrimitives = [],
      grinForeignCalls = [putcharCall],
      grinGlobals =
        [ (grinVarName mainClosure, GrinNode (GrinClosure mainFunction [[]]) []),
          (grinVarName outerActionClosure, GrinNode (GrinClosure outerActionFunction [[]]) []),
          (grinVarName failingActionClosure, GrinNode (GrinClosure failingActionFunction [[]]) []),
          (grinVarName rethrowHandlerClosure, GrinNode (GrinClosure rethrowHandlerFunction [[lifted]]) []),
          (grinVarName outerHandlerClosure, GrinNode (GrinClosure outerHandlerFunction [[lifted]]) []),
          (grinVarName failingThunk, GrinNode (GrinThunk failingThunkFunction) [])
        ],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = mainFunction,
              grinFunctionParameters = [],
              grinFunctionResultRep = lifted,
              grinFunctionBody = GrinCatch lifted (global outerActionClosure) (global outerHandlerClosure) []
            },
          GrinFunction
            { grinFunctionName = outerActionFunction,
              grinFunctionParameters = [],
              grinFunctionResultRep = lifted,
              grinFunctionBody = GrinCatch lifted (global failingActionClosure) (global rethrowHandlerClosure) []
            },
          GrinFunction
            { grinFunctionName = failingActionFunction,
              grinFunctionParameters = [],
              grinFunctionResultRep = lifted,
              grinFunctionBody = GrinEval lifted (global failingThunk)
            },
          GrinFunction
            { grinFunctionName = failingThunkFunction,
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
              grinFunctionParameters = [innerException],
              grinFunctionResultRep = lifted,
              grinFunctionBody = GrinThrow (GrinVarValue innerException)
            },
          GrinFunction
            { grinFunctionName = outerHandlerFunction,
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
    global = GrinGlobalValue . grinVarName

putcharCall :: GrinForeignCall
putcharCall =
  GrinForeignCall
    { grinForeignCallName = "$ffi$putchar",
      grinForeignCallSymbol = "putchar",
      grinForeignCallTarget = GrinForeignFunction,
      grinForeignCallSignature =
        GrinForeignSignature
          { grinForeignArgumentTypes = [GrinForeignInt32],
            grinForeignResultType = GrinForeignInt32,
            grinForeignEffect = GrinForeignPure
          }
    }
