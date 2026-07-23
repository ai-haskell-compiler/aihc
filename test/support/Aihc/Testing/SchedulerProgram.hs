{-# LANGUAGE OverloadedStrings #-}

module Aihc.Testing.SchedulerProgram
  ( blackholeSchedulerProgram,
    schedulerProgram,
    stdioSchedulerProgram,
  )
where

import Aihc.Grin.Syntax
import Aihc.Tc.Types (Levity (..), RuntimeRep (..))

schedulerProgram :: GrinProgram
schedulerProgram =
  GrinProgram
    { grinConstructors = [],
      grinPrimitives =
        [ (GrinVar "fork#" 1 lifted, 2),
          (GrinVar "yield#" 2 lifted, 1)
        ],
      grinForeignCalls = [putcharCall],
      grinExternalGlobals = [],
      grinExternalFunctions = [],
      grinWhnfGlobals =
        [ (mainClosure, GrinNode (GrinClosure mainFunction [[]]) []),
          (childClosure, GrinNode (GrinClosure childFunction [[]]) [])
        ],
      grinCafs = [],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = mainFunction,
              grinFunctionLinkName = Nothing,
              grinFunctionParameters = [],
              grinFunctionResultRep = lifted,
              grinFunctionBody =
                GrinBind [threadId] (GrinPrimitiveCall forkResultRep "fork#" [GrinVarValue childClosure]) $
                  GrinBind [parentBeforeYield] (putchar 'P') $
                    GrinBind [] (GrinPrimitiveCall (TupleRep []) "yield#" []) $
                      GrinBind [parentAfterYield] (putchar 'A') $
                        GrinBind [] (GrinPrimitiveCall (TupleRep []) "yield#" []) $
                          GrinBind [parentAfterSoloYield] (putchar 'B') $
                            GrinConstant [GrinVarValue unitValue]
            },
          GrinFunction
            { grinFunctionName = childFunction,
              grinFunctionLinkName = Nothing,
              grinFunctionParameters = [],
              grinFunctionResultRep = lifted,
              grinFunctionBody =
                GrinBind [childOutput] (putchar 'C') $
                  GrinConstant [GrinVarValue unitValue]
            }
        ]
    }
  where
    lifted = BoxedRep Lifted
    forkResultRep = TupleRep [TupleRep [], BoxedRep Unlifted]
    mainFunction = FunctionName "$scheduler_main"
    childFunction = FunctionName "$scheduler_child"
    mainClosure = GrinVar "main" 3 lifted
    childClosure = GrinVar "child" 4 lifted
    threadId = GrinVar "thread_id" 5 (BoxedRep Unlifted)
    parentBeforeYield = GrinVar "parent_before_yield" 6 Int32Rep
    parentAfterYield = GrinVar "parent_after_yield" 7 Int32Rep
    childOutput = GrinVar "child_output" 8 Int32Rep
    unitValue = GrinVar "()" 9 lifted
    parentAfterSoloYield = GrinVar "parent_after_solo_yield" 10 Int32Rep
    putchar char =
      GrinForeignCallExpr
        putcharCall
        [GrinLitValue (GrinLitInt Int32Rep (toInteger (fromEnum char)))]

stdioSchedulerProgram :: GrinProgram
stdioSchedulerProgram =
  GrinProgram
    { grinConstructors = [],
      grinPrimitives = [(GrinVar "awaitIO#" 30 lifted, 2)],
      grinForeignCalls = [newBufferCall, getBufferCall, setBufferCall, stdinCall, stdoutCall, submitReadCall, submitWriteCall, takeResultCall],
      grinExternalGlobals = [],
      grinExternalFunctions = [],
      grinWhnfGlobals = [(mainClosure, GrinNode (GrinClosure mainFunction [[]]) [])],
      grinCafs = [],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = mainFunction,
              grinFunctionLinkName = Nothing,
              grinFunctionParameters = [],
              grinFunctionResultRep = lifted,
              grinFunctionBody =
                GrinBind [ioBuffer] (GrinForeignCallExpr newBufferCall [int32 64]) $
                  GrinBind [stdinIOHandle] (GrinForeignCallExpr stdinCall []) $
                    GrinBind [readRequest] (GrinForeignCallExpr submitReadCall [GrinVarValue stdinIOHandle, GrinVarValue ioBuffer, int32 0, int32 64]) $
                      GrinBind [] (GrinPrimitiveCall (TupleRep []) "awaitIO#" [GrinVarValue readRequest]) $
                        GrinBind [readCount] (GrinForeignCallExpr takeResultCall [GrinVarValue readRequest]) $
                          GrinBind [firstByte] (GrinForeignCallExpr getBufferCall [GrinVarValue ioBuffer, int32 0]) $
                            GrinBind [setResult] (GrinForeignCallExpr setBufferCall [GrinVarValue ioBuffer, int32 0, GrinVarValue firstByte]) $
                              GrinBind [stdoutIOHandle] (GrinForeignCallExpr stdoutCall []) $
                                GrinBind [writeRequest] (GrinForeignCallExpr submitWriteCall [GrinVarValue stdoutIOHandle, GrinVarValue ioBuffer, int32 0, GrinVarValue readCount]) $
                                  GrinBind [] (GrinPrimitiveCall (TupleRep []) "awaitIO#" [GrinVarValue writeRequest]) $
                                    GrinBind [writeResult] (GrinForeignCallExpr takeResultCall [GrinVarValue writeRequest]) $
                                      GrinConstant [GrinVarValue unitValue]
            }
        ]
    }
  where
    lifted = BoxedRep Lifted
    mainFunction = FunctionName "$stdio_main"
    mainClosure = GrinVar "main" 31 lifted
    ioBuffer = GrinVar "io_buffer" 32 AddrRep
    stdinIOHandle = GrinVar "stdin_handle" 33 AddrRep
    readRequest = GrinVar "read_request" 34 AddrRep
    readCount = GrinVar "read_count" 35 Int32Rep
    firstByte = GrinVar "first_byte" 36 Int32Rep
    setResult = GrinVar "set_result" 37 Int32Rep
    stdoutIOHandle = GrinVar "stdout_handle" 38 AddrRep
    writeRequest = GrinVar "write_request" 39 AddrRep
    writeResult = GrinVar "write_result" 40 Int32Rep
    unitValue = GrinVar "()" 41 lifted
    newBufferCall = runtimeIoCall "aihc_io_buffer_new" [GrinForeignInt32] GrinForeignAddr
    getBufferCall = runtimeIoCall "aihc_io_buffer_get" [GrinForeignAddr, GrinForeignInt32] GrinForeignInt32
    setBufferCall = runtimeIoCall "aihc_io_buffer_set" [GrinForeignAddr, GrinForeignInt32, GrinForeignInt32] GrinForeignInt32
    stdinCall = runtimeIoCall "aihc_io_stdin" [] GrinForeignAddr
    stdoutCall = runtimeIoCall "aihc_io_stdout" [] GrinForeignAddr
    submitReadCall = runtimeIoCall "aihc_io_submit_read" [GrinForeignAddr, GrinForeignAddr, GrinForeignInt32, GrinForeignInt32] GrinForeignAddr
    submitWriteCall = runtimeIoCall "aihc_io_submit_write" [GrinForeignAddr, GrinForeignAddr, GrinForeignInt32, GrinForeignInt32] GrinForeignAddr
    takeResultCall = runtimeIoCall "aihc_io_take_result" [GrinForeignAddr] GrinForeignInt32
    int32 = GrinLitValue . GrinLitInt Int32Rep
    runtimeIoCall symbol arguments result =
      GrinForeignCall
        { grinForeignCallName = "$ffi$" <> symbol,
          grinForeignCallSymbol = symbol,
          grinForeignCallSignature =
            GrinForeignSignature
              { grinForeignArgumentTypes = arguments,
                grinForeignResultType = result,
                grinForeignEffect = GrinForeignRealWorld
              }
        }

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

blackholeSchedulerProgram :: GrinProgram
blackholeSchedulerProgram =
  schedulerProgram
    { grinWhnfGlobals =
        [ (mainClosure, GrinNode (GrinClosure mainFunction [[]]) []),
          (childClosure, GrinNode (GrinClosure childFunction [[]]) [])
        ],
      grinCafs = [(sharedThunk, GrinNode (GrinThunk sharedFunction) [])],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = mainFunction,
              grinFunctionLinkName = Nothing,
              grinFunctionParameters = [],
              grinFunctionResultRep = lifted,
              grinFunctionBody =
                GrinBind [threadId] (GrinPrimitiveCall forkResultRep "fork#" [GrinVarValue childClosure]) $
                  GrinBind [] (GrinPrimitiveCall (TupleRep []) "yield#" []) $
                    GrinBind [mainShared] (GrinEval lifted (GrinVarValue sharedThunk)) $
                      GrinBind [parentOutput] (putchar 'A') $
                        GrinConstant [GrinVarValue unitValue]
            },
          GrinFunction
            { grinFunctionName = childFunction,
              grinFunctionLinkName = Nothing,
              grinFunctionParameters = [],
              grinFunctionResultRep = lifted,
              grinFunctionBody = GrinEval lifted (GrinVarValue sharedThunk)
            },
          GrinFunction
            { grinFunctionName = sharedFunction,
              grinFunctionLinkName = Nothing,
              grinFunctionParameters = [],
              grinFunctionResultRep = lifted,
              grinFunctionBody =
                GrinBind [] (GrinPrimitiveCall (TupleRep []) "yield#" []) $
                  GrinBind [thunkOutput] (putchar 'T') $
                    GrinConstant [GrinVarValue unitValue]
            }
        ]
    }
  where
    lifted = BoxedRep Lifted
    forkResultRep = TupleRep [TupleRep [], BoxedRep Unlifted]
    mainFunction = FunctionName "$blackhole_main"
    childFunction = FunctionName "$blackhole_child"
    sharedFunction = FunctionName "$blackhole_shared"
    mainClosure = GrinVar "main" 20 lifted
    childClosure = GrinVar "child" 21 lifted
    sharedThunk = GrinVar "shared" 22 lifted
    threadId = GrinVar "thread_id" 23 (BoxedRep Unlifted)
    mainShared = GrinVar "main_shared" 24 lifted
    parentOutput = GrinVar "parent_output" 25 Int32Rep
    thunkOutput = GrinVar "thunk_output" 26 Int32Rep
    unitValue = GrinVar "()" 27 lifted
    putchar char =
      GrinForeignCallExpr
        putcharCall
        [GrinLitValue (GrinLitInt Int32Rep (toInteger (fromEnum char)))]
