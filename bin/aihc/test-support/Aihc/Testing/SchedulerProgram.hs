{-# LANGUAGE OverloadedStrings #-}

module Aihc.Testing.SchedulerProgram
  ( blackholeSchedulerProgram,
    schedulerProgram,
    stdioSchedulerProgram,
  )
where

import Aihc.Grin.Syntax

schedulerProgram :: GrinProgram
schedulerProgram =
  GrinProgram
    { grinConstructors = [pubConstructor "()" []],
      grinPrimitives =
        [ (GrinVar "fork#" 1 lifted, 2),
          (GrinVar "yield#" 2 lifted, 1)
        ],
      grinForeignCalls = [putcharCall],
      grinGlobals =
        [ pubGlobal (grinVarName mainClosure) (GrinNode (GrinClosure mainFunction [[]]) []),
          pubGlobal (grinVarName childClosure) (GrinNode (GrinClosure childFunction [[]]) [])
        ],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = mainFunction,
              grinFunctionParameters = [],
              grinFunctionResultRep = ResultRep lifted,
              grinFunctionBody =
                GrinBind [threadId] (GrinPrimitiveCall forkResultRep "fork#" [global childClosure]) $
                  GrinBind [parentBeforeYield] (putchar 'P') $
                    GrinBind [] (GrinPrimitiveCall (TupleRep []) "yield#" []) $
                      GrinBind [parentAfterYield] (putchar 'A') $
                        GrinBind [] (GrinPrimitiveCall (TupleRep []) "yield#" []) $
                          GrinBind [parentAfterSoloYield] (putchar 'B') $
                            GrinConstant [global unitValue]
            },
          GrinFunction
            { grinFunctionName = childFunction,
              grinFunctionParameters = [],
              grinFunctionResultRep = ResultRep lifted,
              grinFunctionBody =
                GrinBind [childOutput] (putchar 'C') $
                  GrinConstant [global unitValue]
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
    global = GrinGlobalValue . grinVarName
    putchar char =
      GrinForeignCallExpr
        putcharCall
        [GrinLitValue (GrinLitInt Int32Rep (toInteger (fromEnum char)))]

stdioSchedulerProgram :: GrinProgram
stdioSchedulerProgram =
  GrinProgram
    { grinConstructors = [pubConstructor "()" []],
      grinPrimitives =
        [ (GrinVar "awaitIO#" 30 lifted, 2),
          (GrinVar "newPinnedByteArray#" 31 (BoxedRep Unlifted), 2),
          (GrinVar "mutableByteArrayContents#" 32 AddrRep, 1),
          (GrinVar "submitIORead#" 43 AddrRep, 4),
          (GrinVar "submitIOWrite#" 44 AddrRep, 4),
          (GrinVar "touch#" 45 lifted, 2)
        ],
      grinForeignCalls = [stdinCall, stdoutCall, takeResultCall],
      grinGlobals = [pubGlobal (grinVarName mainClosure) (GrinNode (GrinClosure mainFunction [[]]) [])],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = mainFunction,
              grinFunctionParameters = [],
              grinFunctionResultRep = ResultRep lifted,
              grinFunctionBody =
                GrinBind [byteArray] (GrinPrimitiveCall (BoxedRep Unlifted) "newPinnedByteArray#" [intValue 64]) $
                  GrinBind [bufferContents] (GrinPrimitiveCall AddrRep "mutableByteArrayContents#" [GrinVarValue byteArray]) $
                    GrinBind [stdinIOHandle] (GrinForeignCallExpr stdinCall []) $
                      GrinBind [readRequest] (GrinPrimitiveCall AddrRep "submitIORead#" [GrinVarValue stdinIOHandle, GrinVarValue bufferContents, intValue 0, intValue 64]) $
                        GrinBind [] (GrinPrimitiveCall (TupleRep []) "awaitIO#" [GrinVarValue readRequest]) $
                          GrinBind [readCount] (GrinForeignCallExpr takeResultCall [GrinVarValue readRequest]) $
                            GrinBind [stdoutIOHandle] (GrinForeignCallExpr stdoutCall []) $
                              GrinBind [writeRequest] (GrinPrimitiveCall AddrRep "submitIOWrite#" [GrinVarValue stdoutIOHandle, GrinVarValue bufferContents, intValue 0, GrinVarValue readCount]) $
                                GrinBind [] (GrinPrimitiveCall (TupleRep []) "awaitIO#" [GrinVarValue writeRequest]) $
                                  GrinBind [writeResult] (GrinForeignCallExpr takeResultCall [GrinVarValue writeRequest]) $
                                    GrinBind [] (GrinPrimitiveCall (TupleRep []) "touch#" [GrinVarValue byteArray]) $
                                      GrinConstant [GrinGlobalValue (grinVarName unitValue)]
            }
        ]
    }
  where
    lifted = BoxedRep Lifted
    mainFunction = FunctionName "$stdio_main"
    mainClosure = GrinVar "main" 33 lifted
    byteArray = GrinVar "byte_array" 34 (BoxedRep Unlifted)
    bufferContents = GrinVar "buffer_contents" 35 AddrRep
    stdinIOHandle = GrinVar "stdin_handle" 36 AddrRep
    readRequest = GrinVar "read_request" 37 AddrRep
    readCount = GrinVar "read_count" 38 IntRep
    stdoutIOHandle = GrinVar "stdout_handle" 39 AddrRep
    writeRequest = GrinVar "write_request" 40 AddrRep
    writeResult = GrinVar "write_result" 41 IntRep
    unitValue = GrinVar "()" 42 lifted
    stdinCall = runtimeIoCall "aihc_io_stdin" [] GrinForeignAddr
    stdoutCall = runtimeIoCall "aihc_io_stdout" [] GrinForeignAddr
    takeResultCall = runtimeIoCall "aihc_io_take_result" [GrinForeignAddr] GrinForeignInt
    intValue = GrinLitValue . GrinLitInt IntRep
    runtimeIoCall symbol arguments result =
      GrinForeignCall
        { grinForeignCallName = "$ffi$" <> symbol,
          grinForeignCallSymbol = symbol,
          grinForeignCallTarget = GrinForeignFunction,
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
      grinForeignCallTarget = GrinForeignFunction,
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
    { grinGlobals =
        [ pubGlobal (grinVarName mainClosure) (GrinNode (GrinClosure mainFunction [[]]) []),
          pubGlobal (grinVarName childClosure) (GrinNode (GrinClosure childFunction [[]]) []),
          pubGlobal (grinVarName sharedThunk) (GrinNode (GrinThunk sharedFunction) [])
        ],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = mainFunction,
              grinFunctionParameters = [],
              grinFunctionResultRep = ResultRep lifted,
              grinFunctionBody =
                GrinBind [threadId] (GrinPrimitiveCall forkResultRep "fork#" [global childClosure]) $
                  GrinBind [] (GrinPrimitiveCall (TupleRep []) "yield#" []) $
                    GrinBind [mainShared] (GrinEval lifted (global sharedThunk)) $
                      GrinBind [parentOutput] (putchar 'A') $
                        GrinConstant [global unitValue]
            },
          GrinFunction
            { grinFunctionName = childFunction,
              grinFunctionParameters = [],
              grinFunctionResultRep = ResultRep lifted,
              grinFunctionBody = GrinEval lifted (global sharedThunk)
            },
          GrinFunction
            { grinFunctionName = sharedFunction,
              grinFunctionParameters = [],
              grinFunctionResultRep = ResultRep lifted,
              grinFunctionBody =
                GrinBind [] (GrinPrimitiveCall (TupleRep []) "yield#" []) $
                  GrinBind [thunkOutput] (putchar 'T') $
                    GrinConstant [global unitValue]
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
    global = GrinGlobalValue . grinVarName
    putchar char =
      GrinForeignCallExpr
        putcharCall
        [GrinLitValue (GrinLitInt Int32Rep (toInteger (fromEnum char)))]
