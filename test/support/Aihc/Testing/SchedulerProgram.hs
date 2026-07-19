{-# LANGUAGE OverloadedStrings #-}

module Aihc.Testing.SchedulerProgram
  ( blackholeSchedulerProgram,
    forkSnapshotEntry,
    forkSnapshotProgram,
    schedulerProgram,
    yieldSnapshotEntry,
    yieldSnapshotProgram,
  )
where

import Aihc.Grin.Syntax
import Aihc.Tc.Types (Levity (..), RuntimeRep (..))

forkSnapshotEntry :: FunctionName
forkSnapshotEntry = FunctionName "$fork_snapshot_main"

forkSnapshotProgram :: GrinProgram
forkSnapshotProgram =
  GrinProgram
    { grinConstructors = [],
      grinPrimitives = [(GrinVar "fork#" 30 lifted, 2)],
      grinForeignCalls = [],
      grinExternalGlobals = [],
      grinExternalFunctions = [],
      grinWhnfGlobals =
        [(childClosure, GrinNode (GrinClosure childFunction [[]]) [])],
      grinCafs = [],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = forkSnapshotEntry,
              grinFunctionLinkName = Nothing,
              grinFunctionParameters = [],
              grinFunctionResultRep = BoxedRep Unlifted,
              grinFunctionBody =
                GrinBind [threadId] (GrinPrimitiveCall forkResultRep "fork#" [GrinVarValue childClosure]) $
                  GrinConstant [GrinVarValue threadId]
            },
          GrinFunction
            { grinFunctionName = childFunction,
              grinFunctionLinkName = Nothing,
              grinFunctionParameters = [],
              grinFunctionResultRep = lifted,
              grinFunctionBody = GrinConstant [GrinVarValue unitValue]
            }
        ]
    }
  where
    lifted = BoxedRep Lifted
    forkResultRep = TupleRep [TupleRep [], BoxedRep Unlifted]
    childFunction = FunctionName "$fork_snapshot_child"
    childClosure = GrinVar "fork_snapshot_child" 31 lifted
    threadId = GrinVar "fork_snapshot_thread_id" 32 (BoxedRep Unlifted)
    unitValue = GrinVar "()" 33 lifted

yieldSnapshotEntry :: FunctionName
yieldSnapshotEntry = FunctionName "$yield_snapshot_main"

yieldSnapshotProgram :: GrinProgram
yieldSnapshotProgram =
  GrinProgram
    { grinConstructors =
        [ ("SchedulerSnapshot", [[BoxedRep Unlifted, lifted]]),
          ("ChildResult", [])
        ],
      grinPrimitives =
        [ (GrinVar "fork#" 40 lifted, 2),
          (GrinVar "yield#" 41 lifted, 1)
        ],
      grinForeignCalls = [],
      grinExternalGlobals = [],
      grinExternalFunctions = [],
      grinWhnfGlobals =
        [(childClosure, GrinNode (GrinClosure childFunction [[]]) [])],
      grinCafs = [(sharedThunk, GrinNode (GrinThunk sharedFunction) [])],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = yieldSnapshotEntry,
              grinFunctionLinkName = Nothing,
              grinFunctionParameters = [],
              grinFunctionResultRep = lifted,
              grinFunctionBody =
                GrinBind [threadId] (GrinPrimitiveCall forkResultRep "fork#" [GrinVarValue childClosure]) $
                  GrinBind [] (GrinPrimitiveCall (TupleRep []) "yield#" []) $
                    GrinStore
                      ( GrinNode
                          (GrinConstructor "SchedulerSnapshot" 0)
                          [GrinVarValue threadId, GrinVarValue sharedThunk]
                      )
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
              grinFunctionBody = GrinStore (GrinNode (GrinConstructor "ChildResult" 0) [])
            }
        ]
    }
  where
    lifted = BoxedRep Lifted
    forkResultRep = TupleRep [TupleRep [], BoxedRep Unlifted]
    childFunction = FunctionName "$yield_snapshot_child"
    sharedFunction = FunctionName "$yield_snapshot_shared"
    childClosure = GrinVar "yield_snapshot_child" 42 lifted
    sharedThunk = GrinVar "yield_snapshot_shared" 43 lifted
    threadId = GrinVar "yield_snapshot_thread_id" 44 (BoxedRep Unlifted)

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
