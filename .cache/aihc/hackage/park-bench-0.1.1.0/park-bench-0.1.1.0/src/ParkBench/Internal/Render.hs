-- | Rendering code that takes lays out raw benchmarking results in a table.
module ParkBench.Internal.Render
  ( estimatesToTable,
  )
where

import qualified Data.Text as Text
import ParkBench.Internal.Array1 (Array1)
import ParkBench.Internal.Named (Named)
import qualified ParkBench.Internal.Named as Named
import ParkBench.Internal.Prelude
import ParkBench.Internal.Pretty
import ParkBench.Internal.RtsStats
import ParkBench.Internal.Statistics

estimatesToTable :: Array1 (Named (Estimate RtsStats)) -> Table
estimatesToTable estimates =
  Table (estimatesToHeader estimates) (estimatesToRowGroups (Named.thing <$> estimates))

estimatesToHeader :: Array1 (Named (Estimate RtsStats)) -> [Cell]
estimatesToHeader estimates =
  if length estimates > 2
    then foldMap namedToHeader estimates ++ ["Total"]
    else foldMap namedToHeader estimates
  where
    namedToHeader :: Named a -> [Cell]
    namedToHeader x =
      [EmptyCell, namedToCell x]

    namedToCell :: Named a -> Cell
    namedToCell =
      Cell Blue . Text.map spaceToDash . Named.name

    spaceToDash :: Char -> Char
    spaceToDash = \case
      ' ' -> '─'
      c -> c

estimatesToRowGroups :: Array1 (Estimate RtsStats) -> [RowGroup]
estimatesToRowGroups summaries =
  [ RowGroup
      "Statistics"
      [ render (R "Samples" (Just . IncomparableWord3Cell . samples)),
        render (R "CV (σ/μ)" (Just . IncomparablePercentageCell . goodness))
      ],
    RowGroup
      "Elapsed time"
      [ render (R "Total" (Just . NanosecondsCell . r2d . nanoseconds . mean)),
        render (R "Mutator" (Just . NanosecondsCell . r2d . mutator_elapsed_ns . value . mean)),
        render (R "Mutator %" (Just . PercentageCell' . r2d . mut_wall_percent . value . mean)),
        render (R "Garbage collector" (Just . NanosecondsCell . r2d . gc_elapsed_ns . value . mean)),
        render (R "Garbage collector %" (Just . PercentageCell . r2d . gc_wall_percent . value . mean))
      ],
    RowGroup
      "CPU time"
      [ render (R "Total" (Just . NanosecondsCell . r2d . cpu_ns . value . mean)),
        render (R "Mutator" (Just . NanosecondsCell . r2d . mutator_cpu_ns . value . mean)),
        render (R "Mutator %" (Just . PercentageCell' . r2d . mut_cpu_percent . value . mean)),
        render (R "Garbage collector" (Just . NanosecondsCell . r2d . gc_cpu_ns . value . mean)),
        render (R "Garbage collector %" (Just . PercentageCell . r2d . gc_cpu_percent . value . mean))
      ],
    RowGroup
      "Memory usage"
      [ render (R "Average" (Just . BytesCell . r2d . average_live_data . value . mean)),
        render (R "Maximum" (Just . BytesCell . r2d . max_live_bytes . value . mean))
      ],
    RowGroup
      "Memory pressure"
      [ render (R "Allocated" (Just . BytesCell . r2d . allocated_bytes . value . mean)),
        render (R "Allocated/sec" (Just . BytesPerSecondCell . r2d . allocated_bytes_per_second . value . mean)),
        render (R "Copied during GC" (Just . BytesCell . r2d . copied_bytes . value . mean))
      ],
    -- TODO nonmoving GC
    RowGroup
      "Garbage collection"
      [ render (R "Total collections" (Just . NumberCell . r2d . gcs . value . mean)),
        render (R "Major collections" (Just . NumberCell . r2d . major_gcs . value . mean)),
        render (R "Average pause" (Just . NanosecondsCell . r2d . gc_average_ns . value . mean)),
        render (R "Work balance" (fmap (PercentageCell' . r2d) . work_balance . value . mean))
      ]
  ]
  where
    render :: Cellular a => R (Estimate RtsStats) a -> Row
    render =
      rowMaker summaries
