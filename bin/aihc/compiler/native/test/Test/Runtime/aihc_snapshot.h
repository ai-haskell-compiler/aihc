#ifndef AIHC_SNAPSHOT_H
#define AIHC_SNAPSHOT_H

#include "../../../runtime/aihc_runtime.h"

typedef enum {
  AIHC_SNAPSHOT_POINTER,
  AIHC_SNAPSHOT_INT,
  AIHC_SNAPSHOT_INT8,
  AIHC_SNAPSHOT_INT16,
  AIHC_SNAPSHOT_INT32,
  AIHC_SNAPSHOT_INT64,
  AIHC_SNAPSHOT_WORD,
  AIHC_SNAPSHOT_WORD8,
  AIHC_SNAPSHOT_WORD16,
  AIHC_SNAPSHOT_WORD32,
  AIHC_SNAPSHOT_WORD64,
  AIHC_SNAPSHOT_ADDR,
  AIHC_SNAPSHOT_FLOAT,
  AIHC_SNAPSHOT_DOUBLE,
} AihcSnapshotRep;

typedef struct {
  uintptr_t info;
  const char *name;
  uint64_t field_count;
  const AihcSnapshotRep *field_reps;
} AihcSnapshotConstructor;

typedef struct {
  uintptr_t info;
  const char *name;
  uint64_t parameter_count;
  const AihcSnapshotRep *parameter_reps;
} AihcSnapshotFunction;

void aihc_snapshot_dump(uint64_t result_count, const AihcSlot *results,
                        const AihcSnapshotRep *result_reps,
                        uint64_t allocation_count, uint64_t constructor_count,
                        const AihcSnapshotConstructor *constructors,
                        uint64_t function_count,
                        const AihcSnapshotFunction *functions);

#endif
