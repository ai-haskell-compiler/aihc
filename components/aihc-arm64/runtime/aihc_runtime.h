#ifndef AIHC_RUNTIME_H
#define AIHC_RUNTIME_H

#include <stdint.h>

enum {
  AIHC_LITERAL_INT = 0,
  AIHC_CONSTRUCTOR = 1,
  AIHC_CLOSURE = 2,
  AIHC_THUNK = 3,
  AIHC_PRIMITIVE = 4,
  AIHC_CELL = 5,
  AIHC_STATE_TOKEN = 6,
};

enum {
  AIHC_CELL_SUSPENDED = 0,
  AIHC_CELL_VALUE = 1,
  AIHC_CELL_BLACKHOLE = 2,
};

typedef struct AihcValue AihcValue;
typedef uintptr_t AihcSlot;

struct AihcValue {
  uint64_t kind;
  uintptr_t info;
  uint64_t arity;
  uint64_t count;
  AihcSlot fields[];
};

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
                        uint64_t constructor_count,
                        const AihcSnapshotConstructor *constructors,
                        uint64_t function_count,
                        const AihcSnapshotFunction *functions);

#endif
