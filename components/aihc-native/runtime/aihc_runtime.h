#ifndef AIHC_RUNTIME_H
#define AIHC_RUNTIME_H

#include <stdint.h>

enum {
  AIHC_TAG_NODE = 0,
  AIHC_TAG_CLOSURE = 1,
  AIHC_TAG_THUNK = 2,
  AIHC_TAG_PARTIAL_CONSTRUCTOR = 3,
  AIHC_TAG_INDIRECTION = 4,
  AIHC_TAG_BLACKHOLE = 5,
};

#define AIHC_TAG_BITS 3
#define AIHC_TAG_MASK ((uintptr_t)((1U << AIHC_TAG_BITS) - 1U))
#define AIHC_SHAPE_ARITY_SHIFT 32
#define AIHC_SHAPE_COUNT_MASK UINT32_MAX

typedef struct AihcValue AihcValue;
typedef uintptr_t AihcSlot;

struct AihcValue {
  /* Low AIHC_TAG_BITS select the physical shape. Remaining bits carry info. */
  uintptr_t header;
  AihcSlot fields[];
};

_Static_assert(sizeof(AihcValue) == sizeof(uintptr_t),
               "AIHC objects must have a one-word base header");

static inline uint64_t aihc_value_tag(const AihcValue *value) {
  return value->header & AIHC_TAG_MASK;
}

static inline uintptr_t aihc_value_info(const AihcValue *value) {
  switch (aihc_value_tag(value)) {
  case AIHC_TAG_CLOSURE:
  case AIHC_TAG_THUNK:
    return value->header & ~AIHC_TAG_MASK;
  case AIHC_TAG_NODE:
  case AIHC_TAG_PARTIAL_CONSTRUCTOR:
    return value->header >> AIHC_TAG_BITS;
  default:
    return 0;
  }
}

static inline int aihc_value_has_shape(const AihcValue *value) {
  uint64_t tag = aihc_value_tag(value);
  return tag == AIHC_TAG_CLOSURE || tag == AIHC_TAG_THUNK ||
         tag == AIHC_TAG_PARTIAL_CONSTRUCTOR;
}

static inline uint64_t aihc_value_arity(const AihcValue *value) {
  return value->fields[0] >> AIHC_SHAPE_ARITY_SHIFT;
}

static inline uint64_t aihc_value_count(const AihcValue *value) {
  return value->fields[0] & AIHC_SHAPE_COUNT_MASK;
}

static inline AihcSlot *aihc_value_fields(AihcValue *value) {
  return value->fields + (aihc_value_has_shape(value) ? 1 : 0);
}

static inline const AihcSlot *aihc_value_fields_const(const AihcValue *value) {
  return value->fields + (aihc_value_has_shape(value) ? 1 : 0);
}

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
