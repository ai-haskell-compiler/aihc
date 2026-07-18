#include "aihc_runtime.h"

#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct {
  AihcValue **objects;
  uint64_t object_count;
  uint64_t object_capacity;
  uint64_t constructor_count;
  const AihcSnapshotConstructor *constructors;
  uint64_t function_count;
  const AihcSnapshotFunction *functions;
} SnapshotState;

static void snapshot_fail(const char *message) {
  fprintf(stderr, "aihc snapshot: %s\n", message);
  exit(1);
}

static const AihcSnapshotConstructor *find_constructor(
    const SnapshotState *state, uintptr_t info) {
  for (uint64_t index = 0; index < state->constructor_count; ++index) {
    if (state->constructors[index].info == info) {
      return &state->constructors[index];
    }
  }
  return NULL;
}

static const AihcSnapshotFunction *find_function(const SnapshotState *state,
                                                  uintptr_t info) {
  for (uint64_t index = 0; index < state->function_count; ++index) {
    if (state->functions[index].info == info) {
      return &state->functions[index];
    }
  }
  return NULL;
}

static uint64_t location_for_object(SnapshotState *state, AihcValue *object) {
  for (uint64_t index = 0; index < state->object_count; ++index) {
    if (state->objects[index] == object) {
      return index;
    }
  }
  if (state->object_count == state->object_capacity) {
    uint64_t capacity =
        state->object_capacity == 0 ? 8 : state->object_capacity * 2;
    AihcValue **objects =
        realloc(state->objects, sizeof(*objects) * capacity);
    if (objects == NULL) {
      snapshot_fail("out of memory");
    }
    state->objects = objects;
    state->object_capacity = capacity;
  }
  uint64_t location = state->object_count++;
  state->objects[location] = object;
  return location;
}

static void print_scalar(AihcSlot value, AihcSnapshotRep rep) {
  union {
    uint32_t bits;
    float value;
  } float_value = {.bits = (uint32_t)value};
  union {
    uint64_t bits;
    double value;
  } double_value = {.bits = (uint64_t)value};

  switch (rep) {
  case AIHC_SNAPSHOT_INT:
  case AIHC_SNAPSHOT_INT64:
    printf("%" PRId64, (int64_t)value);
    return;
  case AIHC_SNAPSHOT_INT8:
    printf("%" PRId8, (int8_t)value);
    return;
  case AIHC_SNAPSHOT_INT16:
    printf("%" PRId16, (int16_t)value);
    return;
  case AIHC_SNAPSHOT_INT32:
    printf("%" PRId32, (int32_t)value);
    return;
  case AIHC_SNAPSHOT_WORD:
  case AIHC_SNAPSHOT_WORD64:
    printf("%" PRIu64, (uint64_t)value);
    return;
  case AIHC_SNAPSHOT_WORD8:
    printf("%" PRIu8, (uint8_t)value);
    return;
  case AIHC_SNAPSHOT_WORD16:
    printf("%" PRIu16, (uint16_t)value);
    return;
  case AIHC_SNAPSHOT_WORD32:
    printf("%" PRIu32, (uint32_t)value);
    return;
  case AIHC_SNAPSHOT_ADDR:
    printf("0x%" PRIxPTR, (uintptr_t)value);
    return;
  case AIHC_SNAPSHOT_FLOAT:
    printf("%.9g", (double)float_value.value);
    return;
  case AIHC_SNAPSHOT_DOUBLE:
    printf("%.17g", double_value.value);
    return;
  case AIHC_SNAPSHOT_POINTER:
    snapshot_fail("attempted to print a pointer as a scalar");
  }
  snapshot_fail("unknown runtime representation");
}

static void discover_value(SnapshotState *state, AihcSlot slot,
                           AihcSnapshotRep rep);

static void discover_fields(SnapshotState *state, const AihcSlot *fields,
                            uint64_t count, uint64_t rep_count,
                            const AihcSnapshotRep *field_reps) {
  if (count > rep_count) {
    snapshot_fail("node contains more fields than its descriptor");
  }
  for (uint64_t index = 0; index < count; ++index) {
    discover_value(state, fields[index], field_reps[index]);
  }
}

static void discover_object(SnapshotState *state, AihcValue *object) {
  uint64_t previous_count = state->object_count;
  location_for_object(state, object);
  if (previous_count == state->object_count) {
    return;
  }

  uint64_t tag = aihc_value_tag(object);
  switch (tag) {
  case AIHC_TAG_NODE:
  case AIHC_TAG_PARTIAL_CONSTRUCTOR: {
    const AihcSnapshotConstructor *constructor =
        find_constructor(state, aihc_value_info(object));
    if (constructor == NULL) {
      snapshot_fail("constructor descriptor is missing");
    }
    uint64_t count = tag == AIHC_TAG_NODE ? constructor->field_count
                                          : aihc_value_count(object);
    discover_fields(state, aihc_value_fields_const(object), count,
                    constructor->field_count, constructor->field_reps);
    return;
  }
  case AIHC_TAG_CLOSURE:
  case AIHC_TAG_THUNK: {
    const AihcSnapshotFunction *function =
        find_function(state, aihc_value_info(object));
    if (function == NULL) {
      snapshot_fail("function descriptor is missing");
    }
    uint64_t count = tag == AIHC_TAG_CLOSURE
                         ? aihc_value_count(object)
                         : function->parameter_count;
    discover_fields(state, aihc_value_fields_const(object), count,
                    function->parameter_count, function->parameter_reps);
    return;
  }
  case AIHC_TAG_PRIMITIVE:
    if (aihc_value_count(object) != 0) {
      snapshot_fail("captured primitive fields are not yet describable");
    }
    return;
  case AIHC_TAG_INDIRECTION:
    discover_value(state, object->fields[0], AIHC_SNAPSHOT_POINTER);
    return;
  case AIHC_TAG_BLACKHOLE:
    return;
  default:
    snapshot_fail("unknown heap object tag");
  }
}

static void discover_value(SnapshotState *state, AihcSlot slot,
                           AihcSnapshotRep rep) {
  if (rep == AIHC_SNAPSHOT_POINTER && slot != 0) {
    discover_object(state, (AihcValue *)slot);
  }
}

static void print_value(SnapshotState *state, AihcSlot slot,
                        AihcSnapshotRep rep) {
  if (rep != AIHC_SNAPSHOT_POINTER) {
    print_scalar(slot, rep);
    return;
  }
  if (slot == 0) {
    fputs("<null>", stdout);
    return;
  }
  printf("@%" PRIu64, location_for_object(state, (AihcValue *)slot));
}

static void print_fields(SnapshotState *state, const AihcSlot *fields,
                         uint64_t count, uint64_t rep_count,
                         const AihcSnapshotRep *field_reps) {
  if (count > rep_count) {
    snapshot_fail("node contains more fields than its descriptor");
  }
  for (uint64_t index = 0; index < count; ++index) {
    putchar(' ');
    print_value(state, fields[index], field_reps[index]);
  }
}

static void print_object(SnapshotState *state, const AihcValue *object) {
  uint64_t tag = aihc_value_tag(object);
  switch (tag) {
  case AIHC_TAG_NODE:
  case AIHC_TAG_PARTIAL_CONSTRUCTOR: {
    const AihcSnapshotConstructor *constructor =
        find_constructor(state, aihc_value_info(object));
    if (constructor == NULL) {
      snapshot_fail("constructor descriptor is missing");
    }
    fputs("C", stdout);
    fputs(constructor->name, stdout);
    uint64_t count;
    if (tag == AIHC_TAG_PARTIAL_CONSTRUCTOR) {
      printf("/%" PRIu64, aihc_value_arity(object));
      count = aihc_value_count(object);
    } else {
      count = constructor->field_count;
    }
    print_fields(state, aihc_value_fields_const(object), count,
                 constructor->field_count, constructor->field_reps);
    return;
  }
  case AIHC_TAG_CLOSURE:
  case AIHC_TAG_THUNK: {
    const AihcSnapshotFunction *function =
        find_function(state, aihc_value_info(object));
    if (function == NULL) {
      snapshot_fail("function descriptor is missing");
    }
    uint64_t count;
    if (tag == AIHC_TAG_CLOSURE) {
      printf("P%s/%" PRIu64, function->name, aihc_value_arity(object));
      count = aihc_value_count(object);
    } else {
      printf("F%s", function->name);
      count = function->parameter_count;
    }
    print_fields(state, aihc_value_fields_const(object), count,
                 function->parameter_count, function->parameter_reps);
    return;
  }
  case AIHC_TAG_PRIMITIVE:
    printf("Prim[%" PRIuPTR "/%" PRIu64 "]", aihc_value_info(object),
           aihc_value_arity(object));
    if (aihc_value_count(object) != 0) {
      snapshot_fail("captured primitive fields are not yet describable");
    }
    return;
  case AIHC_TAG_INDIRECTION:
    fputs("Indirection ", stdout);
    print_value(state, object->fields[0], AIHC_SNAPSHOT_POINTER);
    return;
  case AIHC_TAG_BLACKHOLE:
    fputs("<blackhole>", stdout);
    return;
  default:
    snapshot_fail("unknown heap object tag");
  }
}

void aihc_snapshot_dump(uint64_t result_count, const AihcSlot *results,
                        const AihcSnapshotRep *result_reps,
                        uint64_t constructor_count,
                        const AihcSnapshotConstructor *constructors,
                        uint64_t function_count,
                        const AihcSnapshotFunction *functions) {
  SnapshotState state = {
      .constructor_count = constructor_count,
      .constructors = constructors,
      .function_count = function_count,
      .functions = functions,
  };

  for (uint64_t index = 0; index < result_count; ++index) {
    discover_value(&state, results[index], result_reps[index]);
  }

  fputs("return: ", stdout);
  if (result_count == 0) {
    fputs("()", stdout);
  } else if (result_count == 1) {
    print_value(&state, results[0], result_reps[0]);
  } else {
    putchar('(');
    for (uint64_t index = 0; index < result_count; ++index) {
      if (index != 0) {
        fputs(", ", stdout);
      }
      print_value(&state, results[index], result_reps[index]);
    }
    putchar(')');
  }

  if (state.object_count == 0) {
    fputs("\nheap: []\n", stdout);
  } else {
    fputs("\nheap:\n", stdout);
    for (uint64_t location = 0; location < state.object_count; ++location) {
      printf("  @%" PRIu64 " = ", location);
      print_object(&state, state.objects[location]);
      putchar('\n');
    }
  }
  free(state.objects);
}
