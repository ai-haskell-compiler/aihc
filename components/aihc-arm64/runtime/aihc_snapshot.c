#include "aihc_runtime.h"

#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct {
  AihcValue **cells;
  uint64_t cell_count;
  uint64_t cell_capacity;
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

static uint64_t location_for_cell(SnapshotState *state, AihcValue *cell) {
  for (uint64_t index = 0; index < state->cell_count; ++index) {
    if (state->cells[index] == cell) {
      return index;
    }
  }
  if (state->cell_count == state->cell_capacity) {
    uint64_t capacity = state->cell_capacity == 0 ? 8 : state->cell_capacity * 2;
    AihcValue **cells = realloc(state->cells, sizeof(*cells) * capacity);
    if (cells == NULL) {
      snapshot_fail("out of memory");
    }
    state->cells = cells;
    state->cell_capacity = capacity;
  }
  uint64_t location = state->cell_count++;
  state->cells[location] = cell;
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

static void print_value(SnapshotState *state, AihcSlot slot,
                        AihcSnapshotRep rep, int nested);

static void print_fields(SnapshotState *state, const AihcValue *value,
                         uint64_t rep_count,
                         const AihcSnapshotRep *field_reps) {
  if (value->count > rep_count) {
    snapshot_fail("node contains more fields than its descriptor");
  }
  for (uint64_t index = 0; index < value->count; ++index) {
    putchar(' ');
    print_value(state, value->fields[index], field_reps[index], 1);
  }
}

static void print_node(SnapshotState *state, const AihcValue *value,
                       int nested) {
  int parenthesized = nested && value->count != 0;
  if (parenthesized) {
    putchar('(');
  }
  switch (value->kind) {
  case AIHC_CONSTRUCTOR: {
    const AihcSnapshotConstructor *constructor =
        find_constructor(state, value->info);
    if (constructor == NULL) {
      snapshot_fail("constructor descriptor is missing");
    }
    printf("C%s", constructor->name);
    print_fields(state, value, constructor->field_count,
                 constructor->field_reps);
    break;
  }
  case AIHC_CLOSURE: {
    const AihcSnapshotFunction *function = find_function(state, value->info);
    if (function == NULL) {
      snapshot_fail("closure function descriptor is missing");
    }
    printf("P%s/%" PRIu64, function->name, value->arity);
    print_fields(state, value, function->parameter_count,
                 function->parameter_reps);
    break;
  }
  case AIHC_THUNK: {
    const AihcSnapshotFunction *function = find_function(state, value->info);
    if (function == NULL) {
      snapshot_fail("thunk function descriptor is missing");
    }
    printf("F%s", function->name);
    print_fields(state, value, function->parameter_count,
                 function->parameter_reps);
    break;
  }
  case AIHC_PRIMITIVE:
    printf("Prim[%" PRIuPTR "/%" PRIu64 "]", value->info, value->arity);
    if (value->count != 0) {
      snapshot_fail("captured primitive fields are not yet describable");
    }
    break;
  case AIHC_STATE_TOKEN:
    fputs("<state>", stdout);
    break;
  case AIHC_LITERAL_INT:
    printf("%" PRIuPTR, value->info);
    break;
  case AIHC_CELL:
    snapshot_fail("cell reached node printer");
    break;
  default:
    snapshot_fail("unknown heap object kind");
  }
  if (parenthesized) {
    putchar(')');
  }
}

static void print_value(SnapshotState *state, AihcSlot slot,
                        AihcSnapshotRep rep, int nested) {
  if (rep != AIHC_SNAPSHOT_POINTER) {
    print_scalar(slot, rep);
    return;
  }
  AihcValue *value = (AihcValue *)slot;
  if (value == NULL) {
    fputs("<null>", stdout);
  } else if (value->kind == AIHC_CELL) {
    printf("@%" PRIu64, location_for_cell(state, value));
  } else {
    print_node(state, value, nested);
  }
}

static void print_cell(SnapshotState *state, AihcValue *cell) {
  switch (cell->info) {
  case AIHC_CELL_SUSPENDED:
  case AIHC_CELL_VALUE:
    print_value(state, cell->fields[0], AIHC_SNAPSHOT_POINTER, 0);
    return;
  case AIHC_CELL_BLACKHOLE:
    fputs("<blackhole>", stdout);
    return;
  default:
    snapshot_fail("unknown cell state");
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

  fputs("return: ", stdout);
  if (result_count == 0) {
    fputs("()", stdout);
  } else if (result_count == 1) {
    print_value(&state, results[0], result_reps[0], 0);
  } else {
    putchar('(');
    for (uint64_t index = 0; index < result_count; ++index) {
      if (index != 0) {
        fputs(", ", stdout);
      }
      print_value(&state, results[index], result_reps[index], 0);
    }
    putchar(')');
  }

  if (state.cell_count == 0) {
    fputs("\nheap: []\n", stdout);
  } else {
    fputs("\nheap:\n", stdout);
    for (uint64_t location = 0; location < state.cell_count; ++location) {
      printf("  @%" PRIu64 " = ", location);
      print_cell(&state, state.cells[location]);
      putchar('\n');
    }
  }
  free(state.cells);
}
