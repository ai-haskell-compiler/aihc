// A call to a C function whose type is known only at run time.
//
// The Haskell module Aihc.Testing.ForeignCall is the only caller. It gives
// each argument as one 64-bit slot and gets the result in one 64-bit slot.
// Thus the Haskell code does not read the layout of a libffi structure.

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

// The macOS SDK has the header in the ffi directory. Other systems, and a
// libffi on CPATH, have it at the top level.
#if defined(__has_include)
#if __has_include(<ffi.h>)
#include <ffi.h>
#else
#include <ffi/ffi.h>
#endif
#else
#include <ffi.h>
#endif

// The kinds of a value. Aihc.Testing.ForeignCall has the same numbers.
enum {
  KIND_VOID = 0,
  KIND_SINT8 = 1,
  KIND_UINT8 = 2,
  KIND_SINT16 = 3,
  KIND_UINT16 = 4,
  KIND_SINT32 = 5,
  KIND_UINT32 = 6,
  KIND_SINT64 = 7,
  KIND_UINT64 = 8,
  KIND_FLOAT = 9,
  KIND_DOUBLE = 10,
  KIND_POINTER = 11
};

typedef union {
  ffi_arg word;
  int8_t sint8;
  uint8_t uint8;
  int16_t sint16;
  uint16_t uint16;
  int32_t sint32;
  uint32_t uint32;
  int64_t sint64;
  uint64_t uint64;
  float float_value;
  double double_value;
  void *pointer;
} slot;

static ffi_type *kind_type(int kind) {
  switch (kind) {
  case KIND_VOID:
    return &ffi_type_void;
  case KIND_SINT8:
    return &ffi_type_sint8;
  case KIND_UINT8:
    return &ffi_type_uint8;
  case KIND_SINT16:
    return &ffi_type_sint16;
  case KIND_UINT16:
    return &ffi_type_uint16;
  case KIND_SINT32:
    return &ffi_type_sint32;
  case KIND_UINT32:
    return &ffi_type_uint32;
  case KIND_SINT64:
    return &ffi_type_sint64;
  case KIND_UINT64:
    return &ffi_type_uint64;
  case KIND_FLOAT:
    return &ffi_type_float;
  case KIND_DOUBLE:
    return &ffi_type_double;
  case KIND_POINTER:
    return &ffi_type_pointer;
  default:
    return NULL;
  }
}

// Put the 64-bit slot of an argument into storage of the C type.
static void store_argument(int kind, uint64_t bits, slot *out) {
  uint32_t low = (uint32_t)bits;
  switch (kind) {
  case KIND_SINT8:
    out->sint8 = (int8_t)bits;
    break;
  case KIND_UINT8:
    out->uint8 = (uint8_t)bits;
    break;
  case KIND_SINT16:
    out->sint16 = (int16_t)bits;
    break;
  case KIND_UINT16:
    out->uint16 = (uint16_t)bits;
    break;
  case KIND_SINT32:
    out->sint32 = (int32_t)bits;
    break;
  case KIND_UINT32:
    out->uint32 = low;
    break;
  case KIND_FLOAT:
    memcpy(&out->float_value, &low, sizeof low);
    break;
  case KIND_DOUBLE:
    memcpy(&out->double_value, &bits, sizeof bits);
    break;
  case KIND_POINTER:
    out->pointer = (void *)(uintptr_t)bits;
    break;
  default:
    out->uint64 = bits;
    break;
  }
}

// Read the result of the call into a 64-bit slot. libffi widens an integer
// result that is smaller than ffi_arg to the full ffi_arg.
static uint64_t load_result(int kind, const slot *in) {
  uint32_t low = 0;
  uint64_t bits = 0;
  switch (kind) {
  case KIND_VOID:
    return 0;
  case KIND_SINT8:
    return (uint64_t)(int64_t)(int8_t)in->word;
  case KIND_UINT8:
    return (uint64_t)(uint8_t)in->word;
  case KIND_SINT16:
    return (uint64_t)(int64_t)(int16_t)in->word;
  case KIND_UINT16:
    return (uint64_t)(uint16_t)in->word;
  case KIND_SINT32:
    return (uint64_t)(int64_t)(int32_t)in->word;
  case KIND_UINT32:
    return (uint64_t)(uint32_t)in->word;
  case KIND_FLOAT:
    memcpy(&low, &in->float_value, sizeof low);
    return low;
  case KIND_DOUBLE:
    memcpy(&bits, &in->double_value, sizeof bits);
    return bits;
  case KIND_POINTER:
    return (uint64_t)(uintptr_t)in->pointer;
  default:
    return in->uint64;
  }
}

// Call `function` with `count` arguments. The result is 0 when the call
// was made. A different result means that no call was made.
int aihc_testing_foreign_call(void (*function)(void), int result_kind,
                              uint64_t *result, unsigned count,
                              const int *kinds, const uint64_t *values) {
  ffi_cif cif;
  slot result_slot;
  int status = 1;
  ffi_type *result_type = kind_type(result_kind);
  // One more element than `count`, because a size of 0 is not portable.
  ffi_type **types = malloc((count + 1) * sizeof *types);
  slot *slots = malloc((count + 1) * sizeof *slots);
  void **pointers = malloc((count + 1) * sizeof *pointers);

  if (result_type == NULL || types == NULL || slots == NULL || pointers == NULL)
    goto done;
  for (unsigned i = 0; i < count; i++) {
    types[i] = kind_type(kinds[i]);
    if (types[i] == NULL || kinds[i] == KIND_VOID)
      goto done;
    store_argument(kinds[i], values[i], &slots[i]);
    pointers[i] = &slots[i];
  }
  if (ffi_prep_cif(&cif, FFI_DEFAULT_ABI, count, result_type, types) != FFI_OK)
    goto done;
  memset(&result_slot, 0, sizeof result_slot);
  ffi_call(&cif, function, &result_slot, pointers);
  *result = load_result(result_kind, &result_slot);
  status = 0;

done:
  free(types);
  free(slots);
  free(pointers);
  return status;
}
