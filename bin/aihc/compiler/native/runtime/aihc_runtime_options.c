#include "aihc_runtime_internal.h"

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

static uint8_t *aihc_program_arguments;
static size_t aihc_program_arguments_length;
static AihcRtsConfig aihc_process_rts_config;

static void aihc_program_arguments_install(uint8_t *arguments, size_t length) {
  free(aihc_program_arguments);
  aihc_program_arguments = arguments;
  aihc_program_arguments_length = length;
}

static uint64_t aihc_parse_heap_size(const char *text) {
  if (*text < '0' || *text > '9') {
    aihc_fail("missing size for RTS option -M");
  }
  uint64_t value = 0;
  while (*text >= '0' && *text <= '9') {
    uint64_t digit = (uint64_t)(*text - '0');
    if (value > (UINT64_MAX - digit) / 10) {
      aihc_fail("size for RTS option -M is too large");
    }
    value = value * 10 + digit;
    ++text;
  }
  uint64_t multiplier = 1;
  switch (*text) {
  case 0:
    break;
  case 'K':
  case 'k':
    multiplier = UINT64_C(1024);
    ++text;
    break;
  case 'M':
  case 'm':
    multiplier = UINT64_C(1024) * UINT64_C(1024);
    ++text;
    break;
  case 'G':
  case 'g':
    multiplier = UINT64_C(1024) * UINT64_C(1024) * UINT64_C(1024);
    ++text;
    break;
  case 'T':
  case 't':
    multiplier =
        UINT64_C(1024) * UINT64_C(1024) * UINT64_C(1024) * UINT64_C(1024);
    ++text;
    break;
  default:
    aihc_fail("invalid size for RTS option -M");
  }
  if (*text != 0) {
    aihc_fail("invalid size for RTS option -M");
  }
  if (value > UINT64_MAX / multiplier) {
    aihc_fail("size for RTS option -M is too large");
  }
  return value * multiplier;
}

static void aihc_parse_rts_option(const char *argument) {
  if (argument[0] == '-' && argument[1] == 'M') {
    aihc_process_rts_config.heap_max_bytes = aihc_parse_heap_size(argument + 2);
    aihc_process_rts_config.heap_limit_enabled = 1;
    return;
  }
  if (argument[0] == '-' && argument[1] == 'Z' && argument[2] == 's' &&
      argument[3] == 0) {
    aihc_process_rts_config.static_reference_roots = 1;
    return;
  }
  aihc_fail("unknown RTS option");
}

static int aihc_argument_is(const char *argument, const char *expected) {
  while (*argument != 0 && *argument == *expected) {
    ++argument;
    ++expected;
  }
  return *argument == *expected;
}

static int aihc_argument_end(const uint8_t *buffer, size_t length,
                             size_t offset, size_t *end) {
  for (size_t index = offset; index < length; ++index) {
    if (buffer[index] == 0) {
      *end = index;
      return 1;
    }
  }
  return 0;
}

int64_t aihc_runtime_arguments_initialize(const void *opaque_buffer,
                                          int64_t requested_length) {
  if (requested_length < 0 ||
      (requested_length != 0 && opaque_buffer == NULL)) {
    return -1;
  }
  size_t length = (size_t)requested_length;
  const uint8_t *buffer = opaque_buffer;
  if (length != 0 && buffer[length - 1] != 0) {
    return -1;
  }
  uint8_t *arguments = aihc_allocate_zeroed(length == 0 ? 1 : length);
  size_t input = 0;
  size_t output = 0;
  size_t index = 0;
  int in_rts_options = 0;
  int rts_options_disabled = 0;
  aihc_process_rts_config = (AihcRtsConfig){0};
  while (input < length) {
    size_t end;
    if (!aihc_argument_end(buffer, length, input, &end)) {
      free(arguments);
      return -1;
    }
    const char *argument = (const char *)(buffer + input);
    size_t argument_length = end - input + 1;
    int keep = index == 0;
    if (index != 0 && !rts_options_disabled) {
      if (!in_rts_options && aihc_argument_is(argument, "+RTS")) {
        in_rts_options = 1;
      } else if (!in_rts_options && aihc_argument_is(argument, "--RTS")) {
        rts_options_disabled = 1;
      } else if (in_rts_options && aihc_argument_is(argument, "-RTS")) {
        in_rts_options = 0;
      } else if (in_rts_options) {
        aihc_parse_rts_option(argument);
      } else {
        keep = 1;
      }
    } else if (index != 0) {
      keep = 1;
    }
    if (keep) {
      memcpy(arguments + output, buffer + input, argument_length);
      output += argument_length;
    }
    input = end + 1;
    ++index;
  }
  aihc_program_arguments_install(arguments, output);
  return 0;
}

void aihc_program_arguments_initialize(int argc, char *const argv[]) {
  if (argc < 0 || (argc != 0 && argv == NULL)) {
    aihc_fail("invalid initial program arguments");
  }
  size_t length = 0;
  for (int index = 0; index < argc; ++index) {
    if (argv[index] == NULL) {
      aihc_fail("null initial program argument");
    }
    size_t argument_length = strlen(argv[index]);
    if ((uint64_t)argument_length >= (uint64_t)INT64_MAX - (uint64_t)length) {
      aihc_fail("program arguments are too large");
    }
    length += argument_length + 1;
  }
  uint8_t *arguments = aihc_allocate_zeroed(length == 0 ? 1 : length);
  size_t offset = 0;
  for (int index = 0; index < argc; ++index) {
    size_t argument_length = strlen(argv[index]);
    memcpy(arguments + offset, argv[index], argument_length);
    offset += argument_length + 1;
  }
  if (aihc_runtime_arguments_initialize(arguments, (int64_t)length) != 0) {
    free(arguments);
    aihc_fail("invalid initial program arguments");
  }
  free(arguments);
}

int64_t aihc_program_arguments_size(void) {
  return (int64_t)aihc_program_arguments_length;
}

int64_t aihc_program_arguments_copy(void *opaque_buffer, int64_t capacity) {
  int64_t required = aihc_program_arguments_size();
  if (capacity < 0 || (capacity != 0 && opaque_buffer == NULL)) {
    return -1;
  }
  if (capacity >= required && required != 0) {
    memcpy(opaque_buffer, aihc_program_arguments, (size_t)required);
  }
  return required;
}

int64_t aihc_program_arguments_replace(const void *opaque_buffer,
                                       int64_t requested_length) {
  if (requested_length < 0 ||
      (requested_length != 0 && opaque_buffer == NULL)) {
    return -1;
  }
  size_t length = (size_t)requested_length;
  const uint8_t *buffer = opaque_buffer;
  if (length != 0 && buffer[length - 1] != 0) {
    return -1;
  }
  uint8_t *arguments = length == 0 ? NULL : aihc_allocate_zeroed(length);
  if (length != 0) {
    memcpy(arguments, buffer, length);
  }
  aihc_program_arguments_install(arguments, length);
  return 0;
}

const AihcRtsConfig *aihc_rts_config(void) { return &aihc_process_rts_config; }
