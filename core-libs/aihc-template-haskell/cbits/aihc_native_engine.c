/* The guest side of the engine protocol for a native splice program.

   The compiler runs the splices of a module in a native program when the
   user gives --unsafe-disable-th-sandbox. The program has no sandbox. This
   file gives it the functions that the JavaScript driver of the WebAssembly
   engine gives to an image (bin/aihc/compiler/wasm/runtime/driver.mjs).
   The compiler then reads the same lines from the two engines.

   The protocol uses the standard input and the standard output of the
   process. aihc_native_engine_start moves the protocol to new file
   descriptors. After that, descriptor 1 is a copy of the standard error,
   and descriptor 0 reads /dev/null. Thus, a splice that writes to its
   standard output cannot change the protocol.

   The compiler sends one "run INDEX" line for each splice. The program
   replies with "ok", followed by each message of the splice as
   hexadecimal text, or with "error MESSAGE". Before the reply, a splice
   can write "request BYTES" lines. The next line from the compiler is then
   "reply BYTES". */

// NOLINTNEXTLINE(bugprone-reserved-identifier)
#define _POSIX_C_SOURCE 200809L

#include <fcntl.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

static FILE *engine_input;
static FILE *engine_output;

/* The messages of the splice that runs, as " HEX" for each message. */
static char *messages;
static size_t messages_length;
static size_t messages_capacity;

/* The failure of the splice that runs, or NULL. */
static char *failure;

/* The reply to the last request. */
static unsigned char *reply_bytes;
static size_t reply_length;

/* If a splice runs. The first call of aihc_native_engine_next has no
   splice before it. */
static int running;

static void engine_abort(const char *message) {
  fprintf(stderr, "aihc native engine: %s\n", message);
  exit(70);
}

static void *engine_realloc(void *pointer, size_t size) {
  void *result = realloc(pointer, size == 0 ? 1 : size);
  if (result == NULL)
    engine_abort("out of memory");
  return result;
}

static void append_message(const char *text, size_t length) {
  if (messages_length + length + 1 > messages_capacity) {
    size_t capacity = messages_capacity == 0 ? 256 : messages_capacity;
    while (messages_length + length + 1 > capacity)
      capacity *= 2;
    messages = engine_realloc(messages, capacity);
    messages_capacity = capacity;
  }
  memcpy(messages + messages_length, text, length);
  messages_length += length;
  messages[messages_length] = '\0';
}

static void write_hexadecimal(FILE *stream, const unsigned char *bytes,
                              size_t length) {
  static const char digits[] = "0123456789abcdef";
  for (size_t index = 0; index < length; index++) {
    fputc(digits[bytes[index] >> 4], stream);
    fputc(digits[bytes[index] & 15], stream);
  }
}

static int hexadecimal_digit(char digit) {
  if (digit >= '0' && digit <= '9')
    return digit - '0';
  if (digit >= 'a' && digit <= 'f')
    return digit - 'a' + 10;
  if (digit >= 'A' && digit <= 'F')
    return digit - 'A' + 10;
  return -1;
}

/* Write one line. A line break or a tab in the text becomes a space, as
   in the driver of the WebAssembly engine. */
static void write_line(const char *prefix, const char *text) {
  fputs(prefix, engine_output);
  for (const char *cursor = text; *cursor != '\0'; cursor++) {
    char character = *cursor;
    fputc(character == '\n' || character == '\r' || character == '\t'
              ? ' '
              : character,
          engine_output);
  }
  fputc('\n', engine_output);
  fflush(engine_output);
}

/* The next line from the compiler, without its line break, or NULL at the
   end of the input. The caller frees the line. */
static char *read_line(void) {
  char *line = NULL;
  size_t capacity = 0;
  ssize_t length = getline(&line, &capacity, engine_input);
  if (length < 0) {
    free(line);
    return NULL;
  }
  if (length > 0 && line[length - 1] == '\n')
    line[length - 1] = '\0';
  return line;
}

/* Move the protocol away from the standard streams. Call this before the
   program writes to its standard output. */
void aihc_native_engine_start(void) {
  int input = dup(0);
  int output = dup(1);
  if (input < 0 || output < 0)
    engine_abort("cannot copy the standard streams");
  int empty = open("/dev/null", O_RDONLY);
  if (empty < 0 || dup2(empty, 0) < 0 || dup2(2, 1) < 0)
    engine_abort("cannot replace the standard streams");
  close(empty);
  engine_input = fdopen(input, "r");
  engine_output = fdopen(output, "w");
  if (engine_input == NULL || engine_output == NULL)
    engine_abort("cannot open the protocol streams");
}

/* Reply to the command of the last splice, and give the index of the next
   splice. The result is -1 at the end of the input. */
int64_t aihc_native_engine_next(void) {
  if (!running) {
    write_line("ok", "");
  } else if (failure != NULL) {
    write_line("error ", failure);
  } else {
    write_line("ok", messages == NULL ? "" : messages);
  }
  running = 0;
  free(failure);
  failure = NULL;
  messages_length = 0;
  if (messages != NULL)
    messages[0] = '\0';
  for (;;) {
    char *line = read_line();
    if (line == NULL)
      return -1;
    char *end = NULL;
    long long index = -1;
    if (strncmp(line, "run ", 4) == 0)
      index = strtoll(line + 4, &end, 10);
    if (index >= 0 && end != NULL && end != line + 4 && *end == '\0') {
      free(line);
      running = 1;
      return (int64_t)index;
    }
    write_line("error the guest cannot do the command ", line);
    free(line);
  }
}

/* A splice sends a message to the compiler. */
void aihc_engine_send(const unsigned char *message, int64_t length) {
  static const char digits[] = "0123456789abcdef";
  size_t size = length < 0 ? 0 : (size_t)length;
  append_message(" ", 1);
  for (size_t index = 0; index < size; index++) {
    char pair[2] = {digits[message[index] >> 4], digits[message[index] & 15]};
    append_message(pair, 2);
  }
}

/* A splice sends a request to the compiler and waits for the reply. The
   result is the number of bytes of the reply. */
int64_t aihc_engine_request(const unsigned char *message, int64_t length) {
  fputs("request ", engine_output);
  write_hexadecimal(engine_output, message, (size_t)length);
  fputc('\n', engine_output);
  fflush(engine_output);
  char *line = read_line();
  if (line == NULL)
    engine_abort("the input ended at a request");
  if (strncmp(line, "reply", 5) != 0 || (line[5] != ' ' && line[5] != '\0'))
    engine_abort("the compiler sent a command at a request");
  const char *text = line[5] == '\0' ? line + 5 : line + 6;
  size_t digits = strlen(text);
  if (digits % 2 != 0)
    engine_abort("the reply is not hexadecimal text");
  reply_length = digits / 2;
  reply_bytes = engine_realloc(reply_bytes, reply_length);
  for (size_t index = 0; index < reply_length; index++) {
    int high = hexadecimal_digit(text[2 * index]);
    int low = hexadecimal_digit(text[2 * index + 1]);
    if (high < 0 || low < 0)
      engine_abort("the reply is not hexadecimal text");
    reply_bytes[index] = (unsigned char)(high * 16 + low);
  }
  free(line);
  return (int64_t)reply_length;
}

/* Copy the reply of the last request to the memory of the splice. */
void aihc_engine_reply(unsigned char *target) {
  if (reply_length > 0)
    memcpy(target, reply_bytes, reply_length);
}

/* The splice raised an exception. */
void aihc_native_engine_fail(const char *message, int64_t length) {
  size_t size = length < 0 ? 0 : (size_t)length;
  free(failure);
  failure = engine_realloc(NULL, size + 1);
  memcpy(failure, message, size);
  failure[size] = '\0';
}
