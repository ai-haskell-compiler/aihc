#include <stdlib.h>
#include <stdarg.h>
#include <alloca.h>

typedef char fmt_bool;
const fmt_bool fmt_false = 0, fmt_true = 1;

enum context {pcm_params, params_only};

void make_camel_case (char *dst, const char *src) {
  while (*src != 0) {
    if (*src == '_') {
      src ++;
      if (*src == 0) {break;}
      *dst = *src - ('a'-'A'); // assumes that 'a'>'A'
    } else {
      *dst = *src;
    }
    src++;
    dst++;
  }
  *dst = 0;
}

void init_comma (fmt_bool *comma) {
  *comma = fmt_false;
}

void print_comma (fmt_bool *comma) {
  if (*comma) {
    printf(", ");
  }
  *comma = fmt_true;
}

struct type {
  char *convert;
  char *hsname;
  char *cname;
};

enum param_direction {dir_in, dir_out, dir_inout};

struct parameter {
  struct type *type;
  char *name;
  enum param_direction dir;
};

struct parameter *in (struct type* t, char *name) {
  struct parameter *p = malloc (sizeof(struct parameter));
  p->type = t;
  p->name = name;
  p->dir = dir_in;
  return p;
}

struct parameter *out (struct type* t, char *name) {
  struct parameter *p = malloc (sizeof(struct parameter));
  p->type = t;
  p->name = name;
  p->dir = dir_out;
  return p;
}

struct parameter *inout (struct type* t, char *name) {
  struct parameter *p = malloc (sizeof(struct parameter));
  p->type = t;
  p->name = name;
  p->dir = dir_inout;
  return p;
}


struct type bool_p = {"boolConv", "Bool", "Bool_"};

struct type uint_p = {"Conv.int", "Word", "C.CUInt"};

struct type rate_p = {"Conv.int", "SampleFreq", "C.CUInt"};

struct type time_p = {"Conv.int", "Time", "C.CUInt"};

struct type direction_p = {"ordConv", "Ordering", "Direction"};

struct type uframes_p = {"Conv.int", "Size", "C.CUInt"};

struct type access_p = {"Conv.id", "Access", "Access"};

struct type format_p = {"Conv.id", "Format", "Format"};

struct type subformat_p = {"Conv.id", "Subformat", "Subformat"};

enum result {noresult, boolresult, uintresult, errnoresult, checked};


void hsc_accessor (enum result res, const char *name, enum context ctx, ...) {
  va_list arg;
  struct parameter *p;
  fmt_bool comma;
  char *camelName = alloca (strlen (name) + 1);
  make_camel_case (camelName, name);

  printf("foreign import ccall safe \"alsa/pcm.h snd_pcm_hw_params_%s\"\n",
         name);
  printf("   %s_ :: ", camelName);
  if (ctx==pcm_params) {
//    printf("Ptr (Struct i y) -> Ptr Params -> ");
    printf("Handle i y -> Ptr Params -> ");
  } else {
    printf("Ptr Params -> ");
  }

  va_start (arg, ctx);
  while (p = va_arg (arg, struct parameter *)) {
    if (p->dir == dir_in) {
      printf("%s -> ", p->type->cname);
     } else {
      printf("Ptr %s -> ", p->type->cname);
    }
  }
  va_end (arg);

  printf("IO ");
  if (res == noresult) {
    printf("()");
  } else if (res == errnoresult) {
    printf("E.Errno");
  } else {
    printf("C.CInt");
  }
  printf("\n\n");

  printf("%s :: ", camelName);

  va_start (arg, ctx);
  while (p = va_arg (arg, struct parameter *)) {
    if (p->dir != dir_out) {
      printf("%s -> ", p->type->hsname);
    }
  }
  va_end (arg);

  printf("T i y (");
  init_comma(&comma);
  if (res == uintresult) {
    print_comma(&comma);
    printf("Word");
  } else if (res == errnoresult) {
    print_comma(&comma);
    printf("E.Errno");
  } else if (res == boolresult) {
    print_comma(&comma);
    printf("Bool");
  }
  va_start (arg, ctx);
  while (p = va_arg (arg, struct parameter *)) {
    if (p->dir != dir_in) {
      print_comma(&comma);
      printf("%s", p->type->hsname);
    }
  }
  va_end (arg);
  printf(")\n");

  printf("%s", camelName);
  va_start (arg, ctx);
  while (p = va_arg (arg, struct parameter *)) {
    if (p->dir != dir_out) {
      printf(" %s", p->name);
    }
  }
  va_end (arg);
  printf(" =\n");

  if (ctx==pcm_params) {
//    printf("   Cons $ \\(Handle h) p ->\n");
    printf("   Cons $ \\h p ->\n");
  } else {
    printf("   Cons $ \\ _ p ->\n");
  }
  va_start (arg, ctx);
  while (p = va_arg (arg, struct parameter *)) {
    if (p->dir == dir_out) {
      printf("   alloca $ \\%sPtr ->\n", p->name);
    } else if (p->dir == dir_inout) {
      printf("   Conv.with %s %s $ \\%sPtr ->\n",
         p->type->convert, p->name, p->name);
    }
  }
  va_end (arg);

  printf("   %s_", camelName);
  if (ctx==pcm_params) {
    printf(" h p");
  } else {
    printf(" p");
  }
  va_start (arg, ctx);
  while (p = va_arg (arg, struct parameter *)) {
    if (p->dir == dir_in) {
      printf(" (fromHaskell %s %s)",
                p->type->convert, p->name);
    } else {
      printf(" %sPtr", p->name);
    }
  }
  va_end (arg);
  if (res == uintresult) {
    printf(" >>=\n   checkResult \"HwParams.%s\" >>= \\res ->\n", camelName);
  } else if (res == boolresult || res == errnoresult) {
    printf(" >>= \\res ->\n");
  } else if (res == checked) {
    printf(" >>=\n   checkResult_ \"HwParams.%s\" >>\n", camelName);
  } else {
    printf(" >>\n");
  }

  va_start (arg, ctx);
  while (p = va_arg (arg, struct parameter *)) {
    if (p->dir != dir_in) {
      printf("   Conv.peek %s %sPtr >>= \\%sResult ->\n",
         p->type->convert, p->name, p->name);
    }
  }
  va_end (arg);

  printf("   return (");
  init_comma(&comma);
  if (res == uintresult) {
    print_comma(&comma);
    printf("fromIntegral res");
  } else if (res == errnoresult) {
    print_comma(&comma);
    printf("res");
  } else if (res == boolresult) {
    print_comma(&comma);
    printf("res/=0");
  }
  va_start (arg, ctx);
  while (p = va_arg (arg, struct parameter *)) {
    if (p->dir != dir_in) {
      print_comma(&comma);
      printf("%sResult", p->name);
    }
  }
  va_end (arg);
  printf(")\n");
}
