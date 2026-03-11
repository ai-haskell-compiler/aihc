/*
 * Copyright (c) 2020 Nikita Volkov <nikita.y.volkov@mail.ru>.
 */

#include <string.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>


void rev_poke_int64
(
  int64_t val,
  uint8_t* dst
)
{
  bool negate = false;

  if (val < 0)
  {
    int64_t b4 = val;
    val /= 10;
    if (val)
    {
      *--dst = val * 10 - b4 + 48;
      val = -val;
      negate = true;
    }
    else
    {
      *(dst - 1) = val * 10 - b4 + 48;
      *(dst - 2) = '-';
      return;
    }
  }

  do
  {
    int64_t b4 = val;
    val /= 10;
    *--dst = b4 - val * 10 + 48;
  }
  while (val);

  if (negate)
  {
    *--dst = '-';
  }
}

void rev_poke_uint64
(
  uint64_t val,
  uint8_t* dst
)
{
  do
  {
    uint64_t b4 = val;
    val /= 10;
    *--dst = b4 - val * 10 + 48;
  }
  while (val);
}
