// Copyright (C) 2016  Herbert Valerio Riedel
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

#ifndef HS_BROTLI_H_
#define HS_BROTLI_H_

#include <stddef.h>
#include <stdint.h>
#include <brotli/encode.h>
#include <brotli/decode.h>

typedef enum HsBrotliState
  { HS_BS_NEEDS_INPUT    = 0
  , HS_BS_HAS_OUTPUT     = 1
  , HS_BS_FINISHED       = 2
  , HS_BS_FAIL           = 3
  , HS_BS_INTERNAL_ERROR = 4
  } HsBrotliState;

static inline HsBrotliState
HsBrotliEncoderCompressStream (BrotliEncoderState* state,
                               BrotliEncoderOperation op,
                               size_t* available_in, uint8_t** next_in,
                               size_t* available_out, uint8_t** next_out,
                               size_t* total_out)
{
  const BROTLI_BOOL ok = BrotliEncoderCompressStream (state, op, available_in, (const uint8_t**) next_in, available_out, next_out, total_out);

  switch(ok) {
    case BROTLI_FALSE: return HS_BS_FAIL;
    case BROTLI_TRUE:
      switch(BrotliEncoderHasMoreOutput(state)) {
      case BROTLI_TRUE: return HS_BS_HAS_OUTPUT;
      case BROTLI_FALSE:
        switch(BrotliEncoderIsFinished(state)) {
        case BROTLI_TRUE: return HS_BS_FINISHED;
        case BROTLI_FALSE: return HS_BS_NEEDS_INPUT;
        }
      }
  }

  return HS_BS_INTERNAL_ERROR;
}

static inline HsBrotliState
HsBrotliDecoderDecompressStream (BrotliDecoderState* state,
                                 size_t* available_in, uint8_t** next_in,
                                 size_t* available_out, uint8_t** next_out,
                                 size_t* total_out)
{
  const BrotliDecoderResult res = BrotliDecoderDecompressStream (state, available_in, (const uint8_t**) next_in, available_out, next_out, total_out);

  switch(res) {
  case BROTLI_DECODER_RESULT_ERROR: return HS_BS_FAIL;
  case BROTLI_DECODER_RESULT_NEEDS_MORE_INPUT: return HS_BS_NEEDS_INPUT;
  case BROTLI_DECODER_RESULT_NEEDS_MORE_OUTPUT: return HS_BS_HAS_OUTPUT;
  case BROTLI_DECODER_RESULT_SUCCESS:
    // TODO: are all these branches really reachable?
    switch(BrotliDecoderHasMoreOutput(state)) {
    case BROTLI_TRUE: return HS_BS_HAS_OUTPUT;
    case BROTLI_FALSE:
      switch(BrotliDecoderIsFinished(state)) {
      case BROTLI_TRUE: return HS_BS_FINISHED;
      case BROTLI_FALSE: return HS_BS_NEEDS_INPUT;
      }
    }
  }
  return HS_BS_INTERNAL_ERROR;
}


static inline HsBrotliState
HsBrotliEncoderTakeOutput (BrotliEncoderState* state, size_t* sizep, uint8_t** buf_out)
{
  *buf_out = (uint8_t*) BrotliEncoderTakeOutput(state, sizep);

  const size_t size = *sizep;

  if (!*buf_out && size != 0) {
    *sizep = 0;
    return HS_BS_INTERNAL_ERROR;
  }

  // empty
  if (size == 0) {
    *buf_out = NULL;
    switch(BrotliEncoderIsFinished(state)) {
    case BROTLI_TRUE: return HS_BS_FINISHED;
    case BROTLI_FALSE: return HS_BS_NEEDS_INPUT;
    }
    return HS_BS_INTERNAL_ERROR;
  }

  // non-empty
  switch(BrotliEncoderHasMoreOutput(state)) {
  case BROTLI_TRUE: return HS_BS_HAS_OUTPUT;
  case BROTLI_FALSE:
    switch(BrotliEncoderIsFinished(state)) {
    case BROTLI_TRUE: return HS_BS_FINISHED;
    case BROTLI_FALSE: return HS_BS_NEEDS_INPUT;
    }
  }

  *sizep = 0;
  *buf_out = NULL;
  return HS_BS_INTERNAL_ERROR;
}

static inline HsBrotliState
HsBrotliDecoderTakeOutput (BrotliDecoderState* state, size_t* sizep, uint8_t** buf_out)
{
  *buf_out = (uint8_t*) BrotliDecoderTakeOutput(state, sizep);

  const size_t size = *sizep;

  if (!*buf_out && size != 0) {
    *sizep = 0;
    return HS_BS_INTERNAL_ERROR;
  }

  // empty
  if (size == 0) {
    *buf_out = NULL;
    switch(BrotliDecoderIsFinished(state)) {
    case BROTLI_TRUE: return HS_BS_FINISHED;
    case BROTLI_FALSE: return HS_BS_NEEDS_INPUT;
    }
    return HS_BS_INTERNAL_ERROR;
  }

  // non-empty
  switch(BrotliDecoderHasMoreOutput(state)) {
  case BROTLI_TRUE: return HS_BS_HAS_OUTPUT;
  case BROTLI_FALSE:
    switch(BrotliDecoderIsFinished(state)) {
    case BROTLI_TRUE: return HS_BS_FINISHED;
    case BROTLI_FALSE: return HS_BS_NEEDS_INPUT;
    }
  }

  *sizep = 0;
  *buf_out = NULL;
  return HS_BS_INTERNAL_ERROR;
}

static inline char*
HsBrotliDecoderErrorString (BrotliDecoderErrorCode c)
{
  return (char*) BrotliDecoderErrorString(c);
}

#endif
