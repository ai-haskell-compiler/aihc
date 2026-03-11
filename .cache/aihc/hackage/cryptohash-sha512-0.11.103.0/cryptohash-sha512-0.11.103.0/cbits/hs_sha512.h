/*
 * Copyright (C) 2006-2009 Vincent Hanquez <vincent@snarc.org>
 *               2016      Herbert Valerio Riedel <hvr@gnu.org>
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef CRYPTOHASH_SHA512_H
#define CRYPTOHASH_SHA512_H

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stddef.h>
#include <assert.h>
#include <string.h>
#include <ghcautoconf.h>

struct sha512_ctx
{
  uint64_t sz;
  uint64_t sz_hi;
  uint8_t  buf[128];
  uint64_t h[8];
};

/* keep this synchronised with 'digestSize'/'sizeCtx' in SHA512.hs */
#define SHA512_DIGEST_SIZE	64
#define SHA512t_DIGEST_SIZE(t)	(((t)+7)/8)
#define SHA384_DIGEST_SIZE	48
#define SHA512_CTX_SIZE		208

/* NB: SHA384 and SHA512/t are essentially the same algorithm as SHA512 and
 * therefore share the same data structures and most of the codepaths
 * except for different initialization data and truncating the 512bit
 * digest down to the respective digest bit-length.
 */
static inline void hs_cryptohash_sha384_init(struct sha512_ctx *ctx);
static inline void hs_cryptohash_sha512_init (struct sha512_ctx *ctx);
static inline void hs_cryptohash_sha512t_init(struct sha512_ctx *ctx, const uint16_t t);

static inline void hs_cryptohash_sha512_update (struct sha512_ctx *ctx, const uint8_t *data, size_t len);

static inline uint64_t hs_cryptohash_sha512t_finalize (struct sha512_ctx *ctx, uint16_t outbits, uint8_t *out);

#if defined(static_assert)
static_assert(sizeof(struct sha512_ctx) == SHA512_CTX_SIZE, "unexpected sha512_ctx size");
#else
/* poor man's pre-C11 _Static_assert */
typedef char static_assertion__unexpected_sha512_ctx_size[(sizeof(struct sha512_ctx) == SHA512_CTX_SIZE)?1:-1];
#endif

#define ptr_uint64_aligned(ptr) (!((uintptr_t)(ptr) & 0x7))

static inline uint64_t
ror64(const uint64_t word, const unsigned shift)
{
  /* GCC usually transforms this into a 'ror'-insn */
  return (word >> shift) | (word << (64 - shift));
}

static inline uint64_t
cpu_to_be64(const uint64_t hll)
{
#if WORDS_BIGENDIAN
  return hll;
#elif __GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 3)
  return __builtin_bswap64(hll);
#else
  /* GCC & Clang usually transforms this into a bswap insn */
  return ((hll & 0xff00000000000000) >> 56) |
         ((hll & 0x00ff000000000000) >> 40) |
         ((hll & 0x0000ff0000000000) >> 24) |
         ((hll & 0x000000ff00000000) >>  8) |
         ((hll & 0x00000000ff000000) <<  8) |
         ((hll & 0x0000000000ff0000) << 24) |
         ((hll & 0x000000000000ff00) << 40) |
         ((hll & 0x00000000000000ff) << 56);
#endif
}

static inline void
cpu_to_be64_array(uint64_t *dest, const uint64_t *src, unsigned wordcnt)
{
  while (wordcnt--)
    *dest++ = cpu_to_be64(*src++);
}

static inline void
hs_cryptohash_sha512_init (struct sha512_ctx *ctx)
{
  memset(ctx, 0, SHA512_CTX_SIZE);

  ctx->h[0] = 0x6a09e667f3bcc908ULL;
  ctx->h[1] = 0xbb67ae8584caa73bULL;
  ctx->h[2] = 0x3c6ef372fe94f82bULL;
  ctx->h[3] = 0xa54ff53a5f1d36f1ULL;
  ctx->h[4] = 0x510e527fade682d1ULL;
  ctx->h[5] = 0x9b05688c2b3e6c1fULL;
  ctx->h[6] = 0x1f83d9abfb41bd6bULL;
  ctx->h[7] = 0x5be0cd19137e2179ULL;
}

static inline void
hs_cryptohash_sha384_init (struct sha512_ctx *ctx)
{
  memset(ctx, 0, SHA512_CTX_SIZE);

  ctx->h[0] = 0xcbbb9d5dc1059ed8ULL;
  ctx->h[1] = 0x629a292a367cd507ULL;
  ctx->h[2] = 0x9159015a3070dd17ULL;
  ctx->h[3] = 0x152fecd8f70e5939ULL;
  ctx->h[4] = 0x67332667ffc00b31ULL;
  ctx->h[5] = 0x8eb44a8768581511ULL;
  ctx->h[6] = 0xdb0c2e0d64f98fa7ULL;
  ctx->h[7] = 0x47b5481dbefa4fa4ULL;
}

static inline void
hs_cryptohash_sha512t_init(struct sha512_ctx *ctx, const uint16_t t)
{
  memset(ctx, 0, SHA512_CTX_SIZE);

  if (!((0 < t) && (t < 512)))
    return;

  /* shortcuts for the currently two FIPS 180-4 "approved" parameters */
  switch (t) {
  case 224:
    ctx->h[0] = 0x8c3d37c819544da2ULL;
    ctx->h[1] = 0x73e1996689dcd4d6ULL;
    ctx->h[2] = 0x1dfab7ae32ff9c82ULL;
    ctx->h[3] = 0x679dd514582f9fcfULL;
    ctx->h[4] = 0x0f6d2b697bd44da8ULL;
    ctx->h[5] = 0x77e36f7304c48942ULL;
    ctx->h[6] = 0x3f9d85a86a1d36c8ULL;
    ctx->h[7] = 0x1112e6ad91d692a1ULL;
    return;

  case 256:
    ctx->h[0] = 0x22312194fc2bf72cULL;
    ctx->h[1] = 0x9f555fa3c84c64c2ULL;
    ctx->h[2] = 0x2393b86b6f53b151ULL;
    ctx->h[3] = 0x963877195940eabdULL;
    ctx->h[4] = 0x96283ee2a88effe3ULL;
    ctx->h[5] = 0xbe5e1e2553863992ULL;
    ctx->h[6] = 0x2b0199fc2c85b8aaULL;
    ctx->h[7] = 0x0eb72ddc81c52ca2ULL;
    return;
  }

  /* slow path */
  hs_cryptohash_sha512_init(ctx);
  for (int i = 0; i < 8; i++)
    ctx->h[i] ^= 0xa5a5a5a5a5a5a5a5ULL;

  uint64_t out[8] = { 0, };

  {
    char buf[16] = { 0, };
    const int bufsz = snprintf(buf, 16, "SHA-512/%d", t);
    hs_cryptohash_sha512_update(ctx, (uint8_t*)buf, bufsz);
    hs_cryptohash_sha512t_finalize(ctx, 512, (uint8_t*)out);
  }

  /* re-init the context, otherwise len is changed */
  memset(ctx, 0, SHA512_CTX_SIZE);
  for (int i = 0; i < 8; i++)
    ctx->h[i] = cpu_to_be64(out[i]);

}


/* 232 times the cube root of the first 64 primes 2..311 */
static const uint64_t k[] = {
  0x428a2f98d728ae22ULL, 0x7137449123ef65cdULL, 0xb5c0fbcfec4d3b2fULL,
  0xe9b5dba58189dbbcULL, 0x3956c25bf348b538ULL, 0x59f111f1b605d019ULL,
  0x923f82a4af194f9bULL, 0xab1c5ed5da6d8118ULL, 0xd807aa98a3030242ULL,
  0x12835b0145706fbeULL, 0x243185be4ee4b28cULL, 0x550c7dc3d5ffb4e2ULL,
  0x72be5d74f27b896fULL, 0x80deb1fe3b1696b1ULL, 0x9bdc06a725c71235ULL,
  0xc19bf174cf692694ULL, 0xe49b69c19ef14ad2ULL, 0xefbe4786384f25e3ULL,
  0x0fc19dc68b8cd5b5ULL, 0x240ca1cc77ac9c65ULL, 0x2de92c6f592b0275ULL,
  0x4a7484aa6ea6e483ULL, 0x5cb0a9dcbd41fbd4ULL, 0x76f988da831153b5ULL,
  0x983e5152ee66dfabULL, 0xa831c66d2db43210ULL, 0xb00327c898fb213fULL,
  0xbf597fc7beef0ee4ULL, 0xc6e00bf33da88fc2ULL, 0xd5a79147930aa725ULL,
  0x06ca6351e003826fULL, 0x142929670a0e6e70ULL, 0x27b70a8546d22ffcULL,
  0x2e1b21385c26c926ULL, 0x4d2c6dfc5ac42aedULL, 0x53380d139d95b3dfULL,
  0x650a73548baf63deULL, 0x766a0abb3c77b2a8ULL, 0x81c2c92e47edaee6ULL,
  0x92722c851482353bULL, 0xa2bfe8a14cf10364ULL, 0xa81a664bbc423001ULL,
  0xc24b8b70d0f89791ULL, 0xc76c51a30654be30ULL, 0xd192e819d6ef5218ULL,
  0xd69906245565a910ULL, 0xf40e35855771202aULL, 0x106aa07032bbd1b8ULL,
  0x19a4c116b8d2d0c8ULL, 0x1e376c085141ab53ULL, 0x2748774cdf8eeb99ULL,
  0x34b0bcb5e19b48a8ULL, 0x391c0cb3c5c95a63ULL, 0x4ed8aa4ae3418acbULL,
  0x5b9cca4f7763e373ULL, 0x682e6ff3d6b2b8a3ULL, 0x748f82ee5defb2fcULL,
  0x78a5636f43172f60ULL, 0x84c87814a1f0ab72ULL, 0x8cc702081a6439ecULL,
  0x90befffa23631e28ULL, 0xa4506cebde82bde9ULL, 0xbef9a3f7b2c67915ULL,
  0xc67178f2e372532bULL, 0xca273eceea26619cULL, 0xd186b8c721c0c207ULL,
  0xeada7dd6cde0eb1eULL, 0xf57d4f7fee6ed178ULL, 0x06f067aa72176fbaULL,
  0x0a637dc5a2c898a6ULL, 0x113f9804bef90daeULL, 0x1b710b35131c471bULL,
  0x28db77f523047d84ULL, 0x32caab7b40c72493ULL, 0x3c9ebe0a15c9bebcULL,
  0x431d67c49c100d4cULL, 0x4cc5d4becb3e42b6ULL, 0x597f299cfc657e2aULL,
  0x5fcb6fab3ad6faecULL, 0x6c44198c4a475817ULL
};

#define e0(x)       (ror64(x, 28) ^ ror64(x, 34) ^ ror64(x, 39))
#define e1(x)       (ror64(x, 14) ^ ror64(x, 18) ^ ror64(x, 41))
#define s0(x)       (ror64(x,  1) ^ ror64(x,  8) ^ (x >> 7))
#define s1(x)       (ror64(x, 19) ^ ror64(x, 61) ^ (x >> 6))

static void
sha512_do_chunk_aligned(struct sha512_ctx *ctx, uint64_t w[])
{
  int i;

  for (i = 16; i < 80; i++)
    w[i] = s1(w[i - 2]) + w[i - 7] + s0(w[i - 15]) + w[i - 16];

  uint64_t a = ctx->h[0];
  uint64_t b = ctx->h[1];
  uint64_t c = ctx->h[2];
  uint64_t d = ctx->h[3];
  uint64_t e = ctx->h[4];
  uint64_t f = ctx->h[5];
  uint64_t g = ctx->h[6];
  uint64_t h = ctx->h[7];

#define R(a, b, c, d, e, f, g, h, k, w)             \
    t1 = h + e1(e) + (g ^ (e & (f ^ g))) + k + w;   \
    t2 = e0(a) + ((a & b) | (c & (a | b)));         \
    d += t1;                                        \
    h = t1 + t2

  for (i = 0; i < 80; i += 8) {
    uint64_t t1, t2;

    R(a, b, c, d, e, f, g, h, k[i + 0], w[i + 0]);
    R(h, a, b, c, d, e, f, g, k[i + 1], w[i + 1]);
    R(g, h, a, b, c, d, e, f, k[i + 2], w[i + 2]);
    R(f, g, h, a, b, c, d, e, k[i + 3], w[i + 3]);
    R(e, f, g, h, a, b, c, d, k[i + 4], w[i + 4]);
    R(d, e, f, g, h, a, b, c, k[i + 5], w[i + 5]);
    R(c, d, e, f, g, h, a, b, k[i + 6], w[i + 6]);
    R(b, c, d, e, f, g, h, a, k[i + 7], w[i + 7]);
  }

#undef R

  ctx->h[0] += a;
  ctx->h[1] += b;
  ctx->h[2] += c;
  ctx->h[3] += d;
  ctx->h[4] += e;
  ctx->h[5] += f;
  ctx->h[6] += g;
  ctx->h[7] += h;
}

static void
sha512_do_chunk(struct sha512_ctx *ctx, const uint8_t buf[])
{
  uint64_t w[80]; /* only first 16 words are filled in */
  if (ptr_uint64_aligned(buf)) { /* aligned buf */
    cpu_to_be64_array(w, (const uint64_t *)buf, 16);
  } else { /* unaligned buf */
    memcpy(w, buf, 128);
#if !WORDS_BIGENDIAN
    cpu_to_be64_array(w, w, 16);
#endif
  }
  sha512_do_chunk_aligned(ctx, w);
}

static inline void
hs_cryptohash_sha512_update(struct sha512_ctx *ctx, const uint8_t *data, size_t len)
{
  size_t index = ctx->sz & 0x7f;
  const size_t to_fill = 128 - index;

  ctx->sz += len;
  // handle overflow
  if (ctx->sz < len)
    ctx->sz_hi++;

  /* process partial buffer if there's enough data to make a block */
  if (index && len >= to_fill) {
    memcpy(ctx->buf + index, data, to_fill);
    sha512_do_chunk(ctx, ctx->buf);
    /* memset(ctx->buf, 0, 128); */
    len -= to_fill;
    data += to_fill;
    index = 0;
  }

  /* process as many 128b-blocks as possible */
  while (len >= 128) {
    sha512_do_chunk(ctx, data);
    len -= 128;
    data += 128;
  }

  /* append data into buf */
  if (len)
    memcpy(ctx->buf + index, data, len);
}

static inline uint64_t
hs_cryptohash_sha512t_finalize (struct sha512_ctx *ctx, uint16_t outbits, uint8_t *out)
{
  static const uint8_t padding[128] = { 0x80, };
  const uint64_t sz = ctx->sz;

  /* add padding and update data with it */
  uint64_t bits[2];
  bits[0] = cpu_to_be64((ctx->sz_hi << 3) | (ctx->sz >> 61));
  bits[1] = cpu_to_be64(ctx->sz << 3);

  /* pad out to 112 */
  const size_t index = ctx->sz & 0x7f;
  const size_t padlen = (index < 112) ? (112 - index) : ((128 + 112) - index);
  hs_cryptohash_sha512_update(ctx, padding, padlen);

  /* append length */
  hs_cryptohash_sha512_update(ctx, (uint8_t *) bits, sizeof(bits));

  /* output hash */
  if (outbits > 512)
    outbits = 512; /* clamp to <= 512; better safe than sorry */

  if(outbits % 64 == 0) {
    /* fast path for 0,64,128,192,256,320,384,448,512 */
    cpu_to_be64_array((uint64_t *) out, ctx->h, outbits/64);
  } else {
    /* slow-path */
    uint8_t buf[SHA512_DIGEST_SIZE] = { 0x00, };
    cpu_to_be64_array((uint64_t *) buf, ctx->h, 8);

    const unsigned outbytes = SHA512t_DIGEST_SIZE(outbits);
    assert(outbytes > 0);
    assert(outbytes <= SHA512_DIGEST_SIZE);

    /* Handle case of t parameter not being an integral amount of
     * octects.  FIPS 180-4 states that "the resulting message digest
     * is truncated by selecting an appropriate number of the leftmost
     * bits". Hence in such a case we apply a mask to retain the
     * appropriate number of leftmost bits in the rightmost retained
     * octet.
     */
    const uint8_t outmask = ~(((uint8_t)0xff) >> (outbits % 8));
    if (outmask)
      buf[outbytes-1] &= outmask;

    memcpy(out, buf, outbytes);
  }

  return sz;
}

static inline void
hs_cryptohash_sha512_hash (const uint8_t *data, size_t len, uint8_t *out)
{
  struct sha512_ctx ctx;

  hs_cryptohash_sha512_init(&ctx);

  hs_cryptohash_sha512_update(&ctx, data, len);

  hs_cryptohash_sha512t_finalize(&ctx, 512, out);
}

static inline void
hs_cryptohash_sha384_hash (const uint8_t *data, size_t len, uint8_t *out)
{
  struct sha512_ctx ctx;

  hs_cryptohash_sha384_init(&ctx);

  hs_cryptohash_sha512_update(&ctx, data, len);

  hs_cryptohash_sha512t_finalize(&ctx, 384, out);
}

static inline void
hs_cryptohash_sha512t_hash (const uint8_t *data, size_t len, uint8_t *out, const uint16_t outbits)
{
  struct sha512_ctx ctx;

  if (!(0 < outbits && outbits < 512)) return;

  hs_cryptohash_sha512t_init(&ctx, outbits);

  hs_cryptohash_sha512_update(&ctx, data, len);

  hs_cryptohash_sha512t_finalize(&ctx, outbits, out);
}


#endif
