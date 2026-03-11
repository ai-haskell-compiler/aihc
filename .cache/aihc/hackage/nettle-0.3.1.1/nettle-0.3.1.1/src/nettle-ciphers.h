
#ifndef _HS_NETTLE_CIPHERS_H
#define _HS_NETTLE_CIPHERS_H _HS_NETTLE_CIPHERS_H

#include <nettle/version.h>

#if (NETTLE_VERSION_MAJOR != 3)
#error unsupported nettle version
#endif

#include <sys/types.h>
#include <nettle/cbc.h>
#include <nettle/gcm.h>
#include <nettle/memxor.h>

/* block ciphers */
#include <nettle/aes.h>
#include <nettle/arctwo.h>
#include <nettle/blowfish.h>
#include <nettle/camellia.h>
#include <nettle/cast128.h>
#include <nettle/des.h>
#include <nettle/serpent.h>
#include <nettle/twofish.h>

/* stream ciphers */
#include <nettle/arcfour.h>
#include <nettle/chacha.h>
#include <nettle/salsa20.h>

/* AEAD ciphers */
#include <nettle/chacha-poly1305.h>

#if (CHACHA_POLY1305_NONCE_SIZE != CHACHA_NONCE96_SIZE)
#error unsupported nettle version, require 96-bit nonce chacha-poly1305 variant
#endif

void hs_nettle_cfb_encrypt(void *ctx, nettle_crypt_func *f,
	unsigned block_size, uint8_t *iv,
	unsigned length, uint8_t *dst,
	const uint8_t *src);

/* takes *ENCRYPTION* function f */
void hs_nettle_cfb_decrypt(void *ctx, nettle_crypt_func *f,
	unsigned block_size, uint8_t *iv,
	unsigned length, uint8_t *dst,
	const uint8_t *src);


struct hs_aes128_ctx {
	struct aes128_ctx encrypt, decrypt;
};
void hs_nettle_aes128_init(struct hs_aes128_ctx *ctx, const char *key);

struct hs_aes192_ctx {
	struct aes192_ctx encrypt, decrypt;
};
void hs_nettle_aes192_init(struct hs_aes192_ctx *ctx, const char *key);

struct hs_aes256_ctx {
	struct aes256_ctx encrypt, decrypt;
};
void hs_nettle_aes256_init(struct hs_aes256_ctx *ctx, const char *key);

union hs_aes_ctx_inner {
	struct aes128_ctx inner128;
	struct aes192_ctx inner192;
	struct aes256_ctx inner256;
};
struct hs_aes_ctx {
	enum { AES128, AES192, AES256 } selector;
	union hs_aes_ctx_inner encrypt, decrypt;
};
void hs_nettle_aes_init(struct hs_aes_ctx *ctx, unsigned int key_size, const char *key);
void hs_nettle_aes_encrypt(const struct hs_aes_ctx *ctx, size_t length, uint8_t *dst, const uint8_t *src);
void hs_nettle_aes_decrypt(const struct hs_aes_ctx *ctx, size_t length, uint8_t *dst, const uint8_t *src);

struct hs_camellia128_ctx {
	struct camellia128_ctx encrypt, decrypt;
};
void hs_nettle_camellia128_init(struct hs_camellia128_ctx *ctx, const char *key);

struct hs_camellia192_ctx {
	struct camellia192_ctx encrypt, decrypt;
};
void hs_nettle_camellia192_init(struct hs_camellia192_ctx *ctx, const char *key);

struct hs_camellia256_ctx {
	struct camellia256_ctx encrypt, decrypt;
};
void hs_nettle_camellia256_init(struct hs_camellia256_ctx *ctx, const char *key);

union hs_camellia_ctx_inner {
	struct camellia128_ctx inner128;
	struct camellia192_ctx inner192;
	struct camellia256_ctx inner256;
};
struct hs_camellia_ctx {
	enum { CAMELLIA128, CAMELLIA192, CAMELLIA256 } selector;
	union hs_camellia_ctx_inner encrypt, decrypt;
};
void hs_nettle_camellia_init(struct hs_camellia_ctx *ctx, unsigned int key_size, const char *key);
void hs_nettle_camellia_encrypt(const struct hs_camellia_ctx *ctx, size_t length, uint8_t *dst, const uint8_t *src);
void hs_nettle_camellia_decrypt(const struct hs_camellia_ctx *ctx, size_t length, uint8_t *dst, const uint8_t *src);

#endif
