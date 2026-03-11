
#include "nettle-ciphers.h"

#undef NDEBUG
#include <assert.h>

void hs_nettle_cfb_encrypt(void *ctx, nettle_crypt_func *f,
	unsigned block_size, uint8_t *iv,
	unsigned length, uint8_t *dst,
	const uint8_t *src) {
	unsigned i;

	if (0 == length) return;
	assert(0 != block_size && length % block_size == 0);

	f(ctx, block_size, dst, iv);
	memxor(dst, src, block_size);
	for (i = block_size; i < length; i += block_size) {
		f(ctx, block_size, dst + i, dst + i - block_size);
		memxor(dst +i , src + i, block_size);
	}
}

void hs_nettle_cfb_decrypt(void *ctx, nettle_crypt_func *f,
	unsigned block_size, uint8_t *iv,
	unsigned length, uint8_t *dst,
	const uint8_t *src) {
	unsigned i;

	if (0 == length) return;
	assert(0 != block_size && length % block_size == 0);

	f(ctx, block_size, dst, iv);
	memxor(dst, src, block_size);
	for (i = block_size; i < length; i += block_size) {
		f(ctx, block_size, dst + i, src + i - block_size);
		memxor(dst +i , src + i, block_size);
	}
}

void hs_nettle_aes128_init(struct hs_aes128_ctx *ctx, const char *key) {
	aes128_set_encrypt_key(&ctx->encrypt, key);
	aes128_invert_key(&ctx->decrypt, &ctx->encrypt);
}

void hs_nettle_aes192_init(struct hs_aes192_ctx *ctx, const char *key) {
	aes192_set_encrypt_key(&ctx->encrypt, key);
	aes192_invert_key(&ctx->decrypt, &ctx->encrypt);
}

void hs_nettle_aes256_init(struct hs_aes256_ctx *ctx, const char *key) {
	aes256_set_encrypt_key(&ctx->encrypt, key);
	aes256_invert_key(&ctx->decrypt, &ctx->encrypt);
}

void hs_nettle_aes_init(struct hs_aes_ctx *ctx, unsigned int key_size, const char *key) {
	assert(16 == key_size || 24 == key_size || 32 == key_size);

	switch (key_size) {
	case 16:
		ctx->selector = AES128;
		aes128_set_encrypt_key(&ctx->encrypt.inner128, key);
		aes128_invert_key(&ctx->decrypt.inner128, &ctx->encrypt.inner128);
		break;
	case 24:
		ctx->selector = AES192;
		aes192_set_encrypt_key(&ctx->encrypt.inner192, key);
		aes192_invert_key(&ctx->decrypt.inner192, &ctx->encrypt.inner192);
		break;
	case 32:
		ctx->selector = AES256;
		aes256_set_encrypt_key(&ctx->encrypt.inner256, key);
		aes256_invert_key(&ctx->decrypt.inner256, &ctx->encrypt.inner256);
		break;
	}
}

void hs_nettle_aes_encrypt(const struct hs_aes_ctx *ctx, size_t length, uint8_t *dst, const uint8_t *src) {
	switch (ctx->selector) {
	case AES128:
		aes128_encrypt(&ctx->encrypt.inner128, length, dst, src);
		break;
	case AES192:
		aes192_encrypt(&ctx->encrypt.inner192, length, dst, src);
		break;
	case AES256:
		aes256_encrypt(&ctx->encrypt.inner256, length, dst, src);
		break;
	}
}

void hs_nettle_aes_decrypt(const struct hs_aes_ctx *ctx, size_t length, uint8_t *dst, const uint8_t *src) {
	switch (ctx->selector) {
	case AES128:
		aes128_decrypt(&ctx->decrypt.inner128, length, dst, src);
		break;
	case AES192:
		aes192_decrypt(&ctx->decrypt.inner192, length, dst, src);
		break;
	case AES256:
		aes256_decrypt(&ctx->decrypt.inner256, length, dst, src);
		break;
	}
}

void hs_nettle_camellia128_init(struct hs_camellia128_ctx *ctx, const char *key) {
	camellia128_set_encrypt_key(&ctx->encrypt, key);
	camellia128_invert_key(&ctx->decrypt, &ctx->encrypt);
}

void hs_nettle_camellia192_init(struct hs_camellia192_ctx *ctx, const char *key) {
	camellia192_set_encrypt_key(&ctx->encrypt, key);
	camellia192_invert_key(&ctx->decrypt, &ctx->encrypt);
}

void hs_nettle_camellia256_init(struct hs_camellia256_ctx *ctx, const char *key) {
	camellia256_set_encrypt_key(&ctx->encrypt, key);
	camellia256_invert_key(&ctx->decrypt, &ctx->encrypt);
}

void hs_nettle_camellia_init(struct hs_camellia_ctx *ctx, unsigned int key_size, const char *key) {
	assert(16 == key_size || 24 == key_size || 32 == key_size);

	switch (key_size) {
	case 16:
		ctx->selector = CAMELLIA128;
		camellia128_set_encrypt_key(&ctx->encrypt.inner128, key);
		camellia128_invert_key(&ctx->decrypt.inner128, &ctx->encrypt.inner128);
		break;
	case 24:
		ctx->selector = CAMELLIA192;
		camellia192_set_encrypt_key(&ctx->encrypt.inner192, key);
		camellia192_invert_key(&ctx->decrypt.inner192, &ctx->encrypt.inner192);
		break;
	case 32:
		ctx->selector = CAMELLIA256;
		camellia256_set_encrypt_key(&ctx->encrypt.inner256, key);
		camellia256_invert_key(&ctx->decrypt.inner256, &ctx->encrypt.inner256);
		break;
	}
}

void hs_nettle_camellia_encrypt(const struct hs_camellia_ctx *ctx, size_t length, uint8_t *dst, const uint8_t *src) {
	switch (ctx->selector) {
	case CAMELLIA128:
		camellia128_crypt(&ctx->encrypt.inner128, length, dst, src);
		break;
	case CAMELLIA192:
		camellia192_crypt(&ctx->encrypt.inner192, length, dst, src);
		break;
	case CAMELLIA256:
		camellia256_crypt(&ctx->encrypt.inner256, length, dst, src);
		break;
	}
}

void hs_nettle_camellia_decrypt(const struct hs_camellia_ctx *ctx, size_t length, uint8_t *dst, const uint8_t *src) {
	switch (ctx->selector) {
	case CAMELLIA128:
		camellia128_crypt(&ctx->decrypt.inner128, length, dst, src);
		break;
	case CAMELLIA192:
		camellia192_crypt(&ctx->decrypt.inner192, length, dst, src);
		break;
	case CAMELLIA256:
		camellia256_crypt(&ctx->decrypt.inner256, length, dst, src);
		break;
	}
}
