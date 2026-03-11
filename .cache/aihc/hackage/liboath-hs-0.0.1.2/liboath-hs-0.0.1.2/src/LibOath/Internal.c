
#include <liboath/oath.h>

int inline_c_LibOath_Internal_0_e5c09df3584bfac5d1724487f17015d119521c92() {
return ( oath_init() );
}


int inline_c_LibOath_Internal_1_a3a650f2b04aa545b11a0331d5430727896128b7() {
return ( oath_done() );
}


const char * inline_c_LibOath_Internal_2_b20401b6ccdbfbd806467ae7a5ebccec429550d0(char * minVersion_inline_c_0) {
return ( oath_check_version(minVersion_inline_c_0) );
}


const char * inline_c_LibOath_Internal_3_ffc6a8a5085d32853b6c7f73038d1c1e3ea3fbb2(int err_inline_c_0) {
return ( oath_strerror(err_inline_c_0) );
}


int inline_c_LibOath_Internal_4_74b0c50ca158dbc53ba2bcc21bfdf32cc5634402(const char * in__inline_c_0, size_t inlen_inline_c_1, char ** out_inline_c_2, size_t * outlen_inline_c_3) {
return (
        oath_base32_decode(in__inline_c_0, inlen_inline_c_1, out_inline_c_2, outlen_inline_c_3)
    );
}


int inline_c_LibOath_Internal_5_f3a3187368ef4f44055396679d0405c91018a031(const char * in__inline_c_0, size_t inlen_inline_c_1, char ** out_inline_c_2, size_t * outlen_inline_c_3) {
return (
        oath_base32_encode(in__inline_c_0, inlen_inline_c_1, out_inline_c_2, outlen_inline_c_3)
    );
}


int inline_c_LibOath_Internal_6_aa953bedae5657c3a6df617e2a429a2b85f05ddf(const char * secret_inline_c_0, size_t secretLen_inline_c_1, time_t now_inline_c_2, unsigned timeStepSize_inline_c_3, time_t startOffset_inline_c_4, unsigned digits_inline_c_5, char * outputOtp_inline_c_6) {
return (
        oath_totp_generate(
            secret_inline_c_0,
            secretLen_inline_c_1,
            now_inline_c_2,
            timeStepSize_inline_c_3,
            startOffset_inline_c_4,
            digits_inline_c_5,
            outputOtp_inline_c_6
        )
    );
}

