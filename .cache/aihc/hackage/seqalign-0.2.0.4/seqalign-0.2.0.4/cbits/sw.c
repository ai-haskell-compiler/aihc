#include <stdlib.h>
#include <stdint.h>
#include "ksw.h"

static const int8_t default_mat[25] = {
    1, -1, -1, -1, 0,
    -1, 1, -1, -1, 0,
    -1, -1, 1, -1, 0,
    -1, -1, -1, 1, 0,
    0, 0, 0, 0, 0
};

static const uint8_t seq_nt4_table[256] = {
    4, 4, 4, 4,  4, 4, 4, 4,  4, 4, 4, 4,  4, 4, 4, 4, 
    4, 4, 4, 4,  4, 4, 4, 4,  4, 4, 4, 4,  4, 4, 4, 4, 
    4, 4, 4, 4,  4, 4, 4, 4,  4, 4, 4, 4,  4, 4, 4, 4,
    4, 4, 4, 4,  4, 4, 4, 4,  4, 4, 4, 4,  4, 4, 4, 4, 
    4, 0, 4, 1,  4, 4, 4, 2,  4, 4, 4, 4,  4, 4, 4, 4, 
    4, 4, 4, 4,  3, 4, 4, 4,  4, 4, 4, 4,  4, 4, 4, 4, 
    4, 0, 4, 1,  4, 4, 4, 2,  4, 4, 4, 4,  4, 4, 4, 4, 
    4, 4, 4, 4,  3, 4, 4, 4,  4, 4, 4, 4,  4, 4, 4, 4, 
    4, 4, 4, 4,  4, 4, 4, 4,  4, 4, 4, 4,  4, 4, 4, 4, 
    4, 4, 4, 4,  4, 4, 4, 4,  4, 4, 4, 4,  4, 4, 4, 4, 
    4, 4, 4, 4,  4, 4, 4, 4,  4, 4, 4, 4,  4, 4, 4, 4, 
    4, 4, 4, 4,  4, 4, 4, 4,  4, 4, 4, 4,  4, 4, 4, 4, 
    4, 4, 4, 4,  4, 4, 4, 4,  4, 4, 4, 4,  4, 4, 4, 4, 
    4, 4, 4, 4,  4, 4, 4, 4,  4, 4, 4, 4,  4, 4, 4, 4, 
    4, 4, 4, 4,  4, 4, 4, 4,  4, 4, 4, 4,  4, 4, 4, 4, 
    4, 4, 4, 4,  4, 4, 4, 4,  4, 4, 4, 4,  4, 4, 4, 4
};

inline void trans_nt4(char* qsq, uint8_t* qnt, int qlen) {
    int i;
    for (i = 0; i < qlen; i++) qnt[i] = seq_nt4_table[qsq[i]];
}

void default_align(int qlen, char *query, int tlen, char *target, int gapo, int gape, int xtra, kswr_t* ret) {
    uint8_t qnt4[qlen];
    trans_nt4(query, qnt4, qlen);
    uint8_t tnt4[tlen];
    trans_nt4(target, tnt4, tlen);
    *ret = ksw_align(qlen, qnt4, tlen, tnt4, 5, default_mat, gapo, gape, xtra, 0);
}

int cigar_align(int qlen, char *query, int tlen, char *target, int gapo, int gape, int w, int *n_cigar, uint32_t **cigar) {
    uint8_t qnt4[qlen];
    trans_nt4(query, qnt4, qlen);
    uint8_t tnt4[tlen];
    trans_nt4(target, tnt4, tlen);
    return ksw_global(qlen, qnt4, tlen, tnt4, 5, default_mat, gapo, gape, w, n_cigar, cigar);
}
