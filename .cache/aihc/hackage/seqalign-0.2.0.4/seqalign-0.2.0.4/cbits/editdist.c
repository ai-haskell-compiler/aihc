#define MIN2(a,b) (((a) < (b)) ? (a) : (b))
#define MIN3(x, y, z)  MIN2(z, MIN2(x,y))
#define MIN5(v, w, x, y, z)  MIN3(x, MIN2(y,z), MIN2(v,w))
#include <stdlib.h>
#include <stdint.h>

int c_hamming(const char *s1, const char *s2, int sz) {
    int dif = 0;
    int i;
    for (i=0; i<sz; i++)
        if (s1[i] != s2[i]) 
            dif++;
    
    return dif;
}

int c_levenshtein(const char *s1, int sz1, const char *s2, int sz2) {
    int i, j;

    int matrix[sz1+1][sz2+1];

    for(i=0;i<=sz1;i++){
        matrix[0][i]=i;
    }

    for(i=0;i<=sz2;i++){
        matrix[i][0]=i;
    }

    for(i=1; i<=sz1; i++){
        for (j=1; j<=sz2;j++){
            matrix[i][j]=MIN3(matrix[i-1][j]+1 , matrix[i][j-1]+1, matrix[i-1][j-1]+ ((s1[i-1]==s2[j-1])?0:1 ) );
        }
    }

    return matrix[sz1][sz2];
}

static const uint8_t degeneracies[256] = {
    0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0, 
    0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0, 
    0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0,
    0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0, 
    0, 1, 14, 2,  13, 0, 0, 4,  11, 15, 0, 12,  0, 3, 15, 0, 
    0, 0, 5, 6,  8, 8, 7, 9,  0, 10, 0, 0,  0, 0, 0, 0, 
    0, 1, 14, 2,  13, 0, 0, 4,  11, 15, 0, 12,  0, 3, 15, 0, 
    0, 0, 5, 6,  8, 8, 7, 9,  0, 10, 0, 0,  0, 0, 0, 0, 
    0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0, 
    0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0, 
    0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0, 
    0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0, 
    0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0, 
    0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0, 
    0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0, 
    0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0
};

inline int degen_eq(char a, char b) {
    return !(degeneracies[a] & degeneracies[b]);
}

int c_degenshtein(const char *s1, int sz1, const char *s2, int sz2){
    int  i, j;

    int matrix[sz1+1][sz2+1];

    for(i=0;i<=sz1;i++){
        matrix[0][i]=i;
    }

    for(i=0;i<=sz2;i++){
        matrix[i][0]=i;
    }

    for(i=1; i<=sz1; i++){
        for (j=1; j<=sz2;j++){
            matrix[i][j]=MIN3(matrix[i-1][j]+1 , matrix[i][j-1]+1, matrix[i-1][j-1]+ degen_eq(s1[i-1], s2[j-1]));
        }
    }

    return matrix[sz1][sz2];
}
