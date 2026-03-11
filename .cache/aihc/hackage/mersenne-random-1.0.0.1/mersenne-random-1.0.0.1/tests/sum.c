#include "SFMT.h"

int main() {
    long i;
    long n, acc = 0;
    long lim =100000000; 
    printf("Generating %d randoms ...\n", lim);

    init_gen_rand(5);
    for (i=0;  i < lim; i++) {
        n = gen_rand64();
        if (n < acc) { acc = n; }
    }
    printf("%ld\n", acc);

    return 0;
}
