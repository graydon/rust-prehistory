#include <stdlib.h>
#include <stdio.h>
#include "uthash.h"

typedef struct {
    double veloc;
    /* ... other data ... */
    UT_hash_handle hh;
} veloc_t;

int main(int argc, char *argv[]) {
    veloc_t *v, *v2, *veloc_table = NULL;
    double x = 1/3.0;

    v = (veloc_t*)malloc( sizeof(*v) );
    v->veloc = x;
    HASH_ADD(hh, veloc_table, veloc, sizeof(double), v);
    HASH_FIND(hh, veloc_table, &x, sizeof(double), v2 );

    if (v2) printf("found (%.2f)\n", v2->veloc);
   return 0;
}

