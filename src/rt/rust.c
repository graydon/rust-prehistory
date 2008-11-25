#include "rust.h"
#include <stdio.h>

int __cdecl 
rust_start(void (*thunk)(rust_rt_t*))
{
  rust_rt_t *rt;

  printf("control is in rust runtime library\n");  
  rt = malloc(sizeof(rust_rt_t));
  if (!rt) {
	printf("failed to allocate runtime structure, exiting.\n");
	exit(1);
  }
  printf("calling thunk %ld...\n", (long) thunk);
  /* thunk(rt); */
  printf("returned from thunk, exiting.\n");
  free(rt);
  return 37;
}
