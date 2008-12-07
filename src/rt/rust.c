#include "rust.h"
#include <stdio.h>

static void __cdecl
rust_log_uint32_t(uint32_t i)
{
  printf("rt: %d\n", i);
}

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
  rt->log_uint32_t = rust_log_uint32_t;
  printf("calling thunk 0x%lx...\n", (long) thunk);
  /* thunk(rt); */
  printf("returned from thunk, exiting.\n");
  free(rt);
  return 37;
}

/* 
 * Local Variables:
 * fill-column: 70; 
 * indent-tabs-mode: nil
 * compile-command: "make -k -C .. 2>&1 | sed -e 's/\\/x\\//x:\\//g'"; 
 * End:
 */
