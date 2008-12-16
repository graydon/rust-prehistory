#include "rust.h"
#include <stdio.h>
#include <inttypes.h>

static void* 
xalloc(size_t sz)
{
  void *p = malloc(sz);
  if (!p) {
    printf("rt: allocation of 0x%" PRIxPTR 
           " bytes failed, exiting\n", sz);
    exit(123);
  }
  return p;
}

static void __cdecl
rust_log_uint32_t(uint32_t i)
{
  printf("rt: log_uint32(0x%" PRIx32 ")\n", i);
}

static void __cdecl
rust_log_str(char* c)
{
  printf("rt: log_str(\"%s\")\n", c);
}

int __cdecl 
rust_start(rust_prog_t *prog)
{
  rust_proc_t *proc;

  printf("rt: control is in rust runtime library\n");  
  printf("rt: program: "
         "\n\t[init 0x%" PRIxPTR "]"
         "\n\t[main 0x%" PRIxPTR "]"
         "\n\t[fini 0x%" PRIxPTR "]\n", 
         (intptr_t)prog->init_code, 
         (intptr_t)prog->main_code, 
         (intptr_t)prog->fini_code);
  proc = xalloc(sizeof(rust_proc_t));
  proc->prog = prog;
  proc->rt = xalloc(sizeof(rust_rt_t));
  proc->rt->log_uint32_t = rust_log_uint32_t;
  proc->rt->log_str = rust_log_str;
  
  printf("rt: calling main 0x%" PRIxPTR " with arg 0x%" PRIxPTR "...\n", 
         (intptr_t)prog->main_code,
         (intptr_t)proc);
  prog->main_code(0, proc);
  printf("rt: returned from main, exiting.\n");
  free(proc->rt);
  free(proc);
  return 37;
}

/* 
 * Local Variables:
 * fill-column: 70; 
 * indent-tabs-mode: nil
 * compile-command: "make -k -C .. 2>&1 | sed -e 's/\\/x\\//x:\\//g'"; 
 * End:
 */
