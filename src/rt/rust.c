#include "rust.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <inttypes.h>

static void 
logptr(char const *msg, intptr_t ptrval)
{
  printf("rt: %s 0x%" PRIxPTR "\n", msg, ptrval);
}

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

static void* 
xcalloc(size_t sz)
{
  void *p = xalloc(sz);
  memset(p, 0, sz);
  return p;
}


static void CDECL
rust_log_uint32_t(rust_proc_t *proc, uint32_t i)
{
  printf("rt: log_uint32(0x%" PRIxPTR ", 0x%" PRIx32 ")\n", (intptr_t)proc, i);
}

static void CDECL
rust_log_str(rust_proc_t *proc, char *c)
{
  printf("rt: log_str(0x%" PRIxPTR ", \"%s\")\n", (intptr_t)proc, c);
}

static rust_rt_t*
rust_new_rt()
{
  rust_rt_t *rt = xcalloc(sizeof(rust_rt_t));
  logptr("new rt", (intptr_t)rt);
  rt->log_uint32_t = rust_log_uint32_t;
  rt->log_str = rust_log_str;
  return rt;
}

static void
rust_del_rt(rust_rt_t *rt)
{
  free(rt);
}

/* Get around to using linked-lists of size-doubling stacks, eventually. */
static size_t const rust_init_stk_bytes = 65536;

static rust_stk_seg_t*
rust_new_stk()
{
  size_t sz = sizeof(rust_stk_seg_t) + rust_init_stk_bytes;
  rust_stk_seg_t *stk = xalloc(sz);
  logptr("new stk", (intptr_t)stk);
  memset(stk, 0, sizeof(rust_stk_seg_t));
  stk->size = sz;
  return stk;
}

static void
rust_del_stk(rust_stk_seg_t *stk)
{
  rust_stk_seg_t *nxt;
  do {
    nxt = stk->next; 
    free(stk);
    stk = nxt;
  } while (stk);
}

static rust_proc_t*
rust_new_proc(rust_rt_t *rt, rust_prog_t *prog)
{
  rust_proc_t *proc = xcalloc(sizeof(rust_proc_t));
  logptr("new proc", (intptr_t)proc);
  proc->prog = prog;
  proc->stk = rust_new_stk();
  proc->rt = rt;
  return proc;
}

static void
rust_del_proc(rust_proc_t *proc)
{
  assert(proc->refcnt == 0);
  rust_del_stk(proc->stk);
  free(proc);
}

int CDECL
rust_start(rust_prog_t *prog, void CDECL (*c_to_proc_glue)(void*, rust_proc_t*))
{
  rust_rt_t *rt;
  rust_proc_t *proc;

  printf("rt: control is in rust runtime library\n");  
  logptr("prog->init_code", (intptr_t)prog->init_code);
  logptr("prog->main_code", (intptr_t)prog->main_code);
  logptr("prog->fini_code", (intptr_t)prog->fini_code);

  rt = rust_new_rt();
  proc = rust_new_proc(rt, prog);
  
  //logptr("calling main_code with proc arg", (intptr_t)proc);
  //prog->main_code(0, proc);
  logptr("root proc is ", (intptr_t)proc);
  proc->regs.pc = (uintptr_t) prog->main_code;
  proc->regs.sp = (uintptr_t) &(proc->stk->data[rust_init_stk_bytes]);
  logptr("proc->regs.pc ", (intptr_t)proc->regs.pc);
  logptr("proc->regs.sp ", (intptr_t)proc->regs.sp);
  logptr("calling c_to_proc_glue ", (intptr_t)c_to_proc_glue);
  c_to_proc_glue(0, proc);
  printf("rt: returned from main, exiting.\n");
  rust_del_proc(proc);
  rust_del_rt(rt);
  return 37;
}

/* 
 * Local Variables:
 * fill-column: 70; 
 * indent-tabs-mode: nil
 * compile-command: "make -k -C .. 2>&1 | sed -e 's/\\/x\\//x:\\//g'"; 
 * End:
 */
