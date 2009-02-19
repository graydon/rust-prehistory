#include "rust.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <inttypes.h>

size_t const rust_n_callee_saves = 4;
size_t const rust_n_procs = 1024;


static void
logptr(char const *msg, uintptr_t ptrval)
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
rust_log_uint32_t(uint32_t i)
{
  printf("rt: log_uint32(0x%" PRIx32 ")\n", i);
}

static void CDECL
rust_log_str(char *c)
{
  printf("rt: log_str(\"%s\")\n", c);
}

static rust_rt_t*
rust_new_rt()
{
  rust_rt_t *rt = xcalloc(sizeof(rust_rt_t));
  logptr("new rt", (uintptr_t)rt);
  rt->procs = xcalloc(sizeof(rust_proc_t*) * rust_n_procs);
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
  logptr("new stk", (uintptr_t)stk);
  memset(stk, 0, sizeof(rust_stk_seg_t));
  stk->size = sz;
  return stk;
}

static void
rust_del_stk(rust_stk_seg_t *stk)
{
  rust_stk_seg_t *nxt = 0;
  do {
    nxt = stk->next;
    logptr("freeing stk segment", (uintptr_t)stk);
    free(stk);
    stk = nxt;
  } while (stk);
  printf("rt: freed stacks.\n");
}

static rust_proc_t*
rust_new_proc(rust_rt_t *rt, rust_prog_t *prog)
{
  rust_proc_t *proc = xcalloc(sizeof(rust_proc_t));
  logptr("new proc", (uintptr_t)proc);
  proc->prog = prog;
  proc->stk = rust_new_stk();
  proc->regs.pc = (uintptr_t) prog->main_code;

  /*
     Set sp to last uintptr_t-sized cell of segment
     then align down to 16 boundary, to be safe-ish?
  */
  size_t tos = rust_init_stk_bytes-sizeof(uintptr_t);
  proc->regs.sp = (uintptr_t) &(proc->stk->data[tos]);
  proc->regs.sp &= ~0xf;

  /* "initial args" to the main frame:
   *
   *      *sp+N+16   = proc ptr
   *      *sp+N+8    = NULL = fake outptr
   *      *sp+N+4    = NULL = fake retpc
   *      *sp+N      = NULL = 0th callee-save
   *      ...
   *      *sp        = NULL = Nth callee-save
   */
  uintptr_t *sp = (uintptr_t*) proc->regs.sp;
  proc->regs.sp -= (2 + rust_n_callee_saves) * sizeof(uintptr_t);
  *sp-- = (uintptr_t) proc;
  *sp-- = (uintptr_t) 0;
  *sp-- = (uintptr_t) 0;
  for (size_t j = 0; j < rust_n_callee_saves; ++j) {
    *sp-- = 0;
  }

  proc->rt = rt;
  proc->state = RUST_PROC_STATE_RUNNING;
  return proc;
}

static void
rust_del_proc(rust_proc_t *proc)
{
  logptr("del proc", (uintptr_t)proc);
  assert(proc->refcnt == 0);
  rust_del_stk(proc->stk);
}

static void
rust_spawn_proc(rust_rt_t *rt,
                rust_prog_t *prog)
{
  /* Want there to always be a null on the end. */
  assert(rt->live_procs + 2 < rust_n_procs);
  assert(! rt->procs[rt->live_procs]);
  rt->procs[rt->live_procs] = rust_new_proc(rt, prog);
  rt->live_procs++;
}

static void
rust_exit_curr_proc(rust_rt_t *rt)
{
  assert(rt->procs[rt->curr_proc]);
  assert(rt->live_procs > 0);
  rust_del_proc(rt->procs[rt->curr_proc]);
  if (rt->curr_proc != rt->live_procs-1) {
    printf("rt: swapping proc %d=%" PRIxPTR " <- %d=%" PRIxPTR "\n",
           rt->curr_proc, (uintptr_t) rt->procs[rt->curr_proc],
           rt->live_procs-1, (uintptr_t) rt->procs[rt->live_procs-1]);
    rt->procs[rt->curr_proc] = rt->procs[rt->live_procs-1];
    rt->procs[rt->live_procs-1] = NULL;
  } else {
    printf("rt: clearing curr_proc %d\n", rt->curr_proc);
    rt->procs[rt->curr_proc] = NULL;
  }
  assert(!rt->procs[rt->live_procs-1]);
  rt->live_procs--;
  printf("rt: rt->live_procs = %d\n", rt->live_procs);
}

static rust_proc_t*
rust_sched(rust_rt_t *rt)
{
  rt->curr_proc++;
  rt->curr_proc %= rt->live_procs;
  assert(rt->procs[rt->curr_proc]);
  // logptr("rust_sched chose proc", (uintptr_t)rt->procs[rt->curr_proc]);
  return rt->procs[rt->curr_proc];
}

int CDECL
rust_start(rust_prog_t *prog,
           void CDECL (*c_to_proc_glue)(rust_proc_t*))
{
  rust_rt_t *rt;
  rust_proc_t *proc;

  printf("rt: control is in rust runtime library\n");
  logptr("prog->init_code", (uintptr_t)prog->init_code);
  logptr("prog->main_code", (uintptr_t)prog->main_code);
  logptr("prog->fini_code", (uintptr_t)prog->fini_code);

  rt = rust_new_rt();
  rust_spawn_proc(rt, prog);
  proc = rust_sched(rt);

  logptr("root proc is", (uintptr_t)proc);
  logptr("proc->regs.pc", (uintptr_t)proc->regs.pc);
  logptr("proc->regs.sp", (uintptr_t)proc->regs.sp);
  logptr("c_to_proc_glue", (uintptr_t)c_to_proc_glue);

  while(1) {
    // printf("rt: calling c_proc_glue.\n");
    proc->state = RUST_PROC_STATE_RUNNING;
    c_to_proc_glue(proc);
    // printf("rt: returned from proc in state %d.\n", proc->state);
    if (proc->state == RUST_PROC_STATE_CALLING_C) {
      uintptr_t *sp = (uintptr_t*) proc->regs.sp;
      sp += rust_n_callee_saves;
      sp++;
      // printf("rt: calling fn #%d\n", *sp);
      switch (*sp) {
      case 0:
        rust_log_uint32_t(sp[1]);
        break;
      case 1:
        rust_log_str((char*)sp[1]);
        break;
      case 2:
        rust_spawn_proc(rt, (rust_prog_t*)sp[1]);
        break;
      }
    }
    else if (proc->state == RUST_PROC_STATE_EXITING) {
      logptr("proc exiting", (uintptr_t)proc);
      rust_exit_curr_proc(rt);
    }
    if (rt->live_procs > 0)
      proc = rust_sched(rt);
    else
      break;
  }

  rust_del_rt(rt);
  printf("rt: freed runtime.\n");
  return 37;
}

/*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * compile-command: "make -k -C .. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 */
