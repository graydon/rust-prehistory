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

static void
rust_log_uint32_t(uint32_t i)
{
  printf("rt: log_uint32(0x%" PRIx32 ")\n", i);
}

static void
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

  /*
     Set sp to last uintptr_t-sized cell of segment
     then align down to 16 boundary, to be safe-ish?
  */
  size_t tos = rust_init_stk_bytes-sizeof(uintptr_t);
  proc->sp = (uintptr_t) &(proc->stk->data[tos]);
  proc->sp &= ~0xf;

  /* "initial args" to the main frame:
   *
   *      *sp+N+24   = proc ptr
   *      *sp+N+16   = NULL = fake outptr (spacing)
   *      *sp+N+8    = NULL = fake retpc (spacing)
   *      *sp+N+4    = "retpc" to return to (activation)
   *      *sp+N      = NULL = 0th callee-save
   *      ...
   *      *sp        = NULL = Nth callee-save
   *
   * This is slightly confusing since it looks like we have two copies
   * of retpc; that's intentional. The notion is that when we first
   * activate this frame, we'll be entering via the c-to-proc glue,
   * and that will restore the fake callee-saves here and then
   * return-to the "activation" pc.  That PC will be the first insn of a
   * rust prog that assumes for -- simplicity sake it -- has a
   * same-as-always-laid-out rust frame under it. In particular, one
   * with a retpc. Even though said retpc is bogus -- just spacing --
   * we place it and a fake outptr so that the frame we return to is
   * the right shape.
   */
  uintptr_t *sp = (uintptr_t*) proc->sp;
  proc->sp -= (3 + rust_n_callee_saves) * sizeof(uintptr_t);
  *sp-- = (uintptr_t) proc;
  *sp-- = (uintptr_t) 0;
  *sp-- = (uintptr_t) (uintptr_t) prog->main_code;
  *sp-- = (uintptr_t) (uintptr_t) prog->main_code;
  for (size_t j = 0; j < rust_n_callee_saves; ++j) {
    *sp-- = 0;
  }

  proc->rt = rt;
  proc->state = (uintptr_t)rust_proc_state_running;
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

/* FIXME: use user-controlled buffer size in the future. */
static size_t const rust_port_bufsz = 64;

static rust_port_t*
rust_new_port(rust_proc_t *proc)
{
  size_t sz = sizeof(rust_port_t) + rust_port_bufsz * sizeof(uintptr_t);
  rust_port_t *port = xalloc(sz);
  logptr("new port", (uintptr_t)port);
  memset(port, 0, sizeof(rust_port_t));
  port->buf_sz = rust_port_bufsz;
  port->read = &(port->buf[0]);
  port->owner = proc;
  if (proc->ports) {
    port->next = proc->ports;
    port->prev = port->next->prev;
    port->prev->next = port;
    port->next->prev = port;
  } else {
    proc->ports = port;
    port->next = port->prev = port;
  }
  return port;
}

static void
rust_del_port(rust_port_t *port)
{
  logptr("del port", (uintptr_t)port);
  assert(port->refcnt == 0);
  if (port->next != port) {
    port->next->prev = port->prev;
    port->prev->next = port->next;
  }
  port->next = port->prev = NULL;
  if (port->owner->ports == port)
    port->owner->ports = port->next;
  free(port);
}

static void
rust_check_expr(rust_proc_t *proc, uint32_t i)
{
  if (!i) {
    /* FIXME: throw, don't just exit. */
    printf("\nrt: *** CHECK FAILED ***\n\n");
    proc->state = (uintptr_t)rust_proc_state_exiting;
  }
}

static uintptr_t
rust_malloc(rust_proc_t *proc, size_t nbytes)
{
  void *p = xalloc(nbytes);
  printf("rt: malloc(%u) = 0x%" PRIxPTR "\n", nbytes, (uintptr_t)p);
  fflush(stdout);
  return (uintptr_t) p;
}

static void
rust_free(rust_proc_t *proc, void* ptr)
{
  printf("rt: free(0x%" PRIxPTR ")\n", (uintptr_t)ptr);
  free(ptr);
}


void
rust_handle_upcall(rust_proc_t *proc)
{
  uintptr_t *args = &(proc->upcall_args[0]);

  /* printf("rt: calling fn #%d\n", proc->upcall_code); */
  switch ((rust_upcall_t)proc->upcall_code) {
  case rust_upcall_log_uint32:
    rust_log_uint32_t(args[0]);
    break;
  case rust_upcall_log_str:
    rust_log_str((char*)args[0]);
    break;
  case rust_upcall_spawn:
    rust_spawn_proc(proc->rt, (rust_prog_t*)args[0]);
    break;
  case rust_upcall_check_expr:
    rust_check_expr(proc, args[0]);
    break;
  case rust_upcall_malloc:
    *((uintptr_t*)args[0]) = rust_malloc(proc, (size_t)args[1]);
    break;
  case rust_upcall_free:
    rust_free(proc, (void*)args[0]);
    break;
  case rust_upcall_new_port:
    *((rust_port_t**)args[0]) = rust_new_port(proc);
    break;
  case rust_upcall_del_port:
    rust_del_port((rust_port_t*)args[0]);
    break;
      /*;
  case 3:
    rust_kill_proc(proc->rt, (rust_proc_t*)args[0]);
    break;
  case 6:
    rust_memmove(proc, (void*)args[0], (const void*)args[1], (size_t)args[2]);
    break;
  case 7:
  **retslot_p = rust_memcmp((const void*)args[0], (const void*)args[1], (size_t)args[2]);
    break;
      */
  }
  /* Zero the immediates code slot out so the caller doesn't have to
   * use MOV to update it. x86-ism but harmless on non-x86 platforms that
   * want to use their own MOVs. */
  proc->upcall_code = (rust_upcall_t)0;
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
  logptr("proc->sp", (uintptr_t)proc->sp);
  logptr("c_to_proc_glue", (uintptr_t)c_to_proc_glue);

  while(1) {
    /* printf("rt: entering proc 0x%" PRIxPTR "\n", (uintptr_t)proc); */
    proc->state = (uintptr_t)rust_proc_state_running;
    c_to_proc_glue(proc);
    /* printf("rt: returned from proc 0x%" PRIxPTR " in state %d.\n",
       (uintptr_t)proc, proc->state); */
    switch ((rust_proc_state_t) proc->state) {
    case rust_proc_state_running:
      break;
    case rust_proc_state_calling_c:
      rust_handle_upcall(proc);
      proc->state = rust_proc_state_running;
      break;
    case rust_proc_state_exiting:
      logptr("proc exiting", (uintptr_t)proc);
      rust_exit_curr_proc(rt);
      break;
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
