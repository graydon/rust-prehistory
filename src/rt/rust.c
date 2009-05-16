#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <inttypes.h>

#include "rust.h"
#include "rand.h"

/*
  static void really_free(void*x) { free(x); }
  #define free(x) do { printf("** FREE(%" PRIxPTR ")\n", (uintptr_t)x); really_free(x); } while(0)
*/

typedef enum {
  rust_type_any = 0,
  rust_type_nil = 1,
  rust_type_bool = 2,
  rust_type_int = 3,

  rust_type_char = 4,
  rust_type_str = 5,

  rust_type_tup = 6,
  rust_type_vec = 7,
  rust_type_rec = 8,

  rust_type_tag = 9,
  rust_type_iso = 10,
  rust_type_idx = 11,

  rust_type_fn = 12,
  rust_type_chan = 13,
  rust_type_port = 14,

  rust_type_mod = 15,
  rust_type_prog = 16,

  rust_type_opaque = 17,

  rust_type_constrained = 18,
  rust_type_lim = 19,


  rust_type_u8 = 20,
  rust_type_s8 = 21,
  rust_type_u16 = 22,
  rust_type_s16 = 23,
  rust_type_u32 = 24,
  rust_type_s32 = 25,
  rust_type_u64 = 26,
  rust_type_s64 = 27,

  rust_type_b64 = 28,
  rust_type_b128 = 29

} rust_type_tag_t;

/*
 * We have a variety of pointer-tagging schemes.
 *
 * For interior slots of the 'int' type, we use a 1-bit tag to switch between fixnum and boxed
 * bignum.
 *
 * Exterior subword-sized slots are synonymous with interior subword-sized slots; there is no
 * difference. Subsequently, transplanting a subword-sized datum into an exterior slot is always
 * just a copy. Write aliases can be formed on subword-sized slots; they are just the address of
 * the slot itself, aligned or not.
 *
 * Exterior word-or-greater slots are stored as pointers. Size implies alignment, so we have free
 * tag bits. We use one bit to differentiate crate-offset pseudo-pointers from real heap pointers.
 *
 * Slots of 'any' type need to denote both a type and a value. They do this by stealing 3 bits for
 * tag and assigning thus (on 32-bit platforms):
 *
 *   - 0b000 == mini-fixnum int
 *   - 0b001 == boxed int
 *   - 0b010 == crate-offset pseudo pointer to (type,val) pair
 *   - 0b011 == pure pointer to (type,val) pair
 *   - 0b100 == nil
 *   - 0b101 == bool
 *   - 0b110 == char
 *   - 0b111 == boxed str (strs are always 3 words at least: refs, len, buf)
 *
 * On 64-bit platforms, we have 4 bits to play with since 2 words is 128 bits. So we extend the
 * "stored inline" variants to cover:
 *
 *   - 0b1000 == u8
 *   - 0b1001 == s8
 *   - 0b1010 == u16
 *   - 0b1011 == s16
 *   - 0b1100 == u32
 *   - 0b1101 == s32
 *   - 0b1110 == f64
 *   - 0b1111 == ?? reserved
 *
 */

typedef struct rust_type {
  uintptr_t refs;
  rust_type_tag_t tag;

} rust_type_t;

/* Proc stack segments. Heap allocated and chained together. */

typedef struct rust_stk_seg {
  struct rust_stk_seg *prev;
  struct rust_stk_seg *next;
  size_t size;
  size_t live;
  uint8_t data[];
} rust_stk_seg_t;


typedef enum {
  /* NB: it's important that 'running' be value 0, as it
   * lets us get away with using OR rather than MOV to
   * signal anything-not-running. x86 optimization. */
  rust_proc_state_running    = 0,
  rust_proc_state_calling_c  = 1,
  rust_proc_state_exiting    = 2,
  rust_proc_state_blocked_reading  = 3,
  rust_proc_state_blocked_writing  = 4
} rust_proc_state_t;

typedef enum {
  rust_upcall_log_uint32     = 0,
  rust_upcall_log_str        = 1,
  rust_upcall_spawn          = 2,
  rust_upcall_check_expr     = 3,
  rust_upcall_malloc         = 4,
  rust_upcall_free           = 5,
  rust_upcall_new_port       = 6,
  rust_upcall_del_port       = 7,
  rust_upcall_new_chan       = 8,
  rust_upcall_del_chan       = 9,
  rust_upcall_send           = 10,
  rust_upcall_recv           = 11,
  rust_upcall_sched          = 12
} rust_upcall_t;

#define RUST_PROC_MAX_UPCALL_ARGS   8

struct rust_ptr_vec {
  size_t alloc;
  size_t init;
  void **data;
};

struct rust_rt {
  uintptr_t sp;          /* Saved sp from the C runtime. */
  rust_ptr_vec_t running_procs;
  rust_ptr_vec_t blocked_procs;
  randctx rctx;
};

struct rust_prog {
  void CDECL (*init_code)(void*, rust_proc_t*);
  void CDECL (*main_code)(void*, rust_proc_t*);
  void CDECL (*fini_code)(void*, rust_proc_t*);
};

struct rust_proc {

  rust_rt_t *rt;
  rust_stk_seg_t *stk;
  rust_prog_t *prog;
  uintptr_t sp;           /* saved sp when not running.                     */
  uintptr_t state;
  size_t idx;
  size_t refcnt;

  /* Parameter space for upcalls. */
  /* FIXME: could probably get away with packing upcall code and state
   * into 1 byte each. And having fewer max upcall args. */
  uintptr_t upcall_code;
  uintptr_t upcall_args[RUST_PROC_MAX_UPCALL_ARGS];

  /* Proc accounting. */
  uintptr_t mem_budget;   /* N bytes ownable by this proc.                  */
  uintptr_t curr_mem;     /* N bytes currently owned.                       */
  uintptr_t tick_budget;  /* N ticks in proc lifetime. 0 = unlimited.       */
  uintptr_t curr_ticks;   /* N ticks currently consumed.                    */

  uint8_t data[];         /* Official-style C99 "flexible array" element.    */

};

struct rust_port {
  size_t refcnt;
  uintptr_t blocked_reading;
  rust_ptr_vec_t writers;
};

struct rust_chan {
  rust_port_t *port;
};


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

static void*
xrealloc(void *p, size_t sz)
{
  p = realloc(p, sz);
  if (!p) {
    printf("rt: reallocation of 0x%" PRIxPTR
           " bytes failed, exiting\n", sz);
    exit(123);
  }
  return p;
}

/* Utility type: pointer-vector. */

#define INIT_PTR_VEC_SZ 8

static void
init_ptr_vec(rust_ptr_vec_t *v)
{
  v->alloc = INIT_PTR_VEC_SZ;
  v->init = 0;
  v->data = xalloc(v->alloc);
  assert(v->data);
  printf("rt: init ptr vec %" PRIxPTR ", data=%" PRIxPTR "\n",
         (uintptr_t)v, (uintptr_t)v->data);
}

static void
fini_ptr_vec(rust_ptr_vec_t *v)
{
  assert(v);
  assert(v->data);
  printf("rt: fini ptr vec %" PRIxPTR ", data=%" PRIxPTR "\n",
         (uintptr_t)v, (uintptr_t)v->data);
  assert(v->init == 0);
  free(v->data);
}

static void
ptr_vec_push(rust_ptr_vec_t *v, void *p)
{
  assert(v);
  assert(v->data);
  if (v->init == v->alloc) {
    v->alloc *= 2;
    v->data = xrealloc(v->data, v->alloc);
  }
  v->data[v->init++] = p;
}

static void
ptr_vec_trim(rust_ptr_vec_t *v, size_t init)
{
  assert(v);
  assert(v->data);
  if (init <= (v->alloc / 4) &&
      (v->alloc / 2) >= INIT_PTR_VEC_SZ) {
    v->alloc /= 2;
    assert(v->alloc >= v->init);
    v->data = xrealloc(v->data, v->alloc);
    assert(v->data);
  }
}

static void
ptr_vec_swapdel(rust_ptr_vec_t *v, size_t i)
{
  /* Swap the endpoint into i and decr init. */
  assert(v);
  assert(v->data);
  assert(v->init > 0);
  assert(i < v->init);
  v->init--;
  if (v->init > 0)
    v->data[i] = v->data[v->init];
}


/*
static rust_ptr_vec_t*
new_ptr_vec()
{
  rust_ptr_vec_t *v = xalloc(sizeof(rust_ptr_vec_t));
  init_ptr_vec(v);
  return v;
}
static void
del_ptr_vec(rust_ptr_vec_t *v)
{
  fini_ptr_vec(v);
  free(v);
}
static void
ptr_vec_del(rust_ptr_vec_t *v, size_t i)
{
  assert(v->init > 0);
  assert(i < v->init);
  v->init--;
  if (v->init > 0)
    memmove(v->data + i, v->data + i + 1, v->init - i);
  if (v->init == (v->alloc / 2) && (v->alloc / 2) >= 4) {
    v->alloc /= 2;
    v->data = xrealloc(v->data, v->alloc);
  }
}
*/

/* Stacks */

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

/* Processes */

/* FIXME: ifdef by platform. */
size_t const rust_n_callee_saves = 4;

static rust_proc_t*
rust_new_proc(rust_rt_t *rt, rust_prog_t *prog)
{
  /* FIXME: need to actually convey the proc internal-slots size to here. */
  rust_proc_t *proc = xcalloc(sizeof(rust_proc_t) + 1024);
  logptr("new proc", (uintptr_t)proc);
  logptr("from prog", (uintptr_t)prog);
  logptr("init:", (uintptr_t)prog->init_code);
  logptr("main:", (uintptr_t)prog->main_code);
  logptr("fini:", (uintptr_t)prog->fini_code);
  proc->prog = prog;
  proc->stk = rust_new_stk();

  /*
     Set sp to last uintptr_t-sized cell of segment
     then align down to 16 boundary, to be safe-ish?
  */
  size_t tos = rust_init_stk_bytes-sizeof(uintptr_t);
  proc->sp = (uintptr_t) &proc->stk->data[tos];
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
  free(proc);
}

static rust_proc_t*
rust_spawn_proc(rust_rt_t *rt,
                rust_prog_t *prog)
{
  return rust_new_proc(rt, prog);
}

static void
rust_sched_proc(rust_rt_t *rt,
                rust_proc_t *proc)
{
  proc->idx = rt->running_procs.init;
  ptr_vec_push(&rt->running_procs, proc);
}

static rust_ptr_vec_t*
rust_get_proc_vec(rust_proc_t *proc)
{
  rust_rt_t *rt = proc->rt;
  switch (proc->state) {
  case rust_proc_state_running:
  case rust_proc_state_calling_c:
  case rust_proc_state_exiting:
    return &rt->running_procs;

  case rust_proc_state_blocked_reading:
  case rust_proc_state_blocked_writing:
    return &rt->blocked_procs;
  }
  assert(0);
  return NULL;
}

static size_t
n_live_procs(rust_rt_t *rt)
{
  return rt->running_procs.init + rt->blocked_procs.init;
}

static void
rust_exit_proc(rust_proc_t *proc)
{
  assert(proc);
  assert(proc->rt);
  rust_rt_t *rt = proc->rt;
  assert(n_live_procs(rt) > 0);
  rust_ptr_vec_t *v = rust_get_proc_vec(proc);
  assert(v);
  ptr_vec_swapdel(v, proc->idx);
  if (v->init > 0)
    ((rust_proc_t*)v->data[proc->idx])->idx = proc->idx;
  rust_del_proc(proc);
  ptr_vec_trim(v, n_live_procs(rt));
}

static rust_proc_t*
rust_sched(rust_rt_t *rt)
{
  assert(rt);
  assert(n_live_procs(rt) > 0);
  if (rt->running_procs.init > 0) {
    size_t i = rand(&rt->rctx);
    i %= rt->running_procs.init;
    return rt->running_procs.data[i];
  }
  printf("rt: no schedulable processes.\n");
  exit(1);
}

/* Runtime */

static rust_rt_t*
rust_new_rt()
{
  rust_rt_t *rt = xcalloc(sizeof(rust_rt_t));
  logptr("new rt", (uintptr_t)rt);
  init_ptr_vec(&rt->running_procs);
  init_ptr_vec(&rt->blocked_procs);
  randinit(&rt->rctx);
  return rt;
}

static void
rust_del_all_procs(rust_ptr_vec_t *v) {
  assert(v);
  while (v->init) {
    rust_del_proc((rust_proc_t*) v->data[v->init--]);
  }
}

static void
rust_del_rt(rust_rt_t *rt)
{
  rust_del_all_procs(&rt->running_procs);
  rust_del_all_procs(&rt->blocked_procs);
  fini_ptr_vec(&rt->running_procs);
  fini_ptr_vec(&rt->blocked_procs);
  free(rt);
}

/* Upcalls */

static void
upcall_log_uint32_t(uint32_t i)
{
  printf("rt: log_uint32(0x%" PRIx32 ")\n", i);
}

static void
upcall_log_str(char *c)
{
  printf("rt: log_str(\"%s\")\n", c);
}

static rust_port_t*
upcall_new_port(rust_proc_t *proc)
{
  rust_port_t *port = xcalloc(sizeof(rust_port_t));
  logptr("new port", (uintptr_t)port);
  return port;
}

static void
upcall_del_port(rust_port_t *port)
{
  logptr("del port", (uintptr_t)port);
  assert(port->refcnt == 0);
  fini_ptr_vec(&port->writers);
  free(port);
}

static void
upcall_check_expr(rust_proc_t *proc, uint32_t i)
{
  if (!i) {
    /* FIXME: throw, don't just exit. */
    printf("\nrt: *** CHECK FAILED ***\n\n");
    proc->state = (uintptr_t)rust_proc_state_exiting;
  }
}

static uintptr_t
upcall_malloc(rust_proc_t *proc, size_t nbytes)
{
  void *p = xalloc(nbytes);
  printf("rt: malloc(%u) = 0x%" PRIxPTR "\n", nbytes, (uintptr_t)p);
  return (uintptr_t) p;
}

static void
upcall_free(rust_proc_t *proc, void* ptr)
{
  printf("rt: free(0x%" PRIxPTR ")\n", (uintptr_t)ptr);
  free(ptr);
}

static void
rust_handle_upcall(rust_proc_t *proc)
{
  uintptr_t *args = &proc->upcall_args[0];

  printf("rt: calling fn #%d\n", proc->upcall_code);
  switch ((rust_upcall_t)proc->upcall_code) {
  case rust_upcall_log_uint32:
    upcall_log_uint32_t(args[0]);
    break;
  case rust_upcall_log_str:
    upcall_log_str((char*)args[0]);
    break;
  case rust_upcall_spawn:
    *((rust_proc_t**)args[0]) = rust_spawn_proc(proc->rt, (rust_prog_t*)args[1]);
    break;
  case rust_upcall_sched:
    logptr("scheduling new proc", args[0]);
    rust_sched_proc(proc->rt, (rust_proc_t*)args[0]);
    break;
  case rust_upcall_check_expr:
    upcall_check_expr(proc, args[0]);
    break;
  case rust_upcall_malloc:
    *((uintptr_t*)args[0]) = upcall_malloc(proc, (size_t)args[1]);
    break;
  case rust_upcall_free:
    upcall_free(proc, (void*)args[0]);
    break;
  case rust_upcall_new_port:
    *((rust_port_t**)args[0]) = upcall_new_port(proc);
    break;
  case rust_upcall_del_port:
    upcall_del_port((rust_port_t*)args[0]);
    break;
  case rust_upcall_new_chan:
    printf("rt: new chan\n");
    break;
  case rust_upcall_del_chan:
    printf("rt: del chan\n");
    break;
  case rust_upcall_send:
    logptr("send from", (uintptr_t)proc);
    break;
  case rust_upcall_recv:
    logptr("recv to", (uintptr_t)proc);
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
  rust_sched_proc(rt, rust_spawn_proc(rt, prog));
  proc = rust_sched(rt);

  logptr("root proc is", (uintptr_t)proc);
  logptr("proc->sp", (uintptr_t)proc->sp);
  logptr("c_to_proc_glue", (uintptr_t)c_to_proc_glue);

  while (1) {
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
      rust_exit_proc(proc);
      break;
    case rust_proc_state_blocked_reading:
    case rust_proc_state_blocked_writing:
      assert(0);
      break;
    }
    if (n_live_procs(rt) > 0)
      proc = rust_sched(rt);
    else
      break;
  }

  printf("rt: finished main loop.\n");
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
