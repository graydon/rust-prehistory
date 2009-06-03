#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <inttypes.h>

#include "rust.h"
#include "rand.h"
#include "uthash-1.6/src/uthash.h"
#include "valgrind.h"

#ifdef __WIN32__
#include <windows.h>
#include <wincrypt.h>

static void
win32_require(LPTSTR fn, BOOL ok) {
  if (!ok) {
    LPTSTR buf;
    DWORD err = GetLastError();
    FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER |
                  FORMAT_MESSAGE_FROM_SYSTEM |
                  FORMAT_MESSAGE_IGNORE_INSERTS,
                  NULL, err,
                  MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                  (LPTSTR) &buf, 0, NULL );
    printf("rt: %s failed with error %ld: %s\n", fn, err, buf);
    LocalFree((HLOCAL)buf);
    exit(1);
  }
}
#endif

/*
  static void really_free(void*x) { free(x); }
  #define free(x) do { printf("** FREE(%" PRIxPTR ")\n", (uintptr_t)x); really_free(x); } while(0)
*/

struct ptr_vec;
typedef struct ptr_vec ptr_vec_t;

struct circ_buf;
typedef struct circ_buf circ_buf_t;

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

typedef struct stk_seg {
  struct stk_seg *prev;
  struct stk_seg *next;
  unsigned int valgrind_id;
  size_t size;
  size_t live;
  uint8_t data[];
} stk_seg_t;


typedef enum {
  /* NB: it's important that 'running' be value 0, as it
   * lets us get away with using OR rather than MOV to
   * signal anything-not-running. x86 optimization. */
  proc_state_running    = 0,
  proc_state_calling_c  = 1,
  proc_state_exiting    = 2,
  proc_state_blocked_reading  = 3,
  proc_state_blocked_writing  = 4
} proc_state_t;

typedef enum {
  upcall_code_log_uint32     = 0,
  upcall_code_log_str        = 1,
  upcall_code_spawn          = 2,
  upcall_code_kill           = 3,
  upcall_code_check_expr     = 4,
  upcall_code_malloc         = 5,
  upcall_code_free           = 6,
  upcall_code_new_port       = 7,
  upcall_code_del_port       = 8,
  upcall_code_send           = 9,
  upcall_code_recv           = 10,
  upcall_code_sched          = 11
} upcall_t;

#define PROC_MAX_UPCALL_ARGS   8

struct ptr_vec {
  size_t alloc;
  size_t init;
  void **data;
};

struct circ_buf {
  size_t alloc;
  size_t unit_sz;
  size_t next;
  size_t unread;
  uint8_t *data;
};

struct rust_rt {
  uintptr_t sp;          /* Saved sp from the C runtime. */
  ptr_vec_t running_procs;
  ptr_vec_t blocked_procs;
  randctx rctx;
};

struct rust_prog {
  void CDECL (*init_code)(void*, rust_proc_t*);
  void CDECL (*main_code)(void*, rust_proc_t*);
  void CDECL (*fini_code)(void*, rust_proc_t*);
};

struct rust_proc {

  rust_rt_t *rt;
  stk_seg_t *stk;
  rust_prog_t *prog;
  uintptr_t sp;           /* saved sp when not running.                     */
  uintptr_t state;
  size_t idx;
  size_t refcnt;
  rust_chan_t *chans;

  /* Parameter space for upcalls. */
  /* FIXME: could probably get away with packing upcall code and state
   * into 1 byte each. And having fewer max upcall args. */
  uintptr_t upcall_code;
  uintptr_t upcall_args[PROC_MAX_UPCALL_ARGS];

  /* Proc accounting. */
  uintptr_t mem_budget;   /* N bytes ownable by this proc.                  */
  uintptr_t curr_mem;     /* N bytes currently owned.                       */
  uintptr_t tick_budget;  /* N ticks in proc lifetime. 0 = unlimited.       */
  uintptr_t curr_ticks;   /* N ticks currently consumed.                    */

  uint8_t data[];         /* Official-style C99 "flexible array" element.    */

};

struct rust_port {
  size_t live_refcnt;
  size_t weak_refcnt;
  rust_proc_t *proc;
  size_t unit_sz;
  ptr_vec_t writers;
};

/*
 * The value held in a rust 'chan' slot is actually a rust_port_t*,
 * with liveness of the chan indicated by weak_refcnt.
 *
 * Inside each proc, there is a uthash hashtable that maps ports to
 * rust_chan_t* values, below. The table enforces uniqueness of the
 * channel: one proc has exactly one outgoing channel (buffer) for
 * each port.
 */

struct rust_chan {
  UT_hash_handle hh;
  rust_port_t *port;
  uintptr_t queued;     /* Whether we're in a port->writers vec. */
  size_t idx;           /* Index in the port->writers vec. */
  rust_proc_t *blocked; /* Proc to wake on flush, NULL if nonblocking. */
  circ_buf_t buf;
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
init_ptr_vec(ptr_vec_t *v)
{
  v->alloc = INIT_PTR_VEC_SZ * sizeof(void*);
  v->init = 0;
  v->data = xalloc(v->alloc);
  assert(v->data);
  /*
    printf("rt: init ptr vec %" PRIxPTR ", data=%" PRIxPTR "\n",
    (uintptr_t)v, (uintptr_t)v->data);
  */
}

static void
fini_ptr_vec(ptr_vec_t *v)
{
  assert(v);
  assert(v->data);
  /*
    printf("rt: fini ptr vec %" PRIxPTR ", data=%" PRIxPTR "\n",
    (uintptr_t)v, (uintptr_t)v->data);
  */
  assert(v->init == 0);
  free(v->data);
}

static void
ptr_vec_push(ptr_vec_t *v, void *p)
{
  assert(v);
  assert(v->data);
  if (v->init == v->alloc) {
    v->alloc *= 2;
    v->data = xrealloc(v->data, v->alloc);
  }
  assert(v->init < v->alloc);
  v->data[v->init++] = p;
}

static void
ptr_vec_trim(ptr_vec_t *v, size_t init)
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
ptr_vec_swapdel(ptr_vec_t *v, size_t i)
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

static void
proc_vec_swapdel(ptr_vec_t *v, rust_proc_t *proc)
{
  assert(v);
  assert(proc);
  assert(v->data[proc->idx] == proc);
  ptr_vec_swapdel(v, proc->idx);
  if (v->init > 0) {
    rust_proc_t *pnew = (rust_proc_t*)v->data[proc->idx];
    pnew->idx = proc->idx;
  }
}

static void
chan_vec_swapdel(ptr_vec_t *v, rust_chan_t *chan)
{
  assert(v);
  assert(chan);
  assert(v->data[chan->idx] == chan);
  ptr_vec_swapdel(v, chan->idx);
  if (v->init > 0) {
    rust_chan_t *cnew = (rust_chan_t*)v->data[chan->idx];
    cnew->idx = chan->idx;
  }
}

/*
static ptr_vec_t*
new_ptr_vec()
{
  ptr_vec_t *v = xalloc(sizeof(ptr_vec_t));
  init_ptr_vec(v);
  return v;
}
static void
del_ptr_vec(ptr_vec_t *v)
{
  fini_ptr_vec(v);
  free(v);
}
static void
ptr_vec_del(ptr_vec_t *v, size_t i)
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


/* Utility type: circular buffer. */

#define INIT_CIRC_BUF_UNITS 8
#define MAX_CIRC_BUF_SIZE (1 << 24)

static void
init_circ_buf(circ_buf_t *c, size_t unit_sz)
{
  assert(c);
  assert(unit_sz);
  c->unit_sz = unit_sz;
  c->alloc = INIT_CIRC_BUF_UNITS * unit_sz;
  c->next = 0;
  c->unread = 0;
  c->data = xcalloc(c->alloc);
  /* printf("rt: circ buf initialized, alloc=%d, unread=%d\n", c->alloc, c->unread); */
  assert(c->data);
}

static void
fini_circ_buf(circ_buf_t *c)
{
  assert(c);
  assert(c->data);
  assert(c->unread == 0);
  free(c->data);
}

static void
circ_buf_transfer(circ_buf_t *c, void *dst)
{
  size_t i;
  uint8_t *d = dst;
  for (i = 0; i < c->unread; i += c->unit_sz)
    memcpy(&d[i], &c->data[c->next + i % c->alloc], c->unit_sz);
}

static void
circ_buf_push(circ_buf_t *c, void *src)
{
  size_t i;
  void *tmp;

  assert(c);
  assert(src);
  assert(c->unread <= c->alloc);
  /* Grow if necessary. */
  if (c->unread == c->alloc) {
    assert(c->alloc <= MAX_CIRC_BUF_SIZE);
    tmp = xalloc(c->alloc << 1);
    circ_buf_transfer(c, tmp);
    c->alloc <<= 1;
    free(c->data);
    c->data = tmp;
  }
  /* printf("rt: pushing at: unread %d, alloc %d\n", c->unread, c->alloc); */
  assert(c->unread < c->alloc);
  i = (c->next + c->unread) % c->alloc;
  /*   logptr("dst addr", (uintptr_t)&c->data[i]); */
  /*   logptr("src addr", (uintptr_t)src); */
  /*   logptr("sz", (uintptr_t)c->unit_sz); */
  memcpy(&c->data[i], src, c->unit_sz);
  /* printf("rt: pushed data at index %d = '%d'\n",  i, (uintptr_t)(c->data[i])); */
  c->unread += c->unit_sz;
}

static void
circ_buf_shift(circ_buf_t *c, void *dst)
{
  size_t i;
  void *tmp;

  assert(c);
  assert(dst);
  assert(c->unit_sz > 0);
  assert(c->unread >= c->unit_sz);
  assert(c->unread <= c->alloc);
  assert(c->data);
  i = c->next;
  memcpy(dst, &c->data[i], c->unit_sz);
  /* printf("rt: shifted data from index %d = '%d'\n", i, ((uintptr_t*)dst)[0]); */
  c->unread -= c->unit_sz;
  c->next += c->unit_sz;
  assert(c->next <= c->alloc);
  if (c->next == c->alloc)
    c->next = 0;
  /* Shrink if necessary. */
  if (c->alloc >= INIT_CIRC_BUF_UNITS * c->unit_sz &&
      c->unread <= c->alloc >> 2) {
    tmp = xalloc(c->alloc >> 1);
    circ_buf_transfer(c, tmp);
    c->alloc >>= 1;
    free(c->data);
    c->data = tmp;
  }
}


/* Stacks */

/* Get around to using linked-lists of size-doubling stacks, eventually. */
static size_t const init_stk_bytes = 65536;

static stk_seg_t*
new_stk()
{
  size_t sz = sizeof(stk_seg_t) + init_stk_bytes;
  stk_seg_t *stk = xalloc(sz);
  logptr("new stk", (uintptr_t)stk);
  memset(stk, 0, sizeof(stk_seg_t));
  stk->size = init_stk_bytes;
  stk->valgrind_id = VALGRIND_STACK_REGISTER(&stk->data[0], &stk->data[stk->size]);
  /*
  printf("new stk range: [%" PRIxPTR ", %" PRIxPTR "]\n",
         (uintptr_t)&stk->data[0], (uintptr_t)&stk->data[stk->size]);
  */
  return stk;
}

static void
del_stk(stk_seg_t *stk)
{
  stk_seg_t *nxt = 0;
  do {
    nxt = stk->next;
    logptr("freeing stk segment", (uintptr_t)stk);
    /*
      printf("end stk range: [%" PRIxPTR ", %" PRIxPTR "]\n",
      (uintptr_t)&stk->data[0], (uintptr_t)&stk->data[stk->size]);
    */
    VALGRIND_STACK_DEREGISTER(stk->valgrind_id);
    free(stk);
    stk = nxt;
  } while (stk);
  printf("rt: freed stacks\n");
}

/* Processes */

/* FIXME: ifdef by platform. */
size_t const n_callee_saves = 4;

static rust_proc_t*
new_proc(rust_rt_t *rt, rust_prog_t *prog)
{
  /* FIXME: need to actually convey the proc internal-slots size to here. */
  rust_proc_t *proc = xcalloc(sizeof(rust_proc_t) + 1024);
  logptr("new proc", (uintptr_t)proc);
  logptr("from prog", (uintptr_t)prog);
  logptr("init:", (uintptr_t)prog->init_code);
  logptr("main:", (uintptr_t)prog->main_code);
  logptr("fini:", (uintptr_t)prog->fini_code);
  proc->prog = prog;
  proc->stk = new_stk();

  /*
     Set sp to last uintptr_t-sized cell of segment
     then align down to 16 boundary, to be safe-ish?
  */
  size_t tos = init_stk_bytes-sizeof(uintptr_t);
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
  proc->sp -= (3 + n_callee_saves) * sizeof(uintptr_t);
  *sp-- = (uintptr_t) proc;
  *sp-- = (uintptr_t) 0;
  *sp-- = (uintptr_t) (uintptr_t) prog->main_code;
  *sp-- = (uintptr_t) (uintptr_t) prog->main_code;
  for (size_t j = 0; j < n_callee_saves; ++j) {
    *sp-- = 0;
  }

  proc->rt = rt;
  proc->state = (uintptr_t)proc_state_running;
  return proc;
}

static void
del_proc(rust_proc_t *proc)
{
  printf("rt: del proc %" PRIxPTR " (refcnt: %d)\n",
         (uintptr_t)proc, proc->refcnt);
  assert(proc->refcnt == 0);
  del_stk(proc->stk);
  while (proc->chans) {
    rust_chan_t *c = proc->chans;
    HASH_DEL(proc->chans,c);
    fini_circ_buf(&c->buf);
    free(c);
  }
  free(proc);
}

static rust_proc_t*
spawn_proc(rust_rt_t *rt,
                rust_prog_t *prog)
{
  rust_proc_t *proc = new_proc(rt, prog);
  proc->refcnt = 1;
  return proc;
}

static ptr_vec_t*
get_state_vec(rust_rt_t *rt, proc_state_t state)
{
  switch (state) {
  case proc_state_running:
  case proc_state_calling_c:
  case proc_state_exiting:
    return &rt->running_procs;

  case proc_state_blocked_reading:
  case proc_state_blocked_writing:
    return &rt->blocked_procs;
  }
  assert(0);
  return NULL;
}

static ptr_vec_t*
get_proc_vec(rust_proc_t *proc)
{
  return get_state_vec(proc->rt, proc->state);
}

static void
add_proc_to_state_vec(rust_proc_t *proc)
{
  ptr_vec_t *v = get_proc_vec(proc);
  proc->idx = v->init;
  ptr_vec_push(v, proc);
}


static size_t
n_live_procs(rust_rt_t *rt)
{
  return rt->running_procs.init + rt->blocked_procs.init;
}

static void
remove_proc_from_state_vec(rust_proc_t *proc)
{
  ptr_vec_t *v = get_proc_vec(proc);
  /*
    printf("rt: removing proc %" PRIxPTR " from state %d in vec %" PRIxPTR "\n", 
    (uintptr_t)proc, proc->state, (uintptr_t)v);
  */
  assert((rust_proc_t *) v->data[proc->idx] == proc);
  proc_vec_swapdel(v, proc);
  ptr_vec_trim(v, n_live_procs(proc->rt));
}


static void
proc_state_transition(rust_proc_t *proc,
                      proc_state_t src,
                      proc_state_t dst)
{
  assert(proc->state == src);
  remove_proc_from_state_vec(proc);
  proc->state = dst;
  add_proc_to_state_vec(proc);
}

static void
exit_proc(rust_proc_t *proc)
{
  assert(proc);
  assert(proc->rt);
  rust_rt_t *rt = proc->rt;
  assert(n_live_procs(rt) > 0);
  ptr_vec_t *v = get_proc_vec(proc);
  assert(v);
  proc_vec_swapdel(v, proc);
  //del_proc(proc);
  ptr_vec_trim(v, n_live_procs(rt));
  printf("rt: proc %" PRIxPTR " exited (and deleted)\n", (uintptr_t)proc);
}

static rust_proc_t*
sched(rust_rt_t *rt)
{
  assert(rt);
  assert(n_live_procs(rt) > 0);
  if (rt->running_procs.init > 0) {
    size_t i = rand(&rt->rctx);
    i %= rt->running_procs.init;
    return rt->running_procs.data[i];
  }
  printf("rt: no schedulable processes\n");
  exit(1);
}

/* Runtime */

static rust_rt_t*
new_rt()
{
  rust_rt_t *rt = xcalloc(sizeof(rust_rt_t));
  logptr("new rt", (uintptr_t)rt);
  init_ptr_vec(&rt->running_procs);
  init_ptr_vec(&rt->blocked_procs);

  rt->rctx.randa = 0;
  rt->rctx.randb = 0;

#ifdef __WIN32__
  {
    HCRYPTPROV hProv;
    win32_require
      ("CryptAcquireContext",
       CryptAcquireContext(&hProv, NULL, NULL, PROV_DSS,
                           CRYPT_VERIFYCONTEXT|CRYPT_SILENT));
    win32_require
      ("CryptGenRandom",
       CryptGenRandom(hProv, sizeof(rt->rctx.randrsl),
                      (BYTE*)(&rt->rctx.randrsl)));
    win32_require
      ("CryptReleaseContext",
       CryptReleaseContext(hProv, 0));
  }
#else
#endif

  randinit(&rt->rctx, 1);
  return rt;
}

static void
del_all_procs(ptr_vec_t *v) {
  assert(v);
  while (v->init) {
    del_proc((rust_proc_t*) v->data[v->init--]);
  }
}

static void
del_rt(rust_rt_t *rt)
{
  del_all_procs(&rt->running_procs);
  del_all_procs(&rt->blocked_procs);
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
upcall_new_port(rust_proc_t *proc, size_t unit_sz)
{
  rust_port_t *port = xcalloc(sizeof(rust_port_t));
  port->proc = proc;
  port->unit_sz = unit_sz;
  port->live_refcnt = 1;
  init_ptr_vec(&port->writers);
  logptr("new port", (uintptr_t)port);
  return port;
}

static void
upcall_del_port(rust_port_t *port)
{
  printf("rt: del port %" PRIxPTR " (live refcnt: %d, weak refcnt %d)\n",
         (uintptr_t)port, port->live_refcnt, port->weak_refcnt);
  assert(port->live_refcnt == 0 || port->weak_refcnt == 0);
  if (port->live_refcnt == 0 &&
      port->weak_refcnt == 0) {
    printf("rt: finalizing and freeing port\n");
    /* FIXME: need to force-fail all the queued writers. */
    fini_ptr_vec(&port->writers);
    free(port);
  }
}

/*
 * Buffering protocol:
 *
 *   - Reader attempts to read:
 *     - Set reader to blocked-reading state.
 *     - If buf with data exists:
 *       - Attempt transmission.
 *
 *  - Writer attempts to write:
 *     - Set writer to blocked-writing state.
 *     - Copy data into chan.
 *     - Attempt transmission.
 *
 *  - Transmission:
 *       - Copy data from buf to reader
 *       - Decr buf
 *       - Set reader to running
 *       - If buf now empty and blocked writer:
 *         - Set blocked writer to running
 *
 */

static int
attempt_transmission(rust_chan_t *src,
                     rust_proc_t *dst)
{
  assert(src);
  assert(dst);
  if (dst->state != proc_state_blocked_reading) {
    printf("rt: dst in non-reading state, transmission incomplete\n");
    return 0;
  }
  if (src->blocked) {
    assert(src->blocked->state == proc_state_blocked_writing);
  }
  if (src->buf.unread == 0) {
    printf("rt: buffer empty, transmission incomplete\n");
    return 0;
  }
  uintptr_t *dptr = (uintptr_t*)dst->upcall_args[0];
  circ_buf_shift(&src->buf, dptr);
  if (src->blocked) {
    proc_state_transition(src->blocked,
                          proc_state_blocked_writing,
                          proc_state_running);
    src->blocked = NULL;
  }
  proc_state_transition(dst,
                        proc_state_blocked_reading,
                        proc_state_running);
  printf("rt: transmission complete\n");
  return 1;
}

static void
upcall_send(rust_proc_t *src, rust_port_t *port, void *sptr)
{
  rust_chan_t *chan = NULL;

  if (!port) {
    printf("rt: send to NULL port (possibly throw?)");
    return;
  }

  logptr("send to port", (uintptr_t)port);

  assert(src);
  assert(port);
  assert(sptr);
  HASH_FIND(hh,src->chans,port,sizeof(rust_port_t*),chan);
  if (!chan) {
    /* printf("rt: building new chan to buffer send\n"); */
    chan = xcalloc(sizeof(rust_chan_t));
    chan->port = port;
    init_circ_buf(&chan->buf, port->unit_sz);
    HASH_ADD(hh,src->chans,port,sizeof(rust_port_t*),chan);
  }
  assert(chan);
  assert(chan->blocked == src || !chan->blocked);
  assert(chan->port);
  assert(chan->port == port);

  /* logptr("sending via chan", (uintptr_t)chan); */

  if (port->proc) {
    chan->blocked = src;
    circ_buf_push(&chan->buf, sptr);
    proc_state_transition(src,
                          proc_state_calling_c,
                          proc_state_blocked_writing);
    attempt_transmission(chan, port->proc);
    if (chan->buf.unread && !chan->queued) {
      chan->queued = 1;
      chan->idx = port->writers.init;
      ptr_vec_push(&port->writers, chan);
    }
  } else {
    printf("rt: port has no proc (possibly throw?)\n");
  }
}

static void
upcall_recv(rust_proc_t *dst, rust_port_t *port)
{
  logptr("recv from port", (uintptr_t)port);
  assert(port);
  assert(port->proc);
  assert(dst);
  assert(port->proc == dst);
  proc_state_transition(dst,
                        proc_state_calling_c,
                        proc_state_blocked_reading);
  if (port->writers.init > 0) {
    assert(dst->rt);
    size_t i = rand(&dst->rt->rctx);
    i %= port->writers.init;
    rust_chan_t *schan = (rust_chan_t*)port->writers.data[i];
    assert(schan->idx == i);
    if (attempt_transmission(schan, dst)) {
      chan_vec_swapdel(&port->writers, schan);
      ptr_vec_trim(&port->writers, port->writers.init);
      schan->queued = 0;
    }
  } else {
    logptr("no writers sending to port", (uintptr_t)port);
  }
}


static void
upcall_check_expr(rust_proc_t *proc, uint32_t i)
{
  if (!i) {
    /* FIXME: throw, don't just exit. */
    printf("\nrt: *** CHECK FAILED ***\n\n");
    proc->state = (uintptr_t)proc_state_exiting;
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
handle_upcall(rust_proc_t *proc)
{
  uintptr_t *args = &proc->upcall_args[0];

  /*
  printf("rt: proc %" PRIxPTR " calling fn #%d\n",
         (uintptr_t)proc, proc->upcall_code);
  */
  switch ((upcall_t)proc->upcall_code) {
  case upcall_code_log_uint32:
    upcall_log_uint32_t(args[0]);
    break;
  case upcall_code_log_str:
    upcall_log_str((char*)args[0]);
    break;
  case upcall_code_spawn:
    *((rust_proc_t**)args[0]) = spawn_proc(proc->rt, (rust_prog_t*)args[1]);
    break;
  case upcall_code_kill:
    printf("rt: kill proc with refcnt %d\n", ((rust_proc_t*)args[0])->refcnt);
    break;
  case upcall_code_sched:
    add_proc_to_state_vec((rust_proc_t*)args[0]);
    break;
  case upcall_code_check_expr:
    upcall_check_expr(proc, args[0]);
    break;
  case upcall_code_malloc:
    *((uintptr_t*)args[0]) = upcall_malloc(proc, (size_t)args[1]);
    break;
  case upcall_code_free:
    upcall_free(proc, (void*)args[0]);
    break;
  case upcall_code_new_port:
    *((rust_port_t**)args[0]) = upcall_new_port(proc, (size_t)args[1]);
    break;
  case upcall_code_del_port:
    upcall_del_port((rust_port_t*)args[0]);
    break;
  case upcall_code_send:
    upcall_send(proc, (rust_port_t*)args[0], (void*)args[1]);
    break;
  case upcall_code_recv:
    upcall_recv(proc, (rust_port_t*)args[1]);
    break;
      /*;
  case 3:
    kill_proc(proc->rt, (rust_proc_t*)args[0]);
    break;
  case 6:
    memmove(proc, (void*)args[0], (const void*)args[1], (size_t)args[2]);
    break;
  case 7:
  **retslot_p = memcmp((const void*)args[0], (const void*)args[1], (size_t)args[2]);
    break;
      */
  }
  /* Zero the immediates code slot out so the caller doesn't have to
   * use MOV to update it. x86-ism but harmless on non-x86 platforms that
   * want to use their own MOVs. */
  proc->upcall_code = (upcall_t)0;
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

  rt = new_rt();
  add_proc_to_state_vec(spawn_proc(rt, prog));
  proc = sched(rt);

  logptr("root proc is", (uintptr_t)proc);
  logptr("proc->sp", (uintptr_t)proc->sp);
  logptr("c_to_proc_glue", (uintptr_t)c_to_proc_glue);

  while (1) {
    /* printf("rt: entering proc 0x%" PRIxPTR "\n", (uintptr_t)proc); */
    proc->state = (uintptr_t)proc_state_running;
    c_to_proc_glue(proc);
    /* printf("rt: returned from proc 0x%" PRIxPTR " in state %d\n",
       (uintptr_t)proc, proc->state); */
    switch ((proc_state_t) proc->state) {
    case proc_state_running:
      break;
    case proc_state_calling_c:
      handle_upcall(proc);
      if (proc->state == proc_state_calling_c)
        proc->state = proc_state_running;
      break;
    case proc_state_exiting:
      logptr("proc exiting", (uintptr_t)proc);
      exit_proc(proc);
      break;
    case proc_state_blocked_reading:
    case proc_state_blocked_writing:
      assert(0);
      break;
    }
    if (n_live_procs(rt) > 0)
      proc = sched(rt);
    else
      break;
  }

  printf("rt: finished main loop\n");
  del_rt(rt);
  printf("rt: freed runtime\n");
  return 0;
}

/*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * compile-command: "make -k -C .. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 */
