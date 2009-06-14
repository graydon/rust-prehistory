/*
 * Rust runtime library.
 * Copyright 2008, 2009 Graydon Hoare <graydon@pobox.com>.
 * Released under MIT license.
 * See file COPYING for details.
 */

#include <stdio.h>
#include <string.h>
#include <inttypes.h>

#include "rust.h"
#include "rand.h"
#include "uthash.h"
#include "valgrind.h"

#ifdef __WIN32__
#include <windows.h>
#include <wincrypt.h>
#endif

#define PROC_MAX_UPCALL_ARGS 8
#define INIT_PTR_VEC_SZ 8
#define INIT_CIRC_BUF_UNITS 8
#define MAX_CIRC_BUF_SIZE (1 << 24)
#define I(rt, e) ((e) ? (void)0 :                           \
                  (rt)->srv->fatal((rt)->srv->user,         \
                                   #e, __FILE__, __LINE__))

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


static uint32_t const LOG_ALL = 0xffffffff;
static uint32_t const LOG_ERR =        0x1;
static uint32_t const LOG_MEM =        0x2;
static uint32_t const LOG_COMM =       0x4;
static uint32_t const LOG_PROC =       0x8;
static uint32_t const LOG_UPCALL =    0x10;
static uint32_t const LOG_RT =        0x20;
static uint32_t const LOG_ULOG =      0x40;


static uint32_t
get_logbits()
{
    uint32_t bits = LOG_ULOG|LOG_ERR;
    char *c = getenv("RUST_LOG");
    if (c) {
        bits = 0;
        if (strstr(c, "err"))
            bits |= LOG_ERR;
        if (strstr(c, "mem"))
            bits |= LOG_MEM;
        if (strstr(c, "comm"))
            bits |= LOG_COMM;
        if (strstr(c, "proc"))
            bits |= LOG_PROC;
        if (strstr(c, "upcall"))
            bits |= LOG_UPCALL;
        if (strstr(c, "rt"))
            bits |= LOG_RT;
        if (strstr(c, "ulog"))
            bits |= LOG_ULOG;
    }
    return bits;
}


/*
 * We have a variety of pointer-tagging schemes.
 *
 * For interior slots of the 'int' type, we use a 1-bit tag to switch
 * between fixnum and boxed bignum.
 *
 * Exterior subword-sized slots are synonymous with interior
 * subword-sized slots; there is no difference. Subsequently,
 * transplanting a subword-sized datum into an exterior slot is always
 * just a copy. Write aliases can be formed on subword-sized slots;
 * they are just the address of the slot itself, aligned or not.
 *
 * Exterior word-or-greater slots are stored as pointers. Size implies
 * alignment, so we have free tag bits. We use one bit to
 * differentiate crate-offset pseudo-pointers from real heap pointers.
 *
 * Slots of 'any' type need to denote both a type and a value. They do
 * this by stealing 3 bits for tag and assigning thus (on 32-bit
 * platforms):
 *
 *   - 0b000 == mini-fixnum int
 *   - 0b001 == boxed int
 *   - 0b010 == crate-offset pseudo pointer to (type,val) pair
 *   - 0b011 == pure pointer to (type,val) pair
 *   - 0b100 == nil
 *   - 0b101 == bool
 *   - 0b110 == char
 *   - 0b111 == boxed str (strs are always 3 words at least:
 *                         refs, len, buf)
 *
 * On 64-bit platforms, we have 4 bits to play with since 2 words is
 * 128 bits. So we extend the "stored inline" variants to cover:
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
    /*
     * NB: it's important that 'running' be value 0, as it
     * lets us get away with using OR rather than MOV to
     * signal anything-not-running. x86 optimization.
     */
    proc_state_running    = 0,
    proc_state_calling_c  = 1,
    proc_state_exiting    = 2,
    proc_state_blocked_reading  = 3,
    proc_state_blocked_writing  = 4
} proc_state_t;

static char const * const state_names[] =
    {
        "running",
        "calling_c",
        "exiting",
        "blocked_reading",
        "blocked_writing"
    };

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
    uint32_t logbits;
    rust_srv_t *srv;
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
    uintptr_t sp;           /* saved sp when not running. */
    uintptr_t state;
    size_t idx;
    size_t refcnt;
    rust_chan_t *chans;

    /* Parameter space for upcalls. */
    /*
     * FIXME: could probably get away with packing upcall code and
     * state into 1 byte each. And having fewer max upcall args.
     */
    uintptr_t upcall_code;
    uintptr_t upcall_args[PROC_MAX_UPCALL_ARGS];

    /* Proc accounting. */
    uintptr_t mem_budget;   /* N bytes ownable by this proc.       */
    uintptr_t curr_mem;     /* N bytes currently owned.            */
    uintptr_t tick_budget;  /* N ticks in lifetime. 0 = unlimited. */
    uintptr_t curr_ticks;   /* N ticks currently consumed.         */

    uint8_t data[];         /* C99 "flexible array" element.       */

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
    rust_proc_t *blocked; /* Proc to wake on flush,
                             NULL if nonblocking. */
    circ_buf_t buf;
};

static void
xlog(rust_rt_t *rt, uint32_t logbit, char const *fmt, ...)
{
    char buf[256];
    if (rt->logbits & logbit) {
        va_list args;
        va_start(args, fmt);
        vsnprintf(buf, sizeof(buf), fmt, args);
        rt->srv->log(rt->srv->user, buf);
        va_end(args);
    }
}

static void
logptr(rust_rt_t *rt, char const *msg, uintptr_t ptrval)
{
    xlog(rt, LOG_MEM, "%s 0x%" PRIxPTR, msg, ptrval);
}

static void
xfree(rust_rt_t *rt, void *p)
{
    I(rt, p);
    rt->srv->free(rt->srv->user, p);
}

static void*
xalloc(rust_rt_t *rt, size_t sz)
{
    void *p = rt->srv->malloc(rt->srv->user, sz);
    I(rt, p);
    return p;
}

static void*
xcalloc(rust_rt_t *rt, size_t sz)
{
    void *p = xalloc(rt, sz);
    memset(p, 0, sz);
    return p;
}

static void*
xrealloc(rust_rt_t *rt, void *p, size_t sz)
{
    p = rt->srv->realloc(rt->srv->user, p, sz);
    I(rt, p);
    return p;
}

/* Utility type: pointer-vector. */

static void
init_ptr_vec(rust_rt_t *rt, ptr_vec_t *v)
{
    I(rt, v);
    v->alloc = INIT_PTR_VEC_SZ * sizeof(void*);
    v->init = 0;
    v->data = xalloc(rt, v->alloc);
    I(rt, v->data);
    xlog(rt, LOG_MEM,
         "init ptr vec 0x%" PRIxPTR ", data=0x%" PRIxPTR,
         (uintptr_t)v, (uintptr_t)v->data);
}

static void
fini_ptr_vec(rust_rt_t *rt, ptr_vec_t *v)
{
    I(rt, v);
    I(rt, v->data);
    xlog(rt, LOG_MEM,
         "fini ptr vec 0x%" PRIxPTR ", data=0x%" PRIxPTR,
         (uintptr_t)v, (uintptr_t)v->data);
    I(rt, v->init == 0);
    free(v->data);
}

static void
ptr_vec_push(rust_rt_t *rt, ptr_vec_t *v, void *p)
{
    I(rt, v);
    I(rt, v->data);
    if (v->init == v->alloc) {
        v->alloc *= 2;
        v->data = xrealloc(rt, v->data, v->alloc);
    }
    I(rt, v->init < v->alloc);
    v->data[v->init++] = p;
}

static void
ptr_vec_trim(rust_rt_t *rt, ptr_vec_t *v, size_t init)
{
    I(rt, v);
    I(rt, v->data);
    if (init <= (v->alloc / 4) &&
        (v->alloc / 2) >= INIT_PTR_VEC_SZ) {
        v->alloc /= 2;
        I(rt, v->alloc >= v->init);
        v->data = xrealloc(rt, v->data, v->alloc);
        I(rt, v->data);
    }
}

static void
ptr_vec_swapdel(rust_rt_t *rt, ptr_vec_t *v, size_t i)
{
    /* Swap the endpoint into i and decr init. */
    I(rt, v);
    I(rt, v->data);
    I(rt, v->init > 0);
    I(rt, i < v->init);
    v->init--;
    if (v->init > 0)
        v->data[i] = v->data[v->init];
}

static void
proc_vec_swapdel(rust_rt_t *rt, ptr_vec_t *v, rust_proc_t *proc)
{
    I(rt, proc);
    I(rt, v);
    I(rt, v->data[proc->idx] == proc);
    ptr_vec_swapdel(rt, v, proc->idx);
    if (v->init > 0) {
        rust_proc_t *pnew = (rust_proc_t*)v->data[proc->idx];
        pnew->idx = proc->idx;
    }
}

static void
chan_vec_swapdel(rust_rt_t *rt, ptr_vec_t *v, rust_chan_t *chan)
{
    I(rt, v);
    I(rt, chan);
    I(rt, v->data[chan->idx] == chan);
    ptr_vec_swapdel(rt, v, chan->idx);
    if (v->init > 0) {
        rust_chan_t *cnew = (rust_chan_t*)v->data[chan->idx];
        cnew->idx = chan->idx;
    }
}

/* Utility type: circular buffer. */

static void
init_circ_buf(rust_rt_t *rt, circ_buf_t *c, size_t unit_sz)
{
    I(rt, c);
    I(rt, unit_sz);
    c->unit_sz = unit_sz;
    c->alloc = INIT_CIRC_BUF_UNITS * unit_sz;
    c->next = 0;
    c->unread = 0;
    c->data = xcalloc(rt, c->alloc);
    xlog(rt, LOG_MEM|LOG_COMM,
         "init circ buf 0x" PRIxPTR ", alloc=%d, unread=%d",
         c, c->alloc, c->unread);
    I(rt, c->data);
}

static void
fini_circ_buf(rust_rt_t *rt, circ_buf_t *c)
{
    I(rt, c);
    I(rt, c->data);
    I(rt, c->unread == 0);
    xfree(rt, c->data);
}

static void
circ_buf_transfer(rust_rt_t *rt, circ_buf_t *c, void *dst)
{
    size_t i;
    uint8_t *d = dst;
    I(rt, c);
    I(rt, dst);
    for (i = 0; i < c->unread; i += c->unit_sz)
        memcpy(&d[i], &c->data[c->next + i % c->alloc], c->unit_sz);
}

static void
circ_buf_push(rust_rt_t *rt, circ_buf_t *c, void *src)
{
    size_t i;
    void *tmp;

    I(rt, c);
    I(rt, src);
    I(rt, c->unread <= c->alloc);

    /* Grow if necessary. */
    if (c->unread == c->alloc) {
        I(rt, c->alloc <= MAX_CIRC_BUF_SIZE);
        tmp = xalloc(rt, c->alloc << 1);
        circ_buf_transfer(rt, c, tmp);
        c->alloc <<= 1;
        xfree(rt, c->data);
        c->data = tmp;
    }

    xlog(rt, LOG_MEM|LOG_COMM,
         "circ buf push, unread=%d, alloc=%d, unit_sz=%d",
         c->unread, c->alloc, c->unit_sz);

    I(rt, c->unread < c->alloc);
    I(rt, c->unread + c->unit_sz <= c->alloc);

    i = (c->next + c->unread) % c->alloc;
    memcpy(&c->data[i], src, c->unit_sz);

    xlog(rt, LOG_MEM|LOG_COMM, "pushed data at index %d", i);
    c->unread += c->unit_sz;
}

static void
circ_buf_shift(rust_rt_t *rt, circ_buf_t *c, void *dst)
{
    size_t i;
    void *tmp;

    I(rt, c);
    I(rt, dst);
    I(rt, c->unit_sz > 0);
    I(rt, c->unread >= c->unit_sz);
    I(rt, c->unread <= c->alloc);
    I(rt, c->data);
    i = c->next;
    memcpy(dst, &c->data[i], c->unit_sz);
    xlog(rt, LOG_MEM|LOG_COMM, "shifted data from index %d", i);
    c->unread -= c->unit_sz;
    c->next += c->unit_sz;
    I(rt, c->next <= c->alloc);
    if (c->next == c->alloc)
        c->next = 0;

    /* Shrink if necessary. */
    if (c->alloc >= INIT_CIRC_BUF_UNITS * c->unit_sz &&
        c->unread <= c->alloc / 4) {
        tmp = xalloc(rt, c->alloc / 2);
        circ_buf_transfer(rt, c, tmp);
        c->alloc >>= 1;
        xfree(rt, c->data);
        c->data = tmp;
    }
}


/* Stacks */

/*
 * Get around to using linked-lists of size-doubling stacks,
 * eventually.
 */
static size_t const init_stk_bytes = 65536;

static stk_seg_t*
new_stk(rust_rt_t *rt)
{
    size_t sz = sizeof(stk_seg_t) + init_stk_bytes;
    stk_seg_t *stk = xalloc(rt, sz);
    logptr(rt, "new stk", (uintptr_t)stk);
    memset(stk, 0, sizeof(stk_seg_t));
    stk->size = init_stk_bytes;
    stk->valgrind_id =
        VALGRIND_STACK_REGISTER(&stk->data[0],
                                &stk->data[stk->size]);
    return stk;
}

static void
del_stk(rust_rt_t *rt, stk_seg_t *stk)
{
    stk_seg_t *nxt = 0;
    do {
        nxt = stk->next;
        logptr(rt, "freeing stk segment", (uintptr_t)stk);
        VALGRIND_STACK_DEREGISTER(stk->valgrind_id);
        xfree(rt, stk);
        stk = nxt;
    } while (stk);
    xlog(rt, LOG_MEM, "freed stacks");
}

/* Processes */

/* FIXME: ifdef by platform. */
size_t const n_callee_saves = 4;

static rust_proc_t*
new_proc(rust_rt_t *rt, rust_prog_t *prog)
{
    /* FIXME: need to actually convey the proc internal-slots size to here. */
    rust_proc_t *proc = xcalloc(rt, sizeof(rust_proc_t) + 1024);
    logptr(rt, "new proc", (uintptr_t)proc);
    logptr(rt, "from prog", (uintptr_t)prog);
    logptr(rt, "init", (uintptr_t)prog->init_code);
    logptr(rt, "main", (uintptr_t)prog->main_code);
    logptr(rt, "fini", (uintptr_t)prog->fini_code);
    proc->prog = prog;
    proc->stk = new_stk(rt);

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
     * return-to the "activation" pc. That PC will be the first insn of a
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
del_proc(rust_rt_t *rt, rust_proc_t *proc)
{
    xlog(rt, LOG_MEM|LOG_PROC,
         "del proc 0x%" PRIxPTR ", refcnt=%d",
         (uintptr_t)proc, proc->refcnt);

    I(rt, proc->refcnt == 0);
    del_stk(rt, proc->stk);

    while (proc->chans) {
        rust_chan_t *c = proc->chans;
        HASH_DEL(proc->chans,c);
        fini_circ_buf(rt, &c->buf);
        xfree(rt, c);
    }
    xfree(rt, proc);
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
    I(rt, 0);
    return NULL;
}

static ptr_vec_t*
get_proc_vec(rust_rt_t *rt, rust_proc_t *proc)
{
    return get_state_vec(rt, proc->state);
}

static void
add_proc_to_state_vec(rust_rt_t *rt, rust_proc_t *proc)
{
    ptr_vec_t *v = get_proc_vec(rt, proc);
    proc->idx = v->init;
    xlog(rt, LOG_MEM|LOG_PROC,
         "adding proc 0x%" PRIxPTR " in state '%s' to vec 0x%" PRIxPTR,
         (uintptr_t)proc, state_names[(size_t)proc->state], (uintptr_t)v);
    ptr_vec_push(rt, v, proc);
}


static size_t
n_live_procs(rust_rt_t *rt)
{
    return rt->running_procs.init + rt->blocked_procs.init;
}

static void
remove_proc_from_state_vec(rust_rt_t *rt, rust_proc_t *proc)
{
    ptr_vec_t *v = get_proc_vec(rt, proc);
    xlog(rt, LOG_MEM|LOG_PROC,
         "removing proc 0x%" PRIxPTR " in state '%s' from vec 0x%" PRIxPTR,
         (uintptr_t)proc, state_names[(size_t)proc->state], (uintptr_t)v);
    I(rt, (rust_proc_t *) v->data[proc->idx] == proc);
    proc_vec_swapdel(rt, v, proc);
    ptr_vec_trim(rt, v, n_live_procs(rt));
}


static void
proc_state_transition(rust_rt_t *rt,
                      rust_proc_t *proc,
                      proc_state_t src,
                      proc_state_t dst)
{
    xlog(rt, LOG_PROC,
         "proc 0x%" PRIxPTR " state change '%s' -> '%s'",
         (uintptr_t)proc,
         state_names[(size_t)src],
         state_names[(size_t)dst]);
    I(rt, proc->state == src);
    remove_proc_from_state_vec(rt, proc);
    proc->state = dst;
    add_proc_to_state_vec(rt, proc);
}

static void
exit_proc(rust_rt_t *rt, rust_proc_t *proc)
{
    I(rt, n_live_procs(rt) > 0);
    ptr_vec_t *v = get_proc_vec(rt, proc);
    I(rt, v);
    proc_vec_swapdel(rt, v, proc);
    // del_proc(proc);
    ptr_vec_trim(rt, v, n_live_procs(rt));
    xlog(rt, LOG_MEM|LOG_PROC,
         "proc 0x%" PRIxPTR " exited (and deleted)",
         (uintptr_t)proc);
}

static rust_proc_t*
sched(rust_rt_t *rt)
{
    I(rt, rt);
    I(rt, n_live_procs(rt) > 0);
    if (rt->running_procs.init > 0) {
        size_t i = rand(&rt->rctx);
        i %= rt->running_procs.init;
        return rt->running_procs.data[i];
    }
    xlog(rt, LOG_RT|LOG_PROC,
         "no schedulable processes");
    return NULL;
}

/* Runtime */

#ifdef __WIN32__
static void
win32_require(rust_rt_t *rt, LPTSTR fn, BOOL ok) {
    if (!ok) {
        LPTSTR buf;
        DWORD err = GetLastError();
        FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER |
                      FORMAT_MESSAGE_FROM_SYSTEM |
                      FORMAT_MESSAGE_IGNORE_INSERTS,
                      NULL, err,
                      MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                      (LPTSTR) &buf, 0, NULL );
        xlog(rt, LOG_ERR, "%s failed with error %ld: %s", fn, err, buf);
        LocalFree((HLOCAL)buf);
        I(rt, ok);
    }
}
#endif

static rust_rt_t*
new_rt(rust_srv_t *srv)
{
    rust_rt_t *rt = srv->malloc(srv->user, sizeof(rust_rt_t));
    memset(rt, 0, sizeof(rust_rt_t));
    rt->srv = srv;
    rt->logbits = get_logbits();
    logptr(rt, "new rt", (uintptr_t)rt);
    init_ptr_vec(rt, &rt->running_procs);
    init_ptr_vec(rt, &rt->blocked_procs);

    rt->rctx.randa = 0;
    rt->rctx.randb = 0;

#ifdef __WIN32__
    {
        HCRYPTPROV hProv;
        win32_require
            (rt, "CryptAcquireContext",
             CryptAcquireContext(&hProv, NULL, NULL, PROV_DSS,
                                 CRYPT_VERIFYCONTEXT|CRYPT_SILENT));
        win32_require
            (rt, "CryptGenRandom",
             CryptGenRandom(hProv, sizeof(rt->rctx.randrsl),
                            (BYTE*)(&rt->rctx.randrsl)));
        win32_require
            (rt, "CryptReleaseContext",
             CryptReleaseContext(hProv, 0));
    }
#else
#endif

    randinit(&rt->rctx, 1);
    return rt;
}

static void
del_all_procs(rust_rt_t *rt, ptr_vec_t *v) {
    I(rt, v);
    while (v->init) {
        del_proc(rt, (rust_proc_t*) v->data[v->init--]);
    }
}

static void
del_rt(rust_rt_t *rt)
{
    del_all_procs(rt, &rt->running_procs);
    del_all_procs(rt, &rt->blocked_procs);
    fini_ptr_vec(rt, &rt->running_procs);
    fini_ptr_vec(rt, &rt->blocked_procs);
    xfree(rt, rt);
}

/* Upcalls */

static void
upcall_log_uint32_t(rust_rt_t *rt, uint32_t i)
{
    xlog(rt, LOG_UPCALL|LOG_ULOG,
         "upcall log_uint32(0x%" PRIx32 ")",
         i);
}

static void
upcall_log_str(rust_rt_t *rt, char *c)
{
    xlog(rt, LOG_UPCALL|LOG_ULOG,
         "upcall log_str(\"%s\")",
         c);
}

static rust_port_t*
upcall_new_port(rust_rt_t *rt, rust_proc_t *proc, size_t unit_sz)
{
    rust_port_t *port = xcalloc(rt, sizeof(rust_port_t));
    xlog(rt, LOG_UPCALL|LOG_MEM|LOG_COMM,
         "upcall new_port(proc=0x%" PRIxPTR ", unit_sz=%d) -> port=0x%" PRIxPTR,
         (uintptr_t)proc, unit_sz, (uintptr_t)port);
    port->proc = proc;
    port->unit_sz = unit_sz;
    port->live_refcnt = 1;
    init_ptr_vec(rt, &port->writers);
    return port;
}

static void
upcall_del_port(rust_rt_t *rt, rust_port_t *port)
{
    xlog(rt, LOG_UPCALL|LOG_MEM|LOG_COMM,
         "upcall del_port(0x%" PRIxPTR "), live refcnt=%d, weak refcnt=%d",
         (uintptr_t)port, port->live_refcnt, port->weak_refcnt);

    I(rt, port->live_refcnt == 0 || port->weak_refcnt == 0);

    if (port->live_refcnt == 0 &&
        port->weak_refcnt == 0) {
        xlog(rt, LOG_UPCALL|LOG_COMM|LOG_MEM,
             "finalizing and freeing port 0x" PRIxPTR,
             (uintptr_t)port);
        /* FIXME: need to force-fail all the queued writers. */
        fini_ptr_vec(rt, &port->writers);
        xfree(rt, port);
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
attempt_transmission(rust_rt_t *rt,
                     rust_chan_t *src,
                     rust_proc_t *dst)
{
    I(rt, src);
    I(rt, dst);

    if (dst->state != proc_state_blocked_reading) {
        xlog(rt, LOG_COMM,
             "dst in non-reading state, "
             "transmission incomplete");
        return 0;
    }

    if (src->blocked) {
        I(rt, src->blocked->state == proc_state_blocked_writing);
    }

    if (src->buf.unread == 0) {
        xlog(rt, LOG_COMM,
             "buffer empty, "
             "transmission incomplete");
        return 0;
    }

    uintptr_t *dptr = (uintptr_t*)dst->upcall_args[0];
    circ_buf_shift(rt, &src->buf, dptr);

    if (src->blocked) {
        proc_state_transition(rt, src->blocked,
                              proc_state_blocked_writing,
                              proc_state_running);
        src->blocked = NULL;
    }

    proc_state_transition(rt, dst,
                          proc_state_blocked_reading,
                          proc_state_running);

    xlog(rt, LOG_COMM, "transmission complete");
    return 1;
}

static void
upcall_send(rust_rt_t *rt, rust_proc_t *src,
            rust_port_t *port, void *sptr)
{
    xlog(rt, LOG_UPCALL|LOG_COMM,
         "upcall send(proc=0x%" PRIxPTR ", port=0x%" PRIxPTR ")",
         (uintptr_t)src,
         (uintptr_t)port);

    rust_chan_t *chan = NULL;

    if (!port) {
        xlog(rt, LOG_COMM|LOG_ERR,
             "send to NULL port (possibly throw?)");
        return;
    }

    xlog(rt, LOG_MEM|LOG_COMM,
         "send to port", (uintptr_t)port);

    I(rt, src);
    I(rt, port);
    I(rt, sptr);
    HASH_FIND(hh,src->chans,port,sizeof(rust_port_t*),chan);
    if (!chan) {
        chan = xcalloc(rt, sizeof(rust_chan_t));
        xlog(rt, LOG_MEM|LOG_COMM,
             "new chan 0x%" PRIxPTR, (uintptr_t)chan);
        chan->port = port;
        init_circ_buf(rt, &chan->buf, port->unit_sz);
        HASH_ADD(hh,src->chans,port,sizeof(rust_port_t*),chan);
    }
    I(rt, chan);
    I(rt, chan->blocked == src || !chan->blocked);
    I(rt, chan->port);
    I(rt, chan->port == port);

    xlog(rt, LOG_MEM|LOG_COMM,
         "sending via chan 0x%" PRIxPTR,
         (uintptr_t)chan);

    if (port->proc) {
        chan->blocked = src;
        circ_buf_push(rt, &chan->buf, sptr);
        proc_state_transition(rt, src,
                              proc_state_calling_c,
                              proc_state_blocked_writing);
        attempt_transmission(rt, chan, port->proc);
        if (chan->buf.unread && !chan->queued) {
            chan->queued = 1;
            chan->idx = port->writers.init;
            ptr_vec_push(rt, &port->writers, chan);
        }
    } else {
        xlog(rt, LOG_COMM|LOG_ERR,
             "port has no proc (possibly throw?)");
    }
}

static void
upcall_recv(rust_rt_t *rt, rust_proc_t *dst, rust_port_t *port)
{
    xlog(rt, LOG_UPCALL|LOG_COMM,
         "upcall recv(proc=0x%" PRIxPTR ", port=0x%" PRIxPTR ")",
         (uintptr_t)dst,
         (uintptr_t)port);

    I(rt, port);
    I(rt, port->proc);
    I(rt, dst);
    I(rt, port->proc == dst);

    proc_state_transition(rt, dst,
                          proc_state_calling_c,
                          proc_state_blocked_reading);

    if (port->writers.init > 0) {
        I(rt, dst->rt);
        size_t i = rand(&dst->rt->rctx);
        i %= port->writers.init;
        rust_chan_t *schan = (rust_chan_t*)port->writers.data[i];
        I(rt, schan->idx == i);
        if (attempt_transmission(rt, schan, dst)) {
            chan_vec_swapdel(rt, &port->writers, schan);
            ptr_vec_trim(rt, &port->writers, port->writers.init);
            schan->queued = 0;
        }
    } else {
        xlog(rt, LOG_COMM,
             "no writers sending to port", (uintptr_t)port);
    }
}


static void
upcall_check_expr(rust_rt_t *rt, rust_proc_t *proc, uint32_t i)
{
    if (!i) {
        /* FIXME: throw, don't just exit. */
        xlog(rt, LOG_UPCALL|LOG_ERR, "*** CHECK FAILED ***");
        proc->state = (uintptr_t)proc_state_exiting;
    }
}

static uintptr_t
upcall_malloc(rust_rt_t *rt, rust_proc_t *proc, size_t nbytes)
{
    void *p = xalloc(rt, nbytes);
    xlog(rt, LOG_UPCALL|LOG_MEM,
         "upcall malloc(%u) = 0x%" PRIxPTR,
         nbytes, (uintptr_t)p);
    return (uintptr_t) p;
}

static void
upcall_free(rust_rt_t *rt, void* ptr)
{
    xlog(rt, LOG_UPCALL|LOG_MEM,
         "upcall free(0x%" PRIxPTR ")",
         (uintptr_t)ptr);
    xfree(rt, ptr);
}

static void
handle_upcall(rust_proc_t *proc)
{
    uintptr_t *args = &proc->upcall_args[0];

    switch ((upcall_t)proc->upcall_code) {
    case upcall_code_log_uint32:
        upcall_log_uint32_t(proc->rt, args[0]);
        break;
    case upcall_code_log_str:
        upcall_log_str(proc->rt, (char*)args[0]);
        break;
    case upcall_code_spawn:
        *((rust_proc_t**)args[0]) = spawn_proc(proc->rt, (rust_prog_t*)args[1]);
        break;
    case upcall_code_kill:
        xlog(proc->rt, LOG_PROC,
             "upcall kill_proc(0x%" PRIxPTR "), refcnt=%d",
             (rust_proc_t*)args[0],
             ((rust_proc_t*)args[0])->refcnt);
        break;
    case upcall_code_sched:
        add_proc_to_state_vec(proc->rt, (rust_proc_t*)args[0]);
        break;
    case upcall_code_check_expr:
        upcall_check_expr(proc->rt, proc, args[0]);
        break;
    case upcall_code_malloc:
        *((uintptr_t*)args[0]) =
            upcall_malloc(proc->rt, proc, (size_t)args[1]);
        break;
    case upcall_code_free:
        upcall_free(proc->rt, (void*)args[0]);
        break;
    case upcall_code_new_port:
        *((rust_port_t**)args[0]) =
            upcall_new_port(proc->rt, proc, (size_t)args[1]);
        break;
    case upcall_code_del_port:
        upcall_del_port(proc->rt, (rust_port_t*)args[0]);
        break;
    case upcall_code_send:
        upcall_send(proc->rt, proc, (rust_port_t*)args[0], (void*)args[1]);
        break;
    case upcall_code_recv:
        upcall_recv(proc->rt, proc, (rust_port_t*)args[1]);
        break;
    }
    /* Zero the immediates code slot out so the caller doesn't have to
     * use MOV to update it. x86-ism but harmless on non-x86 platforms that
     * want to use their own MOVs. */
    proc->upcall_code = (upcall_t)0;
}

static void
rust_main_loop(rust_prog_t *prog,
               void CDECL (*c_to_proc_glue)(rust_proc_t*),
               rust_srv_t *srv)
{
    rust_rt_t *rt;
    rust_proc_t *proc;
    rt = new_rt(srv);

    xlog(rt, LOG_RT, "control is in rust runtime library");
    logptr(rt, "root prog", (uintptr_t)prog);
    logptr(rt, "prog->init_code", (uintptr_t)prog->init_code);
    logptr(rt, "prog->main_code", (uintptr_t)prog->main_code);
    logptr(rt, "prog->fini_code", (uintptr_t)prog->fini_code);

    add_proc_to_state_vec(rt, spawn_proc(rt, prog));
    proc = sched(rt);

    logptr(rt, "root proc", (uintptr_t)proc);
    logptr(rt, "proc->sp", (uintptr_t)proc->sp);
    logptr(rt, "c_to_proc_glue", (uintptr_t)c_to_proc_glue);

    while (proc) {

        xlog(rt, LOG_PROC, "activating proc 0x%" PRIxPTR,
             (uintptr_t)proc);

        proc->state = (uintptr_t)proc_state_running;
        c_to_proc_glue(proc);

        xlog(rt, LOG_PROC,
             "returned from proc 0x%" PRIxPTR " in state '%s'",
             (uintptr_t)proc, state_names[proc->state]);

        switch ((proc_state_t) proc->state) {

        case proc_state_running:
            break;

        case proc_state_calling_c:
            handle_upcall(proc);
            if (proc->state == proc_state_calling_c)
                proc->state = proc_state_running;
            break;

        case proc_state_exiting:
            exit_proc(rt, proc);
            break;

        case proc_state_blocked_reading:
        case proc_state_blocked_writing:
            I(rt, 0);
            break;
        }

        if (n_live_procs(rt) > 0)
            proc = sched(rt);
        else
            break;
    }

    xlog(rt, LOG_RT, "finished main loop");
    del_rt(rt);
    xlog(rt, LOG_RT, "freed runtime");
}

static void
srv_log(void *user, char const *str)
{
    printf("rt: %s\n", str);
}

static void*
srv_malloc(void *user, size_t sz)
{
    return malloc(sz);
}

static void*
srv_realloc(void *user, void *p, size_t sz)
{
    return realloc(p, sz);
}

static void
srv_free(void *user, void *p)
{
    free(p);
}

static void
srv_fatal(void *user, char const *expr,
          char const *file, size_t line)
{
    printf("rt: fatal, '%s' failed, %s:%d\n", expr, file, line);
    abort();
}

int CDECL
rust_start(rust_prog_t *prog,
           void CDECL (*c_to_proc_glue)(rust_proc_t*))
{
    rust_srv_t srv;
    srv.log = srv_log;
    srv.malloc = srv_malloc;
    srv.realloc = srv_realloc;
    srv.free = srv_free;
    srv.fatal = srv_fatal;
    srv.user = NULL;
    rust_main_loop(prog, c_to_proc_glue, &srv);
    return 0;
}

/*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * c-basic-offset: 4
 * buffer-file-coding-system: utf-8-unix
 * compile-command: "make -k -C .. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 */
