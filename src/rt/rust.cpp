/*
 * Rust runtime library.
 * Copyright 2008, 2009 Graydon Hoare <graydon@pobox.com>.
 * Released under MIT license.
 * See file COPYING for details.
 */

#define __STDC_LIMIT_MACROS 1
#define __STDC_CONSTANT_MACROS 1
#define __STDC_FORMAT_MACROS 1

#include <stdlib.h>
#include <stdint.h>
#include <inttypes.h>

#include <stdio.h>
#include <string.h>

#include "rust.h"
#include "rand.h"
#include "uthash.h"
#include "valgrind.h"

#if defined(__WIN32__)
extern "C" {
#include <windows.h>
#include <wincrypt.h>
}
#elif defined(__GNUC__)
 /*
  * Only for RTLD_DEFAULT, remove _GNU_SOURCE when that dies. We want
  * to be non-GNU-dependent.
  */
#define _GNU_SOURCE
#include <dlfcn.h>
#include <pthread.h>
#else
#error "Platform not supported."
#endif

#define PROC_MAX_UPCALL_ARGS 8
#define INIT_PTR_VEC_SZ 8
#define INIT_CIRC_BUF_UNITS 8
#define MAX_CIRC_BUF_SIZE (1 << 24)
#define I(rt, e) ((e) ? (void)0 :                           \
                  (rt)->srv->fatal((rt)->srv,               \
                                   #e, __FILE__, __LINE__))

struct ptr_vec;
typedef struct ptr_vec ptr_vec_t;

struct circ_buf;
typedef struct circ_buf circ_buf_t;

struct str;
typedef struct str str_t;

struct vec;
typedef struct vec vec_t;

static uint32_t const LOG_ALL = 0xffffffff;
static uint32_t const LOG_ERR =        0x1;
static uint32_t const LOG_MEM =        0x2;
static uint32_t const LOG_COMM =       0x4;
static uint32_t const LOG_PROC =       0x8;
static uint32_t const LOG_UPCALL =    0x10;
static uint32_t const LOG_RT =        0x20;
static uint32_t const LOG_ULOG =      0x40;
static uint32_t const LOG_TRACE =     0x80;


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
        if (strstr(c, "up"))
            bits |= LOG_UPCALL;
        if (strstr(c, "rt"))
            bits |= LOG_RT;
        if (strstr(c, "ulog"))
            bits |= LOG_ULOG;
        if (strstr(c, "trace"))
            bits |= LOG_TRACE;
        if (strstr(c, "all"))
            bits = 0xffffffff;
    }
    return bits;
}

/* Proc stack segments. Heap allocated and chained together. */

typedef struct stk_seg {
    struct stk_seg *prev;
    struct stk_seg *next;
    unsigned int valgrind_id;
    uintptr_t prev_fp;
    uintptr_t prev_sp;
    uintptr_t limit;
    uint8_t data[];
} stk_seg_t;


typedef enum {
    proc_state_running    = 0,
    proc_state_calling_c  = 1,
    proc_state_blocked_exited   = 2,
    proc_state_blocked_reading  = 3,
    proc_state_blocked_writing  = 4
} proc_state_t;

static char const * const state_names[] =
    {
        "running",
        "calling_c",
        "exited",
        "blocked_reading",
        "blocked_writing"
    };

typedef enum {
    upcall_code_log_uint32     = 0,
    upcall_code_log_str        = 1,
    upcall_code_new_proc       = 2,
    upcall_code_del_proc       = 3,
    upcall_code_fail           = 4,
    upcall_code_malloc         = 5,
    upcall_code_free           = 6,
    upcall_code_new_port       = 7,
    upcall_code_del_port       = 8,
    upcall_code_send           = 9,
    upcall_code_recv           = 10,
    upcall_code_sched          = 11,
    upcall_code_native         = 12,
    upcall_code_new_str        = 13,
    upcall_code_grow_proc      = 14,
    upcall_code_trace_word     = 15,
    upcall_code_trace_str      = 16
} upcall_t;

/* FIXME: change ptr_vec and circ_buf to use flexible-array element
   rather than pointer-to-buf-at-end. */

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


/* Rust types vec and str look identical from our perspective. */

struct vec {
    size_t refcount;
    size_t alloc;
    size_t init;
    uint8_t data[];
};

struct str {
    size_t refcnt;
    size_t alloc;
    size_t init;
    uint8_t data[];         /* C99 "flexible array" element. */
};

struct rust_rt {
    uintptr_t sp;          /* Saved sp from the C runtime. */
    ptr_vec_t running_procs;
    ptr_vec_t blocked_procs;
    randctx rctx;
    uint32_t logbits;
    rust_proc_t *root_proc;
    rust_srv_t *srv;
    rust_port_t *ports;
};

struct rust_prog {
    void CDECL (*init_code)(void*, rust_proc_t*);
    void CDECL (*main_code)(void*, rust_proc_t*);
    void CDECL (*fini_code)(void*, rust_proc_t*);
};

/*
 * "Simple" precise, mark-sweep, single-generation GC.
 *
 *  - Every value (transitively) containing to a mutable slot
 *    is a gc_val.
 *
 *  - gc_vals come from the same simple allocator as all other
 *    values but undergo different storage management.
 *
 *  - Any frame that has pointers to gc_vals in its slots (IOW
 *    "has GC roots") has 2 frame-slots initialized on entry:
 *
 *     fp[-1] = mark-frame-chain function
 *     fp[-2] = previous-GC-fp
 *
 *  - The proc has a gc_frame_chain field pointing to the topmost GC fp.
 *    This means that you can quickly walk the list of all frames;
 *    with GC roots. The topmost mark or sweep function will, in fact,
 *    automatically chase through this list; all you have to do is
 *    call the top one.
 *
 *  - Like gc_vals have *two* extra words at their head, not one.
 *
 *  - The first word at the head of a gc_val is not used as a refcount;
 *    instead it is a pointer to a sweep function, with the low bit
 *    of that pointer used as a mark bit.
 *
 *  - The second word at the head of a gc_val is a linked-list pointer
 *    to the gc_val that was allocated just before it. Following this
 *    list traces through all the currently active gc_vals in a proc.
 *
 *  - The proc has a gc_alloc_chain field that points to the most-recent
 *    gc_val allocated.
 *
 *  - GC proceeds as follows:
 *
 *    - The proc asks its runtime for its gc_frame_chain.
 *
 *    - The proc calls gc_frame_chain[-1](fp=gc_frame_chain) which
 *      marks the frame and then tail-calls to fp[-2][-1](fp=fp[-2]),
 *      recursively.  this marks all the frames with GC roots (each of
 *      those functions in turn may recursively call into the GC
 *      graph, that's for the mark glue to decide).
 *
 *    - The proc then asks its runtime for its gc_alloc_chain.
 *
 *    - The proc calls gc_alloc_chain[0](gc_ptr=&gc_alloc_chain),
 *      which sweeps the allocation. Sweeping involves checking to see
 *      if the gc_val at *gc_ptr was marked. If not, it loads
 *      (*gc_ptr)[1] into tmp, calls drop_ty(*gc_ptr) then
 *      free(*gc_ptr), then *gc_ptr=tmp and recurs. If marked, it
 *      loads gc_ptr[1] and recurs. The key point is that it has to
 *      drop outgoing links into the refcount graph.
 *
 *    - Note that there is no "special gc state" at work here; the
 *      proc looks like it's running normal code that happens to not
 *      perform any gc_val allocation. Mark-bit twiddling is
 *      open-coded into all the (recursive) mark functions, which know
 *      their contents; we only have to do O(gc-frames) indirect calls
 *      to mark, the rest are static. Sweeping costs O(gc-heap)
 *      indirect calls, unfortunately, because the set of sweep
 *      functions to call is arbitrary based on allocation order.
 *
 */

struct rust_proc {

    rust_rt_t *rt;
    stk_seg_t *stk;
    rust_prog_t *prog;
    uintptr_t sp;           /* saved sp when not running. */
    proc_state_t state;
    size_t idx;
    size_t refcnt;
    rust_chan_t *chans;

    uintptr_t gc_frame_chain;  /* linked list of GC frames.        */
    uintptr_t gc_alloc_chain;  /* linked list of GC allocations.   */

    /* Parameter space for upcalls. */
    /*
     * FIXME: could probably get away with packing upcall code and
     * state into 1 byte each. And having fewer max upcall args.
     */
    uintptr_t upcall_code;
    uintptr_t upcall_args[PROC_MAX_UPCALL_ARGS];
};

struct rust_port {
    size_t live_refcnt;
    size_t weak_refcnt;
    rust_proc_t *proc;
    /* FIXME: 'next' and 'prev' fields are only used for collecting
     * dangling ports on abrupt process termination; can remove this
     * when we have unwinding / finishing working.
     */
    rust_port_t *next;
    rust_port_t *prev;
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
        rt->srv->log(rt->srv, buf);
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
    logptr(rt, "xfree", (uintptr_t)p);
    I(rt, p);
    rt->srv->free(rt->srv, p);
}

static void*
xalloc(rust_rt_t *rt, size_t sz)
{
    void *p = rt->srv->malloc(rt->srv, sz);
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
    p = rt->srv->realloc(rt->srv, p, sz);
    I(rt, p);
    return p;
}

/* Utility type: pointer-vector. */

static void
init_ptr_vec(rust_rt_t *rt, ptr_vec_t *v)
{
    I(rt, v);
    v->alloc = INIT_PTR_VEC_SZ;
    v->init = 0;
    v->data = (void **)xalloc(rt, v->alloc * sizeof(void*));
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
        v->data = (void **)xrealloc(rt, v->data, v->alloc);
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
        v->data = (void **)xrealloc(rt, v->data, v->alloc);
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
    c->data = (uint8_t *)xcalloc(rt, c->alloc);
    xlog(rt, LOG_MEM|LOG_COMM,
         "init circ buf 0x%" PRIxPTR ", alloc=%d, unread=%d",
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
    uint8_t *d = (uint8_t *)dst;
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
        c->data = (uint8_t *)tmp;
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
        c->data = (uint8_t *)tmp;
    }
}

/* Ports */
static void
del_port(rust_rt_t *rt, rust_port_t *port)
{
    xlog(rt, LOG_UPCALL|LOG_COMM|LOG_MEM,
         "finalizing and freeing port 0x%" PRIxPTR,
         (uintptr_t)port);
    /* FIXME: need to force-fail all the queued writers. */
    fini_ptr_vec(rt, &port->writers);
    /* FIXME: can remove the chaining-of-ports-to-rt when we have
     * unwinding / finishing working. */
    if (port->prev)
        port->prev->next = port->next;
    else if (rt->ports == port)
        rt->ports = port->next;
    if (port->next)
        port->next->prev = port->prev;
    xfree(rt, port);
}


/* Stacks */

static size_t const min_stk_bytes = 0x300;

static stk_seg_t*
new_stk(rust_rt_t *rt, size_t minsz)
{
    if (minsz < min_stk_bytes)
        minsz = min_stk_bytes;
    size_t sz = sizeof(stk_seg_t) + minsz;
    stk_seg_t *stk = (stk_seg_t *)xalloc(rt, sz);
    logptr(rt, "new stk", (uintptr_t)stk);
    memset(stk, 0, sizeof(stk_seg_t));
    stk->limit = (uintptr_t) &stk->data[minsz];
    logptr(rt, "stk limit", stk->limit);
    stk->valgrind_id =
        VALGRIND_STACK_REGISTER(&stk->data[0],
                                &stk->data[minsz]);
    return stk;
}

static void
del_stk(rust_rt_t *rt, stk_seg_t *stk)
{
    stk_seg_t *nxt = 0;

    /* Rewind to bottom-most stk segment. */
    while (stk->prev)
        stk = stk->prev;

    /* Then free forwards. */
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

static void
upcall_grow_proc(rust_proc_t *proc, size_t n_call_bytes, size_t n_frame_bytes)
{
    /*
     *  We have a stack like this:
     *
     *  | higher frame  |
     *  +---------------+ <-- top of call region
     *  | caller args   |
     *  | ...           |
     *  | ABI operands  |   <-- top of fixed-size call region
     *  | ...           |
     *  | retpc         |
     *  | callee save 1 |
     *  | ...           |
     *  | callee save N |
     *  +---------------+ <-- fp, base of call region
     *  |               |
     *
     * And we were hoping to move fp down by n_frame_bytes to allocate
     * an n_frame_bytes frame for the current function, but we ran out
     * of stack. So rather than subtract fp, we called into this
     * function.
     *
     * This function's job is:
     *
     *  - Check to see if we have an existing stack segment chained on
     *    the end of the stack chain. If so, check to see that it's
     *    big enough for K. If not, or if we lack an existing stack
     *    segment altogether, allocate a new one of size K and chain
     *    it into the stack segments list for this proc.
     *
     *  - Transition to the new segment. This means memcopying the
     *    call region [fp, fp+n_call_bytes) into the new segment and
     *    adjusting the process' fp to point to the new base of the
     *    (transplanted) call region.
     *
     *
     *  K = max(min_stk_bytes, n_call_bytes + n_frame_bytes)
     *
     *  n_call_bytes = (arg_sz + abi_frame_base_sz)
     *
     *  n_frame_bytes = (new_frame_sz +
     *                   new_spill_sz +
     *                   new_call_sz +
     *                   abi_frame_base_sz +
     *                   proc_to_c_glue_sz)
     *
     *  proc_to_c_glue_sz = abi_frame_base_sz
     *
     * Note: there's a lot of stuff in K! You have to reserve enough
     * space for the new frame, enough space for the transplanted call,
     * enough space to *make* an outgoing call out of the new frame,
     * and enough space to perform a proc-to-C glue call to get back to
     * this function when you find you're out of stack again, mid-call.
     *
     */
    rust_rt_t *rt = proc->rt;
    stk_seg_t *nstk = proc->stk->next;
    if (nstk) {
        /* Figure out if the existing chunk is big enough. */
        size_t sz = nstk->limit - ((uintptr_t) &proc->stk->data[0]);
        if (sz < n_frame_bytes) {
            nstk = new_stk(rt, n_frame_bytes);
            nstk->next = proc->stk->next;
            nstk->next->prev = nstk;
        }
    } else {
        /* There is no existing next stack segment, grow. */
        nstk = new_stk(rt, n_frame_bytes);
    }
    I(rt, nstk);
    proc->stk->next = nstk;
    nstk->prev = proc->stk;
    /*
    uintptr_t i;
    for (i = proc->sp + n_call_bytes; i >= proc->sp; i -= sizeof(uintptr_t)) {
        uintptr_t val = *((uintptr_t*)i);
        printf("stk[0x%" PRIxPTR "] = 0x%" PRIxPTR "\n", i, val);
    }
    printf("transplant: n_call_bytes %d, n_frame_bytes %d\n", n_call_bytes, n_frame_bytes);
    */
    uintptr_t target = nstk->limit - n_call_bytes;
    memcpy((void*)target, (void*)proc->sp, n_call_bytes);
    proc->stk = nstk;
    proc->sp = target;
}

static rust_proc_t*
new_proc(rust_rt_t *rt, rust_prog_t *prog)
{
    /* FIXME: need to actually convey the proc internal-slots size to
       here. */
    rust_proc_t *proc = (rust_proc_t *)xcalloc(rt, sizeof(rust_proc_t) + 1024);
    logptr(rt, "new proc", (uintptr_t)proc);
    logptr(rt, "from prog", (uintptr_t)prog);
    logptr(rt, "init", (uintptr_t)prog->init_code);
    logptr(rt, "main", (uintptr_t)prog->main_code);
    logptr(rt, "fini", (uintptr_t)prog->fini_code);
    proc->prog = prog;
    proc->stk = new_stk(rt, 0);

    /*
     * Set sp to last uintptr_t-sized cell of segment
     * then align down to 16 boundary, to be safe-ish for
     * alignment (?)
     *
     * FIXME: actually convey alignment constraint here so
     * we're not just being conservative. I don't *think*
     * there are any platforms alive at the moment with
     * >16 byte alignment constraints, but this is sloppy.
     */
    proc->sp = proc->stk->limit;
    proc->sp -= sizeof(uintptr_t);
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
     * of retpc; that's intentional. The notion is that when we *first*
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
    proc->state = proc_state_running;
    proc->refcnt = 1;
    return proc;
}

static void
del_proc(rust_rt_t *rt, rust_proc_t *proc)
{
    xlog(rt, LOG_MEM|LOG_PROC,
         "del proc 0x%" PRIxPTR ", refcnt=%d",
         (uintptr_t)proc, proc->refcnt);

    /* FIXME: tighten this up, there are some more
       assertions that hold at proc-lifecycle events. */
    I(rt, proc->refcnt == 0 ||
      (proc->refcnt == 1 && proc == rt->root_proc));

    del_stk(rt, proc->stk);

    while (proc->chans) {
        rust_chan_t *c = proc->chans;
        HASH_DEL(proc->chans,c);
        fini_circ_buf(rt, &c->buf);
        xfree(rt, c);
    }

    xfree(rt, proc);
}


static ptr_vec_t*
get_state_vec(rust_rt_t *rt, proc_state_t state)
{
    switch (state) {
    case proc_state_running:
    case proc_state_calling_c:
        return &rt->running_procs;

    case proc_state_blocked_exited:
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
upcall_del_proc(rust_rt_t *rt, rust_proc_t *proc)
{
    xlog(proc->rt, LOG_PROC,
         "upcall del_proc(0x%" PRIxPTR "), refcnt=%d",
         proc, proc->refcnt);
    I(rt, n_live_procs(rt) > 0);
    /* FIXME: when we have dtors, this might force-execute the dtor
     * synchronously? Hmm. */
    remove_proc_from_state_vec(rt, proc);
    del_proc(rt, proc);
    xlog(rt, LOG_MEM|LOG_PROC,
         "proc 0x%" PRIxPTR " killed (and deleted)",
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
        return (rust_proc_t *)rt->running_procs.data[i];
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
    rust_rt_t *rt = (rust_rt_t *)srv->malloc(srv, sizeof(rust_rt_t));
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
        xlog(rt, LOG_PROC, "deleting live proc %" PRIdPTR, v->init-1);
        del_proc(rt, (rust_proc_t*) v->data[--(v->init)]);
    }
}

static void
del_rt(rust_rt_t *rt)
{
    xlog(rt, LOG_PROC, "deleting all running procs");
    del_all_procs(rt, &rt->running_procs);
    xlog(rt, LOG_PROC, "deleting all blocked procs");
    del_all_procs(rt, &rt->blocked_procs);

    xlog(rt, LOG_PROC, "deleting all dangling ports");
    /* FIXME: remove when port <-> proc linkage is obsolete. */
    while (rt->ports)
        del_port(rt, rt->ports);

    fini_ptr_vec(rt, &rt->running_procs);
    fini_ptr_vec(rt, &rt->blocked_procs);
    xfree(rt, rt);
}

/* Upcalls */

static void
upcall_log_uint32_t(rust_rt_t *rt, uint32_t i)
{
    xlog(rt, LOG_UPCALL|LOG_ULOG,
         "upcall log_uint32(0x%" PRIx32 " = %" PRId32 " = '%c')",
         i, i, (char)i);
}

static void
upcall_log_str(rust_rt_t *rt, char const *c)
{
    xlog(rt, LOG_UPCALL|LOG_ULOG,
         "upcall log_str(\"%s\")",
         c);
}

static void
upcall_trace_word(rust_rt_t *rt, uintptr_t i)
{
    xlog(rt, LOG_UPCALL|LOG_TRACE,
         "trace: 0x%" PRIxPTR "",
         i, i, (char)i);
}

static void
upcall_trace_str(rust_rt_t *rt, char const *c)
{
    xlog(rt, LOG_UPCALL|LOG_TRACE,
         "trace: %s",
         c);
}

static rust_port_t*
upcall_new_port(rust_rt_t *rt, rust_proc_t *proc, size_t unit_sz)
{
    rust_port_t *port = (rust_port_t *)xcalloc(rt, sizeof(rust_port_t));
    xlog(rt, LOG_UPCALL|LOG_MEM|LOG_COMM,
         "upcall new_port(proc=0x%" PRIxPTR ", unit_sz=%d) -> port=0x%"
         PRIxPTR, (uintptr_t)proc, unit_sz, (uintptr_t)port);
    port->proc = proc;

    /* FIXME: can remove the chaining-of-ports-to-rt when we have
     * unwinding / finishing working. */
    if (rt->ports)
        rt->ports->prev = port;
    port->next = rt->ports;
    rt->ports = port;

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
        del_port(rt, port);
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
        chan = (rust_chan_t *)xcalloc(rt, sizeof(rust_chan_t));
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
upcall_fail(rust_rt_t *rt, char const *expr, char const *file,
            size_t line)
{
    /* FIXME: throw, don't just exit. */
    xlog(rt, LOG_UPCALL, "upcall fail '%s', %s:%" PRIdPTR,
         expr, file, line);
    rt->srv->fatal(rt->srv, expr, file, line);
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

typedef uintptr_t (CDECL *native_0)();
typedef uintptr_t (CDECL *native_1)(uintptr_t);
typedef uintptr_t (CDECL *native_2)(uintptr_t, uintptr_t);
typedef uintptr_t (CDECL *native_3)(uintptr_t, uintptr_t, uintptr_t);
typedef uintptr_t (CDECL *native_4)(uintptr_t, uintptr_t, uintptr_t,
                                    uintptr_t);

typedef uintptr_t (CDECL *native_proc_0)(rust_proc_t *);
typedef uintptr_t (CDECL *native_proc_1)(rust_proc_t *, uintptr_t);
typedef uintptr_t (CDECL *native_proc_2)(rust_proc_t *, uintptr_t,
                                         uintptr_t);
typedef uintptr_t (CDECL *native_proc_3)(rust_proc_t *, uintptr_t,
                                         uintptr_t, uintptr_t);
typedef uintptr_t (CDECL *native_proc_4)(rust_proc_t *, uintptr_t,
                                         uintptr_t, uintptr_t, uintptr_t);

static void
upcall_native(rust_proc_t *proc,
              char const *sym, uintptr_t *retptr,
              uintptr_t *argv, uintptr_t nargs)
{
    rust_rt_t *rt = proc->rt;
    xlog(rt, LOG_UPCALL|LOG_MEM,
         "upcall native('%s', 0x%" PRIxPTR ", 0x%" PRIxPTR ", %" PRIdPTR ")",
         sym, (uintptr_t)retptr, (uintptr_t)argv, nargs);

    uintptr_t retval;
    uint8_t takes_proc = 0;
    /* FIXME: cache lookups. */
    uintptr_t fn = rt->srv->lookup(rt->srv, sym, &takes_proc);
    xlog(rt, LOG_UPCALL|LOG_MEM,
         "native '%s' resolved to 0x%" PRIxPTR,
         sym, fn);

    I(rt, fn);
    /* FIXME: nargs becomes argstr, incorporate libffi, etc. */
    switch (nargs) {
    case 0:
        if (takes_proc)
            retval = ((native_proc_0)fn)(proc);
        else
            retval = ((native_0)fn)();
        break;
    case 1:
        if (takes_proc)
            retval = ((native_proc_1)fn)(proc, argv[0]);
        else
            retval = ((native_1)fn)(argv[0]);
        break;
    case 2:
        if (takes_proc)
            retval = ((native_proc_2)fn)(proc, argv[0], argv[1]);
        else
            retval = ((native_2)fn)(argv[0], argv[1]);
        break;
    case 3:
        if (takes_proc)
            retval = ((native_proc_3)fn)(proc, argv[0], argv[1], argv[2]);
        else
            retval = ((native_3)fn)(argv[0], argv[1], argv[2]);
        break;
    case 4:
        if (takes_proc)
            retval = ((native_proc_4)fn)(proc, argv[0], argv[1], argv[2], argv[3]);
        else
            retval = ((native_4)fn)(argv[0], argv[1], argv[2], argv[3]);
        break;
    default:
        I(rt, 0);
        break;
    }
    if (retptr)
        *retptr = retval;
}

static size_t
next_power_of_two(size_t s)
{
    size_t tmp = s - 1;
    tmp |= tmp >> 1;
    tmp |= tmp >> 2;
    tmp |= tmp >> 4;
    tmp |= tmp >> 8;
    tmp |= tmp >> 16;
#if SIZE_MAX == UINT64_MAX
    tmp |= tmp >> 32;
#endif
    return tmp + 1;
}


static str_t*
upcall_new_str(rust_rt_t *rt, char const *s, size_t init)
{
    size_t alloc = next_power_of_two(init);
    str_t *st = (str_t*) xalloc(rt, sizeof(str_t) + alloc);
    st->refcnt = 1;
    st->init = init;
    st->alloc = alloc;
    if (s)
        memcpy(&st->data[0], s, init);
    xlog(rt, LOG_UPCALL|LOG_MEM,
         "upcall new_str('%s', %" PRIdPTR ") -> 0x%" PRIxPTR,
         s, init, st);
    return st;
}


static CDECL char const *str_buf(str_t *s);

static void
rust_main_loop(rust_prog_t *prog, rust_srv_t *srv);

struct Ticket {
    rust_prog_t *prog;
    rust_srv_t *srv;

    explicit Ticket(rust_prog_t *prog, rust_srv_t *srv) : prog(prog), srv(srv)
    {}

    ~Ticket()
    {}

    void operator delete(void *ptr)
    {
        rust_srv_t *srv = ((Ticket *)ptr)->srv;
        srv->free(srv, ptr);
    }
};

#ifdef __GNUC__
static void *rust_thread_start(void *ptr)
{
    /*
     * The thread that spawn us handed us a ticket. Read the ticket's content
     * and then deallocate it. Since thread creation is asynchronous, the other
     * thread can't do this for us.
     */
    Ticket *ticket = (Ticket *)ptr;
    rust_prog_t *prog = ticket->prog;
    rust_srv_t *srv = ticket->srv;
    delete ticket;

    /*
     * Start a new rust main loop for this thread.
     */
    rust_main_loop(prog, srv);

    return NULL;
}
#endif

static rust_proc_t *
upcall_new_thread(rust_rt_t *rt, rust_prog_t *prog)
{
    rust_srv_t *srv = rt->srv;
    /*
     * The ticket is not bound to the current runtime, so allocate directly from the
     * service.
     */
    Ticket *ticket = new (srv) Ticket(prog, srv);
    pthread_t thread;
    pthread_create(&thread, NULL, rust_thread_start, (void *)ticket);

    /*
     * Create a proxy proc that will represent the newly created thread in this runtime.
     * All communication will go through this proxy proc.
     */
    return NULL;
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
        upcall_log_str(proc->rt, str_buf((str_t*)args[0]));
        break;
    case upcall_code_new_proc:
        *((rust_proc_t**)args[0]) = new_proc(proc->rt, (rust_prog_t*)args[1]);
        break;
    case upcall_code_del_proc:
        upcall_del_proc(proc->rt, (rust_proc_t*)args[0]);
        break;
    case upcall_code_sched:
        add_proc_to_state_vec(proc->rt, (rust_proc_t*)args[0]);
        break;
    case upcall_code_fail:
        upcall_fail(proc->rt,
                    (char const *)args[0],
                    (char const *)args[1],
                    (size_t)args[2]);
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
    case upcall_code_native:
        upcall_native(proc, (char const *)args[0],
                      (uintptr_t*)args[1],
                      (uintptr_t*)args[2],
                      (uintptr_t)args[3]);
        break;
    case upcall_code_new_str:
        *((str_t**)args[0]) = upcall_new_str(proc->rt,
                                             (char const *)args[1],
                                             (size_t)args[2]);
        break;
    case upcall_code_grow_proc:
        upcall_grow_proc(proc, (size_t)args[0], (size_t)args[1]);
        break;
    case upcall_code_trace_word:
        upcall_trace_word(proc->rt, args[0]);
        break;
    case upcall_code_trace_str:
        upcall_trace_str(proc->rt, (char const *)args[0]);
        break;
    }
}

static void
rust_main_loop(rust_prog_t *prog, rust_srv_t *srv)
{
    rust_rt_t *rt;
    rust_proc_t *proc;
    rt = new_rt(srv);

    xlog(rt, LOG_RT, "control is in rust runtime library");
    logptr(rt, "root prog", (uintptr_t)prog);
    logptr(rt, "prog->init_code", (uintptr_t)prog->init_code);
    logptr(rt, "prog->main_code", (uintptr_t)prog->main_code);
    logptr(rt, "prog->fini_code", (uintptr_t)prog->fini_code);

    rt->root_proc = new_proc(rt, prog);
    add_proc_to_state_vec(rt, rt->root_proc);
    proc = sched(rt);

    logptr(rt, "root proc", (uintptr_t)proc);
    logptr(rt, "proc->sp", (uintptr_t)proc->sp);
    logptr(rt, "c_to_proc_glue", (uintptr_t)srv->c_to_proc_glue);

    while (proc) {

        xlog(rt, LOG_PROC, "activating proc 0x%" PRIxPTR,
             (uintptr_t)proc);

        proc->state = proc_state_running;
        srv->c_to_proc_glue(proc);

        xlog(rt, LOG_PROC,
             "returned from proc 0x%" PRIxPTR " in state '%s'",
             (uintptr_t)proc, state_names[proc->state]);
        /*
        xlog(rt, LOG_MEM,
             "sp:0x%" PRIxPTR ", "
             "stk:[0x%" PRIxPTR ", " "0x%" PRIxPTR "], "
             "stk->prev:0x%" PRIxPTR ", stk->next=0x%" PRIxPTR ", "
             "prev_sp:0x%" PRIxPTR ", " "prev_fp:0x%" PRIxPTR,
             proc->sp, (uintptr_t) &proc->stk->data[0], proc->stk->limit,
             proc->stk->prev, proc->stk->next,
             proc->stk->prev_sp, proc->stk->prev_fp);
        */
        I(rt, proc->sp >= (uintptr_t) &proc->stk->data[0]);
        I(rt, proc->sp < proc->stk->limit);

        switch ((proc_state_t) proc->state) {

        case proc_state_running:
            break;

        case proc_state_calling_c:
            handle_upcall(proc);
            if (proc->state == proc_state_calling_c)
                proc->state = proc_state_running;
            break;

        case proc_state_blocked_exited:
            /* When a proc exits *itself* we do not yet kill it; for
             * the time being we let it linger in the blocked-exiting
             * state, as someone else still "owns" it. */
            proc->state = proc_state_running;
            proc_state_transition(rt, proc,
                                  proc_state_running,
                                  proc_state_blocked_exited);
            break;

        case proc_state_blocked_reading:
        case proc_state_blocked_writing:
            I(rt, 0);
            break;
        }

        proc = sched(rt);
    }

    xlog(rt, LOG_RT, "finished main loop");
    del_rt(rt);
}

static void
srv_log(rust_srv_t *srv, char const *str)
{
    printf("rt: %s\n", str);
}

static void*
srv_malloc(rust_srv_t *srv, size_t sz)
{
    return malloc(sz);
}

static void*
srv_realloc(rust_srv_t *srv, void *p, size_t sz)
{
    return realloc(p, sz);
}

static void
srv_free(rust_srv_t *srv, void *p)
{
    free(p);
}

static void
srv_fatal(rust_srv_t *srv, char const *expr,
          char const *file, size_t line)
{
    char buf[1024];
    snprintf(buf, sizeof(buf), "fatal, '%s' failed, %s:%d",
             expr, file, (int)line);
    srv->log(srv, buf);
    exit(1);
}

/* Native builtins. */

static CDECL char const *
str_buf(str_t *s)
{
    return (char const *)&s->data[0];
}

static CDECL str_t*
implode(rust_proc_t *proc, vec_t *v)
{
    /*
     * We received a vec of u32 unichars. Implode to a string.
     * FIXME: this needs to do a proper utf-8 encoding.
     */
    size_t i;
    str_t *s;

    size_t init = v->init >> 2;
    s = upcall_new_str(proc->rt, NULL, init);

    uint32_t *src = (uint32_t*) &v->data[0];
    uint8_t *dst = &s->data[0];

    for (i = 0; i < init; ++i)
        *dst++ = *src;

    return s;
}


static uintptr_t
srv_lookup(rust_srv_t *srv, char const *sym, uint8_t *takes_proc)
{
    uintptr_t res;

    *takes_proc = 0;

    if (strcmp(sym, "str_buf") == 0) {
        res = (uintptr_t) &str_buf;
    } else if (strcmp(sym, "implode") == 0) {
        *takes_proc = 1;
        res = (uintptr_t) &implode;
    } else {
#ifdef __WIN32__
        /* FIXME: pass library name in as well. And use LoadLibrary not
         * GetModuleHandle, manually refcount. Oh, so much to do
         * differently. */
        HMODULE lib = GetModuleHandle("msvcrt.dll");
        if (!lib)
            srv->fatal(srv, "GetModuleHandle", __FILE__, __LINE__);
        res = (uintptr_t)GetProcAddress(lib, sym);
#else
        /* FIXME: dlopen, as above. */
        res = (uintptr_t)dlsym(RTLD_DEFAULT, sym);
#endif
    }
    if (!res)
        srv->fatal(srv, "srv->lookup", __FILE__, __LINE__);
    return res;
}

extern "C"
int CDECL
rust_start(rust_prog_t *prog, void CDECL (*c_to_proc_glue)(rust_proc_t*))
{
    rust_srv_t srv;
    srv.log = srv_log;
    srv.malloc = srv_malloc;
    srv.realloc = srv_realloc;
    srv.free = srv_free;
    srv.fatal = srv_fatal;
    srv.lookup = srv_lookup;
    srv.c_to_proc_glue = c_to_proc_glue;
    srv.user = NULL;
    rust_main_loop(prog, &srv);
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
