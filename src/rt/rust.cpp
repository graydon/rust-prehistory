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
#include <tchar.h>
#include <wincrypt.h>
}
#elif defined(__GNUC__)
 /*
  * Only for RTLD_DEFAULT, remove _GNU_SOURCE when that dies. We want
  * to be non-GNU-dependent.
  */
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <dlfcn.h>
#include <pthread.h>
#else
#error "Platform not supported."
#endif

#define I(rt, e) ((e) ? (void)0 :                           \
                  (rt)->srv->fatal(#e, __FILE__, __LINE__))

struct rust_proc;
struct rust_port;
struct rust_chan;
struct rust_q;
struct rust_rt;

struct rust_str;
struct rust_vec;

static uint32_t const LOG_ALL = 0xffffffff;
static uint32_t const LOG_ERR =        0x1;
static uint32_t const LOG_MEM =        0x2;
static uint32_t const LOG_COMM =       0x4;
static uint32_t const LOG_PROC =       0x8;
static uint32_t const LOG_UPCALL =    0x10;
static uint32_t const LOG_RT =        0x20;
static uint32_t const LOG_ULOG =      0x40;
static uint32_t const LOG_TRACE =     0x80;

#ifdef __GNUC__
#define LOG_UPCALL_ENTRY(proc)                                          \
    (proc)->rt->log(LOG_UPCALL,                                         \
                    "upcall proc: 0x%" PRIxPTR                          \
                    " retpc: 0x%" PRIxPTR,                              \
                    (proc), __builtin_return_address(0))
#else
#define LOG_UPCALL_ENTRY(rt) ((void)0)                                  \
    (proc)->rt->log(LOG_UPCALL,                                         \
                    "upcall proc: 0x%" PRIxPTR                          \
                    (proc))
#endif

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

struct stk_seg {
    unsigned int valgrind_id;
    uintptr_t limit;
    uint8_t data[];
};

typedef enum {
    proc_state_running,
    proc_state_blocked_reading,
    proc_state_blocked_writing,
    proc_state_blocked_waiting,
    proc_state_dead,
} proc_state_t;

static char const * const state_names[] =
    {
        "running",
        "blocked_reading",
        "blocked_writing",
        "blocked_waiting",
        "dead",
    };

typedef enum {
    abi_code_cdecl = 0,
    abi_code_rust = 1
} abi_t;

struct global_glue_fns {
    void CDECL (*c_to_proc_glue)(rust_proc *);
    uintptr_t main_exit_proc_glue;
    uintptr_t unwind_glue;
    uintptr_t yield_glue;
};

struct frame_glue_fns {
    uintptr_t mark_glue;
    uintptr_t drop_glue;
    uintptr_t reloc_glue;
};

/* FIXME: change ptr_vec and circ_buf to use flexible-array element
   rather than pointer-to-buf-at-end. */

template <typename T>
class ptr_vec {
    static const size_t INIT_SIZE = 8;

    rust_rt *rt;
    size_t alloc;
    size_t fill;
    T **data;

public:
    ptr_vec(rust_rt *rt);
    ~ptr_vec();

    size_t length() {
        return fill;
    }

    T *& operator[](ssize_t offset) {
        return data[offset];
    }

    void push(T *p);
    T *pop();
    void trim(size_t fill);
    void swapdel(T* p);
};

struct circ_buf {
    static const size_t INIT_CIRC_BUF_UNITS = 8;
    static const size_t MAX_CIRC_BUF_SIZE = 1 << 24;

    rust_rt *rt;
    size_t alloc;
    size_t unit_sz;
    size_t next;
    size_t unread;
    uint8_t *data;

    circ_buf(rust_rt *rt, size_t unit_sz);
    ~circ_buf();
    void operator delete(void *ptr);

    void transfer(void *dst);
    void push(void *src);
    void shift(void *dst);
};

// Interrupt transparent queue, Schoen et. al, "On Interrupt-Transparent Synchronization
// in an Embedded Object-Oriented Operating System", 2000. enqueue() is allowed to
// interrupt enqueue()( and dequeue(), however, dequeue() is not allowed to interrupt
// itself.

struct itq_chain {
    itq_chain *next;
};

class itq : public itq_chain {
    itq_chain *tail;
public:
    itq();
    void enqueue(itq_chain *item);
    itq_chain *dequeue();
};

itq::itq() :
    tail(this)
{
}

void
itq::enqueue(itq_chain *item)
{
    item->next = (itq_chain *)0;
    itq_chain *last = tail;
    tail = item;
    while (last->next)
        last = last->next;
    last->next = item;
}

itq_chain *
itq::dequeue()
{
    itq_chain *item = next;
    if (item && !(next = item->next)) {
        tail = (itq_chain *)this;
        if (item->next) {
            itq_chain *lost = item->next;
            itq_chain *help;
            do {
                help = lost->next;
                enqueue(lost);
            } while ((lost = help) != (itq_chain *)0);
        }
    }
    return item;
}

/* Rust types vec and str look identical from our perspective. */

struct rust_vec {
    size_t refcount;
    size_t alloc;
    size_t fill;
    uint8_t data[];
};

struct rust_str {
    size_t refcnt;
    size_t alloc;
    size_t fill;
    uint8_t data[];         /* C99 "flexible array" element. */
};

struct rust_rt {
    rust_srv *srv;
    global_glue_fns *global_glue;
    size_t &live_allocs;
    uint32_t logbits;
    ptr_vec<rust_proc> running_procs;
    ptr_vec<rust_proc> blocked_procs;
    ptr_vec<rust_proc> dead_procs;
    randctx rctx;
    rust_proc *root_proc;
    rust_proc *curr_proc;
    ptr_vec<rust_port> ports; // bug 541584
    ptr_vec<rust_chan> chans;
    int rval;

    rust_rt(rust_srv *srv, global_glue_fns *global_glue, size_t &live_allocs);
    ~rust_rt();

    void activate(rust_proc *proc);
    void log(uint32_t logbit, char const *fmt, ...);
    void logptr(char const *msg, uintptr_t ptrval);
    template<typename T>
    void logptr(char const *msg, T* ptrval);
    void fail();
    void *malloc(size_t sz);
    void *calloc(size_t sz);
    void *realloc(void *data, size_t sz);
    void free(void *p);

#ifdef __WIN32__
    void win32_require(LPCTSTR fn, BOOL ok);
#endif

    size_t n_live_procs();
    void reap_dead_procs();
    rust_proc *sched();
};

inline void *operator new(size_t sz, rust_rt *rt) {
    return rt->malloc(sz);
}

inline void *operator new[](size_t sz, rust_rt *rt) {
    return rt->malloc(sz);
}

inline void *operator new(size_t sz, rust_rt &rt) {
    return rt.malloc(sz);
}

inline void *operator new[](size_t sz, rust_rt &rt) {
    return rt.malloc(sz);
}

/*
 * "Simple" precise, mark-sweep, single-generation GC.
 *
 *  - Every value (transitively) containing to a mutable slot
 *    is a gc_val.
 *
 *  - gc_vals come from the same simple allocator as all other
 *    values but undergo different storage management.
 *
 *  - Every frame has a frame_glue_fns pointer in its fp[-1] slot,
 *    written on function-entry.
 *
 *  - Like gc_vals have *three* extra words at their head, not one.
 *
 *  - A pointer to a gc_val, however, points to the third of these
 *    three words. So a certain quantity of code can treat gc_vals the
 *    same way it would treat refcounted exterior vals.
 *
 *  - The first word at the head of a gc_val is used as a refcount, as
 *    in non-gc allocations.
 *
 *  - The second word at the head of a gc_val is a pointer to a sweep
 *    function, with the low bit of that pointer used as a mark bit.
 *
 *  - The third word at the head of a gc_val is a linked-list pointer
 *    to the gc_val that was allocated (temporally) just before
 *    it. Following this list traces through all the currently active
 *    gc_vals in a proc.
 *
 *  - The proc has a gc_alloc_chain field that points to the most-recent
 *    gc_val allocated.
 *
 *  - GC proceeds as follows:
 *
 *    - The proc calls frame_glue_fns.mark_glue(fp) which marks the
 *      frame and then loops, walking down the frame chain. This marks
 *      all the frames with GC roots (each of those functions in turn
 *      may recursively call into the GC graph, that's for the mark
 *      glue to decide).
 *
 *    - The proc then asks its runtime for its gc_alloc_chain.
 *
 *    - The proc calls
 *
 *        (~1 & gc_alloc_chain[1])(gc_ptr=&gc_alloc_chain)
 *
 *      which sweeps the allocation. Sweeping involves checking to see
 *      if the gc_val at *gc_ptr was marked. If not, it loads
 *      &(*gc_ptr)[2] into tmp, calls drop_ty(*gc_ptr) then
 *      free(*gc_ptr), then gc_ptr=tmp and recurs. If marked, it loads
 *      &(*gc_ptr[2]) into gc_ptr and recurs. The key point is that it
 *      has to call drop_ty, to drop outgoing links into the refcount
 *      graph (and possibly run dtors).
 *
 *    - Note that there is no "special gc state" at work here; the
 *      proc looks like it's running normal code that happens to not
 *      perform any gc_val allocation. Mark-bit twiddling is
 *      open-coded into all the mark functions, which know their
 *      contents; we only have to do O(frames) indirect calls to mark,
 *      the rest are static. Sweeping costs O(gc-heap) indirect calls,
 *      unfortunately, because the set of sweep functions to call is
 *      arbitrary based on allocation order.
 *
 */

struct rust_proc {
    // fields known to the compiler
    rust_rt *rt;
    stk_seg *stk;
    uintptr_t runtime_sp;      // runtime sp while proc running.
    uintptr_t rust_sp;         // saved sp when not running.
    size_t refcnt;
    uintptr_t gc_alloc_chain;  // linked list of GC allocations.

    // fields known only to the runtime
    proc_state_t state;
    uintptr_t fn;
    rust_q *queues;
    uintptr_t* dptr;           // rendezvous pointer for send/recv
    rust_proc *spawner;        // parent-link
    ptr_vec<rust_proc> waiting_procs;
    size_t idx;

    rust_proc(rust_rt *rt,
              rust_proc *spawner,
              uintptr_t exit_proc_glue,
              uintptr_t spawnee_fn,
              size_t callsz);
    ~rust_proc();

    void operator delete(void *ptr);

    void check_active() { I(rt, rt->curr_proc == this); }
    void check_suspended() { I(rt, rt->curr_proc != this); }

    // Swap in some glue code to run when we have returned to the
    // proc's context (assuming we're the active proc).
    void run_after_return(size_t nargs, uintptr_t glue);

    // Swap in some glue code to run when we're next activated
    // (assuming we're the suspended proc).
    void run_on_resume(uintptr_t glue);

    // Save callee-saved registers and return to the main loop.
    void yield(size_t nargs);

    // Fail this proc (assuming caller-on-stack is different proc).
    void kill();

    // Fail self, assuming caller-on-stack is this proc.
    void fail(size_t nargs);

    // Notify procs waiting for us that we are about to die.
    void notify_waiting_procs();

    uintptr_t get_fp();
    uintptr_t get_previous_fp(uintptr_t fp);
    frame_glue_fns *get_frame_glue_fns(uintptr_t fp);
};

struct rust_port {
    // fields known to the compiler
    size_t refcnt;

    // fields known only to the runtime
    rust_proc *proc; // port might outlive proc, so don't rely on it in destructor
    rust_rt *rt;
    size_t unit_sz;
    ptr_vec<rust_q> writers;
    size_t idx; // bug 541584

    rust_port(rust_proc *proc, size_t unit_sz);
    ~rust_port();

    void operator delete(void *ptr)
    {
        rust_rt *rt = ((rust_port *)ptr)->rt;
        rt->free(ptr);
    }
};

struct rust_chan {
    // fields known to the compiler
    size_t refcnt;

    // fields known only to the runtime
    rust_rt* rt;
    rust_port* port;
    size_t idx; // bug 541584

    // queue the chan is sending to (resolved lazily)
    rust_proc *proc;
    rust_q* q;

    rust_chan(rust_proc *proc, rust_port *port);
    ~rust_chan();

    void operator delete(void *ptr)
    {
        rust_rt *rt = ((rust_chan *)ptr)->rt;
        rt->free(ptr);
    }
};

/*
 * The value held in a rust 'chan' slot is actually a rust_port*,
 * with liveness of the chan indicated by weak_refcnt.
 *
 * Inside each proc, there is a uthash hashtable that maps ports to
 * rust_q* values, below. The table enforces uniqueness of the
 * queues: one proc has exactly one outgoing queue (buffer) for
 * each port.
 */

struct rust_q {
    UT_hash_handle hh;
    rust_proc *proc;      // Proc owning this chan.
    rust_port *port;      // Port chan is connected to, NULL if disconnected.
    bool sending;         // Whether we're in a port->writers vec.
    size_t idx;           // Index in the port->writers vec.
    rust_proc *blocked;   // Proc to wake on flush, NULL if nonblocking.
    circ_buf buf;

    rust_q(rust_proc *proc, rust_port *port);
    ~rust_q();

    void operator delete(void *ptr);
    void disconnect();
};

/* Utility type: pointer-vector. */

template <typename T>
ptr_vec<T>::ptr_vec(rust_rt *rt) :
    rt(rt),
    alloc(INIT_SIZE),
    fill(0),
    data(new (rt) T*[alloc])
{
    I(rt, data);
    rt->log(LOG_MEM,
            "new ptr_vec(data=0x%" PRIxPTR ") -> 0x%" PRIxPTR,
            (uintptr_t)data, (uintptr_t)this);
}

template <typename T>
ptr_vec<T>::~ptr_vec()
{
    I(rt, data);
    rt->log(LOG_MEM,
            "~ptr_vec 0x%" PRIxPTR ", data=0x%" PRIxPTR,
            (uintptr_t)this, (uintptr_t)data);
    I(rt, fill == 0);
    rt->free(data);
}

template <typename T>
void
ptr_vec<T>::push(T *p)
{
    I(rt, data);
    I(rt, fill <= alloc);
    if (fill == alloc) {
        alloc *= 2;
        data = (T **)rt->realloc(data, alloc * sizeof(T*));
        I(rt, data);
    }
    I(rt, fill < alloc);
    p->idx = fill;
    data[fill++] = p;
}

template <typename T>
T *
ptr_vec<T>::pop()
{
    return data[--fill];
}

template <typename T>
void
ptr_vec<T>::trim(size_t sz)
{
    I(rt, data);
    if (sz <= (alloc / 4) &&
        (alloc / 2) >= INIT_SIZE) {
        alloc /= 2;
        I(rt, alloc >= fill);
        data = (T **)rt->realloc(data, alloc * sizeof(T*));
        I(rt, data);
    }
}

template <typename T>
void
ptr_vec<T>::swapdel(T *item)
{
    /* Swap the endpoint into i and decr fill. */
    I(rt, data);
    I(rt, fill > 0);
    I(rt, item->idx < fill);
    fill--;
    if (fill > 0) {
        T *subst = data[fill];
        size_t idx = item->idx;
        data[idx] = subst;
        subst->idx = idx;
    }
}

/* Utility type: circular buffer. */

circ_buf::circ_buf(rust_rt *rt, size_t unit_sz) :
    rt(rt),
    alloc(INIT_CIRC_BUF_UNITS * unit_sz),
    unit_sz(unit_sz),
    next(0),
    unread(0),
    data((uint8_t *)rt->calloc(alloc))
{
    I(rt, unit_sz);
    rt->log(LOG_MEM|LOG_COMM,
            "new circ_buf(alloc=%d, unread=%d) -> circ_buf=0x%" PRIxPTR,
            alloc, unread, this);
    I(rt, data);
}

circ_buf::~circ_buf()
{
    rt->log(LOG_MEM|LOG_COMM,
            "~circ_buf 0x%" PRIxPTR,
            this);
    I(rt, data);
    // I(rt, unread == 0);
    rt->free(data);
}

void
circ_buf::operator delete(void *ptr)
{
    rust_rt *rt = ((circ_buf *)ptr)->rt;
    rt->free(ptr);
}

void
circ_buf::transfer(void *dst)
{
    size_t i;
    uint8_t *d = (uint8_t *)dst;
    I(rt, dst);
    for (i = 0; i < unread; i += unit_sz)
        memcpy(&d[i], &data[next + i % alloc], unit_sz);
}

void
circ_buf::push(void *src)
{
    size_t i;
    void *tmp;

    I(rt, src);
    I(rt, unread <= alloc);

    /* Grow if necessary. */
    if (unread == alloc) {
        I(rt, alloc <= MAX_CIRC_BUF_SIZE);
        tmp = rt->malloc(alloc << 1);
        transfer(tmp);
        alloc <<= 1;
        rt->free(data);
        data = (uint8_t *)tmp;
    }

    rt->log(LOG_MEM|LOG_COMM,
            "circ buf push, unread=%d, alloc=%d, unit_sz=%d",
            unread, alloc, unit_sz);

    I(rt, unread < alloc);
    I(rt, unread + unit_sz <= alloc);

    i = (next + unread) % alloc;
    memcpy(&data[i], src, unit_sz);

    rt->log(LOG_MEM|LOG_COMM, "pushed data at index %d", i);
    unread += unit_sz;
}

void
circ_buf::shift(void *dst)
{
    size_t i;
    void *tmp;

    I(rt, dst);
    I(rt, unit_sz > 0);
    I(rt, unread >= unit_sz);
    I(rt, unread <= alloc);
    I(rt, data);
    i = next;
    memcpy(dst, &data[i], unit_sz);
    rt->log(LOG_MEM|LOG_COMM, "shifted data from index %d", i);
    unread -= unit_sz;
    next += unit_sz;
    I(rt, next <= alloc);
    if (next == alloc)
        next = 0;

    /* Shrink if necessary. */
    if (alloc >= INIT_CIRC_BUF_UNITS * unit_sz &&
        unread <= alloc / 4) {
        tmp = rt->malloc(alloc / 2);
        transfer(tmp);
        alloc >>= 1;
        rt->free(data);
        data = (uint8_t *)tmp;
    }
}

/* Ports */

rust_port::rust_port(rust_proc *proc, size_t unit_sz)
    : refcnt(1),
      proc(proc),
      rt(proc->rt),
      unit_sz(unit_sz),
      writers(proc->rt)
{
    rt->log(LOG_MEM|LOG_COMM,
            "new rust_port(proc=0x%" PRIxPTR ", unit_sz=%d) -> port=0x%"
            PRIxPTR, (uintptr_t)proc, unit_sz, (uintptr_t)this);
    rt->ports.push(this);
}

rust_port::~rust_port()
{
    rt->log(LOG_COMM|LOG_MEM,
            "~rust_port 0x%" PRIxPTR,
            (uintptr_t)this);
    /* FIXME: need to force-fail all writers waiting to send to us. */
    for (size_t i = 0; i < writers.length(); ++i)
        writers[i]->disconnect();
    // FIXME (bug 541584): can remove the ports list when we have
    // unwinding / finishing working.
    rt->ports.swapdel(this);
}

rust_chan::rust_chan(rust_proc *proc, rust_port *port) :
    refcnt(1),
    rt(proc->rt),
    port(port),
    proc(NULL),
    q(NULL)
{
    ++(port->refcnt);
    rt->chans.push(this);
}

rust_chan::~rust_chan()
{
    rt->chans.swapdel(this);
}

/* Outgoing message queues */

rust_q::rust_q(rust_proc *proc, rust_port *port)
    : proc(proc),
      port(port),
      sending(false),
      idx(0),
      blocked(NULL),
      buf(port->proc->rt, port->unit_sz)
{
    rust_rt *rt = proc->rt;
    rt->log(LOG_MEM|LOG_COMM,
            "new rust_q(port=0x%" PRIxPTR ") -> 0x%" PRIxPTR,
            port, (uintptr_t)this);
}

void
rust_q::disconnect()
{
    I(proc->rt, sending);
    I(proc->rt, port);
    sending = false;
    port = NULL;
}

rust_q::~rust_q()
{
    rust_rt *rt = proc->rt;
    if (sending) {
        I(rt, port);
        port->writers.swapdel(this);
    }
    rt->log(LOG_MEM|LOG_COMM,
            "~rust_q 0x%" PRIxPTR, (uintptr_t)this);
}

void
rust_q::operator delete(void *ptr)
{
    rust_rt *rt = ((rust_q *)ptr)->proc->rt;
    rt->free(ptr);
}


/* Stacks */

static size_t const min_stk_bytes = 0x300;

static stk_seg*
new_stk(rust_rt *rt, size_t minsz)
{
    if (minsz < min_stk_bytes)
        minsz = min_stk_bytes;
    size_t sz = sizeof(stk_seg) + minsz;
    stk_seg *stk = (stk_seg *)rt->malloc(sz);
    rt->logptr("new stk", (uintptr_t)stk);
    memset(stk, 0, sizeof(stk_seg));
    stk->limit = (uintptr_t) &stk->data[minsz];
    rt->logptr("stk limit", stk->limit);
    stk->valgrind_id =
        VALGRIND_STACK_REGISTER(&stk->data[0],
                                &stk->data[minsz]);
    return stk;
}

static void
del_stk(rust_rt *rt, stk_seg *stk)
{
    VALGRIND_STACK_DEREGISTER(stk->valgrind_id);
    rt->logptr("freeing stk segment", (uintptr_t)stk);
    rt->free(stk);
}

/* Processes */

/* FIXME (bug 541585): ifdef by platform. This is getting absurdly x86-specific. */
size_t const n_callee_saves = 4;
size_t const callee_save_fp = 0;

static uintptr_t
align_down(uintptr_t sp)
{
    // There is no platform we care about that needs more than a 16-byte alignment.
    return sp & ~(16 - 1);
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

extern "C" void
upcall_grow_proc(rust_proc *proc, size_t n_frame_bytes)
{
    LOG_UPCALL_ENTRY(proc);

    rust_rt *rt = proc->rt;
    stk_seg *old_stk = proc->stk;
    uintptr_t old_top = (uintptr_t) old_stk->limit;
    uintptr_t old_bottom = (uintptr_t) &old_stk->data[0];
    uintptr_t rust_sp_disp = old_top - proc->rust_sp;
    size_t ssz = old_top - old_bottom;
    rt->log(LOG_MEM|LOG_PROC, "upcall_grow_proc old size %d bytes (old lim: 0x%" PRIxPTR ")",
            ssz, old_top);
    ssz *= 2;
    if (ssz < n_frame_bytes)
        ssz = n_frame_bytes;
    ssz = next_power_of_two(ssz);

    rt->log(LOG_MEM|LOG_PROC, "upcall_grow_proc growing stk 0x%" PRIxPTR " to %d bytes",
            old_stk, ssz);

    stk_seg *stk = new_stk(rt, ssz);
    uintptr_t new_bottom = (uintptr_t) &stk->data[0];
    uintptr_t new_top = (uintptr_t) &stk->data[ssz];
    size_t n_copy = old_top - old_bottom;
    rt->log(LOG_MEM|LOG_PROC,
            "copying %d bytes of stack from [0x%" PRIxPTR ", 0x%" PRIxPTR "]"
            " to [0x%" PRIxPTR ", 0x%" PRIxPTR "]",
            n_copy,
            old_bottom, old_bottom + n_copy,
            new_top - n_copy, new_top);

    memcpy((void*)(new_top - n_copy), (void*)old_bottom, n_copy);

    stk->valgrind_id = VALGRIND_STACK_REGISTER(new_bottom, new_top);
    stk->limit = new_top;
    proc->stk = stk;
    proc->rust_sp = new_top - rust_sp_disp;

    rt->log(LOG_MEM|LOG_PROC, "processing relocations");

    // FIXME (bug 541586): this is the most ridiculously crude relocation scheme ever.
    // Try actually, you know, writing out reloc descriptors?
    size_t n_relocs = 0;
    for (uintptr_t* p = (uintptr_t*)(new_top - n_copy); p < (uintptr_t*)new_top; ++p) {
        if (old_bottom <= *p && *p < old_top) {
            //rt->log(LOG_MEM, "relocating pointer 0x%" PRIxPTR " by %d bytes",
            //        *p, (new_top - old_top));
            n_relocs++;
            *p += (new_top - old_top);
        }
    }
    rt->log(LOG_MEM|LOG_PROC, "processed %d relocations", n_relocs);

    del_stk(rt, old_stk);
    rt->logptr("grown stk limit", new_top);
}

rust_proc::rust_proc(rust_rt *rt,
                     rust_proc *spawner,
                     uintptr_t exit_proc_glue,
                     uintptr_t spawnee_fn,
                     size_t callsz)
    :
      rt(rt),
      stk(new_stk(rt, 0)),
      runtime_sp(0),
      rust_sp(stk->limit),
      refcnt(1),
      gc_alloc_chain(0),
      state(proc_state_running),
      fn(spawnee_fn),
      queues(NULL),
      dptr(0),
      spawner(spawner),
      waiting_procs(rt),
      idx(0)
{
    rt->logptr("new proc", (uintptr_t)this);
    rt->logptr("exit-proc glue", exit_proc_glue);
    rt->logptr("from spawnee", spawnee_fn);

    // Set sp to last uintptr_t-sized cell of segment and align down (since its a stack).
    rust_sp -= sizeof(uintptr_t);
    rust_sp = align_down(rust_sp);

    // Begin synthesizing frames. There are two: a "fully formed"
    // exit-proc frame at the top of the stack -- that pretends to be
    // mid-execution -- and a just-starting frame beneath it that
    // starts executing the first instruction of the spawnee. The
    // spawnee *thinks* it was called by the exit-proc frame above
    // it. It wasn't; we put that fake frame in place here, but the
    // illusion is enough for the spawnee to return to the exit-proc
    // frame when it's done, and exit.
    uintptr_t *spp = (uintptr_t *)rust_sp;

    // The exit_proc_glue frame we synthesize above the frame we activate:
    *spp-- = (uintptr_t) this;       // proc
    *spp-- = (uintptr_t) 0;          // output
    *spp-- = (uintptr_t) 0;          // retpc
    for (size_t j = 0; j < n_callee_saves; ++j) {
        *spp-- = 0;
    }

    // We want 'frame_base' to point to the last callee-save in this
    // (exit-proc) frame, because we're going to inject this
    // frame-pointer into the callee-save frame pointer value in the
    // *next* (spawnee) frame. A cheap trick, but this means the
    // spawnee frame will restore the proper frame pointer of the glue
    // frame as it runs its epilogue.
    uintptr_t frame_base = (uintptr_t) (spp+1);

    *spp-- = (uintptr_t) 0;          // frame_glue_fns

    // Copy args from spawner to spawnee.
    if (spawner)  {
        uintptr_t *src = (uintptr_t*) spawner->rust_sp;
        src += 1;                  // spawn-call output slot
        src += 1;                  // spawn-call proc slot
        // Memcpy all but the proc and output pointers
        callsz -= (2 * sizeof(uintptr_t));
        spp = (uintptr_t*) (((uintptr_t)spp) - callsz);
        memcpy(spp, src, callsz);

        // Move sp down to point to proc cell.
        spp--;
    } else {
        // We're at root, starting up.
        I(rt, callsz==0);
    }

    // The *implicit* incoming args to the spawnee frame we're
    // activating: FIXME (bug 541587): wire up output-address properly
    // so spawnee can write a return value.
    *spp-- = (uintptr_t) this;            // proc
    *spp-- = (uintptr_t) 0;               // output addr
    *spp-- = (uintptr_t) exit_proc_glue;  // retpc

    // The context the c_to_proc_glue needs to switch stack.
    *spp-- = (uintptr_t) spawnee_fn;      // instruction to start at
    for (size_t j = 0; j < n_callee_saves; ++j) {
        // callee-saves to carry in when we activate
        if (j == callee_save_fp)
            *spp-- = frame_base;
        else
            *spp-- = NULL;
    }

    // Back up one, we overshot where sp should be.
    rust_sp = (uintptr_t) (spp+1);
}


rust_proc::~rust_proc()
{
    rt->log(LOG_MEM|LOG_PROC,
            "~rust_proc 0x%" PRIxPTR ", refcnt=%d",
            (uintptr_t)this, refcnt);

    /*
    for (uintptr_t fp = get_fp(); fp; fp = get_previous_fp(fp)) {
        frame_glue_fns *glue_fns = get_frame_glue_fns(fp);
        rt->log(LOG_MEM|LOG_PROC,
                "~rust_proc, frame fp=0x%" PRIxPTR ", glue_fns=0x%" PRIxPTR,
                fp, glue_fns);
        if (glue_fns) {
            rt->log(LOG_MEM|LOG_PROC, "~rust_proc, mark_glue=0x%" PRIxPTR, glue_fns->mark_glue);
            rt->log(LOG_MEM|LOG_PROC, "~rust_proc, drop_glue=0x%" PRIxPTR, glue_fns->drop_glue);
            rt->log(LOG_MEM|LOG_PROC, "~rust_proc, reloc_glue=0x%" PRIxPTR, glue_fns->reloc_glue);
        }
    }
    */

    /* FIXME: tighten this up, there are some more
       assertions that hold at proc-lifecycle events. */
    I(rt, refcnt == 0 ||
      (refcnt == 1 && this == rt->root_proc));

    del_stk(rt, stk);

    while (queues) {
        rust_q *q = queues;
        HASH_DEL(queues, q);
        delete q;
    }
}

void
push_onto_thread_stack(uintptr_t &sp, uintptr_t value)
{
    asm("xchgl %0, %%esp\n"
        "push %2\n"
        "xchgl %0, %%esp\n"
        : "=r" (sp)
        : "0" (sp), "r" (value)
        : "eax");
}

void
rust_proc::run_after_return(size_t nargs, uintptr_t glue)
{
    // This is only safe to call if we're the currently-running proc.
    check_active();

    uintptr_t sp = runtime_sp;

    // The compiler reserves nargs + 1 word for oldsp on the stack and then aligns it
    sp = align_down(sp - nargs * sizeof(uintptr_t));

    uintptr_t *retpc = ((uintptr_t *) sp) - 1;
    rt->log(LOG_PROC|LOG_MEM,
            "run_after_return: overwriting retpc=0x%" PRIxPTR
            " @ runtime_sp=0x%" PRIxPTR
            " with glue=0x%" PRIxPTR,
            *retpc, sp, glue);

    // Debugging aid for finding off-by-ones.
    /*
    for (int i = -4; i < 4; ++i) {
        uintptr_t *spp = (uintptr_t*)sp;
        rt->log(LOG_MEM,
                "sp[%d] (0x%" PRIxPTR ") = 0x%" PRIxPTR,
                i, spp+i, spp[i]);
    }
    */

    // Move the current return address (which points into rust code) onto the rust
    // stack and pretend we just called into the glue.
    push_onto_thread_stack(rust_sp, *retpc);
    *retpc = glue;
}

void
rust_proc::run_on_resume(uintptr_t glue)
{
    // This is only safe to call if we're suspended.
    check_suspended();

    // Inject glue as resume address in the suspended frame.
    uintptr_t* rsp = (uintptr_t*) rust_sp;
    rsp += n_callee_saves;
    rt->log(LOG_PROC|LOG_MEM,
            "run_on_resume: overwriting retpc=0x%" PRIxPTR
            " @ rust_sp=0x%" PRIxPTR
            " with glue=0x%" PRIxPTR,
            *rsp, rsp, glue);
    *rsp = glue;
}

void
rust_proc::yield(size_t nargs)
{
    rt->log(LOG_PROC,
            "proc 0x%" PRIxPTR " yielding", this);
    run_after_return(nargs, rt->global_glue->yield_glue);
}

void
rust_proc::operator delete(void *ptr)
{
    rust_rt *rt = ((rust_proc *)ptr)->rt;
    rt->free(ptr);
}

static inline uintptr_t
get_callee_save_fp(uintptr_t *top_of_callee_saves)
{
    return top_of_callee_saves[n_callee_saves - (callee_save_fp + 1)];
}

static void
proc_state_transition(rust_rt *rt,
                      rust_proc *proc,
                      proc_state_t src,
                      proc_state_t dst);

void
rust_proc::kill() {
    // Note the distinction here: kill() is when you're in an upcall
    // from process A and want to force-fail process B, you do B->kill().
    // If you want to fail yourself you do self->fail(upcall_nargs).
    rt->log(LOG_PROC, "killing proc 0x%" PRIxPTR, this);
    // Unblock the proc so it can unwind.
    proc_state_transition(rt, this, state, proc_state_running);
    if (this == rt->root_proc)
        rt->fail();
    run_on_resume(rt->global_glue->unwind_glue);
}

void
rust_proc::fail(size_t nargs) {
    // See note in ::kill() regarding who should call this.
    rt->log(LOG_PROC, "proc 0x%" PRIxPTR " failing", this);
    // Unblock the proc so it can unwind.
    proc_state_transition(rt, this, state, proc_state_running);
    if (this == rt->root_proc)
        rt->fail();
    run_after_return(nargs, rt->global_glue->unwind_glue);
    if (spawner) {
        rt->log(LOG_PROC,
                "proc 0x%" PRIxPTR
                " propagating failure to parent 0x%" PRIxPTR,
                this, spawner);
        spawner->kill();
    }
}

void
rust_proc::notify_waiting_procs()
{
    while (waiting_procs.length() > 0) {
        rust_proc *p = waiting_procs.pop();
        proc_state_t state = p->state;
        if (state == proc_state_blocked_waiting) {
            proc_state_transition(rt, p,
                                  proc_state_blocked_waiting,
                                  proc_state_running);
        } else {
            I(rt,
              state != proc_state_running &&
              state != proc_state_blocked_reading &&
              state != proc_state_blocked_writing);
        }
    }
}

uintptr_t
rust_proc::get_fp() {
    // sp in any suspended proc points to the last callee-saved reg on
    // the proc stack.
    return get_callee_save_fp((uintptr_t*)rust_sp);
}

uintptr_t
rust_proc::get_previous_fp(uintptr_t fp) {
    // fp happens to, coincidentally (!) also point to the last
    // callee-save on the proc stack.
    return get_callee_save_fp((uintptr_t*)fp);
}

frame_glue_fns*
rust_proc::get_frame_glue_fns(uintptr_t fp) {
    fp -= sizeof(uintptr_t);
    return *((frame_glue_fns**) fp);
}

static ptr_vec<rust_proc>*
get_state_vec(rust_rt *rt, proc_state_t state)
{
    switch (state) {
    case proc_state_running:
        return &rt->running_procs;

    case proc_state_blocked_reading:
    case proc_state_blocked_writing:
    case proc_state_blocked_waiting:
        return &rt->blocked_procs;

    case proc_state_dead:
        return &rt->dead_procs;
    }

    I(rt, 0);
    return NULL;
}

static ptr_vec<rust_proc>*
get_proc_vec(rust_rt *rt, rust_proc *proc)
{
    return get_state_vec(rt, proc->state);
}

static void
add_proc_state_vec(rust_rt *rt, rust_proc *proc)
{
    ptr_vec<rust_proc> *v = get_proc_vec(rt, proc);
    rt->log(LOG_MEM|LOG_PROC,
            "adding proc 0x%" PRIxPTR " in state '%s' to vec 0x%" PRIxPTR,
            (uintptr_t)proc, state_names[(size_t)proc->state], (uintptr_t)v);
    v->push(proc);
}


static void
remove_proc_from_state_vec(rust_rt *rt, rust_proc *proc)
{
    ptr_vec<rust_proc> *v = get_proc_vec(rt, proc);
    rt->log(LOG_MEM|LOG_PROC,
            "removing proc 0x%" PRIxPTR " in state '%s' from vec 0x%" PRIxPTR,
            (uintptr_t)proc, state_names[(size_t)proc->state], (uintptr_t)v);
    I(rt, (*v)[proc->idx] == proc);
    v->swapdel(proc);
    v->trim(rt->n_live_procs());
}

static void
proc_state_transition(rust_rt *rt,
                      rust_proc *proc,
                      proc_state_t src,
                      proc_state_t dst)
{
    rt->log(LOG_PROC,
            "proc 0x%" PRIxPTR " state change '%s' -> '%s'",
            (uintptr_t)proc,
            state_names[(size_t)src],
            state_names[(size_t)dst]);
    I(rt, proc->state == src);
    remove_proc_from_state_vec(rt, proc);
    proc->state = dst;
    add_proc_state_vec(rt, proc);
}

/* Runtime */

static void
del_all_procs(rust_rt *rt, ptr_vec<rust_proc> *v) {
    I(rt, v);
    while (v->length()) {
        rt->log(LOG_PROC, "deleting proc %" PRIdPTR, v->length() - 1);
        delete v->pop();
    }
}

rust_rt::rust_rt(rust_srv *srv, global_glue_fns *global_glue, size_t &live_allocs) :
    srv(srv),
    global_glue(global_glue),
    live_allocs(live_allocs),
    logbits(get_logbits()),
    running_procs(this),
    blocked_procs(this),
    dead_procs(this),
    root_proc(NULL),
    curr_proc(NULL),
    ports(this),
    chans(this),
    rval(0)
{
    logptr("new rt", (uintptr_t)this);
    memset(&rctx, 0, sizeof(rctx));

#ifdef __WIN32__
    {
        HCRYPTPROV hProv;
        win32_require
            (_T("CryptAcquireContext"),
             CryptAcquireContext(&hProv, NULL, NULL, PROV_DSS,
                                 CRYPT_VERIFYCONTEXT|CRYPT_SILENT));
        win32_require
            (_T("CryptGenRandom"),
             CryptGenRandom(hProv, sizeof(rctx.randrsl),
                            (BYTE*)(&rctx.randrsl)));
        win32_require
            (_T("CryptReleaseContext"),
             CryptReleaseContext(hProv, 0));
    }
#else
    int fd = open("/dev/urandom", O_RDONLY);
    I(this, fd > 0);
    I(this, read(fd, (void*) &rctx.randrsl, sizeof(rctx.randrsl))
      == sizeof(rctx.randrsl));
    I(this, close(fd) == 0);
#endif
    randinit(&rctx, 1);
}

rust_rt::~rust_rt() {
    log(LOG_PROC, "deleting all running procs");
    del_all_procs(this, &running_procs);
    log(LOG_PROC, "deleting all blocked procs");
    del_all_procs(this, &blocked_procs);
    log(LOG_PROC, "deleting all dead procs");
    del_all_procs(this, &dead_procs);

    log(LOG_PROC, "deleting all dangling ports and chans");
    // FIXME (bug 541584): remove when port <-> proc linkage is obsolete.
    while (chans.length() > 0)
        delete chans[0];
    // delete ports last since chans can still hold references to them
    while (ports.length() > 0)
        delete ports[0];
}

void
rust_rt::activate(rust_proc *proc) {
    curr_proc = proc;
    global_glue->c_to_proc_glue(proc);
    curr_proc = NULL;
}

void
rust_rt::log(uint32_t logbit, char const *fmt, ...) {
    char buf[256];
    if (logbits & logbit) {
        va_list args;
        va_start(args, fmt);
        vsnprintf(buf, sizeof(buf), fmt, args);
        srv->log(buf);
        va_end(args);
    }
}

void
rust_rt::logptr(char const *msg, uintptr_t ptrval) {
    log(LOG_MEM, "%s 0x%" PRIxPTR, msg, ptrval);
}

template<typename T> void
rust_rt::logptr(char const *msg, T* ptrval) {
    log(LOG_MEM, "%s 0x%" PRIxPTR, msg, (uintptr_t)ptrval);
}

void
rust_rt::fail() {
    log(LOG_RT, "runtime 0x%" PRIxPTR " root proc failed", this);
    I(this, rval == 0);
    rval = 1;
}

void *
rust_rt::malloc(size_t sz) {
    void *p = srv->malloc(sz);
    I(this, p);
    live_allocs++;
    log(LOG_MEM, "rust_rt::malloc(%d) -> 0x%" PRIxPTR,
        sz, p);
    return p;
}

void *
rust_rt::calloc(size_t sz) {
    void *p = this->malloc(sz);
    memset(p, 0, sz);
    return p;
}

void *
rust_rt::realloc(void *p, size_t sz) {
    void *p1 = srv->realloc(p, sz);
    I(this, p1);
    if (!p)
        live_allocs++;
    log(LOG_MEM, "rust_rt::realloc(0x%" PRIxPTR ", %d) -> 0x%" PRIxPTR,
        p, sz, p1);
    return p1;
}

void
rust_rt::free(void *p) {
    log(LOG_MEM, "rust_rt::free(0x%" PRIxPTR ")", p);
    I(this, p);
    srv->free(p);
    I(this, live_allocs > 0);
    live_allocs--;
}

#ifdef __WIN32__
void
rust_rt::win32_require(LPCTSTR fn, BOOL ok) {
    if (!ok) {
        LPTSTR buf;
        DWORD err = GetLastError();
        FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER |
                      FORMAT_MESSAGE_FROM_SYSTEM |
                      FORMAT_MESSAGE_IGNORE_INSERTS,
                      NULL, err,
                      MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                      (LPTSTR) &buf, 0, NULL );
        log(LOG_ERR, "%s failed with error %ld: %s", fn, err, buf);
        LocalFree((HLOCAL)buf);
        I(this, ok);
    }
}
#endif

size_t
rust_rt::n_live_procs()
{
    return running_procs.length() + blocked_procs.length();
}

void
rust_rt::reap_dead_procs()
{
    for (size_t i = 0; i < dead_procs.length(); ) {
        rust_proc *p = dead_procs[i];
        if (p == root_proc || p->refcnt == 0) {
            I(this, !p->waiting_procs.length());
            dead_procs.swapdel(p);
            log(LOG_PROC, "deleting unreferenced dead proc 0x%" PRIxPTR, p);
            delete p;
            continue;
        }
        ++i;
    }
}

rust_proc *
rust_rt::sched()
{
    I(this, this);
    // FIXME: in the face of failing processes, this is not always right.
    // I(this, n_live_procs() > 0);
    if (running_procs.length() > 0) {
        size_t i = rand(&rctx);
        i %= running_procs.length();
        return (rust_proc *)running_procs[i];
    }
    log(LOG_RT|LOG_PROC,
        "no schedulable processes");
    return NULL;
}

/* Upcalls */

extern "C" CDECL char const *str_buf(rust_proc *proc, rust_str *s);

extern "C" CDECL void
upcall_log_int(rust_proc *proc, int32_t i)
{
    LOG_UPCALL_ENTRY(proc);
    proc->rt->log(LOG_UPCALL|LOG_ULOG,
                  "upcall log_int(0x%" PRIx32 " = %" PRId32 " = '%c')",
                  i, i, (char)i);
}

extern "C" CDECL void
upcall_log_str(rust_proc *proc, rust_str *str)
{
    LOG_UPCALL_ENTRY(proc);
    const char *c = str_buf(proc, str);
    proc->rt->log(LOG_UPCALL|LOG_ULOG,
                  "upcall log_str(\"%s\")",
                  c);
}

extern "C" CDECL void
upcall_trace_word(rust_proc *proc, uintptr_t i)
{
    LOG_UPCALL_ENTRY(proc);
    proc->rt->log(LOG_UPCALL|LOG_TRACE,
                  "trace: 0x%" PRIxPTR "",
                  i, i, (char)i);
}

extern "C" CDECL void
upcall_trace_str(rust_proc *proc, char const *c)
{
    LOG_UPCALL_ENTRY(proc);
    proc->rt->log(LOG_UPCALL|LOG_TRACE,
                  "trace: %s",
                  c);
}

extern "C" CDECL rust_port*
upcall_new_port(rust_proc *proc, size_t unit_sz)
{
    LOG_UPCALL_ENTRY(proc);
    rust_rt *rt = proc->rt;
    rt->log(LOG_UPCALL|LOG_MEM|LOG_COMM,
            "upcall_new_port(proc=0x%" PRIxPTR ", unit_sz=%d)",
            (uintptr_t)proc, unit_sz);
    return new (rt) rust_port(proc, unit_sz);
}

extern "C" CDECL void
upcall_del_port(rust_proc *proc, rust_port *port)
{
    LOG_UPCALL_ENTRY(proc);
    proc->rt->log(LOG_UPCALL|LOG_MEM|LOG_COMM,
                  "upcall del_port(0x%" PRIxPTR ")", (uintptr_t)port);
    I(proc->rt, !port->refcnt);
    delete port;
}

extern "C" CDECL rust_chan*
upcall_new_chan(rust_proc *proc, rust_port *port)
{
    LOG_UPCALL_ENTRY(proc);
    rust_rt *rt = proc->rt;
    rt->log(LOG_UPCALL|LOG_MEM|LOG_COMM,
            "upcall_new_chan(proc=0x%" PRIxPTR ", port=0x%" PRIxPTR ")",
            (uintptr_t)proc, port);
    I(rt, port);
    return new (rt) rust_chan(proc, port);
}

extern "C" CDECL void
upcall_del_chan(rust_proc *proc, rust_chan *chan)
{
    LOG_UPCALL_ENTRY(proc);
    rust_rt *rt = proc->rt;
    rt->log(LOG_UPCALL|LOG_MEM|LOG_COMM,
            "upcall del_chan(0x%" PRIxPTR ")", (uintptr_t)chan);
    I(rt, !chan->refcnt);
    rust_port *port = chan->port;
    if (!--(port->refcnt))
        delete port;
    delete chan;
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
attempt_transmission(rust_rt *rt,
                     rust_q *src,
                     rust_proc *dst)
{
    I(rt, src);
    I(rt, dst);

    if (dst->state != proc_state_blocked_reading) {
        rt->log(LOG_COMM,
                "dst in non-reading state, "
                "transmission incomplete");
        return 0;
    }

    if (src->blocked) {
        I(rt, src->blocked->state == proc_state_blocked_writing);
    }

    if (src->buf.unread == 0) {
        rt->log(LOG_COMM,
                "buffer empty, "
                "transmission incomplete");
        return 0;
    }

    uintptr_t *dptr = dst->dptr;
    I(rt, src->port);
    rt->log(LOG_COMM,
            "receiving %d bytes into dst_proc=0x%" PRIxPTR
            ", dptr=0x%" PRIxPTR,
            src->port->unit_sz, dst, dptr);
    src->buf.shift(dptr);

    if (src->blocked) {
        proc_state_transition(rt, src->blocked,
                              proc_state_blocked_writing,
                              proc_state_running);
        src->blocked = NULL;
    }

    proc_state_transition(rt, dst,
                          proc_state_blocked_reading,
                          proc_state_running);

    rt->log(LOG_COMM, "transmission complete");
    return 1;
}

extern "C" CDECL void
upcall_yield(rust_proc *proc)
{
    LOG_UPCALL_ENTRY(proc);
    rust_rt *rt = proc->rt;
    rt->log(LOG_UPCALL|LOG_COMM, "upcall yield()");
    proc->yield(1);
}

extern "C" CDECL void
upcall_join(rust_proc *proc, rust_proc *other)
{
    LOG_UPCALL_ENTRY(proc);
    rust_rt *rt = proc->rt;
    rt->log(LOG_UPCALL|LOG_COMM,
            "upcall join(other=0x%" PRIxPTR ")",
            (uintptr_t)other);

    // If the other proc is already dying, we dont have to wait for it.
    if (other->state != proc_state_dead) {
        other->waiting_procs.push(proc);
        proc_state_transition(rt, proc,
                              proc_state_running,
                              proc_state_blocked_waiting);
        proc->yield(2);
    }
}

extern "C" CDECL void
upcall_send(rust_proc *proc, rust_chan *chan, void *sptr)
{
    LOG_UPCALL_ENTRY(proc);
    rust_rt *rt = proc->rt;
    rt->log(LOG_UPCALL|LOG_COMM,
            "upcall send(chan=0x%" PRIxPTR ", sptr=0x%" PRIxPTR ")",
            (uintptr_t)chan,
            (uintptr_t)sptr);

    I(rt, chan);
    I(rt, sptr);

    rust_port *port = chan->port;
    rt->log(LOG_MEM|LOG_COMM,
            "send to port", (uintptr_t)port);
    I(rt, port);

    // if the queue wasn't resolved yet or is not ours, lookup the queue
    rust_q *q = chan->q;
    if (!q || chan->proc != proc) {
        HASH_FIND(hh, proc->queues, port, sizeof(rust_q*), q);
        if (!q) {
            q = new (rt) rust_q(proc, port);
            HASH_ADD(hh, proc->queues, port, sizeof(rust_q*), q);
        }
        I(rt, q);
        I(rt, q->blocked == proc || !q->blocked);
        I(rt, q->port);
        I(rt, q->port == port);
        // we have resolved the right queue to send via
        chan->proc = proc;
        chan->q = q;
    }

    I(rt, chan->q->proc == proc);
    I(rt, chan->port == port);

    rt->log(LOG_MEM|LOG_COMM,
            "sending via queue 0x%" PRIxPTR,
            (uintptr_t)q);

    if (port->proc) {
        q->blocked = proc;
        q->buf.push(sptr);
        proc_state_transition(rt, proc,
                              proc_state_running,
                              proc_state_blocked_writing);
        attempt_transmission(rt, q, port->proc);
        if (q->buf.unread && !q->sending) {
            q->sending = true;
            port->writers.push(q);
        }
    } else {
        rt->log(LOG_COMM|LOG_ERR,
                "port has no proc (possibly throw?)");
    }

    if (proc->state != proc_state_running)
        proc->yield(3);
}

extern "C" CDECL void
upcall_recv(rust_proc *proc, uintptr_t *dptr, rust_port *port)
{
    LOG_UPCALL_ENTRY(proc);
    rust_rt *rt = proc->rt;
    rt->log(LOG_UPCALL|LOG_COMM,
            "upcall recv(dptr=0x" PRIxPTR ", port=0x%" PRIxPTR ")",
            (uintptr_t)dptr,
            (uintptr_t)port);

    I(rt, port);
    I(rt, port->proc);
    I(rt, proc);
    I(rt, port->proc == proc);

    proc_state_transition(rt, proc,
                          proc_state_running,
                          proc_state_blocked_reading);

    if (port->writers.length() > 0) {
        I(rt, proc->rt);
        size_t i = rand(&rt->rctx);
        i %= port->writers.length();
        rust_q *q = port->writers[i];
        I(rt, q->idx == i);
        if (attempt_transmission(rt, q, proc)) {
            port->writers.swapdel(q);
            port->writers.trim(port->writers.length());
            q->sending = false;
        }
    } else {
        rt->log(LOG_COMM,
                "no writers sending to port", (uintptr_t)port);
    }

    if (proc->state != proc_state_running) {
        proc->dptr = dptr;
        proc->yield(3);
    }
}

extern "C" CDECL void
upcall_fail(rust_proc *proc, char const *expr, char const *file, size_t line)
{
    LOG_UPCALL_ENTRY(proc);
    proc->rt->log(LOG_UPCALL|LOG_ERR, "upcall fail '%s', %s:%" PRIdPTR,
                  expr, file, line);
    proc->fail(4);
}

extern "C" CDECL void
upcall_kill(rust_proc *proc, rust_proc *target)
{
    LOG_UPCALL_ENTRY(proc);
    proc->rt->log(LOG_UPCALL, "upcall kill target=0x%" PRIxPTR, target);
    target->kill();
}

extern "C" CDECL void
upcall_exit(rust_proc *proc)
{
    LOG_UPCALL_ENTRY(proc);
    proc->rt->log(LOG_UPCALL, "upcall exit");
    proc->state = proc_state_dead;
    proc->notify_waiting_procs();
    proc->yield(1);
}

extern "C" CDECL uintptr_t
upcall_malloc(rust_proc *proc, size_t nbytes)
{
    LOG_UPCALL_ENTRY(proc);
    void *p = proc->rt->malloc(nbytes);
    proc->rt->log(LOG_UPCALL|LOG_MEM,
                  "upcall malloc(%u) = 0x%" PRIxPTR,
                  nbytes, (uintptr_t)p);
    return (uintptr_t) p;
}

extern "C" CDECL void
upcall_free(rust_proc *proc, void* ptr)
{
    LOG_UPCALL_ENTRY(proc);
    rust_rt *rt = proc->rt;
    rt->log(LOG_UPCALL|LOG_MEM,
            "upcall free(0x%" PRIxPTR ")",
            (uintptr_t)ptr);
    rt->free(ptr);
}


extern "C" CDECL rust_str*
upcall_new_str(rust_proc *proc, char const *s, size_t fill)
{
    LOG_UPCALL_ENTRY(proc);
    rust_rt *rt = proc->rt;
    size_t alloc = next_power_of_two(fill);
    rust_str *st = (rust_str*) rt->malloc(sizeof(rust_str) + alloc);
    st->refcnt = 1;
    st->fill = fill;
    st->alloc = alloc;
    if (s)
        memcpy(&st->data[0], s, fill);
    rt->log(LOG_UPCALL|LOG_MEM,
            "upcall new_str('%s', %" PRIdPTR ") = 0x%" PRIxPTR,
            s, fill, st);
    return st;
}


static int
rust_main_loop(uintptr_t main_fn, global_glue_fns *global_glue, rust_srv *srv);

struct rust_ticket {
    uintptr_t main_fn;
    global_glue_fns *global_glue;
    rust_srv *srv;

    explicit rust_ticket(uintptr_t main_fn,
                         global_glue_fns *global_glue,
                         rust_srv *srv)
        : main_fn(main_fn),
          global_glue(global_glue),
          srv(srv)
    {}

    ~rust_ticket()
    {}

    void operator delete(void *ptr)
    {
        rust_srv *srv = ((rust_ticket *)ptr)->srv;
        srv->free(ptr);
    }
};

#if 0
#if defined(__WIN32__)
static DWORD WINAPI rust_thread_start(void *ptr)
#elif defined(__GNUC__)
static void *rust_thread_start(void *ptr)
#else
#error "Platform not supported"
#endif
{
    /*
     * The thread that spawn us handed us a ticket. Read the ticket's content
     * and then deallocate it. Since thread creation is asynchronous, the other
     * thread can't do this for us.
     */
    rust_ticket *ticket = (rust_ticket *)ptr;
    uintptr_t main_fn = ticket->main_fn;
    global_glue_fns *global_glue = ticket->global_glue;
    rust_srv *srv = ticket->srv;
    delete ticket;

    /*
     * Start a new rust main loop for this thread.
     */
    rust_main_loop(main_fn, global_glue, srv);

    return 0;
}
#endif

extern "C" CDECL rust_proc*
upcall_spawn_local(rust_proc *spawner, uintptr_t exit_proc_glue, uintptr_t spawnee_fn, size_t callsz)
{
    LOG_UPCALL_ENTRY(spawner);
    rust_rt *rt = spawner->rt;
    rt->log(LOG_UPCALL|LOG_MEM|LOG_PROC,
         "spawn fn: exit_proc_glue 0x%" PRIxPTR ", spawnee 0x%" PRIxPTR ", callsz %d",
         exit_proc_glue, spawnee_fn, callsz);
    rust_proc *proc = new (rt) rust_proc(rt, spawner, exit_proc_glue, spawnee_fn, callsz);
    add_proc_state_vec(rt, proc);
    return proc;
}

extern "C" CDECL rust_proc *
upcall_spawn_thread(rust_proc *spawner, uintptr_t exit_proc_glue, uintptr_t spawnee_fn, size_t callsz)
{
#if 1
    return upcall_spawn_local(spawner, exit_proc_glue, spawnee_fn, callsz);
#else
    LOG_UPCALL_ENTRY(spawner);
    rust_rt *rt = spawner->rt;
    rust_srv *srv = rt->srv;
    /*
     * The ticket is not bound to the current runtime, so allocate directly from the
     * service.
     */
    rust_ticket *ticket = new (srv) rust_ticket(spawnee_fn, global_glue, srv);

#if defined(__WIN32__)
    DWORD thread;
    CreateThread(NULL, 0, rust_thread_start, (void *)ticket, 0, &thread);
#elif defined(__GNUC__)
    pthread_t thread;
    pthread_create(&thread, NULL, rust_thread_start, (void *)ticket);
#else
#error "Platform not supported"
#endif

    /*
     * Create a proxy proc that will represent the newly created thread in this runtime.
     * All communication will go through this proxy proc.
     */
    return NULL;
#endif
}

static int
rust_main_loop(uintptr_t main_fn, global_glue_fns *global_glue, rust_srv *srv)
{
    int rval;
    size_t live_allocs = 0;
    {
        rust_proc *proc;
        rust_rt rt(srv, global_glue, live_allocs);

        rt.log(LOG_RT, "control is in rust runtime library");
        rt.logptr("main fn", main_fn);
        rt.logptr("main exit-proc glue", global_glue->main_exit_proc_glue);

        rt.root_proc = new (rt) rust_proc(&rt, NULL, global_glue->main_exit_proc_glue, main_fn, 0);
        add_proc_state_vec(&rt, rt.root_proc);
        proc = rt.sched();

        rt.logptr("root proc", (uintptr_t)proc);
        rt.logptr("proc->rust_sp", (uintptr_t)proc->rust_sp);

        while (proc) {

            rt.log(LOG_PROC, "activating proc 0x%" PRIxPTR,
                   (uintptr_t)proc);

            if (proc->state == proc_state_running) {
                rt.activate(proc);

                rt.log(LOG_PROC,
                       "returned from proc 0x%" PRIxPTR " in state '%s'",
                       (uintptr_t)proc, state_names[proc->state]);

                I(&rt, proc->rust_sp >= (uintptr_t) &proc->stk->data[0]);
                I(&rt, proc->rust_sp < proc->stk->limit);

                switch ((proc_state_t) proc->state) {

                case proc_state_running:
                    break;

                case proc_state_dead:
                    // When a proc exits *itself* we do not yet kill
                    // it; for the time being we let it linger in the
                    // blocked-exiting state, as someone else still
                    // has a refcount on it.  The reap loop below will
                    // mop it up when there are no more refs.
                    {
                        proc_state_t tstate = proc->state;
                        proc->state = proc_state_running;
                        proc_state_transition(&rt, proc,
                                              proc_state_running,
                                              tstate);
                    }
                    break;

                case proc_state_blocked_reading:
                case proc_state_blocked_writing:
                case proc_state_blocked_waiting:
                    break;
                }

                rt.reap_dead_procs();
            }
            proc = rt.sched();
        }

        rt.log(LOG_RT, "finished main loop (rt.rval = %d)", rt.rval);
        rval = rt.rval;
    }
    if (live_allocs != 0) {
        srv->fatal("leaked memory in rust main loop", __FILE__, __LINE__);
    }
    return rval;
}

void
rust_srv::log(char const *str)
{
    printf("rt: %s\n", str);
}

void *
rust_srv::malloc(size_t bytes)
{
    return ::malloc(bytes);
}

void *
rust_srv::realloc(void *p, size_t bytes)
{
    return ::realloc(p, bytes);
}

void
rust_srv::free(void *p)
{
    ::free(p);
}

void
rust_srv::fatal(char const *expr, char const *file, size_t line)
{
    char buf[1024];
    snprintf(buf, sizeof(buf), "fatal, '%s' failed, %s:%d", expr, file, (int)line);
    log(buf);
    exit(1);
}

/* Native builtins. */

extern "C" CDECL char const *
str_buf(rust_proc *proc, rust_str *s)
{
    return (char const *)&s->data[0];
}

extern "C" CDECL rust_str*
implode(rust_proc *proc, rust_vec *v)
{
    /*
     * We received a vec of u32 unichars. Implode to a string.
     * FIXME: this needs to do a proper utf-8 encoding.
     */
    size_t i;
    rust_str *s;

    size_t fill = v->fill >> 2;
    s = upcall_new_str(proc, NULL, fill);

    uint32_t *src = (uint32_t*) &v->data[0];
    uint8_t *dst = &s->data[0];

    for (i = 0; i < fill; ++i)
        *dst++ = *src;

    return s;
}

extern "C" CDECL int
rust_start(uintptr_t main_fn,
           global_glue_fns *global_glue)
{
    rust_srv srv;
    int ret = rust_main_loop(main_fn, global_glue, &srv);
    return ret;
}

/*
 * Local Variables:
 * mode: C++
 * fill-column: 70;
 * indent-tabs-mode: nil
 * c-basic-offset: 4
 * buffer-file-coding-system: utf-8-unix
 * compile-command: "make -k -C .. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 */
