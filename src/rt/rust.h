#ifndef RUST_H__
#define RUST_H__

#include <stdlib.h>
#include <stdint.h>

/* Basic scalar types we will use. */

#ifndef UINT64_MAX
#error "need uint64_t support in compiler"
#endif

#ifndef PTRDIFF_MAX
#error "need uintptr_t / ptrdiff_t support in compiler"
#endif

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
 *   - 0b1110 == ?? reserved
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

struct rust_proc;
struct rust_prog;
struct rust_rt;

typedef struct rust_proc rust_proc_t;
typedef struct rust_prog rust_prog_t;
typedef struct rust_rt rust_rt_t;

/* FIXME: ifdef by platform. */
extern size_t const rust_n_callee_saves;

/* FIXME: dynamically resize proc array. */
extern size_t const rust_n_procs;

struct rust_rt {
  uintptr_t sp;          /* Saved sp from the C runtime. */
  size_t curr_proc;      /* Currently active proc.       */
  size_t live_procs;     /* Number of live procs.        */
  rust_proc_t **procs;
};

typedef enum {
  /* NB: it's important that 'running' be value 0, as it
   * lets us get away with using OR rather than MOV to
   * signal anything-not-running. x86 optimization. */
  rust_proc_state_running    = 0,
  rust_proc_state_calling_c  = 1,
  rust_proc_state_exiting    = 2
} rust_proc_state_t;

typedef enum {
  rust_upcall_log_uint32     = 0,
  rust_upcall_log_str        = 1,
  rust_upcall_spawn          = 2,
  rust_upcall_check_expr     = 3,
  rust_upcall_malloc         = 4,
  rust_upcall_free           = 5
} rust_upcall_t;

#define RUST_PROC_MAX_UPCALL_ARGS   8

#ifdef WIN32
#define CDECL __cdecl
#else
#define CDECL __attribute__((cdecl))
#endif

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
  size_t refcnt;

  /* Parameter space for upcalls. */
  uintptr_t upcall_code;
  uintptr_t upcall_args[RUST_PROC_MAX_UPCALL_ARGS];

  /* Proc accounting. */
  uintptr_t mem_budget;   /* N bytes ownable by this proc.                  */
  uintptr_t curr_mem;     /* N bytes currently owned.                       */
  uint64_t tick_budget;   /* N ticks in proc lifetime. 0 = unlimited.       */
  uint64_t curr_ticks;    /* N ticks currently consumed.                    */

  uint8_t data[];         /* Official-style C99 "flexible array" element.    */

};

/* A proc gets activated */
typedef struct rust_active_proc {
  rust_proc_t *proc;
  uint64_t slice_ticks;   /* N ticks remaining before rescheduling.         */

} rust_activation_t;

#endif /* RUST_H__ */

/*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * compile-command: "make -k -C .. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 */
