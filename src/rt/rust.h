
/* Basic scalar types we will use. */

#ifndef UINT64_MAX
#error "need uint64_t support in compiler"
#endif

#ifndef PTRDIFF_MAX
#error "need uintptr_t / ptrdiff_t support in compiler"
#endif

typedef struct rust_regs {
  uintptr_t pc;
  uintptr_t sp;
} rust_regs_t;

/* There are 4 frame types, depending on the frame descriptor. */

typedef struct rust_simple_frame {
  rust_type_t *descriptor;
  rust_regs_t return_regs;
} rust_simple_frame_t;

typedef struct rust_iter_frame {
  rust_type_t *descriptor;
  rust_regs_t return_regs;
  rust_regs_t yield_regs;
} rust_iter_frame_t;

typedef struct rust_closure_frame {
  rust_type_t *descriptor;
  rust_regs_t return_regs;
  rust_env_t *env;
  uint8_t data[];
} rust_closure_frame_t;

typedef struct rust_iter_closure_frame {
  rust_type_t *descriptor;
  rust_regs_t return_regs;
  rust_regs_t yield_regs;
  rust_env_t *env;
  uint8_t data[];
} rust_iter_closure_frame_t;

/* Proc stack segments. Heap allocated and chained together. */

typedef struct rust_stk_seg { 
  struct rust_stk_seg *prev;
  struct rust_stk_seg *next;
  size_t size;
  size_t live;
  uint8_t data[];
} rust_stk_seg_t;

/* Runtime closures over lexical environments. */
typedef struct rust_env {
};


typedef struct rust_proc { 

  /* Proc accounting. */
  uintptr_t mem_budget;   /* N bytes ownable by this proc.                  */
  uintptr_t curr_mem;     /* N bytes currently owned.                       */
  uint64_t tick_budget;   /* N ticks in proc lifetime. 0 = unlimited.       */
  uint64_t curr_ticks;    /* N ticks currently consumed.                    */

  rust_stk_seg_t *stk;    
  rust_env_t *env;
  rust_code_t *code;
  rust_msg_queue_t *q;
  rust_regs_t regs;       /* CPU regs are saved here when not running.       */

  uint8_t data[];         /* Official-style C99 "flexible array" element.    */

} rust_proc_t;

/* A proc gets activated */
typedef struct rust_active_proc {
  rust_proc_t *proc;
  uint64_t slice_ticks;   /* N ticks remaining before rescheduling.         */
  
} rust_activation_t;

struct regs {
  word_t pc;
  word_t sp;
  word_t gprs[n];
}


struct frame { 
  frame_desc *desc; // points to the code + frame descriptor
  void *retpc;
  void *yieldpc;    // optional
  uint8_t *env;      // optional -- closures only
  uint8_t slots[];   // computed
}

struct vec {
  word_t refs;
  word_t size;
  word_t live;
  word_t init;
  uint8_t slots[];  // computed
}
