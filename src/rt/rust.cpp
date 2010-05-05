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
#include <errno.h>
#else
#error "Platform not supported."
#endif

#define I(dom, e) ((e) ? (void)0 :                              \
                   (dom)->srv->fatal(#e, __FILE__, __LINE__))

struct rust_task;
struct rust_port;
struct rust_chan;
struct rust_token;
struct rust_dom;

static uint32_t const LOG_ALL = 0xffffffff;
static uint32_t const LOG_ERR =        0x1;
static uint32_t const LOG_MEM =        0x2;
static uint32_t const LOG_COMM =       0x4;
static uint32_t const LOG_TASK =       0x8;
static uint32_t const LOG_UPCALL =    0x10;
static uint32_t const LOG_DOM =       0x20;
static uint32_t const LOG_ULOG =      0x40;
static uint32_t const LOG_TRACE =     0x80;
static uint32_t const LOG_DWARF =    0x100;
static uint32_t const LOG_LINK =     0x200;

#ifdef __GNUC__
#define LOG_UPCALL_ENTRY(task)                              \
    (task)->dom->log(LOG_UPCALL,                            \
                     "upcall task: 0x%" PRIxPTR             \
                     " retpc: 0x%" PRIxPTR,                 \
                     (task), __builtin_return_address(0))
#else
#define LOG_UPCALL_ENTRY(task)                  \
    (task)->dom->log(LOG_UPCALL,                \
                     "upcall task: 0x%" PRIxPTR \
                     (task))
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
        if (strstr(c, "task"))
            bits |= LOG_TASK;
        if (strstr(c, "up"))
            bits |= LOG_UPCALL;
        if (strstr(c, "dom"))
            bits |= LOG_DOM;
        if (strstr(c, "ulog"))
            bits |= LOG_ULOG;
        if (strstr(c, "trace"))
            bits |= LOG_TRACE;
        if (strstr(c, "dwarf"))
            bits |= LOG_DWARF;
        if (strstr(c, "link"))
            bits |= LOG_LINK;
        if (strstr(c, "all"))
            bits = 0xffffffff;
    }
    return bits;
}

// Every reference counted object should derive from this base class.

template <typename T>
struct rc_base {
    size_t refcnt;

    void ref() {
        ++refcnt;
    }

    void deref() {
        if (--refcnt == 0) {
            delete (T*)this;
        }
    }

    rc_base();
    ~rc_base();
};

template <typename T>
struct dom_owned {
    void operator delete(void *ptr) {
        ((T *)ptr)->dom->free(ptr);
    }
};

template <typename T>
struct task_owned {
    void operator delete(void *ptr) {
        ((T *)ptr)->task->dom->free(ptr);
    }
};

// Task stack segments. Heap allocated and chained together.

struct stk_seg {
    unsigned int valgrind_id;
    uintptr_t limit;
    uint8_t data[];
};

typedef enum {
    abi_code_cdecl = 0,
    abi_code_rust = 1
} abi_t;

struct frame_glue_fns {
    uintptr_t mark_glue_off;
    uintptr_t drop_glue_off;
    uintptr_t reloc_glue_off;
};

struct type_desc {
    size_t size;
    size_t align;
    uintptr_t copy_glue_off;
    uintptr_t drop_glue_off;
    uintptr_t free_glue_off;
};

template <typename T>
class ptr_vec : public dom_owned<ptr_vec<T> > {
    static const size_t INIT_SIZE = 8;

    rust_dom *dom;
    size_t alloc;
    size_t fill;
    T **data;

public:
    ptr_vec(rust_dom *dom);
    ~ptr_vec();

    size_t length() {
        return fill;
    }

    T *& operator[](size_t offset);
    void push(T *p);
    T *pop();
    void trim(size_t fill);
    void swapdel(T* p);
};

struct circ_buf : public dom_owned<circ_buf> {
    static const size_t INIT_CIRC_BUF_UNITS = 8;
    static const size_t MAX_CIRC_BUF_SIZE = 1 << 24;

    rust_dom *dom;
    size_t alloc;
    size_t unit_sz;
    size_t next;
    size_t unread;
    uint8_t *data;

    circ_buf(rust_dom *dom, size_t unit_sz);
    ~circ_buf();

    void transfer(void *dst);
    void push(void *src);
    void shift(void *dst);
};

// Ticket lock-based spinlock implementation.

class spinlock {
    unsigned next_ticket;
    unsigned now_serving;

    void pause();
public:
    spinlock() {
        next_ticket = now_serving = 0;
    }

    void lock();
    void unlock();
};

void
spinlock::pause()
{
    asm volatile("pause\n" : : : "memory");
}

void
spinlock::lock()
{
    unsigned my_ticket = __sync_fetch_and_add(&next_ticket, 1);

    while (now_serving != my_ticket)
        pause();
}

void
spinlock::unlock()
{
    // Only one thread at a time owns the ticket word, so we don't
    // need a synchronized increment here.
    ++next_ticket;
}

// Interrupt transparent queue, Schoen et. al, "On
// Interrupt-Transparent Synchronization in an Embedded
// Object-Oriented Operating System", 2000. enqueue() is allowed to
// interrupt enqueue() and dequeue(), however, dequeue() is not
// allowed to interrupt itself.

struct lockfree_queue_chain {
    lockfree_queue_chain *next;
};

class lockfree_queue : public lockfree_queue_chain {
    lockfree_queue_chain *tail;
public:
    lockfree_queue();
    void enqueue(lockfree_queue_chain *item);
    lockfree_queue_chain *dequeue();
};

lockfree_queue::lockfree_queue() :
    tail(this)
{
}

void
lockfree_queue::enqueue(lockfree_queue_chain *item)
{
    item->next = (lockfree_queue_chain *)0;
    lockfree_queue_chain *last = tail;
    tail = item;
    while (last->next)
        last = last->next;
    last->next = item;
}

lockfree_queue_chain *
lockfree_queue::dequeue()
{
    lockfree_queue_chain *item = next;
    if (item && !(next = item->next)) {
        tail = (lockfree_queue_chain *)this;
        if (item->next) {
            lockfree_queue_chain *lost = item->next;
            lockfree_queue_chain *help;
            do {
                help = lost->next;
                enqueue(lost);
            } while ((lost = help) != (lockfree_queue_chain *)0);
        }
    }
    return item;
}

// Rust types vec and str look identical from our perspective.

struct rust_vec : public rc_base<rust_vec> {
    size_t alloc;
    size_t fill;
    uint8_t data[];
    rust_vec(rust_dom *dom, size_t alloc, size_t fill, uint8_t const *data);
    ~rust_vec();
};

typedef rust_vec rust_str;

class rust_crate;
class rust_crate_cache;

template<typename T> T*
crate_rel(rust_crate const *crate, T *t) {
    return (T*)(((uintptr_t)crate) + ((ptrdiff_t)t));
}

template<typename T> T const*
crate_rel(rust_crate const *crate, T const *t) {
    return (T const*)(((uintptr_t)crate) + ((ptrdiff_t)t));
}

struct rust_dom {
    // NB: the root crate must remain in memory until the root of the
    // tree of domains exits. All domains within this tree have a
    // copy of this root_crate value and use it for finding utility
    // glue.
    rust_crate const *root_crate;
    rust_srv *srv;
    uint32_t logbits;
    ptr_vec<rust_task> running_tasks;
    ptr_vec<rust_task> blocked_tasks;
    ptr_vec<rust_task> dead_tasks;
    ptr_vec<rust_crate_cache> caches;
    randctx rctx;
    rust_task *root_task;
    rust_task *curr_task;
    int rval;
    lockfree_queue *incoming; // incoming messages from other threads
#ifndef __WIN32__
    pthread_attr_t attr;
#endif

    rust_dom(rust_srv *srv, rust_crate const *root_crate);
    ~rust_dom();

    void activate(rust_task *task);
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

    rust_crate_cache *get_cache(rust_crate const *crate);
    size_t n_live_tasks();
    void add_task_to_state_vec(ptr_vec<rust_task> *v, rust_task *task);
    void remove_task_from_state_vec(ptr_vec<rust_task> *v, rust_task *task);
    const char *state_vec_name(ptr_vec<rust_task> *v);

    void reap_dead_tasks();
    rust_task *sched();
};

inline void *operator new(size_t sz, void *mem) {
    return mem;
}

inline void *operator new(size_t sz, rust_dom *dom) {
    return dom->malloc(sz);
}

inline void *operator new[](size_t sz, rust_dom *dom) {
    return dom->malloc(sz);
}

inline void *operator new(size_t sz, rust_dom &dom) {
    return dom.malloc(sz);
}

inline void *operator new[](size_t sz, rust_dom &dom) {
    return dom.malloc(sz);
}

// Crates.

typedef void CDECL (*activate_glue_ty)(rust_task *);

class rust_crate
{
    // The following fields are emitted by the compiler for the static
    // rust_crate object inside each compiled crate.

    ptrdiff_t image_base_off;     // Offset from this to the loaded image base.
    uintptr_t self_addr;          // Un-relocated addres of 'this'.

    ptrdiff_t debug_abbrev_off;   // Offset from this to .debug_abbrev.
    size_t debug_abbrev_sz;       // Size of .debug_abbrev.

    ptrdiff_t debug_info_off;     // Offset from this to .debug_info.
    size_t debug_info_sz;         // Size of .debug_info.

    ptrdiff_t activate_glue_off;
    ptrdiff_t main_exit_task_glue_off;
    ptrdiff_t unwind_glue_off;
    ptrdiff_t yield_glue_off;

public:

    size_t n_rust_syms;
    size_t n_c_syms;
    size_t n_libs;

    // Crates are immutable, constructed by the compiler.

    uintptr_t get_image_base() const {
        return ((uintptr_t)this + image_base_off);
    }

    ptrdiff_t get_relocation_diff() const {
        return ((uintptr_t)this - self_addr);
    }

    activate_glue_ty get_activate_glue() const {
        return (activate_glue_ty) ((uintptr_t)this + activate_glue_off);
    }

    uintptr_t get_main_exit_task_glue() const {
        return ((uintptr_t)this + main_exit_task_glue_off);
    }

    uintptr_t get_unwind_glue() const {
        return ((uintptr_t)this + unwind_glue_off);
    }

    uintptr_t get_yield_glue() const {
        return ((uintptr_t)this + yield_glue_off);
    }

    struct mem_area
    {
        rust_dom *dom;
        uintptr_t base;
        uintptr_t lim;

        mem_area(rust_dom *dom, uintptr_t pos, size_t sz)
            : dom(dom),
              base(pos),
              lim(pos + sz)
        {
            dom->log(LOG_MEM, "new mem_area [0x%" PRIxPTR ",0x%" PRIxPTR "]",
                     base, lim);
        }
    };

    mem_area get_debug_info(rust_dom *dom) const {
        return mem_area(dom, ((uintptr_t)this + debug_info_off),
                        debug_info_sz);
    }

    mem_area get_debug_abbrev(rust_dom *dom) const {
        return mem_area(dom, ((uintptr_t)this + debug_abbrev_off),
                        debug_abbrev_sz);
    }

};

enum dw_form {
    DW_FORM_addr = 0x01,
    DW_FORM_block2 = 0x03,
    DW_FORM_block4 = 0x04,
    DW_FORM_data2 = 0x05,
    DW_FORM_data4 = 0x06,
    DW_FORM_data8 = 0x07,
    DW_FORM_string = 0x08,
    DW_FORM_block = 0x09,
    DW_FORM_block1 = 0x0a,
    DW_FORM_data1 = 0x0b,
    DW_FORM_flag = 0x0c,
    DW_FORM_sdata = 0x0d,
    DW_FORM_strp = 0x0e,
    DW_FORM_udata = 0x0f,
    DW_FORM_ref_addr = 0x10,
    DW_FORM_ref1 = 0x11,
    DW_FORM_ref2 = 0x12,
    DW_FORM_ref4 = 0x13,
    DW_FORM_ref8 = 0x14,
    DW_FORM_ref_udata = 0x15,
    DW_FORM_indirect = 0x16
};

enum dw_at {
    DW_AT_sibling = 0x01,
    DW_AT_location = 0x02,
    DW_AT_name = 0x03,
    DW_AT_ordering = 0x09,
    DW_AT_byte_size = 0x0b,
    DW_AT_bit_offset = 0x0c,
    DW_AT_bit_size = 0x0d,
    DW_AT_stmt_list = 0x10,
    DW_AT_low_pc = 0x11,
    DW_AT_high_pc = 0x12,
    DW_AT_language = 0x13,
    DW_AT_discr = 0x15,
    DW_AT_discr_value = 0x16,
    DW_AT_visibility = 0x17,
    DW_AT_import = 0x18,
    DW_AT_string_length = 0x19,
    DW_AT_common_reference = 0x1a,
    DW_AT_comp_dir = 0x1b,
    DW_AT_const_value = 0x1c,
    DW_AT_containing_type = 0x1d,
    DW_AT_default_value = 0x1e,
    DW_AT_inline = 0x20,
    DW_AT_is_optional = 0x21,
    DW_AT_lower_bound = 0x22,
    DW_AT_producer = 0x25,
    DW_AT_prototyped = 0x27,
    DW_AT_return_addr = 0x2a,
    DW_AT_start_scope = 0x2c,
    DW_AT_bit_stride = 0x2e,
    DW_AT_upper_bound = 0x2f,
    DW_AT_abstract_origin = 0x31,
    DW_AT_accessibility = 0x32,
    DW_AT_address_class = 0x33,
    DW_AT_artificial = 0x34,
    DW_AT_base_types = 0x35,
    DW_AT_calling_convention = 0x36,
    DW_AT_count = 0x37,
    DW_AT_data_member_location = 0x38,
    DW_AT_decl_column = 0x39,
    DW_AT_decl_file = 0x3a,
    DW_AT_decl_line = 0x3b,
    DW_AT_declaration = 0x3c,
    DW_AT_discr_list = 0x3d,
    DW_AT_encoding = 0x3e,
    DW_AT_external = 0x3f,
    DW_AT_frame_base = 0x40,
    DW_AT_friend = 0x41,
    DW_AT_identifier_case = 0x42,
    DW_AT_macro_info = 0x43,
    DW_AT_namelist_item = 0x44,
    DW_AT_priority = 0x45,
    DW_AT_segment = 0x46,
    DW_AT_specification = 0x47,
    DW_AT_static_link = 0x48,
    DW_AT_type = 0x49,
    DW_AT_use_location = 0x4a,
    DW_AT_variable_parameter = 0x4b,
    DW_AT_virtuality = 0x4c,
    DW_AT_vtable_elem_location = 0x4d,
    DW_AT_allocated = 0x4e,
    DW_AT_associated = 0x4f,
    DW_AT_data_location = 0x50,
    DW_AT_byte_stride = 0x51,
    DW_AT_entry_pc = 0x52,
    DW_AT_use_UTF8 = 0x53,
    DW_AT_extension = 0x54,
    DW_AT_ranges = 0x55,
    DW_AT_trampoline = 0x56,
    DW_AT_call_column = 0x57,
    DW_AT_call_file = 0x58,
    DW_AT_call_line = 0x59,
    DW_AT_description = 0x5a,
    DW_AT_binary_scale = 0x5b,
    DW_AT_decimal_scale = 0x5c,
    DW_AT_small = 0x5d,
    DW_AT_decimal_sign = 0x5e,
    DW_AT_digit_count = 0x5f,
    DW_AT_picture_string = 0x60,
    DW_AT_mutable = 0x61,
    DW_AT_threads_scaled = 0x62,
    DW_AT_explicit = 0x63,
    DW_AT_object_pointer = 0x64,
    DW_AT_endianity = 0x65,
    DW_AT_elemental = 0x66,
    DW_AT_pure = 0x67,
    DW_AT_recursive = 0x68,
    DW_AT_lo_user = 0x2000,
    DW_AT_hi_user = 0x3fff
};

enum dw_tag {
    DW_TAG_array_type = 0x01,
    DW_TAG_class_type = 0x02,
    DW_TAG_entry_point = 0x03,
    DW_TAG_enumeration_type = 0x04,
    DW_TAG_formal_parameter = 0x05,
    DW_TAG_imported_declaration = 0x08,
    DW_TAG_label = 0x0a,
    DW_TAG_lexical_block = 0x0b,
    DW_TAG_member = 0x0d,
    DW_TAG_pointer_type = 0x0f,
    DW_TAG_reference_type = 0x10,
    DW_TAG_compile_unit = 0x11,
    DW_TAG_string_type = 0x12,
    DW_TAG_structure_type = 0x13,
    DW_TAG_subroutine_type = 0x15,
    DW_TAG_typedef = 0x16,
    DW_TAG_union_type = 0x17,
    DW_TAG_unspecified_parameters = 0x18,
    DW_TAG_variant = 0x19,
    DW_TAG_common_block = 0x1a,
    DW_TAG_common_inclusion = 0x1b,
    DW_TAG_inheritance = 0x1c,
    DW_TAG_inlined_subroutine = 0x1d,
    DW_TAG_module = 0x1e,
    DW_TAG_ptr_to_member_type = 0x1f,
    DW_TAG_set_type = 0x20,
    DW_TAG_subrange_type = 0x21,
    DW_TAG_with_stmt = 0x22,
    DW_TAG_access_declaration = 0x23,
    DW_TAG_base_type = 0x24,
    DW_TAG_catch_block = 0x25,
    DW_TAG_const_type = 0x26,
    DW_TAG_constant = 0x27,
    DW_TAG_enumerator = 0x28,
    DW_TAG_file_type = 0x29,
    DW_TAG_friend = 0x2a,
    DW_TAG_namelist = 0x2b,
    DW_TAG_namelist_item = 0x2c,
    DW_TAG_packed_type = 0x2d,
    DW_TAG_subprogram = 0x2e,
    DW_TAG_template_type_parameter = 0x2f,
    DW_TAG_template_value_parameter = 0x30,
    DW_TAG_thrown_type = 0x31,
    DW_TAG_try_block = 0x32,
    DW_TAG_variant_part = 0x33,
    DW_TAG_variable = 0x34,
    DW_TAG_volatile_type = 0x35,
    DW_TAG_dwarf_procedure = 0x36,
    DW_TAG_restrict_type = 0x37,
    DW_TAG_interface_type = 0x38,
    DW_TAG_namespace = 0x39,
    DW_TAG_imported_module = 0x3a,
    DW_TAG_unspecified_type = 0x3b,
    DW_TAG_partial_unit = 0x3c,
    DW_TAG_imported_unit = 0x3d,
    DW_TAG_condition = 0x3f,
    DW_TAG_shared_type = 0x40,
    DW_TAG_lo_user = 0x4080,
    DW_TAG_hi_user = 0xffff,
};

class rust_crate_reader
{
    class mem_reader
    {
    protected:
        rust_crate::mem_area &mem;
        bool ok;
        uintptr_t pos;
    public:

        bool is_ok() {
            return ok;
        }

        bool at_end() {
            return pos == mem.lim;
        }

        void fail() {
            ok = false;
        }

        void reset() {
            pos = mem.base;
            ok = true;
        }

        mem_reader(rust_crate::mem_area &m)
            : mem(m),
              ok(true),
              pos(m.base)
        {}

        size_t tell_abs() {
            return pos;
        }

        size_t tell_off() {
            return pos - mem.base;
        }

        void seek_abs(uintptr_t p) {
            if (!ok || p < mem.base || p >= mem.lim)
                ok = false;
            else
                pos = p;
        }

        void seek_off(uintptr_t p) {
            seek_abs(p + mem.base);
        }

        template<typename T>
        void get(T &out) {
            if (pos < mem.base
                || pos >= mem.lim
                || pos + sizeof(T) > mem.lim)
                ok = false;
            if (!ok)
                return;
            out = *((T*)(pos));
            pos += sizeof(T);
            ok &= !at_end();
            I(mem.dom, at_end() || (mem.base <= pos && pos < mem.lim));
        }

        template<typename T>
        void get_uleb(T &out) {
            out = T(0);
            for (size_t i = 0; i < sizeof(T) && ok; ++i) {
                uint8_t byte;
                get(byte);
                out <<= 7;
                out |= byte & 0x7f;
                if (!(byte & 0x80))
                    break;
            }
            I(mem.dom, at_end() || (mem.base <= pos && pos < mem.lim));
        }

        bool adv_zstr(size_t sz) {
            sz = 0;
            while (ok) {
                char c;
                get(c);
                ++sz;
                if (c == '\0')
                    return true;
            }
            return false;
        }

        bool get_zstr(char const *&c, size_t &sz) {
            if (!ok)
                return false;
            c = (char const *)(pos);
            return adv_zstr(sz);
        }

        void adv(size_t amt) {
            if (pos < mem.base
                || pos >= mem.lim
                || pos + amt > mem.lim)
                ok = false;
            if (!ok)
                return;
            // mem.dom->log(LOG_MEM, "adv %d bytes", amt);
            pos += amt;
            ok &= !at_end();
            I(mem.dom, at_end() || (mem.base <= pos && pos < mem.lim));
        }

        template<typename T>
        void adv_sizeof(T &) {
            adv(sizeof(T));
        }
    };


    struct abbrev : dom_owned<abbrev>
    {
        rust_dom *dom;
        uintptr_t body_off;
        size_t body_sz;
        uintptr_t tag;
        uint8_t has_children;
        size_t idx;
        abbrev(rust_dom *dom, uintptr_t body_off, size_t body_sz,
               uintptr_t tag, uint8_t has_children) :
            dom(dom),
            body_off(body_off),
            tag(tag),
            has_children(has_children),
            idx(0)
        {}
    };

    class abbrev_reader : public mem_reader
    {
        ptr_vec<abbrev> abbrevs;
    public:
        abbrev_reader(rust_crate::mem_area &abbrev_mem)
            : mem_reader(abbrev_mem),
              abbrevs(abbrev_mem.dom)
        {
            rust_dom *dom = mem.dom;
            while (is_ok()) {

                // dom->log(LOG_DWARF, "reading new abbrev at 0x%" PRIxPTR,
                //          tell_off());

                uintptr_t idx, tag;
                uint8_t has_children;
                get_uleb(idx);
                get_uleb(tag);
                get(has_children);

                uintptr_t attr, form;
                size_t body_off = tell_off();
                while (is_ok() && step_attr_form_pair(attr, form));

                // dom->log(LOG_DWARF,
                //         "finished scanning attr/form pairs, pos=0x%"
                //         PRIxPTR ", lim=0x%" PRIxPTR ", is_ok=%d, at_end=%d",
                //        pos, mem.lim, is_ok(), at_end());

                if (is_ok() || at_end()) {
                    dom->log(LOG_DWARF, "read abbrev: %" PRIdPTR, idx);
                    I(dom, idx = abbrevs.length() + 1);
                    abbrevs.push(new (dom) abbrev(dom, body_off,
                                                  tell_off() - body_off,
                                                  tag, has_children));
                }
            }
        }

        abbrev *get_abbrev(size_t i) {
            i -= 1;
            if (i < abbrevs.length())
                return abbrevs[i];
            return NULL;
        }

        bool step_attr_form_pair(uintptr_t &attr, uintptr_t &form)
        {
            attr = 0;
            form = 0;
            // mem.dom->log(LOG_DWARF, "reading attr/form pair at 0x%" PRIxPTR,
            //              tell_off());
            get_uleb(attr);
            get_uleb(form);
            // mem.dom->log(LOG_DWARF, "attr 0x%" PRIxPTR ", form 0x%" PRIxPTR,
            //              attr, form);
            return ! (attr == 0 && form == 0);
        }
        ~abbrev_reader() {
            while (abbrevs.length()) {
                delete abbrevs.pop();
            }
        }
    };

    rust_dom *dom;
    size_t idx;
    rust_crate const *crate;

    rust_crate::mem_area abbrev_mem;
    abbrev_reader abbrevs;

    rust_crate::mem_area die_mem;

public:

    class die_reader : public mem_reader
    {

        struct attr {
            dw_form form;
            dw_at at;
            union {
                struct {
                    char const *s;
                    size_t sz;
                } str;
                uintptr_t num;
            } val;

            bool is_numeric() const {
                switch (form) {
                case DW_FORM_ref_addr:
                case DW_FORM_addr:
                case DW_FORM_data4:
                case DW_FORM_data1:
                case DW_FORM_flag:
                    return true;
                default:
                    break;
                }
                return false;
            }

            bool is_string() const {
                return form == DW_FORM_string;
            }

            size_t get_ssz(rust_dom *dom) const {
                I(dom, is_string());
                return val.str.sz;
            }

            char const *get_str(rust_dom *dom) const {
                I(dom, is_string());
                return val.str.s;
            }

            uintptr_t get_num(rust_dom *dom) const {
                I(dom, is_numeric());
                return val.num;
            }

            bool is_unknown() const {
                return !(is_numeric() || is_string());
            }
        };

        struct rdr_sess
        {
            die_reader *rdr;
            rdr_sess(die_reader *rdr) : rdr(rdr)
            {
                I(rdr->mem.dom, !rdr->in_use);
                rdr->in_use = true;
            }
            ~rdr_sess()
            {
                rdr->in_use = false;
            }
        };

    public:

        struct die {
            die_reader *rdr;
            uintptr_t off;
            abbrev *ab;
            bool using_rdr;

            die(die_reader *rdr, uintptr_t off)
                : rdr(rdr),
                  off(off),
                  using_rdr(false)
            {
                rust_dom *dom = rdr->mem.dom;
                rdr_sess use(rdr);

                rdr->reset();
                rdr->seek_off(off);
                if (!rdr->is_ok()) {
                    ab = NULL;
                    return;
                }
                size_t ab_idx;
                rdr->get_uleb(ab_idx);
                if (!ab_idx) {
                    ab = NULL;
                    dom->log(LOG_DWARF, "DIE <0x%" PRIxPTR "> (null)", off);
                } else {
                    ab = rdr->abbrevs.get_abbrev(ab_idx);
                    dom->log(LOG_DWARF, "DIE <0x%" PRIxPTR "> abbrev 0x%"
                             PRIxPTR, off, ab_idx);
                    dom->log(LOG_DWARF, "  tag 0x%x, has children: %d",
                             ab->tag, ab->has_children);
                }
            }

            bool is_null() const {
                return ab == NULL;
            }

            bool has_children() const {
                return (!is_null()) && ab->has_children;
            }

            dw_tag tag() const {
                if (is_null())
                    return (dw_tag) (-1);
                return (dw_tag) ab->tag;
            }

            bool start_attrs() const {
                if (is_null())
                    return false;
                rdr->reset();
                rdr->seek_off(off + 1);
                rdr->abbrevs.reset();
                rdr->abbrevs.seek_off(ab->body_off);
                return rdr->is_ok();
            }

            bool step_attr(attr &a) const
            {
                uintptr_t ai, fi;
                if (rdr->abbrevs.step_attr_form_pair(ai, fi) && rdr->is_ok()) {
                    a.at = (dw_at)ai;
                    a.form = (dw_form)fi;

                    uint32_t u32;
                    uint8_t u8;

                    switch (a.form) {
                    case DW_FORM_string:
                        return rdr->get_zstr(a.val.str.s, a.val.str.sz);
                        break;

                    case DW_FORM_ref_addr:
                        I(rdr->mem.dom, sizeof(uintptr_t) == 4);
                    case DW_FORM_addr:
                    case DW_FORM_data4:
                        rdr->get(u32);
                        a.val.num = (uintptr_t)u32;
                        return rdr->is_ok() || rdr->at_end();
                        break;

                    case DW_FORM_data1:
                    case DW_FORM_flag:
                        rdr->get(u8);
                        a.val.num = u8;
                        return rdr->is_ok() || rdr->at_end();
                        break;

                    case DW_FORM_block1:
                        rdr->get(u8);
                        rdr->adv(u8);
                        return rdr->is_ok() || rdr->at_end();
                        break;

                    default:
                        rdr->mem.dom->log(LOG_DWARF, "  unknown dwarf form: 0x%"
                                          PRIxPTR, a.form);
                        rdr->fail();
                        break;
                    }
                }
                return false;
            }

            bool find_str_attr(dw_at at, char const *&c) {
                rdr_sess use(rdr);
                if (is_null())
                    return false;
                if (start_attrs()) {
                    attr a;
                    while (step_attr(a)) {
                        if (a.at == at && a.is_string()) {
                            c = a.get_str(rdr->mem.dom);
                            return true;
                        }
                    }
                }
                return false;
            }

            bool find_num_attr(dw_at at, uintptr_t &n) {
                rdr_sess use(rdr);
                if (is_null())
                    return false;
                if (start_attrs()) {
                    attr a;
                    while (step_attr(a)) {
                        if (a.at == at && a.is_numeric()) {
                            n = a.get_num(rdr->mem.dom);
                            return true;
                        }
                    }
                }
                return false;
            }

            bool is_transparent() {
                // "semantically transparent" DIEs are those with
                // children that serve to structure the tree but have
                // tags that don't reflect anything in the rust-module
                // name hierarchy.
                switch (tag()) {
                case DW_TAG_compile_unit:
                case DW_TAG_lexical_block:
                    return (has_children());
                default:
                    break;
                }
                return false;
            }

            bool find_child_by_name(char const *c,
                                    die &child,
                                    bool exact=false) {
                rust_dom *dom = rdr->mem.dom;
                I(dom, has_children());
                I(dom, !is_null());

                for (die ch = next(); !ch.is_null(); ch = ch.next_sibling()) {
                    char const *ac;
                    if (!exact && ch.is_transparent()) {
                        if (ch.find_child_by_name(c, child, exact)) {
                            return true;
                        }
                    }
                    else if (ch.find_str_attr(DW_AT_name, ac)) {
                        if (strcmp(ac, c) == 0) {
                            child = ch;
                            return true;
                        }
                    }
                }
                return false;
            }

            bool find_child_by_tag(dw_tag tag, die &child) {
                rust_dom *dom = rdr->mem.dom;
                I(dom, has_children());
                I(dom, !is_null());

                for (child = next(); !child.is_null();
                     child = child.next_sibling()) {
                    if (child.tag() == tag)
                        return true;
                }
                return false;
            }

            die next() const {
                rust_dom *dom = rdr->mem.dom;

                if (is_null()) {
                    rdr->seek_off(off + 1);
                    return die(rdr, rdr->tell_off());
                }

                {
                    rdr_sess use(rdr);
                    if (start_attrs()) {
                        attr a;
                        while (step_attr(a)) {
                            I(dom, !(a.is_numeric() && a.is_string()));
                            if (a.is_numeric())
                                dom->log(LOG_DWARF, "  attr num: 0x%"
                                         PRIxPTR, a.get_num(dom));
                            else if (a.is_string())
                                dom->log(LOG_DWARF, "  attr str: %s",
                                         a.get_str(dom));
                            else
                                dom->log(LOG_DWARF, "  attr ??:");
                        }
                    }
                }
                return die(rdr, rdr->tell_off());
            }

            die next_sibling() const {
                // FIXME: use DW_AT_sibling, when present.
                if (has_children()) {
                    // rdr->mem.dom->log(LOG_DWARF, "+++ children of die 0x%"
                    //                   PRIxPTR, off);
                    die child = next();
                    while (!child.is_null())
                        child = child.next_sibling();
                    // rdr->mem.dom->log(LOG_DWARF, "--- children of die 0x%"
                    //                   PRIxPTR, off);
                    return child.next();
                } else {
                    return next();
                }
            }
        };

        abbrev_reader &abbrevs;
        uint32_t cu_unit_length;
        uintptr_t cu_base;
        uint16_t dwarf_vers;
        uint32_t cu_abbrev_off;
        uint8_t sizeof_addr;
        bool in_use;

    public:

        die first_die() {
            reset();
            seek_off(cu_base
                     + sizeof(dwarf_vers)
                     + sizeof(cu_abbrev_off)
                     + sizeof(sizeof_addr));
            return die(this, tell_off());
        }

        void dump() {
            rust_dom *dom = mem.dom;
            die d = first_die();
            while (!d.is_null())
                d = d.next_sibling();
            I(dom, d.is_null());
            I(dom, d.off == mem.lim - mem.base);
        }


        die_reader(rust_crate::mem_area &die_mem,
                   abbrev_reader &abbrevs)
            : mem_reader(die_mem),
              abbrevs(abbrevs),
              cu_unit_length(0),
              cu_base(0),
              dwarf_vers(0),
              cu_abbrev_off(0),
              sizeof_addr(0),
              in_use(false)
        {
            rust_dom *dom = mem.dom;

            rdr_sess use(this);

            get(cu_unit_length);
            cu_base = tell_off();

            get(dwarf_vers);
            get(cu_abbrev_off);
            get(sizeof_addr);

            if (is_ok()) {
                dom->log(LOG_DWARF, "new root CU at 0x%" PRIxPTR, die_mem.base);
                dom->log(LOG_DWARF, "CU unit length: %" PRId32, cu_unit_length);
                dom->log(LOG_DWARF, "dwarf version: %" PRId16, dwarf_vers);
                dom->log(LOG_DWARF, "CU abbrev off: %" PRId32, cu_abbrev_off);
                dom->log(LOG_DWARF, "size of address: %" PRId8, sizeof_addr);
                I(dom, sizeof_addr == sizeof(uintptr_t));
                I(dom, dwarf_vers >= 2);
                I(dom, cu_base + cu_unit_length == die_mem.lim - die_mem.base);
            } else {
                dom->log(LOG_DWARF, "failed to read root CU header");
            }
        }

        ~die_reader() {
        }
    };

    die_reader dies;

    rust_crate_reader(rust_dom *dom,
                      rust_crate const *crate)
        : dom(dom),
          crate(crate),
          abbrev_mem(crate->get_debug_abbrev(dom)),
          abbrevs(abbrev_mem),
          die_mem(crate->get_debug_info(dom)),
          dies(die_mem, abbrevs)
    {
        dom->log(LOG_MEM, "crate_reader on crate: 0x%" PRIxPTR, this);
        dom->log(LOG_MEM, "debug_abbrev: 0x%" PRIxPTR, abbrev_mem.base);
        dom->log(LOG_MEM, "debug_info: 0x%" PRIxPTR, die_mem.base);
        // For now, perform diagnostics only.
        dies.dump();
    }
};


// A cond(ition) is something we can block on. This can be a channel
// (writing), a port (reading) or a task (waiting).

struct rust_cond {
};

// An alarm can be put into a wait queue and the task will be notified
// when the wait queue is flushed.

struct rust_alarm {
    rust_task *receiver;
    size_t idx;

    rust_alarm(rust_task *receiver);
};

typedef ptr_vec<rust_alarm> rust_wait_queue;

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
 *    gc_vals in a task.
 *
 *  - The task has a gc_alloc_chain field that points to the most-recent
 *    gc_val allocated.
 *
 *  - GC proceeds as follows:
 *
 *    - The task calls frame_glue_fns.mark_glue(fp) which marks the
 *      frame and then loops, walking down the frame chain. This marks
 *      all the frames with GC roots (each of those functions in turn
 *      may recursively call into the GC graph, that's for the mark
 *      glue to decide).
 *
 *    - The task then asks its domain for its gc_alloc_chain.
 *
 *    - The task calls
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
 *      task looks like it's running normal code that happens to not
 *      perform any gc_val allocation. Mark-bit twiddling is
 *      open-coded into all the mark functions, which know their
 *      contents; we only have to do O(frames) indirect calls to mark,
 *      the rest are static. Sweeping costs O(gc-heap) indirect calls,
 *      unfortunately, because the set of sweep functions to call is
 *      arbitrary based on allocation order.
 *
 */

class rust_crate_cache;

struct rust_task : public rc_base<rust_task>,
                   public dom_owned<rust_task>,
                   public rust_cond {
    // fields known to the compiler
    stk_seg *stk;
    uintptr_t runtime_sp;      // runtime sp while task running.
    uintptr_t rust_sp;         // saved sp when not running.
    uintptr_t gc_alloc_chain;  // linked list of GC allocations.
    rust_dom *dom;
    rust_crate_cache *cache;

    // fields known only to the runtime
    ptr_vec<rust_task> *state;
    rust_cond *cond;
    uintptr_t* dptr;           // rendezvous pointer for send/recv
    rust_task *spawner;        // parent-link
    size_t idx;

    // wait queue for tasks waiting for this task
    rust_wait_queue waiting_tasks;
    rust_alarm alarm;

    rust_task(rust_dom *dom,
              rust_task *spawner);
    ~rust_task();

    void start(uintptr_t exit_task_glue,
               uintptr_t spawnee_fn,
               uintptr_t args,
               size_t callsz);
    bool running();
    bool blocked();
    bool blocked_on(rust_cond *cond);
    bool dead();

    const char *state_str();
    void transition(ptr_vec<rust_task> *svec, ptr_vec<rust_task> *dvec);

    void block(rust_cond *on);
    void wakeup(rust_cond *from);
    void die();
    void unblock();

    void check_active() { I(dom, dom->curr_task == this); }
    void check_suspended() { I(dom, dom->curr_task != this); }

    // Swap in some glue code to run when we have returned to the
    // task's context (assuming we're the active task).
    void run_after_return(size_t nargs, uintptr_t glue);

    // Swap in some glue code to run when we're next activated
    // (assuming we're the suspended task).
    void run_on_resume(uintptr_t glue);

    // Save callee-saved registers and return to the main loop.
    void yield(size_t nargs);

    // Fail this task (assuming caller-on-stack is different task).
    void kill();

    // Fail self, assuming caller-on-stack is this task.
    void fail(size_t nargs);

    // Notify tasks waiting for us that we are about to die.
    void notify_waiting_tasks();

    uintptr_t get_fp();
    uintptr_t get_previous_fp(uintptr_t fp);
    frame_glue_fns *get_frame_glue_fns(uintptr_t fp);
};

struct rust_port : public rc_base<rust_port>,
                   public task_owned<rust_port>,
                   public rust_cond {
    // fields known only to the runtime
    rust_task *task;
    size_t unit_sz;
    ptr_vec<rust_token> writers;
    ptr_vec<rust_chan> chans;

    rust_port(rust_task *task, size_t unit_sz);
    ~rust_port();
};

struct rust_token : public rust_cond {
    rust_chan *chan;      // Link back to the channel this token belongs to
    size_t idx;           // Index into port->writers.
    bool submitted;       // Whether token is in a port->writers.

    rust_token(rust_chan *chan);
    ~rust_token();

    bool pending() const;
    void submit();
    void withdraw();
};

struct rust_chan : public rc_base<rust_chan>,
                   public task_owned<rust_chan> {
    // fields known only to the runtime
    rust_task* task;
    rust_port* port;
    circ_buf buf;
    size_t idx;           // Index into port->chans.

    // token belonging to this chan, it will be placed into a port's
    // writers vector if we have something to send to the port
    rust_token token;

    rust_chan(rust_task *task, rust_port *port);
    ~rust_chan();

    void disassociate();
};

// Reference counted objects

template <typename T>
rc_base<T>::rc_base() :
    refcnt(1)
{
}

template <typename T>
rc_base<T>::~rc_base()
{
}

// Utility type: pointer-vector.

template <typename T>
ptr_vec<T>::ptr_vec(rust_dom *dom) :
    dom(dom),
    alloc(INIT_SIZE),
    fill(0),
    data(new (dom) T*[alloc])
{
    I(dom, data);
    dom->log(LOG_MEM,
             "new ptr_vec(data=0x%" PRIxPTR ") -> 0x%" PRIxPTR,
             (uintptr_t)data, (uintptr_t)this);
}

template <typename T>
ptr_vec<T>::~ptr_vec()
{
    I(dom, data);
    dom->log(LOG_MEM,
             "~ptr_vec 0x%" PRIxPTR ", data=0x%" PRIxPTR,
             (uintptr_t)this, (uintptr_t)data);
    I(dom, fill == 0);
    dom->free(data);
}

template <typename T> T *&
ptr_vec<T>::operator[](size_t offset) {
    I(dom, data[offset]->idx == offset);
    return data[offset];
}

template <typename T>
void
ptr_vec<T>::push(T *p)
{
    I(dom, data);
    I(dom, fill <= alloc);
    if (fill == alloc) {
        alloc *= 2;
        data = (T **)dom->realloc(data, alloc * sizeof(T*));
        I(dom, data);
    }
    I(dom, fill < alloc);
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
    I(dom, data);
    if (sz <= (alloc / 4) &&
        (alloc / 2) >= INIT_SIZE) {
        alloc /= 2;
        I(dom, alloc >= fill);
        data = (T **)dom->realloc(data, alloc * sizeof(T*));
        I(dom, data);
    }
}

template <typename T>
void
ptr_vec<T>::swapdel(T *item)
{
    /* Swap the endpoint into i and decr fill. */
    I(dom, data);
    I(dom, fill > 0);
    I(dom, item->idx < fill);
    fill--;
    if (fill > 0) {
        T *subst = data[fill];
        size_t idx = item->idx;
        data[idx] = subst;
        subst->idx = idx;
    }
}

// Utility type: circular buffer.

circ_buf::circ_buf(rust_dom *dom, size_t unit_sz) :
    dom(dom),
    alloc(INIT_CIRC_BUF_UNITS * unit_sz),
    unit_sz(unit_sz),
    next(0),
    unread(0),
    data((uint8_t *)dom->calloc(alloc))
{
    I(dom, unit_sz);
    dom->log(LOG_MEM|LOG_COMM,
             "new circ_buf(alloc=%d, unread=%d) -> circ_buf=0x%" PRIxPTR,
             alloc, unread, this);
    I(dom, data);
}

circ_buf::~circ_buf()
{
    dom->log(LOG_MEM|LOG_COMM,
             "~circ_buf 0x%" PRIxPTR,
             this);
    I(dom, data);
    // I(dom, unread == 0);
    dom->free(data);
}

void
circ_buf::transfer(void *dst)
{
    size_t i;
    uint8_t *d = (uint8_t *)dst;
    I(dom, dst);
    for (i = 0; i < unread; i += unit_sz)
        memcpy(&d[i], &data[next + i % alloc], unit_sz);
}

void
circ_buf::push(void *src)
{
    size_t i;
    void *tmp;

    I(dom, src);
    I(dom, unread <= alloc);

    /* Grow if necessary. */
    if (unread == alloc) {
        I(dom, alloc <= MAX_CIRC_BUF_SIZE);
        tmp = dom->malloc(alloc << 1);
        transfer(tmp);
        alloc <<= 1;
        dom->free(data);
        data = (uint8_t *)tmp;
    }

    dom->log(LOG_MEM|LOG_COMM,
             "circ buf push, unread=%d, alloc=%d, unit_sz=%d",
             unread, alloc, unit_sz);

    I(dom, unread < alloc);
    I(dom, unread + unit_sz <= alloc);

    i = (next + unread) % alloc;
    memcpy(&data[i], src, unit_sz);

    dom->log(LOG_MEM|LOG_COMM, "pushed data at index %d", i);
    unread += unit_sz;
}

void
circ_buf::shift(void *dst)
{
    size_t i;
    void *tmp;

    I(dom, dst);
    I(dom, unit_sz > 0);
    I(dom, unread >= unit_sz);
    I(dom, unread <= alloc);
    I(dom, data);
    i = next;
    memcpy(dst, &data[i], unit_sz);
    dom->log(LOG_MEM|LOG_COMM, "shifted data from index %d", i);
    unread -= unit_sz;
    next += unit_sz;
    I(dom, next <= alloc);
    if (next == alloc)
        next = 0;

    /* Shrink if necessary. */
    if (alloc >= INIT_CIRC_BUF_UNITS * unit_sz &&
        unread <= alloc / 4) {
        tmp = dom->malloc(alloc / 2);
        transfer(tmp);
        alloc >>= 1;
        dom->free(data);
        data = (uint8_t *)tmp;
    }
}

// Vectors

rust_vec::rust_vec(rust_dom *dom, size_t alloc, size_t fill, uint8_t const *d) :
    alloc(alloc),
    fill(fill)
{
    if (d || fill) {
        I(dom, d);
        I(dom, fill);
        memcpy(&data[0], d, fill);
    }
}

rust_vec::~rust_vec()
{
}

// Ports

rust_port::rust_port(rust_task *task, size_t unit_sz) :
    task(task),
    unit_sz(unit_sz),
    writers(task->dom),
    chans(task->dom)
{
    rust_dom *dom = task->dom;
    dom->log(LOG_MEM|LOG_COMM,
             "new rust_port(task=0x%" PRIxPTR ", unit_sz=%d) -> port=0x%"
             PRIxPTR, (uintptr_t)task, unit_sz, (uintptr_t)this);
}

rust_port::~rust_port()
{
    rust_dom *dom = task->dom;
    dom->log(LOG_COMM|LOG_MEM,
             "~rust_port 0x%" PRIxPTR,
             (uintptr_t)this);
    while (chans.length() > 0)
        chans.pop()->disassociate();
}

rust_chan::rust_chan(rust_task *task, rust_port *port) :
    task(task),
    port(port),
    buf(task->dom, port->unit_sz),
    token(this)
{
    if (port)
        port->chans.push(this);
}

rust_chan::~rust_chan()
{
    if (port) {
        if (token.pending())
            token.withdraw();
        port->chans.swapdel(this);
    }
}

void
rust_chan::disassociate()
{
    I(task->dom, port);

    if (token.pending())
        token.withdraw();

    // delete reference to the port
    port = NULL;
}

/* Tokens */

rust_token::rust_token(rust_chan *chan) :
    chan(chan),
    idx(0),
    submitted(false)
{
}

rust_token::~rust_token()
{
}

bool
rust_token::pending() const
{
    return submitted;
}

void
rust_token::submit()
{
    rust_port *port = chan->port;
    rust_dom *dom = chan->task->dom;

    I(dom, port);
    I(dom, !submitted);

    port->writers.push(this);
    submitted = true;
}

void
rust_token::withdraw()
{
    rust_task *task = chan->task;
    rust_port *port = chan->port;
    rust_dom *dom = task->dom;

    I(dom, port);
    I(dom, submitted);

    if (task->blocked())
        task->wakeup(this); // must be blocked on us (or dead)
    port->writers.swapdel(this);
    submitted = false;
}

/* Stacks */

static size_t const min_stk_bytes = 0x300;

static stk_seg*
new_stk(rust_dom *dom, size_t minsz)
{
    if (minsz < min_stk_bytes)
        minsz = min_stk_bytes;
    size_t sz = sizeof(stk_seg) + minsz;
    stk_seg *stk = (stk_seg *)dom->malloc(sz);
    dom->logptr("new stk", (uintptr_t)stk);
    memset(stk, 0, sizeof(stk_seg));
    stk->limit = (uintptr_t) &stk->data[minsz];
    dom->logptr("stk limit", stk->limit);
    stk->valgrind_id =
        VALGRIND_STACK_REGISTER(&stk->data[0],
                                &stk->data[minsz]);
    return stk;
}

static void
del_stk(rust_dom *dom, stk_seg *stk)
{
    VALGRIND_STACK_DEREGISTER(stk->valgrind_id);
    dom->logptr("freeing stk segment", (uintptr_t)stk);
    dom->free(stk);
}

// Tasks

// FIXME (bug 541585): ifdef by platform. This is getting absurdly
// x86-specific.

size_t const n_callee_saves = 4;
size_t const callee_save_fp = 0;

static uintptr_t
align_down(uintptr_t sp)
{
    // There is no platform we care about that needs more than a
    // 16-byte alignment.
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
upcall_grow_task(rust_task *task, size_t n_frame_bytes)
{
    LOG_UPCALL_ENTRY(task);

    rust_dom *dom = task->dom;
    stk_seg *old_stk = task->stk;
    uintptr_t old_top = (uintptr_t) old_stk->limit;
    uintptr_t old_bottom = (uintptr_t) &old_stk->data[0];
    uintptr_t rust_sp_disp = old_top - task->rust_sp;
    size_t ssz = old_top - old_bottom;
    dom->log(LOG_MEM|LOG_TASK|LOG_UPCALL,
             "upcall_grow_task(%" PRIdPTR
             "), old size %" PRIdPTR
             " bytes (old lim: 0x%" PRIxPTR ")",
             n_frame_bytes, ssz, old_top);
    ssz *= 2;
    if (ssz < n_frame_bytes)
        ssz = n_frame_bytes;
    ssz = next_power_of_two(ssz);

    dom->log(LOG_MEM|LOG_TASK, "upcall_grow_task growing stk 0x%"
             PRIxPTR " to %d bytes", old_stk, ssz);

    stk_seg *stk = new_stk(dom, ssz);
    uintptr_t new_bottom = (uintptr_t) &stk->data[0];
    uintptr_t new_top = (uintptr_t) &stk->data[ssz];
    size_t n_copy = old_top - old_bottom;
    dom->log(LOG_MEM|LOG_TASK,
             "copying %d bytes of stack from [0x%" PRIxPTR ", 0x%" PRIxPTR "]"
             " to [0x%" PRIxPTR ", 0x%" PRIxPTR "]",
             n_copy,
             old_bottom, old_bottom + n_copy,
             new_top - n_copy, new_top);

    memcpy((void*)(new_top - n_copy), (void*)old_bottom, n_copy);

    stk->valgrind_id = VALGRIND_STACK_REGISTER(new_bottom, new_top);
    stk->limit = new_top;
    task->stk = stk;
    task->rust_sp = new_top - rust_sp_disp;

    dom->log(LOG_MEM|LOG_TASK, "processing relocations");

    // FIXME (bug 541586): this is the most ridiculously crude
    // relocation scheme ever. Try actually, you know, writing out
    // reloc descriptors?
    size_t n_relocs = 0;
    for (uintptr_t* p = (uintptr_t*)(new_top - n_copy);
         p < (uintptr_t*)new_top; ++p) {
        if (old_bottom <= *p && *p < old_top) {
            //dom->log(LOG_MEM, "relocating pointer 0x%" PRIxPTR " by %d bytes",
            //        *p, (new_top - old_top));
            n_relocs++;
            *p += (new_top - old_top);
        }
    }
    dom->log(LOG_MEM|LOG_TASK, "processed %d relocations", n_relocs);

    del_stk(dom, old_stk);
    dom->logptr("grown stk limit", new_top);
}

rust_alarm::rust_alarm(rust_task *receiver) :
    receiver(receiver)
{
}

class
rust_crate_cache : public dom_owned<rust_crate_cache>,
                   public rc_base<rust_crate_cache>
{
public:

    class lib : public rc_base<lib>, public dom_owned<lib>
    {
        uintptr_t handle;
    public:
        rust_dom *dom;
        lib(rust_dom *dom, char const *name)
            : handle(0),
              dom(dom)
        {
#if defined(__WIN32__)
            handle = (uintptr_t)LoadLibrary(_T(name));
            // dom->win32_require(_T("LoadLibrary"), handle != 0);
#else
            handle = (uintptr_t)dlopen(name, RTLD_LOCAL|RTLD_LAZY);
#endif
            dom->log(LOG_LINK, "loaded library '%s' as 0x%"  PRIxPTR,
                     name, handle);
        }

        uintptr_t get_handle() {
            return handle;
        }

        ~lib() {
            dom->log(LOG_LINK, "~rust_crate_cache::lib(0x%" PRIxPTR ")",
                     handle);
            if (handle) {
#if defined(__WIN32__)
                FreeLibrary((HMODULE)handle);
#else
                dlclose((void*)handle);
#endif
            }
        }
    };


    class c_sym : public rc_base<c_sym>, public dom_owned<c_sym>
    {
        uintptr_t val;
        lib *library;

    public:
        rust_dom *dom;
        c_sym(rust_dom *dom, lib *library, char const *name)
            : val(0),
              library(library),
              dom(dom)
        {
            library->ref();
            uintptr_t handle = library->get_handle();
            if (handle) {
#if defined(__WIN32__)
                val = (uintptr_t)GetProcAddress((HMODULE)handle, _T(name));
                // dom->win32_require(_T("GetProcAddress"), val != 0);
#else
                val = (uintptr_t)dlsym((void*)handle, name);
#endif
                dom->log(LOG_LINK, "resolved symbol '%s' to 0x%"  PRIxPTR,
                         name, val);
            } else {
                dom->log(LOG_LINK, "unresolved symbol '%s', null lib handle",
                         name);
            }
        }

        uintptr_t get_val() {
            return val;
        }

        ~c_sym() {
            dom->log(LOG_LINK, "~rust_crate_cache::c_sym(0x%" PRIxPTR ")", val);
            library->deref();
        }
    };

    class rust_sym : public rc_base<rust_sym>, public dom_owned<rust_sym>
    {
        uintptr_t val;
        c_sym *crate_sym;

    public:
        rust_dom *dom;
        rust_sym(rust_dom *dom, rust_crate const *curr_crate,
                 c_sym *crate_sym, char const **path)
            : val(0),
              crate_sym(crate_sym),
              dom(dom)
        {
            crate_sym->ref();
            typedef rust_crate_reader::die_reader::die die;
            rust_crate const *crate = (rust_crate*)crate_sym->get_val();
            if (!crate) {
                dom->log(LOG_LINK,
                         "failed to resolve symbol, null crate symbol");
                return;
            }
            rust_crate_reader rdr(dom, crate);
            bool found_root = false;
            bool found_leaf = false;
            for (die d = rdr.dies.first_die();
                 !(found_root || d.is_null());
                 d = d.next_sibling()) {

                die t1 = d;
                die t2 = d;
                for (char const **c = crate_rel(curr_crate, path);
                     (*c
                      && !t1.is_null()
                      && t1.find_child_by_name(crate_rel(curr_crate, *c), t2));
                     ++c, t1=t2) {
                    dom->log(LOG_DWARF|LOG_LINK, "matched die <0x%"  PRIxPTR
                             ">, child '%s' = die<0x%" PRIxPTR ">",
                             t1.off, crate_rel(curr_crate, *c), t2.off);
                    found_root = found_root || true;
                    if (!*(c+1) && t2.find_num_attr(DW_AT_low_pc, val)) {
                        dom->log(LOG_DWARF|LOG_LINK,
                                 "found relative address: 0x%"  PRIxPTR, val);
                        dom->log(LOG_DWARF|LOG_LINK,
                                 "plus image-base 0x%"  PRIxPTR,
                                 crate->get_image_base());
                        val += crate->get_image_base();
                        found_leaf = true;
                        break;
                    }
                }
                if (found_root || found_leaf)
                    break;
            }
            if (found_leaf) {
                dom->log(LOG_LINK, "resolved symbol to 0x%"  PRIxPTR, val);
            } else {
                dom->log(LOG_LINK, "failed to resolve symbol");
            }
        }

        uintptr_t get_val() {
            return val;
        }

        ~rust_sym() {
            dom->log(LOG_LINK,
                     "~rust_crate_cache::rust_sym(0x%" PRIxPTR ")", val);
            crate_sym->deref();
        }
    };

    lib *get_lib(size_t n, char const *name)
    {
        I(dom, n < crate->n_libs);
        lib *library = libs[n];
        if (!library) {
            library = new (dom) lib(dom, name);
            libs[n] = library;
        }
        return library;
    }

    c_sym *get_c_sym(size_t n, lib *library, char const *name)
    {
        I(dom, n < crate->n_c_syms);
        c_sym *sym = c_syms[n];
        dom->log(LOG_LINK, "cached C symbol %s = 0x%" PRIxPTR, name, sym);
        if (!sym) {
            sym = new (dom) c_sym(dom, library, name);
            c_syms[n] = sym;
        }
        return sym;
    }

    rust_sym *get_rust_sym(size_t n,
                           rust_dom *dom,
                           rust_crate const *curr_crate,
                           c_sym *crate_sym,
                           char const **path)
    {
        I(dom, n < crate->n_rust_syms);
        rust_sym *sym = rust_syms[n];
        if (!sym) {
            sym = new (dom) rust_sym(dom, curr_crate, crate_sym, path);
            rust_syms[n] = sym;
        }
        return sym;
    }

private:

    // Caches.
    rust_sym **rust_syms;
    c_sym **c_syms;
    lib **libs;

public:

    rust_crate const *crate;
    rust_dom *dom;
    size_t idx;

    rust_crate_cache(rust_dom *dom,
                     rust_crate const *crate)
        : rust_syms((rust_sym**)
                    dom->calloc(sizeof(rust_sym*) * crate->n_rust_syms)),
          c_syms((c_sym**) dom->calloc(sizeof(c_sym*) * crate->n_c_syms)),
          libs((lib**) dom->calloc(sizeof(lib*) * crate->n_libs)),
          crate(crate),
          dom(dom),
          idx(0)
    {
        I(dom, rust_syms);
        I(dom, c_syms);
        I(dom, libs);
    }

    void flush() {
        dom->log(LOG_LINK, "rust_crate_cache::flush()");
        for (size_t i = 0; i < crate->n_rust_syms; ++i) {
            rust_sym *s = rust_syms[i];
            if (s) {
                dom->log(LOG_LINK, "rust_crate_cache::flush() deref rust_sym %"
                         PRIdPTR " (rc=%" PRIdPTR ")", i, s->refcnt);
                s->deref();
            }
            rust_syms[i] = NULL;
        }

        for (size_t i = 0; i < crate->n_c_syms; ++i) {
            c_sym *s = c_syms[i];
            if (s) {
                dom->log(LOG_LINK, "rust_crate_cache::flush() deref c_sym %"
                         PRIdPTR " (rc=%" PRIdPTR ")", i, s->refcnt);
                s->deref();
            }
            c_syms[i] = NULL;
        }

        for (size_t i = 0; i < crate->n_libs; ++i) {
            lib *l = libs[i];
            if (l) {
                dom->log(LOG_LINK, "rust_crate_cache::flush() deref lib %"
                         PRIdPTR " (rc=%" PRIdPTR ")", i, l->refcnt);
                l->deref();
            }
            libs[i] = NULL;
        }
    }

    ~rust_crate_cache()
    {
        flush();
        dom->free(rust_syms);
        dom->free(c_syms);
        dom->free(libs);
    }

};

rust_task::rust_task(rust_dom *dom, rust_task *spawner) :
    stk(new_stk(dom, 0)),
    runtime_sp(0),
    rust_sp(stk->limit),
    gc_alloc_chain(0),
    dom(dom),
    cache(NULL),
    state(&dom->running_tasks),
    cond(NULL),
    dptr(0),
    spawner(spawner),
    idx(0),
    waiting_tasks(dom),
    alarm(this)
{
    dom->logptr("new task", (uintptr_t)this);
}

rust_task::~rust_task()
{
    dom->log(LOG_MEM|LOG_TASK,
             "~rust_task 0x%" PRIxPTR ", refcnt=%d",
             (uintptr_t)this, refcnt);

    /*
      for (uintptr_t fp = get_fp(); fp; fp = get_previous_fp(fp)) {
      frame_glue_fns *glue_fns = get_frame_glue_fns(fp);
      dom->log(LOG_MEM|LOG_TASK,
      "~rust_task, frame fp=0x%" PRIxPTR ", glue_fns=0x%" PRIxPTR,
      fp, glue_fns);
      if (glue_fns) {
      dom->log(LOG_MEM|LOG_TASK, "~rust_task, mark_glue=0x%" PRIxPTR,
               glue_fns->mark_glue);
      dom->log(LOG_MEM|LOG_TASK, "~rust_task, drop_glue=0x%" PRIxPTR,
               glue_fns->drop_glue);
      dom->log(LOG_MEM|LOG_TASK, "~rust_task, reloc_glue=0x%" PRIxPTR,
               glue_fns->reloc_glue);
      }
      }
    */

    /* FIXME: tighten this up, there are some more
       assertions that hold at task-lifecycle events. */
    I(dom, refcnt == 0 ||
      (refcnt == 1 && this == dom->root_task));

    del_stk(dom, stk);
    if (cache)
        cache->deref();
}

void
rust_task::start(uintptr_t exit_task_glue,
                 uintptr_t spawnee_fn,
                 uintptr_t args,
                 size_t callsz)
{
    dom->logptr("exit-task glue", exit_task_glue);
    dom->logptr("from spawnee", spawnee_fn);

    // Set sp to last uintptr_t-sized cell of segment and align down.
    rust_sp -= sizeof(uintptr_t);
    rust_sp = align_down(rust_sp);

    // Begin synthesizing frames. There are two: a "fully formed"
    // exit-task frame at the top of the stack -- that pretends to be
    // mid-execution -- and a just-starting frame beneath it that
    // starts executing the first instruction of the spawnee. The
    // spawnee *thinks* it was called by the exit-task frame above
    // it. It wasn't; we put that fake frame in place here, but the
    // illusion is enough for the spawnee to return to the exit-task
    // frame when it's done, and exit.
    uintptr_t *spp = (uintptr_t *)rust_sp;

    // The exit_task_glue frame we synthesize above the frame we activate:
    *spp-- = (uintptr_t) this;       // task
    *spp-- = (uintptr_t) 0;          // output
    *spp-- = (uintptr_t) 0;          // retpc
    for (size_t j = 0; j < n_callee_saves; ++j) {
        *spp-- = 0;
    }

    // We want 'frame_base' to point to the last callee-save in this
    // (exit-task) frame, because we're going to inject this
    // frame-pointer into the callee-save frame pointer value in the
    // *next* (spawnee) frame. A cheap trick, but this means the
    // spawnee frame will restore the proper frame pointer of the glue
    // frame as it runs its epilogue.
    uintptr_t frame_base = (uintptr_t) (spp+1);

    *spp-- = (uintptr_t) dom->root_crate;  // crate ptr
    *spp-- = (uintptr_t) 0;                // frame_glue_fns

    // Copy args from spawner to spawnee.
    if (args)  {
        uintptr_t *src = (uintptr_t *)args;
        src += 1;                  // spawn-call output slot
        src += 1;                  // spawn-call task slot
        // Memcpy all but the task and output pointers
        callsz -= (2 * sizeof(uintptr_t));
        spp = (uintptr_t*) (((uintptr_t)spp) - callsz);
        memcpy(spp, src, callsz);

        // Move sp down to point to task cell.
        spp--;
    } else {
        // We're at root, starting up.
        I(dom, callsz==0);
    }

    // The *implicit* incoming args to the spawnee frame we're
    // activating: FIXME (bug 541587): wire up output-address properly
    // so spawnee can write a return value.
    *spp-- = (uintptr_t) this;            // task
    *spp-- = (uintptr_t) 0;               // output addr
    *spp-- = (uintptr_t) exit_task_glue;  // retpc

    // The context the activate_glue needs to switch stack.
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

    dom->add_task_to_state_vec(&dom->running_tasks, this);
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
rust_task::run_after_return(size_t nargs, uintptr_t glue)
{
    // This is only safe to call if we're the currently-running task.
    check_active();

    uintptr_t sp = runtime_sp;

    // The compiler reserves nargs + 1 word for oldsp on the stack and
    // then aligns it.
    sp = align_down(sp - nargs * sizeof(uintptr_t));

    uintptr_t *retpc = ((uintptr_t *) sp) - 1;
    dom->log(LOG_TASK|LOG_MEM,
             "run_after_return: overwriting retpc=0x%" PRIxPTR
             " @ runtime_sp=0x%" PRIxPTR
             " with glue=0x%" PRIxPTR,
             *retpc, sp, glue);

    // Move the current return address (which points into rust code)
    // onto the rust stack and pretend we just called into the glue.
    push_onto_thread_stack(rust_sp, *retpc);
    *retpc = glue;
}

void
rust_task::run_on_resume(uintptr_t glue)
{
    // This is only safe to call if we're suspended.
    check_suspended();

    // Inject glue as resume address in the suspended frame.
    uintptr_t* rsp = (uintptr_t*) rust_sp;
    rsp += n_callee_saves;
    dom->log(LOG_TASK|LOG_MEM,
             "run_on_resume: overwriting retpc=0x%" PRIxPTR
             " @ rust_sp=0x%" PRIxPTR
             " with glue=0x%" PRIxPTR,
             *rsp, rsp, glue);
    *rsp = glue;
}

void
rust_task::yield(size_t nargs)
{
    dom->log(LOG_TASK,
             "task 0x%" PRIxPTR " yielding", this);
    run_after_return(nargs, dom->root_crate->get_yield_glue());
}

static inline uintptr_t
get_callee_save_fp(uintptr_t *top_of_callee_saves)
{
    return top_of_callee_saves[n_callee_saves - (callee_save_fp + 1)];
}

void
rust_task::kill() {
    // Note the distinction here: kill() is when you're in an upcall
    // from task A and want to force-fail task B, you do B->kill().
    // If you want to fail yourself you do self->fail(upcall_nargs).
    dom->log(LOG_TASK, "killing task 0x%" PRIxPTR, this);
    // Unblock the task so it can unwind.
    unblock();
    if (this == dom->root_task)
        dom->fail();
    run_on_resume(dom->root_crate->get_unwind_glue());
}

void
rust_task::fail(size_t nargs) {
    // See note in ::kill() regarding who should call this.
    dom->log(LOG_TASK, "task 0x%" PRIxPTR " failing", this);
    // Unblock the task so it can unwind.
    unblock();
    if (this == dom->root_task)
        dom->fail();
    run_after_return(nargs, dom->root_crate->get_unwind_glue());
    if (spawner) {
        dom->log(LOG_TASK,
                 "task 0x%" PRIxPTR
                 " propagating failure to parent 0x%" PRIxPTR,
                 this, spawner);
        spawner->kill();
    }
}

void
rust_task::notify_waiting_tasks()
{
    while (waiting_tasks.length() > 0) {
        rust_task *t = waiting_tasks.pop()->receiver;
        if (!t->dead())
            t->wakeup(this);
    }
}

uintptr_t
rust_task::get_fp() {
    // sp in any suspended task points to the last callee-saved reg on
    // the task stack.
    return get_callee_save_fp((uintptr_t*)rust_sp);
}

uintptr_t
rust_task::get_previous_fp(uintptr_t fp) {
    // fp happens to, coincidentally (!) also point to the last
    // callee-save on the task stack.
    return get_callee_save_fp((uintptr_t*)fp);
}

frame_glue_fns*
rust_task::get_frame_glue_fns(uintptr_t fp) {
    fp -= sizeof(uintptr_t);
    return *((frame_glue_fns**) fp);
}

bool
rust_task::running()
{
    return state == &dom->running_tasks;
}

bool
rust_task::blocked()
{
    return state == &dom->blocked_tasks;
}

bool
rust_task::blocked_on(rust_cond *on)
{
    return blocked() && cond == on;
}

bool
rust_task::dead()
{
    return state == &dom->dead_tasks;
}

void
rust_task::transition(ptr_vec<rust_task> *src, ptr_vec<rust_task> *dst)
{
    I(dom, state == src);
    dom->log(LOG_TASK,
             "task 0x%" PRIxPTR " state change '%s' -> '%s'",
             (uintptr_t)this,
             dom->state_vec_name(src),
             dom->state_vec_name(dst));
    dom->remove_task_from_state_vec(src, this);
    dom->add_task_to_state_vec(dst, this);
    state = dst;
}

void
rust_task::block(rust_cond *on)
{
    I(dom, on);
    transition(&dom->running_tasks, &dom->blocked_tasks);
    dom->log(LOG_TASK,
             "task 0x%" PRIxPTR " blocking on 0x%" PRIxPTR,
             (uintptr_t)this,
             (uintptr_t)on);
    cond = on;
}

void
rust_task::wakeup(rust_cond *from)
{
    transition(&dom->blocked_tasks, &dom->running_tasks);
    I(dom, cond == from);
}

void
rust_task::die()
{
    transition(&dom->running_tasks, &dom->dead_tasks);
}

void
rust_task::unblock()
{
    if (blocked())
        wakeup(cond);
}

// Domains

static void
del_all_tasks(rust_dom *dom, ptr_vec<rust_task> *v) {
    I(dom, v);
    while (v->length()) {
        dom->log(LOG_TASK, "deleting task %" PRIdPTR, v->length() - 1);
        delete v->pop();
    }
}

rust_dom::rust_dom(rust_srv *srv, rust_crate const *root_crate) :
    root_crate(root_crate),
    srv(srv),
    logbits(get_logbits()),
    running_tasks(this),
    blocked_tasks(this),
    dead_tasks(this),
    caches(this),
    root_task(NULL),
    curr_task(NULL),
    rval(0)
{
    logptr("new dom", (uintptr_t)this);
    memset(&rctx, 0, sizeof(rctx));

#ifdef __WIN32__
    {
        HCRYPTPROV hProv;
        win32_require
            (_T("CryptAcquireContext"),
             CryptAcquireContext(&hProv, NULL, NULL, PROV_RSA_FULL,
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
    pthread_attr_init(&attr);
    pthread_attr_setstacksize(&attr, 1024 * 1024);
    pthread_attr_setdetachstate(&attr, true);
#endif
    randinit(&rctx, 1);

    root_task = new (this) rust_task(this, NULL);
}

rust_dom::~rust_dom() {
    log(LOG_TASK, "deleting all running tasks");
    del_all_tasks(this, &running_tasks);
    log(LOG_TASK, "deleting all blocked tasks");
    del_all_tasks(this, &blocked_tasks);
    log(LOG_TASK, "deleting all dead tasks");
    del_all_tasks(this, &dead_tasks);
#ifndef __WIN32__
    pthread_attr_destroy(&attr);
#endif
    while (caches.length())
        delete caches.pop();
}

void
rust_dom::activate(rust_task *task) {
    curr_task = task;
    root_crate->get_activate_glue()(task);
    curr_task = NULL;
}

void
rust_dom::log(uint32_t logbit, char const *fmt, ...) {
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
rust_dom::logptr(char const *msg, uintptr_t ptrval) {
    log(LOG_MEM, "%s 0x%" PRIxPTR, msg, ptrval);
}

template<typename T> void
rust_dom::logptr(char const *msg, T* ptrval) {
    log(LOG_MEM, "%s 0x%" PRIxPTR, msg, (uintptr_t)ptrval);
}


void
rust_dom::fail() {
    log(LOG_DOM, "domain 0x%" PRIxPTR " root task failed", this);
    I(this, rval == 0);
    rval = 1;
}

void *
rust_dom::malloc(size_t sz) {
    void *p = srv->malloc(sz);
    I(this, p);
    log(LOG_MEM, "rust_dom::malloc(%d) -> 0x%" PRIxPTR,
        sz, p);
    return p;
}

void *
rust_dom::calloc(size_t sz) {
    void *p = this->malloc(sz);
    memset(p, 0, sz);
    return p;
}

void *
rust_dom::realloc(void *p, size_t sz) {
    void *p1 = srv->realloc(p, sz);
    I(this, p1);
    log(LOG_MEM, "rust_dom::realloc(0x%" PRIxPTR ", %d) -> 0x%" PRIxPTR,
        p, sz, p1);
    return p1;
}

void
rust_dom::free(void *p) {
    log(LOG_MEM, "rust_dom::free(0x%" PRIxPTR ")", p);
    I(this, p);
    srv->free(p);
}

#ifdef __WIN32__
void
rust_dom::win32_require(LPCTSTR fn, BOOL ok) {
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
rust_dom::n_live_tasks()
{
    return running_tasks.length() + blocked_tasks.length();
}

void
rust_dom::add_task_to_state_vec(ptr_vec<rust_task> *v, rust_task *task)
{
    log(LOG_MEM|LOG_TASK,
        "adding task 0x%" PRIxPTR " in state '%s' to vec 0x%" PRIxPTR,
        (uintptr_t)task, state_vec_name(v), (uintptr_t)v);
    v->push(task);
}


void
rust_dom::remove_task_from_state_vec(ptr_vec<rust_task> *v, rust_task *task)
{
    log(LOG_MEM|LOG_TASK,
        "removing task 0x%" PRIxPTR " in state '%s' from vec 0x%" PRIxPTR,
        (uintptr_t)task, state_vec_name(v), (uintptr_t)v);
    I(this, (*v)[task->idx] == task);
    v->swapdel(task);
}

const char *
rust_dom::state_vec_name(ptr_vec<rust_task> *v)
{
    if (v == &running_tasks)
        return "running";
    if (v == &blocked_tasks)
        return "blocked";
    I(this, v == &dead_tasks);
    return "dead";
}

void
rust_dom::reap_dead_tasks()
{
    for (size_t i = 0; i < dead_tasks.length(); ) {
        rust_task *t = dead_tasks[i];
        if (t == root_task || t->refcnt == 0) {
            I(this, !t->waiting_tasks.length());
            dead_tasks.swapdel(t);
            log(LOG_TASK, "deleting unreferenced dead task 0x%" PRIxPTR, t);
            delete t;
            continue;
        }
        ++i;
    }
}

rust_task *
rust_dom::sched()
{
    I(this, this);
    // FIXME: in the face of failing tasks, this is not always right.
    // I(this, n_live_tasks() > 0);
    if (running_tasks.length() > 0) {
        size_t i = rand(&rctx);
        i %= running_tasks.length();
        return (rust_task *)running_tasks[i];
    }
    log(LOG_DOM|LOG_TASK,
        "no schedulable tasks");
    return NULL;
}

/* Upcalls */

extern "C" CDECL char const *str_buf(rust_task *task, rust_str *s);

extern "C" CDECL void
upcall_log_int(rust_task *task, int32_t i)
{
    LOG_UPCALL_ENTRY(task);
    task->dom->log(LOG_UPCALL|LOG_ULOG,
                   "upcall log_int(0x%" PRIx32 " = %" PRId32 " = '%c')",
                   i, i, (char)i);
}

extern "C" CDECL void
upcall_log_str(rust_task *task, rust_str *str)
{
    LOG_UPCALL_ENTRY(task);
    const char *c = str_buf(task, str);
    task->dom->log(LOG_UPCALL|LOG_ULOG,
                   "upcall log_str(\"%s\")",
                   c);
}

extern "C" CDECL void
upcall_trace_word(rust_task *task, uintptr_t i)
{
    LOG_UPCALL_ENTRY(task);
    task->dom->log(LOG_UPCALL|LOG_TRACE,
                   "trace: 0x%" PRIxPTR "",
                   i, i, (char)i);
}

extern "C" CDECL void
upcall_trace_str(rust_task *task, char const *c)
{
    LOG_UPCALL_ENTRY(task);
    task->dom->log(LOG_UPCALL|LOG_TRACE,
                   "trace: %s",
                   c);
}

extern "C" CDECL rust_port*
upcall_new_port(rust_task *task, size_t unit_sz)
{
    LOG_UPCALL_ENTRY(task);
    rust_dom *dom = task->dom;
    dom->log(LOG_UPCALL|LOG_MEM|LOG_COMM,
             "upcall_new_port(task=0x%" PRIxPTR ", unit_sz=%d)",
             (uintptr_t)task, unit_sz);
    return new (dom) rust_port(task, unit_sz);
}

extern "C" CDECL void
upcall_del_port(rust_task *task, rust_port *port)
{
    LOG_UPCALL_ENTRY(task);
    task->dom->log(LOG_UPCALL|LOG_MEM|LOG_COMM,
                   "upcall del_port(0x%" PRIxPTR ")", (uintptr_t)port);
    I(task->dom, !port->refcnt);
    delete port;
}

extern "C" CDECL rust_chan*
upcall_new_chan(rust_task *task, rust_port *port)
{
    LOG_UPCALL_ENTRY(task);
    rust_dom *dom = task->dom;
    dom->log(LOG_UPCALL|LOG_MEM|LOG_COMM,
             "upcall_new_chan(task=0x%" PRIxPTR ", port=0x%" PRIxPTR ")",
             (uintptr_t)task, port);
    I(dom, port);
    return new (dom) rust_chan(task, port);
}

extern "C" CDECL void
upcall_del_chan(rust_task *task, rust_chan *chan)
{
    LOG_UPCALL_ENTRY(task);
    rust_dom *dom = task->dom;
    dom->log(LOG_UPCALL|LOG_MEM|LOG_COMM,
             "upcall del_chan(0x%" PRIxPTR ")", (uintptr_t)chan);
    I(dom, !chan->refcnt);
    delete chan;
}

extern "C" CDECL rust_chan *
upcall_clone_chan(rust_task *task, rust_task *owner, rust_chan *chan)
{
    LOG_UPCALL_ENTRY(task);
    rust_dom *dom = task->dom;
    dom->log(LOG_UPCALL|LOG_MEM|LOG_COMM,
             "upcall clone_chan(owner 0x%" PRIxPTR ", chan 0x%" PRIxPTR ")",
             (uintptr_t)owner, (uintptr_t)chan);
    return new (owner->dom) rust_chan(owner, chan->port);
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
attempt_transmission(rust_dom *dom,
                     rust_chan *src,
                     rust_task *dst)
{
    I(dom, src);
    I(dom, dst);

    rust_port *port = src->port;
    if (!port) {
        dom->log(LOG_COMM,
                 "src died, transmission incomplete");
        return 0;
    }

    circ_buf *buf = &src->buf;
    if (buf->unread == 0) {
        dom->log(LOG_COMM,
                 "buffer empty, transmission incomplete");
        return 0;
    }

    if (!dst->blocked_on(port)) {
        dom->log(LOG_COMM,
                 "dst in non-reading state, transmission incomplete");
        return 0;
    }

    uintptr_t *dptr = dst->dptr;
    dom->log(LOG_COMM,
             "receiving %d bytes into dst_task=0x%" PRIxPTR
             ", dptr=0x%" PRIxPTR,
             port->unit_sz, dst, dptr);
    buf->shift(dptr);

    // Wake up the sender if its waiting for the send operation.
    rust_task *sender = src->task;
    rust_token *token = &src->token;
    if (sender->blocked_on(token))
        sender->wakeup(token);

    // Wakeup the receiver, there is new data.
    dst->wakeup(port);

    dom->log(LOG_COMM, "transmission complete");
    return 1;
}

extern "C" CDECL void
upcall_yield(rust_task *task)
{
    LOG_UPCALL_ENTRY(task);
    rust_dom *dom = task->dom;
    dom->log(LOG_UPCALL|LOG_COMM, "upcall yield()");
    task->yield(1);
}

extern "C" CDECL void
upcall_join(rust_task *task, rust_task *other)
{
    LOG_UPCALL_ENTRY(task);
    rust_dom *dom = task->dom;
    dom->log(LOG_UPCALL|LOG_COMM,
             "upcall join(other=0x%" PRIxPTR ")",
             (uintptr_t)other);

    // If the other task is already dying, we dont have to wait for it.
    if (!other->dead()) {
        other->waiting_tasks.push(&task->alarm);
        task->block(other);
        task->yield(2);
    }
}

extern "C" CDECL void
upcall_send(rust_task *task, rust_chan *chan, void *sptr)
{
    LOG_UPCALL_ENTRY(task);
    rust_dom *dom = task->dom;
    dom->log(LOG_UPCALL|LOG_COMM,
             "upcall send(chan=0x%" PRIxPTR ", sptr=0x%" PRIxPTR ")",
             (uintptr_t)chan,
             (uintptr_t)sptr);

    I(dom, chan);
    I(dom, sptr);

    rust_port *port = chan->port;
    dom->log(LOG_MEM|LOG_COMM,
             "send to port", (uintptr_t)port);
    I(dom, port);

    rust_token *token = &chan->token;
    dom->log(LOG_MEM|LOG_COMM,
             "sending via token 0x%" PRIxPTR,
             (uintptr_t)token);

    if (port->task) {
        chan->buf.push(sptr);
        task->block(token);
        attempt_transmission(dom, chan, port->task);
        if (chan->buf.unread && !token->pending())
            token->submit();
    } else {
        dom->log(LOG_COMM|LOG_ERR,
                 "port has no task (possibly throw?)");
    }

    if (!task->running())
        task->yield(3);
}

extern "C" CDECL void
upcall_recv(rust_task *task, uintptr_t *dptr, rust_port *port)
{
    LOG_UPCALL_ENTRY(task);
    rust_dom *dom = task->dom;
    dom->log(LOG_UPCALL|LOG_COMM,
             "upcall recv(dptr=0x" PRIxPTR ", port=0x%" PRIxPTR ")",
             (uintptr_t)dptr,
             (uintptr_t)port);

    I(dom, port);
    I(dom, port->task);
    I(dom, task);
    I(dom, port->task == task);

    task->block(port);

    if (port->writers.length() > 0) {
        I(dom, task->dom);
        size_t i = rand(&dom->rctx);
        i %= port->writers.length();
        rust_token *token = port->writers[i];
        rust_chan *chan = token->chan;
        if (attempt_transmission(dom, chan, task))
            token->withdraw();
    } else {
        dom->log(LOG_COMM,
                 "no writers sending to port", (uintptr_t)port);
    }

    if (!task->running()) {
        task->dptr = dptr;
        task->yield(3);
    }
}

extern "C" CDECL void
upcall_fail(rust_task *task, char const *expr, char const *file, size_t line)
{
    LOG_UPCALL_ENTRY(task);
    task->dom->log(LOG_UPCALL|LOG_ERR, "upcall fail '%s', %s:%" PRIdPTR,
                   expr, file, line);
    task->fail(4);
}

extern "C" CDECL void
upcall_kill(rust_task *task, rust_task *target)
{
    LOG_UPCALL_ENTRY(task);
    task->dom->log(LOG_UPCALL, "upcall kill target=0x%" PRIxPTR, target);
    target->kill();
}

extern "C" CDECL void
upcall_exit(rust_task *task)
{
    LOG_UPCALL_ENTRY(task);

    rust_dom *dom = task->dom;
    dom->log(LOG_UPCALL, "upcall exit");
    task->die();
    task->notify_waiting_tasks();
    task->yield(1);
}

extern "C" CDECL uintptr_t
upcall_malloc(rust_task *task, size_t nbytes)
{
    LOG_UPCALL_ENTRY(task);

    void *p = task->dom->malloc(nbytes);
    task->dom->log(LOG_UPCALL|LOG_MEM,
                   "upcall malloc(%u) = 0x%" PRIxPTR,
                   nbytes, (uintptr_t)p);
    return (uintptr_t) p;
}

extern "C" CDECL void
upcall_free(rust_task *task, void* ptr)
{
    LOG_UPCALL_ENTRY(task);

    rust_dom *dom = task->dom;
    dom->log(LOG_UPCALL|LOG_MEM,
             "upcall free(0x%" PRIxPTR ")",
             (uintptr_t)ptr);
    dom->free(ptr);
}

extern "C" CDECL rust_str *
upcall_new_str(rust_task *task, char const *s, size_t fill)
{
    LOG_UPCALL_ENTRY(task);
    rust_dom *dom = task->dom;
    dom->log(LOG_UPCALL|LOG_MEM,
             "upcall new_str('%s', %" PRIdPTR ")", s, fill);
    size_t alloc = next_power_of_two(sizeof(rust_str) + fill);
    void *mem = dom->malloc(alloc);
    if (!mem) {
        task->fail(3);
        return NULL;
    }
    rust_str *st = new (mem) rust_str(dom, alloc, fill, (uint8_t const *)s);
    dom->log(LOG_UPCALL|LOG_MEM,
             "upcall new_str('%s', %" PRIdPTR ") = 0x%" PRIxPTR,
             s, fill, st);
    return st;
}

/*
 * Here we have dst = a + b for strings.  If dst and a point to the
 * same rust_str, *and* that rust_str is singly-referenced, then we
 * optimise by growing it in place.  Otherwise, we want dst to point
 * to a brand-new rust_str, formed by concatenating a and b (note that
 * dst and a might still be the same in this case, but not
 * singly-referenced).
 *
 * Returns the new value for the slot dst.
 */
extern "C" CDECL rust_str *
upcall_str_concat(rust_task *task, rust_str *dst, rust_str *a, rust_str *b)
{
    LOG_UPCALL_ENTRY(task);

    size_t fill = a->fill + b->fill - 1;
    size_t alloc = next_power_of_two(sizeof(rust_str) + fill);

    // fast path
    if (dst == a && a->refcnt == 1) {
        if (alloc > a->alloc) {
            void *mem = task->dom->realloc(a, alloc);
            a = static_cast<rust_str *>(mem);
            a->alloc = alloc;
        }
        memcpy(&a->data[a->fill - 1], b->data, b->fill);
        a->fill = fill;
        return a;
    }

    // regular ol' path
    // FIXME implement?
    return NULL;
}

extern "C" CDECL rust_str *
upcall_vec_realloc(rust_task *task, rust_vec *v, size_t n_bytes)
{
    LOG_UPCALL_ENTRY(task);
    rust_dom *dom = task->dom;
    size_t alloc = next_power_of_two(sizeof(rust_vec) + n_bytes);
    if (v->refcnt == 1) {
        v = (rust_vec*)dom->realloc(v, alloc);
        if (!v) {
            task->fail(3);
            return NULL;
        }
        v->alloc = alloc;
    } else {
        void *mem = dom->malloc(alloc);
        if (!mem) {
            task->fail(3);
            return NULL;
        }
        v = new (mem) rust_vec(dom, alloc, v->fill, &v->data[0]);
    }
    return v;
}


rust_crate_cache *
rust_dom::get_cache(rust_crate const *crate) {
    log(LOG_LINK, "looking for crate-cache for crate 0x%" PRIxPTR, crate);
    rust_crate_cache *cache = NULL;
    for (size_t i = 0; i < caches.length(); ++i) {
        rust_crate_cache *c = caches[i];
        if (c->crate == crate) {
            cache = c;
            break;
        }
    }
    if (!cache) {
        log(LOG_LINK, "making new crate-cache for crate 0x%" PRIxPTR, crate);
        cache = new (this) rust_crate_cache(this, crate);
        caches.push(cache);
    }
    cache->ref();
    return cache;
}

static rust_crate_cache::c_sym *
fetch_c_sym(rust_task *task,
            rust_crate const *curr_crate,
            size_t lib_num,
            size_t c_sym_num,
            char const *library,
            char const *symbol)
{
    rust_dom *dom = task->dom;

    if (task->cache && task->cache->crate != curr_crate) {
        task->dom->log(LOG_LINK, "lookup uses different crate");
        task->cache->deref();
        task->cache = NULL;
    }

    if (!task->cache) {
        task->dom->log(LOG_LINK, "fetching cache for current crate");
        task->cache = dom->get_cache(curr_crate);
    }

    rust_crate_cache *cache = task->cache;
    rust_crate_cache::lib *l = cache->get_lib(lib_num, library);
    return cache->get_c_sym(c_sym_num, l, symbol);
}

extern "C" CDECL uintptr_t
upcall_require_rust_sym(rust_task *task,
                        rust_crate const *curr_crate,
                        size_t lib_num,      // # of lib
                        size_t c_sym_num,    // # of C sym "rust_crate" in lib
                        size_t rust_sym_num, // # of rust sym
                        char const *library,
                        char const **path)
{
    LOG_UPCALL_ENTRY(task);
    rust_dom *dom = task->dom;

    dom->log(LOG_UPCALL|LOG_LINK,
             "upcall require rust sym: lib #%" PRIdPTR
             " = %s, c_sym #%" PRIdPTR
             ", rust_sym #%" PRIdPTR
             ", curr_crate = 0x%" PRIxPTR,
             lib_num, library, c_sym_num, rust_sym_num,
             curr_crate);
    for (char const **c = crate_rel(curr_crate, path); *c; ++c) {
        dom->log(LOG_UPCALL, " + %s", crate_rel(curr_crate, *c));
    }

    dom->log(LOG_UPCALL|LOG_LINK,
             "require C symbol 'rust_crate' from lib #%" PRIdPTR,lib_num);
    rust_crate_cache::c_sym *c =
        fetch_c_sym(task, curr_crate, lib_num, c_sym_num,
                    library, "rust_crate");

    dom->log(LOG_UPCALL|LOG_LINK, "require rust symbol inside crate");
    rust_crate_cache::rust_sym *s =
        task->cache->get_rust_sym(rust_sym_num, dom, curr_crate, c, path);

    uintptr_t addr = s->get_val();
    if (addr) {
        dom->log(LOG_UPCALL|LOG_LINK,
                 "found-or-cached addr: 0x%" PRIxPTR, addr);
    } else {
        dom->log(LOG_UPCALL|LOG_LINK,
                 "failed to resolve symbol");
        task->fail(7);
    }
    return addr;
}

extern "C" CDECL uintptr_t
upcall_require_c_sym(rust_task *task,
                     rust_crate const *curr_crate,
                     size_t lib_num,      // # of lib
                     size_t c_sym_num,    // # of C sym
                     char const *library,
                     char const *symbol)
{
    LOG_UPCALL_ENTRY(task);
    rust_dom *dom = task->dom;

    dom->log(LOG_UPCALL|LOG_LINK,
             "upcall require c sym: lib #%" PRIdPTR
             " = %s, c_sym #%" PRIdPTR
             " = %s"
             ", curr_crate = 0x%" PRIxPTR,
             lib_num, library, c_sym_num, symbol, curr_crate);

    rust_crate_cache::c_sym *c =
        fetch_c_sym(task, curr_crate, lib_num, c_sym_num, library, symbol);

    uintptr_t addr = c->get_val();
    if (addr) {
        dom->log(LOG_UPCALL|LOG_LINK,
                 "found-or-cached addr: 0x%" PRIxPTR, addr);
    } else {
        dom->log(LOG_UPCALL|LOG_LINK,
                 "failed to resolve symbol");
        task->fail(6);
    }
    return addr;
}

static int
rust_main_loop(rust_dom *dom);

#if defined(__WIN32__)
static DWORD WINAPI rust_thread_start(void *ptr)
#elif defined(__GNUC__)
    static void *rust_thread_start(void *ptr)
#else
#error "Platform not supported"
#endif
{
    // We were handed the domain we are supposed to run.
    rust_dom *dom = (rust_dom *)ptr;

    // Start a new rust main loop for this thread.
    rust_main_loop(dom);

    rust_srv *srv = dom->srv;
    delete dom;
    delete srv;

    return 0;
}

extern "C" CDECL rust_task *
upcall_new_task(rust_task *spawner)
{
    LOG_UPCALL_ENTRY(spawner);

    rust_dom *dom = spawner->dom;
    rust_task *task = new (dom) rust_task(dom, spawner);
    dom->log(LOG_UPCALL|LOG_MEM|LOG_TASK,
             "upcall new_task(spawner 0x%" PRIxPTR ") = 0x%" PRIxPTR,
             spawner, task);
    return task;
}

extern "C" CDECL rust_task *
upcall_start_task(rust_task *spawner,
                  rust_task *task,
                  uintptr_t exit_task_glue,
                  uintptr_t spawnee_fn,
                  size_t callsz)
{
    LOG_UPCALL_ENTRY(spawner);

    rust_dom *dom = spawner->dom;
    dom->log(LOG_UPCALL|LOG_MEM|LOG_TASK,
             "upcall start_task(task 0x%" PRIxPTR
             " exit_task_glue 0x%" PRIxPTR
             ", spawnee 0x%" PRIxPTR
             ", callsz %" PRIdPTR ")",
             task, exit_task_glue, spawnee_fn, callsz);
    task->start(exit_task_glue, spawnee_fn, spawner->rust_sp, callsz);
    return task;
}

extern "C" CDECL rust_task *
upcall_new_thread(rust_task *task)
{
    LOG_UPCALL_ENTRY(task);

    rust_dom *old_dom = task->dom;
    rust_dom *new_dom = new rust_dom(old_dom->srv->clone(),
                                     old_dom->root_crate);
    new_dom->log(LOG_UPCALL|LOG_MEM,
                 "upcall new_thread() = 0x%" PRIxPTR,
                 new_dom->root_task);
    return new_dom->root_task;
}

extern "C" CDECL rust_task *
upcall_start_thread(rust_task *spawner,
                    rust_task *root_task,
                    uintptr_t exit_task_glue,
                    uintptr_t spawnee_fn,
                    size_t callsz)
{
    LOG_UPCALL_ENTRY(spawner);

    rust_dom *dom = spawner->dom;
    dom->log(LOG_UPCALL|LOG_MEM|LOG_TASK,
             "upcall start_thread(exit_task_glue 0x%" PRIxPTR
             ", spawnee 0x%" PRIxPTR
             ", callsz %" PRIdPTR ")",
             exit_task_glue, spawnee_fn, callsz);
    root_task->start(exit_task_glue, spawnee_fn, spawner->rust_sp, callsz);

#if defined(__WIN32__)
    DWORD thread;
    CreateThread(NULL, 0, rust_thread_start, root_task->dom, 0, &thread);
#else
    pthread_t thread;
    pthread_create(&thread, &dom->attr, rust_thread_start,
                   (void *)root_task->dom);
#endif

    return 0; /* nil */
}

static int
rust_main_loop(rust_dom *dom)
{
    int rval;
    rust_task *task;

    dom->log(LOG_DOM, "running main-loop on domain 0x%" PRIxPTR, dom);
    dom->logptr("main exit-task glue",
                dom->root_crate->get_main_exit_task_glue());

    while ((task = dom->sched()) != NULL) {
        I(dom, task->running());

        dom->log(LOG_TASK, "activating task 0x%" PRIxPTR ", sp=0x%" PRIxPTR,
                 (uintptr_t)task, task->rust_sp);

        dom->activate(task);

        dom->log(LOG_TASK,
                 "returned from task 0x%" PRIxPTR
                 " in state '%s', sp=0x%" PRIxPTR,
                 (uintptr_t)task,
                 dom->state_vec_name(task->state),
                 task->rust_sp);

        I(dom, task->rust_sp >= (uintptr_t) &task->stk->data[0]);
        I(dom, task->rust_sp < task->stk->limit);

        dom->reap_dead_tasks();
    }

    dom->log(LOG_DOM, "finished main-loop (dom.rval = %d)", dom->rval);
    rval = dom->rval;

    return rval;
}

rust_srv::rust_srv() :
    live_allocs(0)
{
}

rust_srv::~rust_srv()
{
    if (live_allocs != 0) {
        char msg[128];
        snprintf(msg, sizeof(msg),
                 "leaked memory in rust main loop (%" PRIuPTR " objects)",
                 live_allocs);
        fatal(msg, __FILE__, __LINE__);
    }
}

void
rust_srv::log(char const *str)
{
    printf("rt: %s\n", str);
}

void *
rust_srv::malloc(size_t bytes)
{
    ++live_allocs;
    return ::malloc(bytes);
}

void *
rust_srv::realloc(void *p, size_t bytes)
{
    if (!p)
        live_allocs++;
    return ::realloc(p, bytes);
}

void
rust_srv::free(void *p)
{
    if (live_allocs < 1)
        fatal("live_allocs < 1", __FILE__, __LINE__);
    live_allocs--;
    ::free(p);
}

void
rust_srv::fatal(char const *expr, char const *file, size_t line)
{
    char buf[1024];
    snprintf(buf, sizeof(buf),
             "fatal, '%s' failed, %s:%d",
             expr, file, (int)line);
    log(buf);
    exit(1);
}

rust_srv *
rust_srv::clone()
{
    return new rust_srv();
}

/* Native builtins. */
extern "C" CDECL rust_str*
str_alloc(rust_task *task, size_t n_bytes)
{
    rust_dom *dom = task->dom;
    size_t alloc = next_power_of_two(sizeof(rust_str) + n_bytes);
    void *mem = dom->malloc(alloc);
    if (!mem) {
        task->fail(2);
        return NULL;
    }
    rust_str *st = new (mem) rust_str(dom, alloc, 1, (uint8_t const *)"");
    return st;
}

extern "C" CDECL rust_str*
last_os_error(rust_task *task) {
    rust_dom *dom = task->dom;
    dom->log(LOG_TASK, "last_os_error()");

#ifdef __WIN32__
    LPTSTR buf;
    DWORD err = GetLastError();
    FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER |
                  FORMAT_MESSAGE_FROM_SYSTEM |
                  FORMAT_MESSAGE_IGNORE_INSERTS,
                  NULL, err,
                  MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                  (LPTSTR) &buf, 0, NULL );
#else
    char cbuf[1024];
    char *buf = strerror_r(errno, cbuf, sizeof(cbuf));
    if (!buf) {
        task->fail(1);
        return NULL;
    }
#endif

    size_t fill = strlen(buf) + 1;
    size_t alloc = next_power_of_two(sizeof(rust_str) + fill);
    void *mem = dom->malloc(alloc);
    if (!mem) {
        task->fail(1);
        return NULL;
    }
    rust_str *st = new (mem) rust_str(dom, alloc, fill, (const uint8_t *)buf);

#ifdef __WIN32__
    LocalFree((HLOCAL)buf);
#endif
    return st;
}

extern "C" CDECL rust_vec*
vec_alloc(rust_task *task, type_desc *t, size_t n_elts)
{
    rust_dom *dom = task->dom;
    dom->log(LOG_MEM, "vec_alloc %" PRIdPTR " elements of size %" PRIdPTR,
             n_elts, t->size);
    size_t fill = n_elts * t->size;
    size_t alloc = next_power_of_two(sizeof(rust_vec) + fill);
    void *mem = dom->malloc(alloc);
    if (!mem) {
        task->fail(3);
        return NULL;
    }
    rust_vec *vec = new (mem) rust_vec(dom, alloc, 0, NULL);
    return vec;
}

extern "C" CDECL char const *
str_buf(rust_task *task, rust_str *s)
{
    return (char const *)&s->data[0];
}

extern "C" CDECL void *
vec_buf(rust_task *task, type_desc *ty, rust_vec *v)
{
    return (void *)&v->data[0];
}

extern "C" CDECL size_t
vec_len(rust_task *task, type_desc *ty, rust_vec *v)
{
    return v->fill;
}

struct command_line_args
{
    rust_dom &dom;
    int argc;
    char **argv;

    // vec[str] passed to rust_task::start.
    rust_vec *args;

    command_line_args(rust_dom &dom,
                      int sys_argc,
                      char **sys_argv)
        : dom(dom),
          argc(sys_argc),
          argv(sys_argv),
          args(NULL)
    {
#if defined(__WIN32__)
        LPCWSTR cmdline = GetCommandLineW();
        LPWSTR *wargv = CommandLineToArgvW(cmdline, &argc);
        dom.win32_require("CommandLineToArgvW", argv != NULL);
        argv = (char **) dom.malloc(sizeof(char*) * argc);
        for (int i = 0; i < argc; ++i) {
            int n_chars = WideCharToMultiByte(CP_UTF8, 0, wargv[i], -1,
                                              NULL, 0, NULL, NULL);
            dom.win32_require("WideCharToMultiByte(0)", n_chars != 0);
            argv[i] = (char *) dom.malloc(n_chars);
            n_chars = WideCharToMultiByte(CP_UTF8, 0, wargv[i], -1,
                                          argv[i], n_chars, NULL, NULL);
            dom.win32_require("WideCharToMultiByte(1)", n_chars != 0);
        }
        LocalFree(wargv);
#endif
        size_t vec_fill = sizeof(rust_str *) * argc;
        size_t vec_alloc = next_power_of_two(sizeof(rust_vec) + vec_fill);
        void *mem = dom.malloc(vec_alloc);
        args = new (mem) rust_vec(&dom, vec_alloc, 0, NULL);
        rust_str **strs = (rust_str**) &args->data[0];
        for (int i = 0; i < argc; ++i) {
            size_t str_fill = strlen(argv[i]) + 1;
            size_t str_alloc = next_power_of_two(sizeof(rust_str) + str_fill);
            mem = dom.malloc(str_alloc);
            strs[i] = new (mem) rust_str(&dom, str_alloc, str_fill, (uint8_t const *)argv[i]);
        }
        args->fill = vec_fill;
        // FIXME (bug 560452): horrible hack to handle non-cascading vec drop glue.
        args->ref();
    }

    ~command_line_args() {
        if (args) {
            // FIXME (bug 560452): horrible hack to handle non-cascading vec drop glue.
            rust_str **strs = (rust_str**) &args->data[0];
            for (int i = 0; i < argc; ++i)
                dom.free(strs[i]);
            dom.free(args);
        }

#ifdef __WIN32__
        for (int i = 0; i < argc; ++i) {
            dom.free(argv[i]);
        }
        dom.free(argv);
#endif
    }
};

extern "C" CDECL int
rust_start(uintptr_t main_fn, rust_crate const *crate, int argc, char **argv)
{
    int ret;
    {
        rust_srv srv;
        rust_dom dom(&srv, crate);
        command_line_args args(dom, argc, argv);

        dom.log(LOG_DOM, "startup: %d args", args.argc);
        for (int i = 0; i < args.argc; ++i)
            dom.log(LOG_DOM, "startup: arg[%d] = '%s'", i, args.argv[i]);

        if (dom.logbits & LOG_DWARF) {
            rust_crate_reader rdr(&dom, crate);
        }

        uintptr_t main_args[3] = { 0, 0, (uintptr_t)args.args };

        dom.root_task->start(crate->get_main_exit_task_glue(),
                             main_fn,
                             (uintptr_t)&main_args,
                             sizeof(main_args));

        ret = rust_main_loop(&dom);
    }

#if !defined(__WIN32__)
    // Don't take down the process if the main thread exits without an
    // error.
    if (!ret)
        pthread_exit(NULL);
#endif
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
