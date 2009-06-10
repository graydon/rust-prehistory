#ifndef RUST_H__
#define RUST_H__

/*
 * Rust runtime library.
 * Copyright 2008, 2009 Graydon Hoare <graydon@pobox.com>.
 * Released under MIT license.
 * See file COPYING for details.
 */

#include <stdlib.h>
#include <stdint.h>

/* Basic scalar types we will use. */

#ifndef UINT64_MAX
#error "need uint64_t support in compiler"
#endif

#ifndef PTRDIFF_MAX
#error "need uintptr_t / ptrdiff_t support in compiler"
#endif

struct rust_proc;
struct rust_prog;
struct rust_port;
struct rust_chan;
struct rust_srv;
struct rust_rt;

typedef struct rust_proc rust_proc_t;
typedef struct rust_prog rust_prog_t;
typedef struct rust_port rust_port_t;
typedef struct rust_chan rust_chan_t;
typedef struct rust_srv rust_srv_t;
typedef struct rust_rt rust_rt_t;

struct rust_srv {
    void *user;
    void (*log)(void *, char const *);
    void (*fatal)(void*, char const *, char const *, size_t);
    void* (*malloc)(void*, size_t);
    void* (*realloc)(void*, void*, size_t);
    void (*free)(void*, void*);
};

#ifdef __WIN32__
#define CDECL __cdecl
#else
#define CDECL __attribute__((cdecl))
#endif

/*
 * Local Variables:
 * fill-column: 70;
 * indent-tabs-mode: nil
 * c-basic-offset: 4
 * compile-command: "make -k -C .. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
 * End:
 */

#endif /* RUST_H__ */
