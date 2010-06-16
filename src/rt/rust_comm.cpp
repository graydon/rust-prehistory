
#include "rust_internal.h"

template class ptr_vec<rust_token>;
template class ptr_vec<rust_alarm>;
template class ptr_vec<rust_chan>;

rust_alarm::rust_alarm(rust_task *receiver) :
    receiver(receiver)
{
}

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


// Circular buffers.

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


// Ports.

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


// Channels.

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

    // Delete reference to the port/
    port = NULL;
}


// Tokens.

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


//
// Local Variables:
// mode: C++
// fill-column: 70;
// indent-tabs-mode: nil
// c-basic-offset: 4
// buffer-file-coding-system: utf-8-unix
// compile-command: "make -k -C .. 2>&1 | sed -e 's/\\/x\\//x:\\//g'";
// End:
//
