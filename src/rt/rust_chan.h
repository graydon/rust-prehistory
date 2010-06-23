/*
 * rust_chan.h
 *
 *  Created on: Jun 21, 2010
 *      Author:
 */

#ifndef RUST_CHAN_H_
#define RUST_CHAN_H_

class rust_chan : public rc_base<rust_chan>, public task_owned<rust_chan> {
public:
	rust_chan(rust_task *task, rust_port *port);
	~rust_chan();

    rust_task *task;
    rust_port *port;
    circ_buf buffer;
    size_t idx;           // Index into port->chans.

    // Token belonging to this chan, it will be placed into a port's
    // writers vector if we have something to send to the port.
    rust_token token;

    void disassociate();
};

#endif /* RUST_CHAN_H_ */



