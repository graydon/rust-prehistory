/*
 * FairTicketLock.h
 *
 *  Created on: Jun 16, 2010
 *      Author: Michael Bebenita
 */

#ifndef FAIRTICKETLOCK_H_
#define FAIRTICKETLOCK_H_

class fair_ticket_lock {
	unsigned next_ticket;
	unsigned now_serving;
	void pause();
public:
	fair_ticket_lock();
	virtual ~fair_ticket_lock();
	void lock();
	void unlock();
};

#endif /* FAIRTICKETLOCK_H_ */
