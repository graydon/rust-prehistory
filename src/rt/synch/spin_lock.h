/*
 * spin_lock.h
 *
 *  Created on: Jun 16, 2010
 *      Author: Michael Bebenita
 */

#ifndef UNFAIRTICKETLOCK_H_
#define UNFAIRTICKETLOCK_H_

class spin_lock {
	unsigned ticket;
	void pause();
public:
	spin_lock();
	virtual ~spin_lock();
	void lock();
	void unlock();
};

#endif /* UNFAIRTICKETLOCK_H_ */
