/*
 * lock_free_queue.h
 *
 *  Created on: Jun 18, 2010
 *      Author: Michael Bebenita
 *       Notes: Interrupt transparent queue, Schoen et. al,
 *              "On Interrupt-Transparent Synchronization in an Embedded
 *              Object-Oriented Operating System", 2000. enqueue() is
 *              allowed to interrupt enqueue() and dequeue(),
 *       		however, dequeue() is not allowed to interrupt itself.
 */

#ifndef LOCK_FREE_QUEUE_H_
#define LOCK_FREE_QUEUE_H_

class lock_free_queue_node {
	lock_free_queue_node *next;
};

class lock_free_queue {
public:
	lock_free_queue();
	void enqueue(lock_free_queue_node *item);
	lock_free_queue_node *dequeue();
};

#endif /* LOCK_FREE_QUEUE_H_ */
