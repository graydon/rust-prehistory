#include <stdlib.h>    /* malloc       */
#include <stddef.h>    /* offsetof     */
#include <stdio.h>     /* printf       */
#include <string.h>    /* memset       */
#include <time.h>      /* struct timeval */
#include <sys/time.h>  /* struct timeval */
#include "uthash.h"

struct my_event {
    struct timeval tv;         /* key is aggregate of this field */ 
    char event_code;           /* and this field.                */    
    int user_id;
    UT_hash_handle hh;         /* makes this structure hashable */
};


int main(int argc, char *argv[]) {
    struct my_event *e, ev, *events = NULL;
    unsigned i, keylen;

    keylen =   offsetof(struct my_event, event_code) + sizeof(char)                         
             - offsetof(struct my_event, tv);

    for(i = 0; i < 10; i++) {
        e = (struct my_event*)malloc(sizeof(struct my_event));
        memset(e,0,sizeof(struct my_event));
        e->tv.tv_sec = i * (60*60*24*365);          /* i years (sec)*/
        e->tv.tv_usec = 0;
        e->event_code = 'a'+(i%2);                   /* meaningless */
        e->user_id = i;

        HASH_ADD( hh, events, tv, keylen, e);
    }

    /* look for one specific event */
    memset(&ev,0,sizeof(struct my_event));
    ev.tv.tv_sec = 5 * (60*60*24*365);          
    ev.tv.tv_usec = 0;
    ev.event_code = 'b';
    HASH_FIND( hh, events, &ev.tv, keylen , e);
    if (e) printf("found: user %d, unix time %ld\n", e->user_id, e->tv.tv_sec);
   return 0;
}
