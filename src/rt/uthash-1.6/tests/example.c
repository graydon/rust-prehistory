#include <stdio.h>   /* gets */
#include <stdlib.h>  /* atoi, malloc */
#include <string.h>  /* strcpy */
#include "uthash.h"

struct my_struct {
    int id;                    /* key */
    char name[10];             
    UT_hash_handle hh;         /* makes this structure hashable */
};

struct my_struct *users = NULL;

int add_user(int user_id, char *name) {
    struct my_struct *s;

    s = (struct my_struct*)malloc(sizeof(struct my_struct));
    s->id = user_id;
    strcpy(s->name, name);
    HASH_ADD_INT( users, id, s );  /* id: name of key field */
}

struct my_struct *find_user(int user_id) {
    struct my_struct *s;

    HASH_FIND_INT( users, &user_id, s );  /* s: output pointer */
    return s;
}

void delete_user(struct my_struct *user) {
    HASH_DEL( users, user);  /* user: pointer to deletee */
    free(user);
}

void delete_all() {
  struct my_struct *current_user; 

  while(users) { 
    current_user = users;          /* grab pointer to first item */
    HASH_DEL(users,current_user);  /* delete it (users advances to next) */
    free(current_user);            /* free it */
  } 
}

void print_users() {
    struct my_struct *s;

    for(s=users; s != NULL; s=(struct my_struct*)(s->hh.next)) {
        printf("user id %d: name %s\n", s->id, s->name);
    }
}

int name_sort(struct my_struct *a, struct my_struct *b) {
    return strcmp(a->name,b->name);
}

int id_sort(struct my_struct *a, struct my_struct *b) {
    return (a->id - b->id);
}

void sort_by_name() {
    HASH_SORT(users, name_sort);
}

void sort_by_id() {
    HASH_SORT(users, id_sort);
}

int main(int argc, char *argv[]) {
    char in[10];
    int id=1;
    struct my_struct *s;
    unsigned num_users;

    while (1) {
        printf("1. add user\n");
        printf("2. find user\n");
        printf("3. delete user\n");
        printf("4. delete all users\n");
        printf("5. sort items by name\n");
        printf("6. sort items by id\n");
        printf("7. print users\n");
        printf("8. count users\n");
        gets(in);
        switch(atoi(in)) {
            case 1:
                printf("name?\n");
                add_user(id++, gets(in));
                break;
            case 2:
                printf("id?\n");
                s = find_user(atoi(gets(in)));
                printf("user: %s\n", s ? s->name : "unknown");
                break;
            case 3:
                printf("id?\n");
                s = find_user(atoi(gets(in)));
                if (s) delete_user(s);
                else printf("id unknown\n");
                break;
            case 4:
                delete_all();
                break;
            case 5:
                sort_by_name();
                break;
            case 6:
                sort_by_id();
                break;
            case 7:
                print_users();
                break;
            case 8:
                num_users=HASH_COUNT(users);
                printf("there are %u users\n", num_users);
                break;
        }
    }
}
