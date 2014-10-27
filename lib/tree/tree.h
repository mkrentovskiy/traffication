#ifndef __TREE_H__
#define __TREE_H__

#include <stdint.h>
#include <stdbool.h>

typedef struct node node_t;
typedef struct elem elem_t;

typedef struct {
    uint8_t key;
    bool is_payload; /* if it's true - childs point to payload and must cast to void * */ 
    node_t *childs;
} branch_t;

struct node {
    uint8_t n;
    branch_t b[NODE_STEP];
    node_t *next;
};

struct elem {
    node_t n;
    elem_t *next;
};

typedef struct {
    elem_t *free;
    elem_t *blocks[MAX_BLOCKS];
    unsigned int k;
} elems_t;

int elem_init(elems_t *);
void elem_cleanup(elems_t *);

node_t * node_allocate(elems_t *);
void node_free(elems_t * , node_t *);

typedef void (*payload_drop_t)(void *);
typedef bool (*key_compare_t)(uint8_t, uint8_t, unsigned int);

node_t * add_seq(elems_t *, node_t *, uint8_t *, unsigned int, void *);
bool del_seq(elems_t *, node_t *, uint8_t *, unsigned int, payload_drop_t);
bool find_seq(node_t *, uint8_t *, unsigned int);
void * find(node_t *, uint8_t *, unsigned int);
void * findp(node_t *, uint8_t *, unsigned int, key_compare_t, void *);

typedef unsigned int (*payload_count_t)(void *);
typedef unsigned int (*payload_pack_t)(void *, uint8_t *);
typedef unsigned int (*payload_unpack_t)(uint8_t *, branch_t *);

unsigned int count_for_pack(node_t *, payload_count_t);
unsigned int pack_node(node_t *, uint8_t *, payload_pack_t);
unsigned int unpack_node(uint8_t *, elems_t *, node_t *, payload_unpack_t);

void print_node(node_t *, int);

#endif