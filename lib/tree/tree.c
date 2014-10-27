#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <malloc.h>
#include <errno.h>

#include "tree_config.h"
#include "tree.h"

/*
    Allocations
*/

int elem_init(elems_t *es)
{
    if(es->free == NULL && es->k < MAX_BLOCKS) {
        unsigned int i;
        elem_t * block = (elem_t *) malloc(ELEM_POLL_SIZE);

        if(block) {
            memset(block, 0, ELEM_POLL_SIZE);

            for(i = 0; i < ELEM_POLL_STEP - 1; i++) 
                block[i].next = &block[i + 1];            
            es->free = block;            
            es->blocks[es->k] = block;
            es->k++;

            return 0;
        } else {
            LOG("ERROR: can't allocate %lu bytes.", ELEM_POLL_SIZE);
            return -ENOMEM;
        }   
    } else {
        LOG("ERROR: no blocks - %d", es->k);
        return -ENOENT;
    }
}

void elem_cleanup(elems_t *es)
{
    unsigned int i;

    es->free = NULL;
    for(i = 0; i < es->k; i++) free(es->blocks[i]);
    es->k = 0;
}


node_t * node_allocate(elems_t *es)
{
    if(es->free == NULL) {
        if(elem_init(es) < 0) 
            return NULL;
    }       
    if(es->free) {
        elem_t *free_top = es->free;
 
        es->free = free_top->next;   
        return (node_t *) free_top;
    }

    return NULL;
}

void node_free(elems_t *es, node_t *del_node)
{
    if(del_node && es) {
        elem_t *free_el = (elem_t *) del_node;

        memset(del_node, 0, sizeof(node_t));
        free_el->next = es->free;
        es->free = free_el;                   
    }
}


/*
    Operations
*/

node_t * add_seq(elems_t *es, node_t *root, uint8_t *seq, unsigned int size, void *payload)
{
    uint8_t key = seq[0];
    int i, p = -1;

    if(size == 0) return NULL;
    if(root == NULL) root = node_allocate(es);
 
    for(i = 0; i < root->n; i++) {
        if(root->b[i].key == key) p = i;
    }

    if(p == -1) {
        if(i < NODE_STEP) {
            i = root->n++;
            root->b[i].key = key;
            
            if(size > 1) {
                root->b[i].is_payload = false;
                root->b[i].childs = add_seq(es, root->b[i].childs, seq + 1, size - 1, payload);
            } else {
                root->b[i].is_payload = true;
                root->b[i].childs = payload;
            } 
        } else {
            root->next = add_seq(es, root->next, seq, size, payload);
        }   
    } else {
        if(size > 1) {
            root->b[p].childs = add_seq(es, root->b[p].childs, seq + 1, size - 1, payload);
        } else {
            /* we haven't any sub-childs - just set payload here */
            if(root->b[p].childs == NULL) {
                root->b[p].is_payload = true;
                root->b[p].childs = payload;
            }    
        }
    }

    return root;
}

/* NOTE: if del_seq return true, tree must be empty (but not free) */
bool del_seq(elems_t *es, node_t *root, uint8_t *seq, unsigned int size, payload_drop_t drop)
{
    unsigned int i;
    node_t *pi = root;
    node_t *prev = NULL;

    if(size == 0) return root == NULL;
    if(root == NULL || root->n == 0) return false;
    
    do {
        for(i = 0; i < pi->n; i++) {
            if(pi->b[i].key == seq[0]) {
                if(pi->b[i].is_payload) {
                    /* destruct payload */
                    if(drop) drop((void *) pi->b[i].childs);
                    return true;
                } else {
                    /* drop point to childs */
                    if(del_seq(es, pi->b[i].childs, seq + 1, size - 1, drop)) {
                        node_free(es, pi->b[i].childs);    
                        pi->b[i].childs = NULL;

                        if(i == 0 && pi->n == 1) {
                            if(prev) {
                                prev->next = pi->next ? pi->next : NULL;
                                node_free(es, pi);
                                return false;
                            } else {
                                return true;
                            }
                        } else {
                            pi->b[i] = pi->b[pi->n];
                            pi->n--;
                            
                            return false;
                        }
                    } else {
                        return false;
                    }
                }
            }     
        }
        prev = pi;
        pi = pi->next;
    } while(pi);

    return false;
}

/*
    Search
*/

bool find_seq(node_t *root, uint8_t *seq, unsigned int size)
{
    unsigned int i;
    node_t *pi = root;
    
    if(size == 0) return true;
    if(root == NULL || root->n == 0) return false;
    do {
        for(i = 0; i < pi->n; i++) {
            if(pi->b[i].key == seq[0]) {
                if(pi->b[i].is_payload) return size == 1; 
                else return find_seq(pi->b[i].childs, seq + 1, size - 1);
            }
        }
        pi = pi->next;
    } while(pi);

    return false;
}


void * findp(node_t *root, uint8_t *seq, unsigned int size, key_compare_t cmp, void *def)
{
    unsigned int i;
    node_t *pi = root;
    
    if(size == 0) return def;
    if(root == NULL || root->n == 0) return def;
    do {
        for(i = 0; i < pi->n; i++) {
            if(cmp(pi->b[i].key, seq[0], size)) {
                if(pi->b[i].is_payload) return (void *) pi->b[i].childs; 
                else return find(pi->b[i].childs, seq + 1, size - 1);
            }
        }
        pi = pi->next;
    } while(pi);

    return def;
}

void * findd(node_t *root, uint8_t *seq, unsigned int size, void *def)
{
    bool equal(uint8_t a , uint8_t b, unsigned int _s) { return a == b; };
    return findp(root, seq, size, equal, def);
}

void * find(node_t *root, uint8_t *seq, unsigned int size)
{
    return findd(root, seq, size, NULL);
}

/*
    Pack-unpack
*/
    
unsigned int count_for_pack(node_t * root, payload_count_t pp) 
{
    unsigned int c = 1, i;
    node_t *pi = root;

    if(root == NULL || root->n == 0) return 1;
    do {
        c += 2 * pi->n;
        for(i = 0; i < pi->n; i++) {
            if(pi->b[i].is_payload) c += pp ? pp(pi->b[i].childs) : sizeof(long);
            else c += pi->b[i].childs ? count_for_pack(pi->b[i].childs, pp) : 1;
        }
        pi = pi->next;
    } while(pi);

    return c;
}

unsigned int pack_node(node_t *root, uint8_t *s, payload_pack_t pp)
{
    unsigned int c = 1, i;
    node_t *pi = root;    

    if(root == NULL || root->n == 0) {
        s[0] = 0;
        return 1;
    }

    /* first of all, collect all keys */
    do {
        for(i = 0; i < pi->n; i++) *(s + c + i) = pi->b[i].key;
        c += pi->n;
        pi = pi->next;
    } while(pi);
    s[0] = c;

    /* next, we need to know how many payloads here */
    pi = root;
    do {
        for(i = 0; i < pi->n; i++) *(s + c + i) = pi->b[i].is_payload;
        c += pi->n;
        pi = pi->next;
    } while(pi);

    /* lets pack all sub-nodes */
    pi = root;
    do {
        for(i = 0; i < pi->n; i++) {
            if(!pi->b[i].is_payload) c += pack_node(pi->b[i].childs, s + c, pp);            
        }
        pi = pi->next;
    } while(pi);
    
    /* at the end, pack all payload */
    pi = root;
    do {
        for(i = 0; i < pi->n; i++) {
            if(pi->b[i].is_payload) {
                if(pp) { 
                    c += pp(pi->b[i].childs, s + c);
                } else {
                    *((long *)(s + c)) = (long)pi->b[i].childs;
                    c += sizeof(long);
                } 
            }           
        }
        pi = pi->next;
    } while(pi);

    return c;
}



unsigned int unpack_node(uint8_t *s, elems_t *es, node_t *root, payload_unpack_t pp)
{
    unsigned int c = s[0], i;
    node_t *pi = root;

    if(c == 0) return 1;
    if(root == NULL) root = node_allocate(es);

    /* extract keys and payload markers */
    for(i = 1; i < c; i++) {
        unsigned int li = (i - 1) % NODE_STEP;

        if(li == 0 && i > 1) {
            pi->next = node_allocate(es);
            pi = pi->next;
        }
        pi->n += 1;
        pi->b[li].key = s[i];
        pi->b[li].is_payload = s[i + c - 1];
    }
    c += c - 1;

    /* unpack sub-nodes */
    pi = root;
    do {
        for(i = 0; i < pi->n; i++) {
            uint8_t * subs = s + c; 
            
            pi->b[i].childs = NULL;
            if(!pi->b[i].is_payload) {
                if(subs[0]) {
                    pi->b[i].childs = node_allocate(es);
                    c += unpack_node(subs, es, pi->b[i].childs, pp);    
                } else {
                    c++;
                }
            }
        }
        pi = pi->next;
    } while(pi); 

    /* unpack payloads */
    pi = root;
    do {
        for(i = 0; i < pi->n; i++) {
            uint8_t * subs = s + c; 
            
            if(pi->b[i].is_payload) {
                if(pp) {
                    c += pp(subs, &pi->b[i]);    
                } else {
                    pi->b[i].childs = (void *)(*((long *) subs));
                    c += sizeof(long);                        
                }   
            }
        }
        pi = pi->next;
    } while(pi); 

    return c;
}

/*
    Misc
*/

void print_node(node_t *root, int level) 
{
    int i, j;

    if(root == NULL) {
        for(j = 0; j < level; j++) printf("\t");
        printf("> Leaf\n");
        return;
    } 
        
    for(j = 0; j < level; j++) printf("\t");
    printf("Node[%d]\n", root->n);
    
    for(i = 0; i < root->n; i++) {
        for(j = 0; j < level; j++) printf("\t");
        if(root->b[i].is_payload) {
            printf("%2d: Branch %d -> %lu \n", i, root->b[i].key, (long) root->b[i].childs);
        } else {
            printf("%2d: Branch %d -> \n", i, root->b[i].key);
            print_node(root->b[i].childs, level + 1);
        }
    }

    for(j = 0; j < level; j++) printf("\t");
    if(root->next) {
        printf("Next node ->\n");
        print_node(root->next, level);
    } else {
        printf("^^^\n");
    }
}

    
/*
    Testing
*/

#ifdef __TREE_TEST__

int main()
{
    elems_t es = { .free = NULL, .k = 0 };

    uint8_t a1[4] = {11,12,13,14};
    uint8_t a2[4] = {11,12,14,13};
    uint8_t a3[4] = {11,13,14,11};

    uint8_t a4[4] = {13,13,13,13};

    elem_init(&es);
    
    node_t *root = add_seq(&es, NULL, a1, 4, (void *) 10);
    add_seq(&es, root, a2, 4, (void *) 11);
    add_seq(&es, root, a3, 4, (void *) 12);
    print_node(root, 0);

    /* Search */
    bool tr = find_seq(root, a2, 4);
    bool fl = find_seq(root, a4, 4);

    printf("\n\nFind true = %d false = %d\n", tr, fl);

    long r = (long) find(root, a1, 4);
    printf("Find value = %ld\n\n", r);

    del_seq(&es, root, a3, 4, NULL);
    print_node(root, 0);

    /* Pack/unpack */
    uint8_t *m = (uint8_t *) malloc(count_for_pack(root, NULL));
    unsigned int c;

    void prnt(uint8_t *m, unsigned int c) {
        unsigned int i;

        printf("\nLen: %d bytes.", c);
        for(i = 0; i < c; i++) {
            if(i % 8 == 0) printf(" ");
            if(i % 16 == 0) printf("\n");

            printf("%02X ", m[i]);
        }
        printf("\n\n");
    }

    add_seq(&es, root, a2, 4, (void *) 128);
    print_node(root, 0);
    
    c = pack_node(root, m, NULL);
    prnt(m, c);

    node_free(&es, root);
    root = node_allocate(&es);

    unpack_node(m, &es, root, NULL);
    print_node(root, 0);

    free(m);
    
    /* terminal states */

    add_seq(&es, root, a2, 4, (void *) 128);
    add_seq(&es, root, a2, 4, (void *) 128);
    add_seq(&es, root, a2, 4, (void *) 128);
    add_seq(&es, root, a2, 4, (void *) 128);
    print_node(root, 0);

    del_seq(&es, root, a2, 4, NULL);
    del_seq(&es, root, a2, 4, NULL);
    del_seq(&es, root, a2, 4, NULL);
    del_seq(&es, root, a2, 4, NULL);
    print_node(root, 0);
        
    elem_cleanup(&es);
}

#endif