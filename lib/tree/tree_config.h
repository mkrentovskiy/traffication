#ifndef __TREE_CONFIG_H__
#define __TREE_CONFIG_H__

#define NODE_STEP 24
#define ELEM_POLL_STEP 1024 * 1024
#define ELEM_POLL_SIZE ELEM_POLL_STEP * sizeof(elem_t)
#define MAX_BLOCKS 32

#define NODE_SIZE sizeof(node_t)
#define LOG printf

#endif