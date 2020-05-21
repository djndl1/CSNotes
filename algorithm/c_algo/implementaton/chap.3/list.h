#pragma once

#include <stdbool.h>
#include <stdio.h>

struct Node;
struct List;
typedef struct Node *node_t;
typedef struct List *list_t;
typedef void (*destructor_t)(void*);
typedef int (*comparator_t)(const void*, const void*);
typedef void (*node_func)(node_t);

/**
 * Create a node with its element of size `sz`
 */
node_t node_new(size_t sz, destructor_t destr);

#define NODE_NEW(type, destr) \
    node_new(sizeof(type), destr)

/**
 * Create a node with element having the same content as elem
 */
node_t node_copy_new(const void *elem, size_t sz, destructor_t destr);

#define NODE_COPY_NEW(handle, type, destr)    \
    node_copy_new(handle, sizeof(type), destr)

/**
 * Destroy a node and all its resource
 */
void node_destroy(node_t self);

#define NODE_DESTROY(handle) \
    node_destroy(handle)

/**
 * Retrieve the element inside a node
 */
void *node_retrieve(node_t self);

#define NODE_RETRIEVE(handle) \
    node_retrieve(handle)

/**
 * Create an empty list
 */
list_t list_new();
