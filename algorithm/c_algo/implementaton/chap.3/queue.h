#pragma once

#include <stdbool.h>

struct Queue;
typedef struct Queue *queue;

struct Element;
typedef struct Element element_t;

bool queue_isEmpty(queue q);

bool queue_isFull(queue q);

queue makeQueue(int maxsize);

void queue_destroy(queue q);

void queue_makeEmpty(queue q);

element_t queue_front(queue q);

bool queue_dequeue(queue q);

bool queue_enqueue(queue q, element_t elm);

element_t queue_front_and_dequeue(queue q);
