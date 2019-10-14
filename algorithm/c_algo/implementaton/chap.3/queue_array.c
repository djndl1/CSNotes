#include "queue.h"

#include <stdlib.h>
#include <stddef.h>

struct Queue {
        int capacity;
        int front;
        int rear;
        int size;
        element_t *array;
};

struct Element {
        int dummy;
};

bool queue_isEmpty(queue q)
{
        return q->size == 0;
}

bool queue_isFull(queue q)
{
        return q->size == q->capacity;
}

queue makeQueue(int maxsize)
{
        queue q = malloc(sizeof(struct Queue));
        if (q == NULL)
                goto queue_fail;
        q->array = calloc(maxsize, sizeof(struct Element));
        if (q->array == NULL)
                goto array_fail;

        q->capacity = maxsize;
        queue_makeEmpty(q);

        return q;

array_fail:
        free(q);
queue_fail:
        return NULL;
}

void queue_destroy(queue q)
{
        free(q->array);
        free(q);
}

void queue_makeEmpty(queue q)
{
        q->size = 0;
        q->front = 0;
        q->rear = 0;
}

element_t queue_front(queue q)
{
        return q->array[q->front];
}

bool queue_dequeue(queue q)
{
        if (queue_isEmpty(q))
                return false;
        q->front++;
        q->front = q->front % q->capacity;
}

bool queue_enqueue(queue q, element_t elm)
{
        if (queue_isFull(q))
                return false;
        q->array[q->rear] = elm;
        q->rear++;
        q->rear = q->rear % q->capacity;

        return true;
}

element_t queue_front_and_dequeue(queue q)
{
        int curpos = q->front;
        q->front++;
        q->front = q->front % q->capacity;
        return q->array[curpos];
}
