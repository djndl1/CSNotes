#include "stack_array.h"

#include <stdlib.h>
#include <stddef.h>

struct Stack {
        int capacity;
        int top;
        element_t *array;
};

struct Element {
        int dummy;
};

bool stack_isEmpty(stack_t s)
{
        return s->top == -1;
}

bool stack_isFull(stack_t s)
{
        return s->top == s->capacity - 1;
}

stack_t makeStack(int max_size)
{
        stack_t s = malloc(sizeof(struct Stack));
        if (s == NULL)
                goto stack_fail;
        s->array = calloc(max_size, sizeof(struct Element));
        if (s->array == NULL)
                goto array_fail;
        s->capacity = max_size;
        s->top = -1;

        return s;

array_fail:
        free(s);
stack_fail:
        return NULL;
}

void stack_destroy(stack_t s)
{
        if (s == NULL)
                return;

        free(s->array);
        free(s);
}

void stack_makeEmpty(stack_t s)
{
        s->top = -1;
}

void stack_push(stack_t s, element_t x)
{
        if (s == NULL || stack_isFull(s))
                return;
        s->array[++s->top] = x;
}

element_t stack_top(stack_t s)
{
        return s->array[s->top];
}

void stack_pop(stack_t s)
{
        if (s != NULL && stack_isEmpty(s))
                s->top--;
}

element_t stack_pop_top(stack_t s)
{
        return s->array[s->top--];
}
