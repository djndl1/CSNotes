#pragma once

#include <stdbool.h>
#include <stddef.h>

struct Stack;
typedef struct Stack *stack_t;
struct Element;
typedef struct Element element_t;

bool stack_isEmpty(stack_t s);

bool stack_isFull(stack_t s);

stack_t makeStack(int max_size);

void stack_destroy(stack_t s);

void stack_makeEmpty(stack_t s);

void stack_push(stack_t s, element_t x);

element_t stack_top(stack_t s);

void stack_pop(stack_t s);

element_t stack_pop_top(stack_t s);
