#pragma once

#include <stdbool.h>

struct Element;
typedef struct Element ElementType;
struct Node;
typedef struct Node* node_t;
typedef node_t stack;

bool isEmpty(stack s);

stack makeStack(void);

void disposeStack(stack s);

int makeEmpty(stack s);

void push(stack s, ElementType x);

node_t top(stack s);

void pop(stack s);
