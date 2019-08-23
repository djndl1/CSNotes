#pragma once

#include <stdbool.h>

#define elmType int

typedef elmType  ElementType;

struct Node;
typedef struct Node *pNode;
typedef pNode List;
typedef pNode Position;

List makeEmpty(List L);

bool isEmpty(List L);

bool isLast(List L, Position P);

Position find(List L, ElementType X);

void delete(List L, ElementType X);

Position findPrevious(List L, ElementType X);

void insert(List L, ElementType X, Position P);

void deleteList(List L);

Position Header(List L);

Position First(List L);

Position next(Position P);

ElementType retrieve(Position P);
