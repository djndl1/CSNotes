#include "list.h"

#include <stdlib.h>
#include <stdio.h>

#include "../lib/xmalloc.h"

struct Node {
        ElementType Element;
        Position    Next;
};




List MakeEmpty(List L)
{
        L = xmalloc(sizeof(struct Node));
        L->Next = NULL;
        L->Element = 0;

        return L;
}

bool isEmpty(List L)
{
        return L->Next == NULL;
}

bool isLast(List L, Position P)
{
        return P->Next == NULL;
}

Position find(List L, ElementType X)
{
        Position P = L->Next;

        while (P != NULL && P->Element != X)
                P = P->Next;

        return P;
}

void delete(List L, ElementType X)
{
        Position P, tempCell;

        P = findPrevious(L, X);

        if ( !isLast(L, P)) {
                tempCell = P->Next;
                P->Next = tempCell->Next;
                free(tempCell);
        }
}

Position findPrevious(List L, ElementType X)
{
        Position P;

        P = L;
        while (P->Next != NULL && P->Next->Element != X)
                P = P->Next;

        return P;
}

void insert(List L, ElementType X, Position P)
{
        Position tempCell;

        tempCell = xmalloc(sizeof(struct Node));
        tempCell->Element = X;
        tempCell->Next = P->Next;
        P->Next = tempCell;
}

void deleteList(List L)
{
        Position tmp;

        Position P = L->Next;
        while (P != NULL) {
                tmp = P;
                P = P->Next;
                free(tmp);
        }

        free(L);
}

Position Header(List L)
{
        return L;
}

Position First(List L)
{
        return L->Next;
}

Position next(Position P)
{
        return P->Next;
}

ElementType retrieve(Position P)
{
        return P->Element;
}
