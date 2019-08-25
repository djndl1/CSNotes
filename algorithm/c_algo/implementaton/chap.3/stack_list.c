#include "stack.h"

#include <stdbool.h>
#include <stdlib.h>


struct Element {
        int i;
};

struct Node {
        ElementType elm;
        node_t next;
};

static node_t makeNode(ElementType elm)
{
        node_t n = malloc(sizeof(struct Node));
        if (n == NULL)
                return NULL;
        n->elm = elm;
        n->next = NULL;

        return n;

}

bool isEmpty(stack s)
{
        return s->next == NULL;
}

stack makeStack(void)
{
        stack s = malloc(sizeof(struct Node));
        if (s == NULL)
                return NULL;
        makeEmpty(s);
        return s;
}

void disposeStack(stack s)
{
        node_t tmp = s->next;
        free(s);
        while(tmp != NULL) {
                node_t prv = tmp;
                tmp = tmp->next;
                free(prv);
        }
}

int makeEmpty(stack s)
{
        if (s == NULL)
                return -1;
        else
                while (!isEmpty(s))
                        pop(s);
        return 0;
}

void push(stack s, ElementType x)
{
        node_t n = makeNode(x);
        n->next = s;
        s->next = n;
}

node_t top(stack s)
{
        if (!isEmpty(s))
                return s->next;
        return NULL;
}

void pop(stack s)
{
        if (s == NULL)
                return;
        node_t tmp = s->next;
        s->next = tmp->next;
        free(tmp);
}
