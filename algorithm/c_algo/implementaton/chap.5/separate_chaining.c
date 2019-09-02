#include "separate_chaining.h"
#include "hash.h"

#include <stddef.h>
#include <stdlib.h>

struct Element {
        int dummy;
};


struct ListNode {
        element_t elm;
        pNode next;
};

struct HashTbl {
        size_t size;
        List *lists;
};

static inline unsigned long nextPrime(unsigned long n)
{
        if (n % 2 == 0)

                return n + 1;
        else
                return n;
}

pNode node_ctor(element_t elm)
{
        pNode n = malloc(sizeof(struct ListNode));
        if (n == NULL)
                return NULL;
        n->elm = elm;
        n->next = NULL;

        return n;
}

void node_dtor(pNode n)
{
        if (n == NULL)
                return;
        free(n);
}

List list_ctor_empty(void)
{
        List l = malloc(sizeof(ListHeader));
        if (l == NULL)
                return NULL;
        l->next = NULL;
        return l;
}

void list_dtor(List l)
{
        if (l == NULL)
                return;

        pNode tmp = l->next;
        pNode prv = l;
        while (tmp != NULL) {
                node_dtor(prv);
                prv = tmp;
                tmp = tmp->next;
        }
        node_dtor(prv);
}

HashTable hashtbl_ctor(size_t size)
{
        if (size < 5) {
                goto table_fail;
        }
        HashTable tbl = malloc(sizeof(struct HashTbl));
        if (tbl == NULL)
                goto table_fail;
        tbl->size = nextPrime(size);

        tbl->lists = calloc(tbl->size, sizeof(List));
        if (tbl->lists == NULL)
                goto array_fail;

        for (size_t i = 0; i < tbl->size; i++) {
                tbl->lists[i] = list_ctor_empty();
        }

        return tbl;

array_fail:
        free(tbl);
table_fail:
        return NULL;
}

void hashtbl_dtor(HashTable tbl)
{
        if (tbl == NULL)
                return;
        for (size_t i = 0; i < tbl->size; i++)
                list_dtor(tbl->lists[i]);
        free(tbl->lists);
        free(tbl);
}

pNode hashtbl_find(HashTable tbl, element_t key)
{
        List l = tbl->lists[hash_int(key.dummy, tbl->size)];
        pNode n = l->next;
        while (n != NULL && n->elm.dummy != key.dummy)
                n = n->next;
        return n;
}

void hashtbl_insert(HashTable tbl, element_t key)
{
        List l = tbl->lists[hash_int(key.dummy, tbl->size)];
        pNode n = l->next;
        while (n != NULL && n->elm.dummy != key.dummy)
                n = n->next;
        if (n == NULL) {
                n = node_ctor(key);
                n->next = l->next;
                l->next = n;
        }
}
