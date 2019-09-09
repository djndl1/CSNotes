#include "open_addressing.h"

#include <stddef.h>
#include <stdlib.h>

index_t hash(element_t, size_t);
int element_comp(const void* lhs, const void* rhs);

struct Element {
        int dummy;
};

enum entry_type { LEGIT, EMPTY, DELETED};

struct hash_entry {
        element_t elm;
        enum entry_type state; // since we cannot decide if it's occupied
};

typedef struct hash_entry cell_t;

struct HashTbl {
        size_t capacity;
        cell_t *cells;
};

hashtbl_t hashtbl_ctro(size_t capacity)
{
        if (capacity <= 3) {
                return NULL;
        }

        hashtbl_t tbl = malloc(struct HashTbl);
        if (tbl == NULL)
                goto tbl_fail;
        tbl->capacity = capacity;
        tbl->cells = calloc(capacity, sizeof(cell_t));
        if (tbl->cells == NULL)
                goto cells_fail;

        for (size_t i = 0; i < capacity; i++)
                tbl->cells[i].state = EMPTY;

        return tbl;

cells_fail:
        free(tbl->cells);
tbl_fail:
        return NULL;
}

// find the position where elm should be inserted into, not if it is already there
position_t hashtbl_find(hashtbl_t tbl, element_t elm)
{
        position_t cur_pos = hash(elm, tbl->capacity);
        size_t collision_num = 0;

        /* DELTED ones can still are not marked EMPTY
        so that find can go on, but it can cause the table
        to get too full prematurely */
        while (tbl->cells[cur_pos].state != EMPTY &&
               element_comp(&(tbl->cells[cur_pos].elm), &elm) != 0) {
                // something must be there so that comparsion can be done
                cur_pos += 2* collision_num++ + 1; // (i+1)^2 = i^2 + 2i + 1
                if (cur_pos >= tbl->capacity)
                        cur_pos = cur_pos - tbl->capacity;
        }
        return cur_pos;
}

void hashtbl_insert(hashtbl_t tbl, element_t elm)
{
        position_t pos = find(tbl, elm);

        if (tbl->cells[pos].state != LEGIT) {
                tbl->cells[pos].state = LEGIT;
                tbl->cells[pos].elm = elm;
        }
}

void hashtbl_dtor(hashtbl_t tbl)
{
        free(tbl->cells);
        free(tbl);
}

hashtbl_t hashtbl_rehash(hashtbl_t tbl)
{
        size_t old_size = tbl->capacity;
        cell_t *old_cells = tbl->cells;

        hashtbl_t new_tbl = hashtbl_ctro(2 * old_size);
        if (new_tbl == NULL)
                return NULL;

        for (size_t i = 0; i < old_size; i++) {
                if (old_cells[i].state == LEGIT)
                        hashtbl_insert(new_tbl, old_cells[i].elm);
        }
        hashtbl_dtor(tbl);

        return new_tbl;
}
