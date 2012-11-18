/*
 * hash_table.c
 *
 * Creation, search and update operations on hash table.
 *
 * Copyright (C) 2012-11-04 liutos <mat.liutos@gmail.com>
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "object.h"
#include "types.h"

hash_table_t make_hash_table_t(unsigned int size, unsigned int (*hash_fn)(void *, unsigned int), BOOL (*compare_fn)(void *, void *))
{
    hash_table_t tbl;

    tbl = malloc(sizeof(struct hash_table_t));
    tbl->elements = calloc(size, sizeof(table_entry_t));
    tbl->hash_fn = hash_fn;
    tbl->compare_fn = compare_fn;
    tbl->size = size;
    tbl->count = 0;

    return tbl;
}

HashTable make_hash_table(unsigned int size, unsigned int (*hash_fn)(void *, unsigned int), BOOL (*compare_fn)(void *, void *))
{
    HashTable object;

    object = make_object();
    /* theHASH_TABLE(object) = make_hash_table_aux(size, hash_fn, compare_fn); */

    return object;
}

table_entry_t find_table_entry(void *key, hash_table_t table)
{
    BOOL (*compare_fn)(void *, void *);
    table_entry_t list;
    unsigned int index;
    unsigned int (*hash_fn)(void *, unsigned int);

    hash_fn = table->hash_fn;
    index = hash_fn(key, table->size);
    list = (table->elements)[index];
    compare_fn = table->compare_fn;
    while (list != NULL) {
        if (TRUE == compare_fn(list->key, key))
            return list;
        list = list->next;
    }

    return NULL;
}

void add_key_value(void *key, void *value, hash_table_t table)
{
    table_entry_t ent;
    table_entry_t list;
    unsigned int index;
    unsigned int (*hash_fn)(void *, unsigned int);

    hash_fn = table->hash_fn;
    index = hash_fn(key, table->size);
    list = (table->elements)[index];
    ent = malloc(sizeof(struct table_entry_t));
    ent->key = key;
    ent->value = value;
    ent->next = list;
    (table->elements)[index] = ent;
    table->count++;
}

void update_key_value(void *key, void *value, hash_table_t table)
{
    table_entry_t ent;

    ent = find_table_entry(key, table);
    if (ent != NULL)
        ent->value = value;
    else
        add_key_value(key, value, table);
}

void *search_key(void *key, hash_table_t table)
{
    table_entry_t ent;

    ent = find_table_entry(key, table);
    if (ent != NULL)
        return ent->value;
    else
        return NULL;
}

unsigned int hash_ptr(void *ptr, unsigned int size)
{
    return (int)ptr % size;
}

unsigned int hash_string(void *ptr, unsigned int size)
/* Algorithm from _Data Structures and Algorithm Analysis in C_. */
{
    char *name;
    unsigned int val;

    name = (char *)ptr;
    for (val = 0; *name != '\0'; name++)
        val = (val << 5) + *name;

    return val % size;
}

BOOL ptr_cmp(void *p1, void *p2)
{
    return p1 == p2;
}

BOOL string_cmp(void *s1, void *s2)
{
    return 0 == strcmp((char *)s1, (char *)s2)? TRUE: FALSE;
}

void hash_table_iterate(hash_table_t table, void (*fn)(void *, void *))
{
    int count, i;
    table_entry_t element;

    for (i = table->size, count = table->count; i >= 0; i--) {
        if (0 == count)
            break;
        element = table->elements[i];
        while (element != NULL) {
            fn(element->key, element->value);
            element = element->next;
            count--;
        }
    }
}
