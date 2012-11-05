/*
 * hash_table.c
 *
 *
 *
 * Copyright (C) 2012-11-04 liutos <mat.liutos@gmail.com>
 */
#include <stdio.h>
#include <stdlib.h>

#include "object.h"
#include "types.h"

hash_table_t make_hash_table_t(unsigned int size)
{
    hash_table_t tbl = malloc(sizeof(struct hash_table_t));
    tbl->elements = calloc(size, sizeof(table_entry_t));
    tbl->hash_fn = NULL;
    tbl->compare_fn = NULL;
    tbl->size = size;
    tbl->count = 0;

    return tbl;
}

HashTable make_hash_table(unsigned int size)
{
    HashTable object = make_object();
    theHASH_TABLE(object) = make_hash_table_t(size);

    return object;
}

table_entry_t find_table_entry(LispObject key, HashTable table)
{
    unsigned int (*hash_fn)(LispObject, unsigned int);
    unsigned int index;
    table_entry_t list;
    BOOL (*compare_fn)(LispObject, LispObject);

    hash_fn = HASH_FN(table);
    index = hash_fn(key, TABLE_SIZE(table));
    list = TABLE_ELEMENTS(table)[index];
    compare_fn = HASH_CMP(table);
    while (list != NULL) {
        if (0 == compare_fn(list->key, key))
            return list;
        list = list->next;
    }

    return NULL;
}

void add_key_value(LispObject key, LispObject value, HashTable table)
{
    unsigned int (*hash_fn)(LispObject, unsigned int);
    unsigned int index;
    table_entry_t list;
    table_entry_t ent = malloc(sizeof(struct table_entry_t));
    hash_fn = HASH_FN(table);
    index = hash_fn(key, TABLE_SIZE(table));
    list = TABLE_ELEMENTS(table)[index];
    ent->key = key;
    ent->value = value;
    ent->next = list;
    TABLE_COUNT(table)++;
}

void update_key_value(LispObject key, LispObject value, HashTable table)
{
    table_entry_t ent = find_table_entry(key, table);
    if (ent != NULL)
        ent->value = value;
    else
        add_key_value(key, value, table);
}

BOOL search_key(LispObject key, HashTable table, LispObject *result)
{
    table_entry_t ent = find_table_entry(key, table);
    if (ent != NULL) {
        *result = ent->value;

        return TRUE;
    } else
        return FALSE;
}
