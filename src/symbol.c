/*
 * symbol.c
 *
 *
 *
 * Copyright (C) 2012-11-07 liutos <mat.liutos@gmail.com>
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "hash_table.h"
#include "types.h"

Symbol lt_t, lt_nil;

Symbol get_symbol(char *name, hash_table_t table)
{
    return (Symbol)search_key(name, table);
}

symbol_t make_symbol_aux(char *name)
{
    symbol_t symbol;

    symbol = malloc(sizeof(struct symbol_t));
    symbol->name = name;
    symbol->value = NULL;
    symbol->function = NULL;
    symbol->package = NULL;

    return symbol;
}

Symbol make_symbol(char *name)
{
    return TO_SYMBOL(make_symbol_aux(name));
}

void put_symbol(Symbol symbol, hash_table_t table)
{
    char *name;

    name = SYMBOL_NAME(symbol);
    add_key_value(name, symbol, table);
}

/* Ensure the symbol with name `name' exists uniquely in the hash table `table'. */
Symbol ensure_symbol_exists(char *name, hash_table_t table)
{
    Symbol symbol;

    symbol = get_symbol(name, table);
    if (symbol != NULL)
	return symbol;
    else {
	symbol = make_symbol(name);
	put_symbol(symbol, table);

	return symbol;
    }
}

hash_table_t make_symbol_table(unsigned int size)
{
    hash_table_t tbl;

    tbl = make_hash_table_aux(size, hash_string, string_cmp);

    return tbl;
}
