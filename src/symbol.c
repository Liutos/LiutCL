/*
 * symbol.c
 *
 * Creation of symbols and symbol table.
 *
 * Copyright (C) 2012-11-07 liutos <mat.liutos@gmail.com>
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "hash_table.h"
#include "package.h"
#include "types.h"

Symbol lt_t, lt_nil;

Symbol get_symbol(char *name, hash_table_t table)
{
    return (Symbol)search_key(name, table);
}

symbol_t make_symbol_aux(char *name, Package pkg)
{
    symbol_t symbol;

    symbol = malloc(sizeof(struct symbol_t));
    symbol->name = name;
    symbol->value = gunbound;
    symbol->function = gunbound;
    symbol->package = pkg;

    return symbol;
}

Symbol make_symbol(char *name, Package pkg)
{
    return TO_SYMBOL(make_symbol_aux(name, pkg));
}

void put_symbol(Symbol symbol, hash_table_t table)
{
    char *name;

    name = SYMBOL_NAME(symbol);
    add_key_value(name, symbol, table);
}

/* Ensure the symbol with name `name' exists uniquely in the hash table `table'. */
Symbol ensure_symbol_exists(char *name, Package pkg)
{
    Symbol symbol;

    symbol = get_symbol(name, PACKAGE_HASH_TABLE(pkg));
    if (symbol != NULL)
	return symbol;
    else {
	symbol = make_symbol(name, pkg);
	put_symbol(symbol, PACKAGE_HASH_TABLE(pkg));

	return symbol;
    }
}

BOOL is_keyword(Symbol sym)
{
    return eq(pkg_kw, SYMBOL_PACKAGE(sym));
}

hash_table_t make_symbol_table(unsigned int size)
{
    hash_table_t tbl;

    tbl = make_hash_table_t(size, hash_string, string_cmp);

    return tbl;
}

Symbol gen_symbol(char *name, Package pkg)
{
    return ensure_symbol_exists(name, pkg);
}

Symbol gen_keyword(char *name)
{
    return gen_symbol(name, pkg_kw);
}
