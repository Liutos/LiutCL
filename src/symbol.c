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

/* Constructor */
Symbol make_symbol
(char *name,
 Package package,
 List property_list,
 LispObject value,
 Function function)
{
    symbol_t symbol;

    symbol = malloc(sizeof(struct symbol_t));
    symbol->name = name;
    symbol->package = package;
    symbol->property_list = property_list;
    symbol->value = value;
    symbol->function = function;

    return TO_SYMBOL(symbol);
}

/* Slot readers */
char *symbol_name(Symbol symbol)
{
    return theSYMBOL(symbol)->name;
}

Package symbol_package(Symbol symbol)
{
    return theSYMBOL(symbol)->package;
}

List symbol_plist(Symbol symbol)
{
    return theSYMBOL(symbol)->property_list;
}

LispObject symbol_value(Symbol symbol)
{
    return theSYMBOL(symbol)->value;
}

Function symbol_function(Symbol symbol)
{
    return theSYMBOL(symbol)->function;
}

/* Other operations */
Symbol get_symbol(char *name, hash_table_t table)
{
    return (Symbol)search_key(name, table);
}

void put_symbol(Symbol symbol, hash_table_t table)
{
    char *name;

    name = symbol_name(symbol);
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
        symbol = make_symbol(name, pkg, NULL, NULL, NULL);
	put_symbol(symbol, PACKAGE_HASH_TABLE(pkg));

	return symbol;
    }
}

BOOL is_keyword(Symbol sym)
{
    return eq(pkg_kw, symbol_package(sym));
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
