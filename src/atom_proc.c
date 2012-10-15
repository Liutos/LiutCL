/*
 * atom_proc.c
 *
 * Operators on objects of type Atom
 *
 * Copyright (C) 2012-10-05 liutos
 */
#include "object.h"
#include "types.h"
#include "symbol_table.h"
#include "env_types.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

SymbolTable symbol_table = NULL;

Symbol lt_nil;		/* The inner representation of void in defining language. */
Symbol lt_t;		/* The canonical representation of logical true. */
/* Special operators in Scheme */
Symbol lt_quote;
Symbol lt_if;
Symbol lt_begin;
Symbol lt_set;
Symbol lt_lambda;

Symbol lt_dset;
Symbol lt_dynamic;
Symbol lt_catch;
Symbol lt_throw;

Symbol make_symbol(char *symbol_name)
{
    Symbol sym;

    sym = new_object();
    sym->type = SYMBOL;
    sym->symbol_name = symbol_name;

    return sym;
}

Symbol get_symbol(char *symbol_name)
{
    SymbolTable tbl;

    tbl = symbol_table;
    while (tbl != NULL) {
        if (0 == strcmp(tbl->symbol_name, symbol_name))
            return tbl->symbol;
        if (strcmp(symbol_name, tbl->symbol_name) < 0)
            tbl = tbl->left;
        else
            tbl = tbl->right;
    }

    return NULL;
}

StrSymMapNode make_StrSymMapNode(Symbol symbol)
{
    StrSymMapNode node;

    node = malloc(sizeof(struct StrSymMapNode));
    node->symbol_name = symbol->symbol_name;
    node->symbol = symbol;
    node->left = node->parent = node->right = NULL;

    return node;
}

void put_symbol(Symbol symbol)
{
    StrSymMapNode node, parent, root;

    node = make_StrSymMapNode(symbol);
    root = symbol_table;
    parent = NULL;
    while (root != NULL) {
        parent = root;
        if (strcmp(symbol->symbol_name, root->symbol_name) < 0)
            root = root->left;
        else
            root = root->right;
    }
    node->parent = parent;
    if (NULL == parent)
        symbol_table = node;
    else if (strcmp(symbol->symbol_name, parent->symbol_name) < 0)
        parent->left = node;
    else
        parent->right = node;
}

Symbol ensure_symbol_exists(char *symbol_name)
{
    Symbol symbol;

    symbol = get_symbol(symbol_name);
    if (symbol != NULL)
	return symbol;
    else {
	symbol = make_symbol(symbol_name);
	put_symbol(symbol);

	return symbol;
    }
}

void init_symbol_table(void)
{
    lt_quote = ensure_symbol_exists("quote");
    lt_if = ensure_symbol_exists("if");
    lt_begin = ensure_symbol_exists("begin");
    lt_set = ensure_symbol_exists("set!");
    lt_lambda = ensure_symbol_exists("lambda");
    /* The symbols below names the special operators of myself. */
    lt_dset = ensure_symbol_exists("lt/dset!");
    lt_dynamic = ensure_symbol_exists("lt/dynamic");
    lt_catch = ensure_symbol_exists("lt/catch");
    lt_throw = ensure_symbol_exists("lt/throw");
}

Function new_function(void)
{
    Function obj = new_object();

    FUNCTION(obj) = malloc(sizeof(struct function_t));

    return obj;
}

Function make_c_fun_object(primitive_t prim)
{
    Function func;

    func = new_function();
    TYPE(func) = FUNCTION;
    FUNC_FLAG(func) = TRUE;
    PRIMITIVE(func) = prim;

    return func;
}

BOOL is_true_obj(LispObject obj)
{
    return lt_nil != obj;	/* Everything is true except the object lt_false */
}

Function make_i_fun_object(Cons parms, LispObject expr, Environment cenv, Environment denv)
/* The parameter 'cenv' points to the environment at the creation time. */
/* In Lisp-2, a closure should also stores the dynamic environment at the
   creating time. */
{
    Function fun;

    fun = new_function();
    TYPE(fun) = FUNCTION;
    FUNC_FLAG(fun) = FALSE;
    PARAMETERS(fun) = parms;
    EXPRESSION(fun) = expr;
    LOCAL_ENV(fun) = cenv;	/* This lexical environment could be modified 
				   after the creation of the closure */
    FUNC_DENV(fun) = denv;	/* The dynamic environment. */

    return fun;
}

BOOL is_atom_object(LispObject object)
{
    return TYPE(object) != CONS;
}

BOOL is_tail(LispObject object)
{
    return lt_nil == object || is_atom_object(object);
}

BOOL is_symbol(LispObject object)
{
    return SYMBOL == object->type;
}
