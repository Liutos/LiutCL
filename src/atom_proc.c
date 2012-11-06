/*
 * atom_proc.c
 *
 * 
 *
 * Copyright (C) 2012-10-05 liutos
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cons.h"
#include "env_types.h"
#include "object.h"
#include "symbol_table.h"
#include "types.h"

Symbol lt_nil;
Symbol lt_t;

Symbol lt_block;
Symbol lt_catch;
Symbol lt_defvar;
Symbol lt_fset;
Symbol lt_function;
Symbol lt_progn;
#ifndef FS
Symbol lt_if;
Symbol lt_lambda;
Symbol lt_quote;
#endif
Symbol lt_return_from;
Symbol lt_setq;
Symbol lt_throw;

Character make_char(char C_character)
{
    return TO_CHAR(C_character);
}

Fixnum make_fixnum(int C_integer)
{
    return TO_FIXNUM(C_integer);
}

String make_string(char *C_string)
{
    string_t object = malloc(sizeof(struct string_t));

    object->content = strdup(C_string);
    object->length = strlen(C_string);

    return TO_STRING(object);
}

BOOL is_true_obj(LispObject obj)
{
    return lt_nil != obj;
}

BOOL is_atom_object(LispObject object)
{
    return !CONS_P(object);
}

BOOL is_tail(LispObject object)
{
    return lt_nil == object || is_atom_object(object);
}

BOOL is_symbol(LispObject object)
{
    return SYMBOL_P(object);
}
