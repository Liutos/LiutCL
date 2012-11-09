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
#include "symbol.h"
#include "types.h"

Character make_char(char C_character)
{
    return TO_CHAR(C_character);
}

Fixnum make_fixnum(int C_integer)
{
    return TO_FIXNUM(C_integer);
}

Float make_float(double C_float)
{
    Float f;

    f = make_object();
    f->type = FLOAT;
    theFLOAT(f) = C_float;

    return f;
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
    return is_atom_object(object);
}

BOOL is_symbol(LispObject object)
{
    return SYMBOL_P(object);
}
