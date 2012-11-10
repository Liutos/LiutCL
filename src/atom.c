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
#include "object.h"
#include "types.h"

BOOL is_atom_object(LispObject object)
{
    return !CONS_P(object);
}

BOOL is_tail(LispObject object)
{
    return is_atom_object(object);
}

BOOL is_true_obj(LispObject obj)
{
    return lt_nil != obj;
}

BOOL is_symbol(LispObject object)
{
    return SYMBOL_P(object);
}

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
    string_t object;

    object = malloc(sizeof(struct string_t));
    object->content = strdup(C_string);
    object->length = strlen(C_string);
    object->size = object->length;

    return TO_STRING(object);
}

/* The caller must ensure the argument `n' and `m' have same type. */
BOOL numeric_eq(Number n, Number m)
{
    if (FIXNUM_P(n))
        return n == m;
    if (FLOAT_P(n))
        return theFLOAT(n) == theFLOAT(m);

    return FALSE;
}

String str_add_char(String str, Character ch)
{
    unsigned int index, size;

    index = STRING_LENGTH(str); /* The position to inserted. */
    size = STRING_SIZE(str);
    /* Enhance the storage space of the string. */
    if (index == size) {
        size = size * 2;
        STRING_CONTENT(str) = realloc(STRING_CONTENT(str), size * sizeof(char));
    }
    STRING_CONTENT(str)[index++] = theCHAR(ch);

    return str;
}
