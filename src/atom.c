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

#include "object.h"
#include "types.h"

LispObject gunbound;

BOOL is_unbound(LispObject obj)
{
    return obj == gunbound;
}

/* Constructors */
Character make_char(char C_character)
{
    return TO_CHAR(C_character);
}

String str_add_char(String str, Character ch)
{
    unsigned int index, size;

    index = STRING_LENGTH(str); /* The position to inserted. */
    size = STRING_SIZE(str);
    /* Enhance the storage space of the string. */
    if (index >= size) {
        size = index * 2;
        STRING_CONTENT(str) = realloc(STRING_CONTENT(str), size * sizeof(char));
    }
    STRING_CONTENT(str)[index] = theCHAR(ch);
    STRING_LENGTH(str)++;

    return str;
}
