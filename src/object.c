/*
 * object.c
 *
 * 
 *
 * Copyright (C) 2012-10-05 liutos
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "types.h"

LispObject g_unbound;

inline LispObject make_object(void)
{
    return malloc(sizeof(struct lisp_object_t));
}

stream_t make_C_file_stream(FILE *fp)
{
    stream_t stream = malloc(sizeof(struct stream_t));
    stream->type = FILE_STREAM;
    stream->u.file = fp;

    return stream;
}

stream_t make_C_string_stream(char *string)
{
    stream_t stream = malloc(sizeof(struct stream_t));
    stream->type = CHARACTER_STREAM;
    stream->u.s.string = strdup(string);

    return stream;
}

LispType enum_type_of(LispObject object)
{
    if (CHAR_P(object)) return CHARACTER;
    if (CONS_P(object)) return CONS;
    if (FIXNUM_P(object)) return FIXNUM;
    if (FUNCTION_P(object)) return FUNCTION;
    if (STRING_P(object)) return STRING;
    if (SYMBOL_P(object)) return SYMBOL;
    else return object->type;
}
