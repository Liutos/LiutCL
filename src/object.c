/*
 * object.c
 *
 * Some operations common among all type of objects.
 *
 * Copyright (C) 2012-10-05 liutos
 */
#include "types.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

LispObject new_object(void)
{
    return malloc(sizeof(struct LispObject));
}

symbol_t make_C_symbol(char *symbol_name)
{
    symbol_t symbol = malloc(sizeof(struct symbol_t));

    symbol->symbol_name = symbol_name;
    symbol->value_cell = NULL;
    symbol->function_cell = NULL;

    return symbol;
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
    stream->u.string = strdup(string);

    return stream;
}
