/*
 * string.c
 *
 *
 *
 * Copyright (C) 2012-11-19 liutos <mat.liutos@gmail.com>
 */
#include <stdlib.h>
#include <string.h>

#include "types.h"

String make_string(char *C_string)
{
    string_t object;

    object = malloc(sizeof(struct string_t));
    object->content = strdup(C_string);
    object->length = strlen(C_string);
    object->size = object->length + 1;

    return TO_STRING(object);
}

