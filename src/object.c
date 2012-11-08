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

LispObject make_object(void)
{
    return malloc(sizeof(struct lisp_object_t));
}

LispType type_of(LispObject object)
{
    if (POINTER_P(object))
        return object->type;
    else
        return TAGOF(object);
}
