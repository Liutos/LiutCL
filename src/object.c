/*
 * object.c
 *
 * Some operations common among all type of objects.
 *
 * Copyright (C) 2012-10-05 liutos
 */
#include "types.h"

#include <stdlib.h>

LispObject new_object(void)
{
    return malloc(sizeof(struct LispObject));
}

