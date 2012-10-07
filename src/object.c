/*
 * object.c
 *
 *
 *
 * Copyright (C) 2012-10-05 liutos
 */
#include "types.h"
#include <stdlib.h>

LispObject new_object(void)
{
    return malloc(sizeof(struct LispObject));
}

