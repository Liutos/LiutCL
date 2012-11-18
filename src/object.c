/*
 * object.c
 *
 * 
 *
 * Copyright (C) 2012-10-05 liutos
 */
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cons.h"
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

Values make_values_aux(int cnt, va_list ap)
{
    values_t vals;
    LispObject obj;

    vals = malloc(sizeof(struct values_t));
    vals->count = cnt;
    vals->objs = malloc(cnt * sizeof(LispObject));
    obj = va_arg(ap, LispObject);
    for (int i = 0; i < cnt; i++) {
        vals->objs[i] = obj;
        obj = va_arg(ap, LispObject);
    }
    va_end(ap);

    return TO_VALUES(vals);
}

Values make_values(int _, ...)
{
    va_list ap;

    va_start(ap, _);

    return make_values_aux(_, ap);
}
