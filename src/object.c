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

Values cons2values(Cons objs)
{
    size_t count;
    values_t vals;

    count = cons_length(objs);
    vals = malloc(sizeof(struct values_t));
    vals->count = count;
    vals->objs = malloc(count * sizeof(LispObject));
    for (int i = 0; i < count; i++) {
        vals->objs[i] = CAR(objs);
        objs = CDR(objs);
    }

    return TO_VALUES(vals);
}
