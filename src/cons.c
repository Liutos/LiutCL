/*
 * cons.c
 *
 * Operators on objects of type Cons
 *
 * Copyright (C) 2012-10-04 liutos
 */
#include <stdlib.h>
#include "types.h"
#include "object.h"

Cons make_cons_cell(LispObject car, LispObject cdr)
{
    Cons cons;

    cons = new_object();
    cons->type = CONS;
    CAR(cons) = car;
    CDR(cons) = cdr;

    return cons;
}
