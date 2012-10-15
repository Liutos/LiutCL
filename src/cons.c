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
#include "atom_proc.h"

Cons make_cons_cell(LispObject car, LispObject cdr)
{
    Cons cons;

    cons = new_object();
    cons->type = CONS;
    CAR(cons) = car;
    CDR(cons) = cdr;

    return cons;
}

LispObject safe_car(LispObject obj)
{
    return lt_nil == obj ? lt_nil: obj->car;
}

LispObject safe_cdr(LispObject obj)
{
    return lt_nil == obj ? lt_nil: obj->cdr;
}
