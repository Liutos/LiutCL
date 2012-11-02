/*
 * cons.c
 *
 * Operators on objects of type Cons
 *
 * Copyright (C) 2012-10-04 liutos
 */
#include <stdlib.h>
#include <assert.h>

#include "types.h"
#include "object.h"
#include "atom_proc.h"

/* Tagged Pointer version */
Cons make_cons_cell(LispObject car, LispObject cdr)
{
    cons_t cons = malloc(sizeof(struct cons_t));

    cons->car = car;
    cons->cdr = cdr;

    return MAKE_CONS(cons);
}

void free_cons_cell(Cons cons)
{
    assert(lt_nil == cons || CONS_P(cons));
    free(GET_CONS(cons));
}

LispObject safe_car(LispObject obj)
{
    assert(lt_nil == obj || CONS_P(obj));

    return lt_nil == obj ? lt_nil: GET_CONS(obj)->car;
}

LispObject safe_cdr(LispObject obj)
{
    assert(lt_nil == obj || CONS_P(obj));

    return lt_nil == obj ? lt_nil: GET_CONS(obj)->cdr;
}
