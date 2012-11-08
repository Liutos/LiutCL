/*
 * cons.c
 *
 * 
 *
 * Copyright (C) 2012-10-04 liutos
 */
#include <assert.h>
#include <stdlib.h>

#include "atom.h"
#include "object.h"
#include "types.h"

Cons make_cons(LispObject car, LispObject cdr)
{
    cons_t cons = malloc(sizeof(struct cons_t));
    cons->car = car;
    cons->cdr = cdr;

    return TO_CONS(cons);
}

void free_cons(Cons cons)
{
    assert(CONS_P(cons));

    free(theCONS(cons));
}

unsigned int cons_length(Cons cons)
{
    if (lt_nil == cons) return 0;
    else return 1 + cons_length(CDR(cons));
}
