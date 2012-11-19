/*
 * cons.c
 *
 * Construction and other operations on cons objects.
 *
 * Copyright (C) 2012-10-04 liutos
 */
#include <assert.h>
#include <stdlib.h>

#include "object.h"
#include "package.h"
#include "stream.h"
#include "symbol.h"
#include "types.h"

/* Constructors */
Cons make_cons(LispObject car, LispObject cdr)
{
    cons_t cons;

    cons = malloc(sizeof(struct cons_t));
    cons->car = car;
    cons->cdr = cdr;

    return TO_CONS(cons);
}

List make_list(LispObject arg0, LispObject arg1)
{
    return make_cons(arg0, make_cons(arg1, lt_nil));
}

/* Deconstructor */
void free_cons(Cons cons)
{
    assert(CONS_P(cons));

    free(theCONS(cons));
}

/* Slot readers */
LispObject car(Cons cons)
{
    return theCONS(cons)->car;
}

LispObject cdr(Cons cons)
{
    return theCONS(cons)->cdr;
}

/* Slot writers */
void set_car(Cons cons, LispObject value)
{
    theCONS(cons)->car = value;
}

void set_cdr(Cons cons, LispObject value)
{
    theCONS(cons)->cdr = value;
}

/* Other operations */
unsigned int cons_length(Cons cons)
{
    unsigned int length;

    for (length = 0; !eq(lt_nil, cons); cons = CDR(cons))
        length++;

    return length;
}

LispObject get_by_key(LispObject key, List list)
{
    while (CONS_P(list)) {
        if (eq(key, CAR(list)))
            return CAR(CDR(list));
        list = CDR(CDR(list));
    }

    return gunbound;
}

void set_by_key(Symbol sym, LispObject val, List alist)
{
    while (CONS_P(alist)) {
        Cons ele;

        ele = CAR(alist);
        if (eq(sym, CAR(ele)))
            /* _CDR(ele) = val; */
            set_cdr(ele, val);
        alist = CDR(alist);
    }
}

LispObject assoc(LispObject key, List alist)
{
    while (CONS_P(alist)) {
        if (eq(key, CAR(CAR(alist))))
            return CDR(CAR(alist));
        alist = CDR(alist);
    }

    return gunbound;
}

LispObject get_by_keyword(char *name, List kws)
{
    return assoc(gen_keyword(name), kws);
}

List make_list_aux(int size, LispObject init_value)
{
    Cons cur, pre, head;

    pre = head = make_cons(lt_nil, lt_nil);
    while (size-- != 0) {
        cur = make_cons(init_value, lt_nil);
        /* _CDR(pre) = cur; */
        set_car(pre, cur);
        pre = cur;
    }

    return CDR(head);
}

LispObject nth_car(unsigned int index, List list)
{
    while (index-- != 0)
        list = CDR(list);

    return CAR(list);
}

Cons nth_cdr(unsigned int n, List list)
{
    while (n-- != 0)
        list = CDR(list);

    return list;
}

void set_nth(unsigned int index, List list, LispObject value)
{
    while (index-- != 0) {
        if (!CONS_P(list)) {
            error_format("Index too large.\n");
            longjmp(toplevel, 1);
        }
        list = CDR(list);
    }
    /* _CAR(list) = value; */
    set_car(list, value);
}
