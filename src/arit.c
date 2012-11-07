/*
 * arit.c
 *
 *
 *
 * Copyright (C) 2012-11-04 liutos <mat.liutos@gmail.com>
 */
#include "atom_proc.h"
#include "cons.h"
#include "types.h"
#include "vm_stack.h"

#include "pdecls.h"

PHEAD(add)
{
    /* int n1, n2; */
    int n2 = theFIXNUM(pop_object());
    int n1 = theFIXNUM(pop_object());
    /* ACCESS_PARM2(n1, theFIXNUM, n2, theFIXNUM); */

    return TO_FIXNUM(n1 + n2);
}

PHEAD(sub)
{
    /* int n1, n2; */
    int n2 = theFIXNUM(pop_object());
    int n1 = theFIXNUM(pop_object());
    /* ACCESS_PARM2(n1, theFIXNUM, n2, theFIXNUM); */

    return TO_FIXNUM(n1 - n2);
}

PHEAD(mul)
{
    /* int n1, n2; */
    int n2 = theFIXNUM(pop_object());
    int n1 = theFIXNUM(pop_object());
    /* ACCESS_PARM2(n1, theFIXNUM, n2, theFIXNUM); */

    return TO_FIXNUM(n1 * n2);
}

PHEAD(div)
{
    /* int n1, n2; */
    int n2 = theFIXNUM(pop_object());
    int n1 = theFIXNUM(pop_object());
    /* ACCESS_PARM2(n1, theFIXNUM, n2, theFIXNUM); */

    return TO_FIXNUM(n1 / n2);
}

PHEAD(gt)
{
    /* int n1, n2; */
    int n2 = theFIXNUM(pop_object());
    int n1 = theFIXNUM(pop_object());
    /* ACCESS_PARM2(n1, theFIXNUM, n2, theFIXNUM); */

    return n1 > n2 ? lt_t: lt_nil;
}

PHEAD(fixnum_eq)
{
    /* int n1, n2; */
    int n2 = theFIXNUM(pop_object());
    int n1 = theFIXNUM(pop_object());
    /* ACCESS_PARM2(n1, theFIXNUM, n2, theFIXNUM); */

    return n1 == n2 ? lt_t: lt_nil;
}
