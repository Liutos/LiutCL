/*
 * arit.c
 *
 *
 *
 * Copyright (C) 2012-11-04 liutos <mat.liutos@gmail.com>
 */
#include "cons.h"
#include "pdecls.h"
#include "types.h"

PHEAD(add)
{
    int n1, n2;

    n1 = theFIXNUM(ARG1);
    n2 = theFIXNUM(ARG2);
    RETURN(TO_FIXNUM(n1 + n2));
}

PHEAD(fixnum_eq)
{
    int n1, n2;

    n1 = theFIXNUM(ARG1);
    n2 = theFIXNUM(ARG2);
    RETURN(n1 == n2 ? lt_t: lt_nil);
}

PHEAD(float_eq)
{
    double f1, f2;

    f1 = theFLOAT(ARG1);
    f2 = theFLOAT(ARG2);
    RETURN(f1 == f2? lt_t: lt_nil);
}

PHEAD(gt)
{
    int n1, n2;

    n1 = theFIXNUM(ARG1);
    n2 = theFIXNUM(ARG2);
    RETURN(n1 > n2 ? lt_t: lt_nil);
}

PHEAD(div)
{
    int n1, n2;

    n1 = theFIXNUM(ARG1);
    n2 = theFIXNUM(ARG2);
    RETURN(TO_FIXNUM(n1 / n2));
}

PHEAD(mul)
{
    int n1, n2;

    n1 = theFIXNUM(ARG1);
    n2 = theFIXNUM(ARG2);
    RETURN(TO_FIXNUM(n1 * n2));
}

PHEAD(sub)
{
    int n1, n2;

    n1 = theFIXNUM(ARG1);
    n2 = theFIXNUM(ARG2);
    RETURN(TO_FIXNUM(n1 - n2));
}
