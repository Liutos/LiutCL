/*
 * primitives.c
 *
 * Defining primitives.
 *
 * Copyright (C) 2012-10-05 liutos
 */
#include "types.h"
#include "object.h"
#include <stdio.h>
#include <stdlib.h>

extern Boolean lt_true, lt_false;

PHEAD(plus_two)
{
    int n1, n2;
    LispObject result;

    n1 = INTEGER(CAR(args));
    n2 = INTEGER(CAR(CDR(args)));
    result = new_object();
    result->type = INTEGER;
    INTEGER(result) = n1 + n2;

    return result;
}

PHEAD(mult_two)
{
    int n1, n2;
    LispObject result;

    n1 = INTEGER(FIRST(args));
    n2 = INTEGER(SECOND(args));
    result = new_object();
    result->type = INTEGER;
    INTEGER(result) = n1 * n2;

    return result;
}

PHEAD(quit)
{
    printf("Quiting......\n");
    exit(0);
}

PHEAD(gt_two)
{
    int n1, n2;
    Boolean result;

    n1 = INTEGER(FIRST(args));
    n2 = INTEGER(SECOND(args));
    result = n1 > n2 ? lt_true: lt_false;

    return result;
}

PHEAD(and_two)
{
    LispObject arg1, arg2;

    arg1 = FIRST(args);
    arg2 = SECOND(args);

    return ((arg1 != lt_false) && (arg2 != lt_false)) ? lt_true: lt_false;
}
