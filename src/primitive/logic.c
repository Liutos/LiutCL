/*
 * logic.c
 *
 *
 *
 * Copyright (C) 2012-11-04 liutos <mat.liutos@gmail.com>
 */
#include "../atom_proc.h"
#include "../cons.h"
#include "../types.h"

#define PARM2(o1, o2)                           \
    do {                                        \
        o1 = FIRST(args);                       \
        o2 = SECOND(args);                      \
    } while(0)
#define PHEAD(fn_name) LispObject fn_name(Cons args)

PHEAD(and_two)
{
    LispObject arg1, arg2;

    PARM2(arg1, arg2);

    return (is_true_obj(arg1) && is_true_obj(arg2)) ? lt_t: lt_nil;
}

PHEAD(or_two)
{
    LispObject arg1, arg2;

    PARM2(arg1, arg2);

    return (is_true_obj(arg1) || is_true_obj(arg2)) ? lt_t: lt_nil;
}
