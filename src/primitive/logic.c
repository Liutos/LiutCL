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
#include "../vm_stack.h"
#include "pdecls.h"

/* Greedy logical-and operation. */
PHEAD(and_two)
{
    /* LispObject arg1, arg2; */
    LispObject arg2 = pop_object();
    LispObject arg1 = pop_object();
    /* PARM2(arg1, arg2); */

    return (is_true_obj(arg1) && is_true_obj(arg2)) ? lt_t: lt_nil;
}

/* Greedy logical-or operation. */
PHEAD(or_two)
{
    /* LispObject arg1, arg2; */
    LispObject arg2 = pop_object();
    LispObject arg1 = pop_object();
    /* PARM2(arg1, arg2); */

    return (is_true_obj(arg1) || is_true_obj(arg2)) ? lt_t: lt_nil;
}
