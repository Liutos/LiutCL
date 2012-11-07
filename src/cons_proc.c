/*
 * cons_proc.c
 *
 *
 *
 * Copyright (C) 2012-11-06 liutos <mat.liutos@gmail.com>
 */
#include "cons.h"
#include "pdecls.h"
#include "types.h"

PHEAD(lt_car)
{
    return safe_car(FIRST(args));
}

PHEAD(lt_cdr)
{
    return safe_cdr(FIRST(args));
}

PHEAD(lt_cons)
{
    LispObject o1, o2;

    PARM2(o1, o2);

    return make_cons(o1, o2);
}
