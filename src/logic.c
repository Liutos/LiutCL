/*
 * logic.c
 *
 *
 *
 * Copyright (C) 2012-11-04 liutos <mat.liutos@gmail.com>
 */
#include "pdecls.h"
#include "types.h"

PHEAD(and2)
{
    RETURN(ARG1 != lt_nil && ARG2 != lt_nil ? lt_t: lt_nil);
}

PHEAD(or2)
{
    RETURN(ARG1 != lt_nil || ARG2 != lt_nil ? lt_t: lt_nil);
}

void init_logic(Environment env)
{
    freg("AND2", pkg_cl, and2, req2);
    freg("OR2", pkg_cl, or2, req2);
}
