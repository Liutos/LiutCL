/*
 * logic.c
 *
 *
 *
 * Copyright (C) 2012-11-04 liutos <mat.liutos@gmail.com>
 */
#include "atom.h"
#include "cons.h"
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
