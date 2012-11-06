/*
 * object_proc.c
 *
 *
 *
 * Copyright (C) 2012-11-06 liutos <mat.liutos@gmail.com>
 */
#include <stdio.h>
#include <stdlib.h>

#include "../atom_proc.h"
#include "../cons.h"
#include "../object.h"
#include "../types.h"
#include "../symbol_table.h"
#include "pdecls.h"

PHEAD(lt_eq)
{
    return FIRST(args) == SECOND(args) ? lt_t: lt_nil;
}

PHEAD(lt_type_of)
{
    LispObject object = FIRST(args);

    switch (TYPE(object)) {
    case CONS: return ensure_symbol_exists("CONS");
    case SYMBOL: return ensure_symbol_exists("SYMBOL");
    case FIXNUM: return ensure_symbol_exists("FIXNUM");
    case FUNCTION: return ensure_symbol_exists("FUNCTION");
    case STRING: return ensure_symbol_exists("STRING");
    case STREAM: return ensure_symbol_exists("STREAM");
    default :
        fprintf(stderr, "Unknown data type %d. How can you define that?\n", TYPE(object));
        exit(1);
    }
}
