/*
 * object_proc.c
 *
 *
 *
 * Copyright (C) 2012-11-06 liutos <mat.liutos@gmail.com>
 */
#include <stdio.h>
#include <stdlib.h>

#include "atom_proc.h"
#include "cons.h"
#include "object.h"
#include "pdecls.h"
#include "types.h"
#include "stream.h"
#include "symbol_table.h"

PHEAD(lt_eq)
{
    return FIRST(args) == SECOND(args) ? lt_t: lt_nil;
}

PHEAD(lt_type_of)
{
    LispObject object = FIRST(args);

    switch (enum_type_of(object)) {
    case CONS: return ensure_symbol_exists("CONS");
    case SYMBOL: return ensure_symbol_exists("SYMBOL");
    case FIXNUM: return ensure_symbol_exists("FIXNUM");
    case FUNCTION: return ensure_symbol_exists("FUNCTION");
    case STRING: return ensure_symbol_exists("STRING");
    case STREAM: return ensure_symbol_exists("STREAM");
    default :
        write_format(standard_error, "Unknown data type %d. How can you define that?\n", enum_type_of(object));
        exit(1);
    }
}
