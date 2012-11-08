/*
 * object_proc.c
 *
 *
 *
 * Copyright (C) 2012-11-06 liutos <mat.liutos@gmail.com>
 */
#include <stdio.h>
#include <stdlib.h>

#include "atom.h"
#include "cons.h"
#include "object.h"
#include "pdecls.h"
#include "types.h"
#include "stream.h"
#include "symbol_table.h"

#define case_type(name) case name: return ensure_symbol_exists("#name")

PHEAD(lt_eq)
{
    RETURN(ARG1 == ARG2 ? lt_t: lt_nil);
}

PHEAD(lt_type_of)
{
    switch (enum_type_of(ARG1)) {
    /* case CONS: return ensure_symbol_exists("CONS"); */
        case_type(CONS);
    case SYMBOL: return ensure_symbol_exists("SYMBOL");
    case FIXNUM: return ensure_symbol_exists("FIXNUM");
    case FUNCTION: return ensure_symbol_exists("FUNCTION");
    case STRING: return ensure_symbol_exists("STRING");
    case STREAM: return ensure_symbol_exists("STREAM");
    default :
        write_format(standard_error, "Unknown data type %d. How can you define that?\n", enum_type_of(ARG1));
        exit(1);
    }
}
