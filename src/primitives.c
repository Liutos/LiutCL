/*
 * primitives.c
 *
 * 
 *
 * Copyright (C) 2012-10-05 liutos
 */
#include <stdio.h>
#include <stdlib.h>

#include "atom_proc.h"
#include "cons.h"
#include "object.h"
#include "stream.h"
#include "symbol_table.h"
#include "types.h"

#define PARM2(o1, o2)                           \
    do {                                        \
        o1 = FIRST(args);                       \
        o2 = SECOND(args);                      \
    } while(0)
#define ACCESS_PARM2(o1, ac1, o2, ac2)          \
    do {                                        \
        o1 = ac1(FIRST(args));                  \
        o2 = ac2(SECOND(args));                 \
    } while (0)
#define PHEAD(fn_name) LispObject fn_name(Cons args)

PHEAD(quit)
{
    printf("Quiting......\n");
    exit(0);
}

PHEAD(lt_car)
{ return safe_car(FIRST(args)); }

PHEAD(lt_cdr)
{ return safe_cdr(FIRST(args)); }

PHEAD(lt_cons)
{
    LispObject o1, o2;

    PARM2(o1, o2);

    return make_cons(o1, o2);
}

inline PHEAD(lt_eq)
{ return FIRST(args) == SECOND(args) ? lt_t: lt_nil; }

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
