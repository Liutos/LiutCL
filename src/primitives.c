/*
 * primitives.c
 *
 * Defining primitives.
 *
 * Copyright (C) 2012-10-05 liutos
 */
#include "types.h"
#include "object.h"
#include "atom_proc.h"
#include "cons.h"
#include <stdio.h>
#include <stdlib.h>

PHEAD(add_two)
{
    int n1, n2;
    LispObject result;

    n1 = INTEGER(FIRST(args));
    n2 = INTEGER(SECOND(args));
    result = new_object();
    result->type = INTEGER;
    INTEGER(result) = n1 + n2;

    return result;
}

PHEAD(sub_two)
{
    int n1, n2;
    LispObject result;

    n1 = INTEGER(FIRST(args));
    n2 = INTEGER(SECOND(args));
    result = new_object();
    result->type = INTEGER;
    INTEGER(result) = n1 - n2;

    return result;
}

PHEAD(mul_two)
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

PHEAD(div_two)
{
    int n1, n2;
    LispObject result;

    n1 = INTEGER(FIRST(args));
    n2 = INTEGER(SECOND(args));
    result = new_object();
    result->type = INTEGER;
    INTEGER(result) = n1 / n2;

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
    result = n1 > n2 ? lt_t: lt_nil;

    return result;
}

PHEAD(and_two)
{
    LispObject arg1, arg2;

    arg1 = FIRST(args);
    arg2 = SECOND(args);

    return (is_true_obj(arg1) && is_true_obj(arg2)) ? lt_t: lt_nil;
}

PHEAD(or_two)
{
    LispObject arg1, arg2;

    arg1 = FIRST(args);
    arg2 = SECOND(args);

    return (is_true_obj(arg1) || is_true_obj(arg2)) ? lt_t: lt_nil;
}

/* PHEAD(get_cons_car) */
/* { */
/*     Cons cons; */

/*     cons = FIRST(args); */

/*     return SCAR(cons); */
/* } */

/* PHEAD(get_cons_cdr) */
/* { */
/*     Cons cons; */

/*     cons = FIRST(args); */

/*     return SCDR(cons); */
/* } */

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
    LispObject o1 = FIRST(args); /* Define-initialize style */
    LispObject o2 = SECOND(args);

    return make_cons_cell(o1, o2);
}

PHEAD(numeric_eq)
{
    int n1, n2;

    n1 = INTEGER(FIRST(args));
    n2 = INTEGER(SECOND(args));

    return n1 == n2 ? lt_t: lt_nil;
}

PHEAD(lt_eq)
{
    return FIRST(args) == SECOND(args) ? lt_t: lt_nil;
}

PHEAD(lt_type_of)
{
    LispObject object = FIRST(args);

    switch (TYPE(object)) {
    case CONS: return ensure_symbol_exists("cons");
    case SYMBOL: return ensure_symbol_exists("symbol");
    case INTEGER: return ensure_symbol_exists("integer");
    case FUNCTION: return ensure_symbol_exists("function");
    case STRING: return ensure_symbol_exists("string");
    default :
        fprintf(stderr, "Unknown data type %d. How can you define that?\n", TYPE(object));
        exit(1);
    }
}
