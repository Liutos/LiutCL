/*
 * primitives.c
 *
 * 
 *
 * Copyright (C) 2012-10-05 liutos
 */
#include <stdio.h>
#include <stdlib.h>

#include "atom.h"
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
