/*
 * io.c
 *
 *
 *
 * Copyright (C) 2012-11-04 liutos <mat.liutos@gmail.com>
 */
#include "../atom_proc.h"
#include "../cons.h"
#include "../stream.h"
#include "../types.h"

#define PARM2(o1, o2)                           \
    do {                                        \
        o1 = FIRST(args);                       \
        o2 = SECOND(args);                      \
    } while(0)
#define PHEAD(fn_name) LispObject fn_name(Cons args)

PHEAD(lt_read_a_char)
{
    Stream str = FIRST(args);

    return read_char(str);
}

PHEAD(lt_write_a_char)
{
    Stream str;
    Character c;

    PARM2(str, c);
    write_char(str, c);

    return lt_nil;
}
