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
#include "pdecls.h"

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
