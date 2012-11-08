/*
 * io.c
 *
 *
 *
 * Copyright (C) 2012-11-04 liutos <mat.liutos@gmail.com>
 */
#include "atom.h"
#include "cons.h"
#include "stream.h"
#include "pdecls.h"
#include "types.h"

PHEAD(lt_read_a_char)
{
    RETURN(read_char(ARG1));
}

PHEAD(lt_write_a_char)
{
    write_char(ARG2, ARG1);
    RETURN(lt_nil);
}
