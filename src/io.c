/*
 * io.c
 *
 *
 *
 * Copyright (C) 2012-11-04 liutos <mat.liutos@gmail.com>
 */
#include <stdlib.h>

#include "atom.h"
#include "cons.h"
#include "stream.h"
#include "pdecls.h"
#include "types.h"

PHEAD(lt_read_char)
{
    /* RETURN(read_char(ARG1)); */
    Boolean eof_error_p;
    LispObject Lisp_char, eof_value;
    Stream stream;

    stream = ARG1;
    eof_error_p = ARG2;
    eof_value = ARG3;
    Lisp_char = read_char(ARG1);
    if (eq(TO_CHAR(-1), Lisp_char) && eq(lt_t, eof_error_p)) {
        error_format("End of file\n");
        exit(1);
    }
    RETURN(Lisp_char);
}

PHEAD(lt_write_a_char)
{
    write_char(ARG2, ARG1);
    RETURN(lt_nil);
}
