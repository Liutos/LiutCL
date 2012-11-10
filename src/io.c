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
    Boolean eof_error_p;
    LispObject Lisp_char, eof_value;
    Stream stream;

    stream = ARG1;
    eof_error_p = ARG2;
    eof_value = ARG3;
    Lisp_char = read_char(ARG1);
    if (eq(TO_CHAR(-1), Lisp_char) && eq(lt_t, eof_error_p)) {
        error_format("READ-CHAR: End of file.\n");
        exit(1);
    }
    RETURN(Lisp_char);
}

PHEAD(lt_read_line)
{
    Boolean eof_error_p;
    Character ch;
    LispObject eof_value;
    Stream stream;
    String line;

    stream = ARG1;
    eof_error_p = ARG2;
    eof_value = ARG3;
    ch = read_char(stream);
    line = make_string("");
    while (ch != TO_CHAR('\n')) {
        if (TO_CHAR(-1) == ch)
            break;
        str_add_char(line, ch);
        ch = read_char(stream);
    }
    if (TO_CHAR(-1) == ch) {
        if (eq(lt_t, eof_error_p)) {
            error_format("READ-LINE: End of file.\n");
            longjmp(toplevel, 1);
        } else
            RETURN(eof_value);
    }
    str_add_char(line, TO_CHAR('\0'));
    RETURN(line);
}

PHEAD(lt_write_a_char)
{
    write_char(ARG2, ARG1);
    RETURN(lt_nil);
}
