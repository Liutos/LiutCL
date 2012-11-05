/*
 * read_sexp.c
 *
 *
 *
 * Copyright (C) 2012-10-13 liutos mat.liutos@gmail.com
 */
#include <stdio.h>
#include <stdlib.h>

#include "types.h"
#include "stream.h"

#define DELTA 80

char *read_sexp(FILE *fp)
{
    char *str = NULL, c;
    int len, i, burden;;
    BOOL reading_cons;

    c = fgetc(fp);
    len = i = burden = 0;
    reading_cons = '(' == c ? TRUE: FALSE;
    while (EOF != c) {
        if (i > len - 1) {
            len += DELTA;
            str = realloc(str, len * sizeof(char));
        }
        str[i++] = c;
        if (reading_cons) {     /* When reading list, keep parenthesis
                                   balanced. */
            if ('(' == c) burden++;
            if (')' == c) burden--;
        }
        if ('\n' == c && (!reading_cons || 0 == burden)) break;
        c = fgetc(fp);
    }
    if (NULL == str) {
        write_format(standard_error, "Encounter EOF.\n");
        exit(1);
    }
    str[i] = '\0';

    return str;
}
