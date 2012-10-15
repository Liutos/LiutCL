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

#define DELTA 80

char *read_sexp(FILE *fp)
{
    char *str, c;
    int len, i, burden;
    BOOL reading_cons;

    c = fgetc(fp);
    str = NULL;
    burden = i = len = 0;
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
            if (0 == burden) break;
        } else if ('\n' == c)
            break;
        c = fgetc(fp);
    }
    if (NULL == str) {
        fprintf(stderr, "Encounter EOF.\n");
        exit(1);
    }
    str[i] = '\0';

    return str;
}
