/*
 * xread_sexp.c
 *
 *
 *
 * Copyright (C) 2012-10-13 liutos mat.liutos@gmail.com
 */
#include "read_sexp.h"
#include <stdio.h>

int main(int argc, char *argv[])
{
    printf("%s\n", read_sexp(stdin));

    return 0;
}

