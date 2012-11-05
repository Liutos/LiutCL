/*
 * xcons.c
 *
 * Test for cons.c file.
 *
 * Copyright (C) 2012-11-02 liutos <mat.liutos@gmail.com>
 */
#include "cons.h"
#include "types.h"
#include "atom_proc.h"

#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[])
{
    printf("sizeof(LispObject) is %d.\n", sizeof(LispObject));
    printf("sizeof(struct lisp_object_t) is %d.\n", sizeof(struct lisp_object_t));
    printf("sizeof(struct function_t) is %d.\n", sizeof(struct function_t));

    return 0;
}

