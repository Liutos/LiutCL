/*
 * vm_stack.c
 *
 *
 *
 * Copyright (C) 2012-11-05 liutos <mat.liutos@gmail.com>
 */
#include <stdlib.h>

#include "atom_proc.h"
#include "print_sexp.h"
#include "stream.h"
#include "types.h"

LispObject global_stack[20];
int global_stack_top = -1;

BOOL is_stack_full(void)
{
    return sizeof(global_stack) / sizeof(LispObject) == global_stack_top;
}

void push_object(LispObject object)
{
    if (is_stack_full()) {
        write_string(standard_error, TO_STRING("Stack Overflow\n"));
        exit(1);
    }
    global_stack[++global_stack_top] = object;
}

LispObject pop_object(void)
{
    return global_stack[global_stack_top--];
}

void describe_global_stack(void)
{
    int i;

    for (i = 0; i <= global_stack_top; ++i) {
        write_char(standard_output, TO_CHAR('['));
        write_fixnum(standard_output, TO_FIXNUM(i));
        write_char(standard_output, TO_CHAR(']'));
        print_object(global_stack[i], standard_output);
    }
}
