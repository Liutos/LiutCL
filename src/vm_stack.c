/*
 * vm_stack.c
 *
 *
 *
 * Copyright (C) 2012-11-05 liutos <mat.liutos@gmail.com>
 */
#include <stdlib.h>

#include "atom_proc.h"
#include "stream.h"
#include "types.h"

LispObject global_stack[20];
int global_stack_top = -1;

BOOL is_stack_empty(void)
{
    return -1 == global_stack_top;
}

BOOL is_stack_full(void)
{
    return sizeof(global_stack) / sizeof(LispObject) == global_stack_top;
}

void describe_global_stack(void)
{
    for (int i = 0; i <= global_stack_top; ++i)
        write_format(standard_output, "[%d] %!\n",
                     TO_FIXNUM(i), global_stack[i]);
}

LispObject pop_object(void)
{
    if (is_stack_empty()) {
        write_format(standard_error, "Stack Underflow\n");
        exit(1);
    }

    return global_stack[global_stack_top--];
}

void push_object(LispObject object)
{
    if (is_stack_full()) {
        write_format(standard_error, "Stack Overflow\n");
        exit(1);
    }
    global_stack[++global_stack_top] = object;
}
