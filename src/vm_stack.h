#ifndef VM_STACK_H
#define VM_STACK_H

#include "types.h"

extern LispObject pop_object(void);
extern void describe_global_stack(void);
extern void push_object(LispObject);

extern LispObject global_stack[];
extern int global_stack_top;

#endif
