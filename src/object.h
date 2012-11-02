#ifndef OBJECT_H
#define OBJECT_H

#include "types.h"

#include <stdio.h>

extern LispObject new_object(void);
extern symbol_t make_C_symbol(char *);
extern stream_t make_C_file_stream(FILE *);
extern stream_t make_C_string_stream(char *);
extern LispType enum_type_of(LispObject);

#endif
