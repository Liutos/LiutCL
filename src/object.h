#ifndef OBJECT_H
#define OBJECT_H

#include <stdio.h>

#include "types.h"

extern LispObject make_object(void);
extern LispType enum_type_of(LispObject);
extern stream_t make_C_file_stream(FILE *);
extern stream_t make_C_string_stream(char *);

extern LispObject g_unbound;

#endif
