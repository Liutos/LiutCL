#ifndef OBJECT_H
#define OBJECT_H

#include <stdio.h>

#include "types.h"

extern LispObject make_object(void);
extern LispType type_of(LispObject);
extern Values cons2values(Cons);
extern stream_t make_C_file_stream(FILE *);
extern stream_t make_C_string_stream(char *);

#endif
