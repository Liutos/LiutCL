#ifndef STREAM_H
#define STREAM_H

#include <stdio.h>

#include "types.h"

extern Character read_char(Stream);
extern Stream make_file_stream(FILE *);
extern void write_address(Stream, LispObject);
extern void write_char(Stream, Character);
extern void write_fixnum(Stream, Fixnum);
extern void write_format(Stream, const char *, ...);
extern void write_string(Stream, String);

extern Stream standard_error;
extern Stream standard_input;
extern Stream standard_output;

#endif
