#ifndef STREAM_H
#define STREAM_H

#include "types.h"

#include <stdio.h>

extern Stream make_file_stream(FILE *);
extern Character read_stream_char(Stream);
extern void write_stream_char(Stream, Character);
extern void write_stream_string(Stream, String);
extern void write_file_stream_integer(Stream, Integer);

extern Stream standard_output;
extern Stream standard_input;

#endif
