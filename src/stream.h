#ifndef STREAM_H
#define STREAM_H

#include <stdio.h>

#include "types.h"

extern Character read_char(Stream);
extern Stream make_file_stream(FILE *, MODE);
extern void error_format(const char *, ...);
extern void write_address(Stream, LispObject);
extern void write_bignum(Stream, Bignum);
extern void write_char(Stream, Character);
extern void write_fixnum(Stream, Fixnum);
extern void write_format(Stream, const char *, ...);
extern void write_single_float(Stream, SingleFloat);
extern void write_string(Stream, String);

#endif
