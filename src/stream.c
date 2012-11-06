/*
 * stream.c
 *
 * 
 *
 * Copyright (C) 2012-10-31 liutos <mat.liutos@gmail.com>
 */
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#include "atom_proc.h"
#include "object.h"
#include "types.h"

Stream standard_output;
Stream standard_input;
Stream standard_error;

extern void print_object_notln(LispObject, Stream);

Stream make_file_stream(FILE *fp)
{
    Stream object = make_object();
    object->type = STREAM;
    theSTREAM(object) = make_C_file_stream(fp);

    return object;
}

Stream make_string_stream(char *string)
{
    Stream object = make_object();
    object->type = STREAM;
    theSTREAM(object) = make_C_string_stream(string);

    return object;
}

Character read_file_stream_char(Stream file_stream)
{
    return TO_CHAR(fgetc(theSTREAM(file_stream)->u.file));
}

void write_string(Stream stream, String string)
{
    fputs(STRING_CONTENT(string), STREAM_FILE(stream));
}

Character read_char(Stream stream)
{
    switch (theSTREAM(stream)->type) {
    case FILE_STREAM:
        return read_file_stream_char(stream);
        break;
    default :
        write_string(standard_error, TO_STRING("Unknown stream type\n"));
        exit(1);
    }
}

void write_file_stream_char(Stream stream, Character c)
{
    fputc(theCHAR(c), STREAM_FILE(stream));
}

void write_file_stream_fixnum(Stream stream, Fixnum number)
{
    fprintf(STREAM_FILE(stream), "%d", theFIXNUM(number));
}

void write_address(Stream stream, LispObject object)
{
    fprintf(STREAM_FILE(stream), "%p", thePOINTER(object));
}

void write_char(Stream stream, Character c)
{
    write_file_stream_char(stream, c);
}

void write_fixnum(Stream stream, Fixnum number)
{
    write_file_stream_fixnum(stream, number);
}

void write_format(Stream dest, const char *format, ...)
{
    va_list ap;
    char c;

    va_start(ap, format);
    while ((c = *format++))
        if ('%' == c)
            switch (*format++) {
            case '!':
                print_object_notln(va_arg(ap, LispObject), dest);
                break;
            case '%':
                write_char(dest, TO_CHAR('%'));
                break;
            case 'c':
                write_char(dest, va_arg(ap, Character));
                break;
            case 'd':
                write_fixnum(dest, va_arg(ap, Fixnum));
                break;
            case 'p':
                write_address(dest, va_arg(ap, LispObject));
                break;
            case 's':
                write_string(dest, va_arg(ap, String));
                break;
            default :
                write_string(standard_error, TO_STRING("Unknown directive\n"));
                exit(1);
            }
        else
            write_char(dest, TO_CHAR(c));
}
