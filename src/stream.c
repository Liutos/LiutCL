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
#include <string.h>

#include "atom.h"
#include "object.h"
#include "types.h"

Stream standard_error;
Stream standard_input;
Stream standard_output;

extern void print_object_notln(LispObject, Stream);

stream_t make_C_file_stream(FILE *fp)
{
    stream_t stream;

    stream = malloc(sizeof(struct stream_t));
    stream->type = FILE_STREAM;
    stream->u.file = fp;

    return stream;
}

Stream make_file_stream(FILE *fp)
{
    Stream object;

    object = make_object();
    object->type = STREAM;
    theSTREAM(object) = make_C_file_stream(fp);

    return object;
}

stream_t make_C_string_stream(char *string)
{
    stream_t stream;

    stream = malloc(sizeof(struct stream_t));
    stream->type = CHARACTER_STREAM;
    stream->u.s.string = strdup(string);

    return stream;
}

Stream make_string_stream(char *string)
{
    Stream object;

    object = make_object();
    object->type = STREAM;
    theSTREAM(object) = make_C_string_stream(string);

    return object;
}

void write_file_stream_string(Stream stream, String string)
{
    FILE *fp;
    char *str;

    fp = STREAM_FILE(stream);
    str = STRING_CONTENT(string);
    fputs(str, fp);
}

void write_string(Stream stream, String string)
{
    write_file_stream_string(stream, string);
}

void write_file_stream_char(Stream stream, char c)
{
    fputc(c, STREAM_FILE(stream));
}

void write_file_stream_fixnum(Stream stream, int number)
{
    fprintf(STREAM_FILE(stream), "%d", number);
}

void write_address(Stream stream, LispObject object)
{
    fprintf(STREAM_FILE(stream), "%p", thePOINTER(object));
}

void write_char(Stream stream, Character c)
{
    write_file_stream_char(stream, theCHAR(c));
}

void write_fixnum(Stream stream, Fixnum number)
{
    write_file_stream_fixnum(stream, theFIXNUM(number));
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

Character read_file_stream_char(Stream file_stream)
{
    return TO_CHAR(fgetc(theSTREAM(file_stream)->u.file));
}

Character read_char(Stream stream)
{
    switch (theSTREAM(stream)->type) {
    case FILE_STREAM:
        return read_file_stream_char(stream);
        break;
    default :
        write_string(standard_error, make_string("Unknown stream type\n"));
        exit(1);
    }
}
