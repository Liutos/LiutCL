/*
 * stream.c
 *
 * Implementation of operations on objects of type stream. Maybe it's
 * suitable for implementing some low-level operations here.
 *
 * Copyright (C) 2012-10-31 liutos <mat.liutos@gmail.com>
 */
#include "types.h"
#include "object.h"
#include "atom_proc.h"

#include <stdio.h>
#include <stdlib.h>

Stream standard_output;         /* Stream object forwards standard output. */
Stream standard_input;          /* Stream object forwards standard input. */

Stream make_file_stream(FILE *fp)
{
    Stream object = new_object();

    object->type = STREAM;
    theSTREAM(object) = make_C_file_stream(fp);

    return object;
}

Stream make_string_stream(char *string)
{
    Stream object = new_object();

    object->type = STREAM;
    theSTREAM(object) = make_C_string_stream(string);

    return object;
}

Character read_file_stream_char(Stream file_stream)
{
    return make_char(fgetc(theSTREAM(file_stream)->u.file));
}

Character read_stream_char(Stream stream)
{
    switch (theSTREAM(stream)->type) {
    case FILE_STREAM:
        return read_file_stream_char(stream);
        break;
    default :
        fprintf(stderr, "Unknown stream type.\n");
        exit(1);
    }
}

void write_file_stream_char(Stream stream, Character c_object)
{
    fputc(CHARACTER(c_object), FILE_STREAM(theSTREAM(stream)));
}

void write_file_stream_integer(Stream stream, Integer i_object)
{
    fprintf(FILE_STREAM(theSTREAM(stream)), "%d", INTEGER(i_object));
}

void write_stream_char(Stream str, Character c) /* 我可能要写很多个版本的write_stream_*函数了，因为要对付许多不同的类型。 */
{
    switch (theSTREAM(str)->type) {
    case FILE_STREAM:
        return write_file_stream_char(str, c);
        break;
    default :
        fprintf(stderr, "Unknown stream type %d.\n", theSTREAM(str)->type);
        exit(1);
    }
}

void write_stream_string(Stream stream, String string)
{
    fprintf(FILE_STREAM(theSTREAM(stream)), "%s", STRING(string));
}
