/*
 * stream.c
 *
 * Implementation of operations on objects of type stream
 *
 * Copyright (C) 2012-10-31 liutos <mat.liutos@gmail.com>
 */
#include "types.h"
#include "object.h"

#include <stdio.h>
#include <stdlib.h>

stream_t _make_file_stream(FILE *fp)
{
    stream_t stream = malloc(sizeof(struct stream_t));

    stream->type = FILE_STREAM;
    stream->u.file = fp;

    return stream;
}

Stream make_file_stream(FILE *fp)
{
    Stream object = new_object();

    TYPE(object) = STREAM;
    object->u.stream = _make_file_stream(fp);

    return object;
}
