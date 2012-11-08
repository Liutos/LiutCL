/*
 * symbol.h
 *
 *
 *
 * Copyright (C) 2012-11-07 liutos <mat.liutos@gmail.com>
 */
#ifndef SYMBOL_H
#define SYMBOL_H

#include "types.h"

extern Symbol ensure_symbol_exists(char *, hash_table_t);
extern hash_table_t make_symbol_table(unsigned int);

#endif
