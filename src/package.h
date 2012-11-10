/*
 * package.h
 *
 *
 *
 * Copyright (C) 2012-11-08 liutos <mat.liutos@gmail.com>
 */
#ifndef PACKAGE_H
#define PACKAGE_H

#include "types.h"

extern Package find_package(char *);
extern Package make_package(char *);
extern Symbol gen_keyword(char *);
extern Symbol gen_pkg_sym(char *, Package);

extern Package pkg_cl;
extern Package pkg_kw;
extern Package pkg_lt;
extern hash_table_t packages;

#endif
