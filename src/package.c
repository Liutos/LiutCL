/*
 * package.c
 *
 *
 *
 * Copyright (C) 2012-11-08 liutos <mat.liutos@gmail.com>
 */
#include <stdlib.h>

#include "hash_table.h"
#include "object.h"
#include "symbol.h"
#include "types.h"

Package pkg_cl;                     /* Package :common-lisp */
Package pkg_lt;                 /* Package :liutcl */
hash_table_t packages;          /* A hash table contains all packages */

package_t make_package_aux(char *name)
{
    package_t pkg;

    pkg = malloc(sizeof(struct package_t));
    pkg->name = name;
    pkg->table = make_symbol_table(47);

    return pkg;
}

Package make_package(char *name)
{
    Package pkg;

    pkg = make_object();
    pkg->type = PACKAGE;
    thePACKAGE(pkg) = make_package_aux(name);
    update_key_value(name, pkg, packages);

    return pkg;
}

Package find_package(char *name)
{
    return (Package)search_key(name, packages);
}

Symbol gen_pkg_sym(char *name, Package pkg)
{
    return ensure_symbol_exists(name, PACKAGE_HASH_TABLE(pkg));
}
