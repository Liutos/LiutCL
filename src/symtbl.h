#ifndef SYMTBL_H
#define SYMTBL_H

#include "types.h"

/* typedef struct StrSymMap { */
/*     char *symbol_name; */
/*     struct LispObject *symbol; */
/*     struct StrSymMap *next; */
/* } *SymbolTable; */

/* Implements the symbol table as a binary search tree.
   AVL tree or red-black tree is better. */

/* AVL tree version */
typedef struct StrSymMapNode {
    /* char *symbol_name; */          /* This field should be removed. */
    struct LispObject *symbol;
    struct StrSymMapNode *left, *right, *parent;
    int height;                 /* Height of the tree. */
    int bf;                     /* balance factor */
} *SymbolTable, *StrSymMapNode;

#endif
