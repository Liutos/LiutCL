#ifndef SYMTBL_H
#define SYMTBL_H

#include "types.h"

/* AVL tree implementation */
typedef struct StrSymMapNode *StrSymMapNode;
typedef struct StrSymMapNode *SymbolTable;
struct StrSymMapNode {
    LispObject symbol;
    StrSymMapNode left, parent, right;
    int bf;                     /* Balance factor */
    int height;                 /* Height of the tree */
};

#endif
