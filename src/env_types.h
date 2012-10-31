#ifndef ENV_TYPES_H
#define ENV_TYPES_H

#include <setjmp.h>

typedef struct SymValMap {
    Symbol symbol;
    LispObject value;
    struct SymValMap *next;
} *SymValMap;			/* Kepps the mapping between symbol and
				   value. */
typedef struct Environment {
    SymValMap map;
    struct Environment *next_env;
} *Environment;			/* Keeps a series of mapping described above. */

/* Environment used for block special form. */
typedef struct block_environment {
    Symbol name;
    jmp_buf context;
    struct block_environment *prev;
} *BlockEnvironment;

#endif
