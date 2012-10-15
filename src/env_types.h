#ifndef ENV_TYPES_H
#define ENV_TYPES_H

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

#endif
