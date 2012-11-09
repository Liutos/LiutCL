#ifndef ENV_TYPES_H
#define ENV_TYPES_H

#include <setjmp.h>

#include "decls.h"

struct block_environment_t {
    Symbol name;
    jmp_buf context;
    BlockEnvironment prev;
};

struct env_entry_t {
    Symbol symbol;
    LispObject value;
    env_entry_t next;
};

struct environment_t {
    env_entry_t map;
    Environment next;
};

struct go_environment_t {
    List tags;
    jmp_buf context;
    GoEnvironment prev;
};

#endif
