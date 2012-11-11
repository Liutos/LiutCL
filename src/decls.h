#ifndef DECLS_H
#define DECLS_H

typedef enum bool_t BOOL;
typedef struct block_environment_t *BlockEnvironment;
typedef struct environment_t *Environment;
typedef struct frame_t *Frame;
typedef struct go_environment_t *GoEnvironment;
typedef struct hash_table_t *hash_table_t;
typedef struct lisp_object_t *LispObject;
typedef struct env_entry_t *EnvEntry;
typedef struct env_entry_t *env_entry_t;

typedef LispObject Atom;
typedef LispObject Boolean;
typedef LispObject Character;
typedef LispObject Cons;
typedef LispObject Fixnum;
typedef LispObject Float;
typedef LispObject Function;
typedef LispObject HashTable;
/* Type `List' means proper list */
typedef LispObject List;
typedef Atom Number;
typedef LispObject Package;
typedef LispObject Ratio;
typedef LispObject Stream;
typedef LispObject String;
typedef LispObject Symbol;
typedef LispObject Values;
typedef LispObject Vector;
typedef LispObject (*primitive_t)(Frame, Environment, Environment, Environment, BlockEnvironment, GoEnvironment);

extern Symbol lt_nil;
extern Symbol lt_t;

extern jmp_buf toplevel;

#endif
