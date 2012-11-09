#ifndef DECLS_H
#define DECLS_H

typedef enum bool_t BOOL;
typedef struct block_environment_t *BlockEnvironment;
typedef struct environment_t *Environment;
typedef struct go_environment_t *GoEnvironment;
typedef struct lisp_object_t *LispObject;
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
typedef LispObject Package;
typedef LispObject Ratio;
typedef LispObject Stream;
typedef LispObject String;
typedef LispObject Symbol;
typedef LispObject Vector;
typedef LispObject (*primitive_t)(LispObject, Environment, Environment, Environment, BlockEnvironment, GoEnvironment);
typedef struct env_entry_t *env_entry_t;

extern Symbol lt_nil;
extern Symbol lt_t;

#endif
