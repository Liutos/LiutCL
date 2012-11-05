#ifndef DECLS_H
#define DECLS_H

typedef enum bool_t BOOL;
typedef struct lisp_object_t *LispObject;
typedef LispObject Atom;
typedef LispObject Boolean;
typedef LispObject Character;
typedef LispObject Cons;
typedef LispObject Fixnum;
typedef LispObject Function;
typedef LispObject HashTable;
typedef LispObject List;
typedef LispObject Package;
typedef LispObject Stream;
typedef LispObject String;
typedef LispObject Symbol;
typedef LispObject Vector;
typedef LispObject (*primitive_t)(LispObject);
typedef struct block_environment_t *BlockEnvironment;
typedef struct env_entry_t *env_entry_t;
typedef struct environment_t *Environment;

#endif
