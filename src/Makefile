CC= cc
CFLAGS= -Wall -g -std=c99 -D_GNU_SOURCE
CPPFLAGS=

OBJS= \
atom_proc.o \
cons.o \
environment.o \
eval_sexp.o \
function.o \
hash_table.o \
init.o \
object.o \
parse_sexp.o \
primitive/arit.o \
primitive/cons_proc.o \
primitive/io.o \
primitive/logic.o \
primitive/object_proc.o \
print_sexp.o \
read_sexp.o \
stream.o \
symbol_table.o \
vm_stack.o

types.h: decls.h
env_types.h: decls.h

atom_proc.o: atom_proc.c cons.h object.h types.h symbol_table.h

cons.o: cons.c types.h

environment.o: environment.c atom_proc.h env_types.h print_sexp.h symbol_table.h types.h

eval_sexp.o: eval_sexp.c types.h environment.h atom_proc.h primitives.h stream.h

function.o: function.c types.h

hash_table.o: hash_table.c object.h types.h

init.o: init.c atom_proc.h environment.h types.h primitive/arit.h primitive/cons_proc.h primitive/io.h primitive/object_proc.h primitive/logic.h stream.h

object.o: object.c types.h

parse_sexp.o: parse_sexp.c atom_proc.h cons.h object.h symbol_table.h types.h

print_sexp.o: print_sexp.c atom_proc.h object.h stream.h types.h

primitive/arit.o: primitive/arit.c atom_proc.h cons.h primitive/pdecls.h types.h

primitive/cons_proc.o: primitive/cons_proc.c cons.h types.h primitive/pdecls.h

primitive/io.o: primitive/io.c atom_proc.h cons.h stream.h types.h

primitive/logic.o: primitive/logic.c atom_proc.h cons.h types.h

primitive/object_proc.o: primitive/object_proc.c atom_proc.h cons.h object.h symbol_table.h types.h primitive/pdecls.h

primitive/spec.o: primitive/spec.c cons.h types.h

read_sexp.o: read_sexp.c

stream.o: stream.c types.h object.h atom_proc.h

symbol_table.o: symbol_table.c symtbl.h types.h

vm_stack.o: vm_stack.c atom_proc.h stream.h types.h


liutcl.o: liutcl.c env_types.h eval_sexp.h environment.h init.h parse_sexp.h print_sexp.h read_sexp.h stream.h types.h

liutcl: liutcl.o $(OBJS)
	$(CC) $(CFLAGS) $^ -o $@
liutcl2: liutcl.o $(OBJS) primitive/spec.o
	$(CC) $(CFLGAS) -DFS $^ -o liutcl

xcons.o: xcons.c types.h cons.h

xcons: xcons.o $(OBJS)
	$(CC) $(CFLAGS) $^ -o $@

xread-print.o: xread-print.c stream.h

xread-print: xread-print.o atom_proc.o cons.o object.o stream.o symbol_table.o
	$(CC) $(CFLAGS) $^ -o $@

xread_sexp.o: xread_sexp.c read_sexp.h

xread_sexp: xread_sexp.o read_sexp.o
	$(CC) $(CFLAGS) $^ -o $@

.PHONY: clean
clean:
	rm *.o xread-print
