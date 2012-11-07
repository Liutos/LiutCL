CC= cc
CFLAGS= -Wall -g -std=c99 -D_GNU_SOURCE
CPPFLAGS=

OBJS= \
arit.o \
atom_proc.o \
cons_proc.o \
cons.o \
environment.o \
eval_sexp.o \
function.o \
hash_table.o \
init.o \
io.o \
logic.o \
object_proc.o \
object.o \
parse_sexp.o \
print_sexp.o \
read_sexp.o \
stream.o \
symbol_table.o \
vm_stack.o

# Header files
env_types.h: decls.h
types.h: decls.h

# Source files
arit.o: arit.c atom_proc.h cons.h pdecls.h types.h

atom_proc.o: atom_proc.c cons.h object.h types.h symbol_table.h

cons_proc.o: cons_proc.c cons.h types.h pdecls.h

cons.o: cons.c types.h

environment.o: environment.c atom_proc.h env_types.h print_sexp.h symbol_table.h types.h

eval_sexp.o: eval_sexp.c types.h environment.h atom_proc.h primitives.h stream.h

function.o: function.c types.h

hash_table.o: hash_table.c object.h types.h

init.o: init.c atom_proc.h environment.h types.h arit.h cons_proc.h io.h object_proc.h logic.h stream.h

io.o: io.c atom_proc.h cons.h stream.h types.h

logic.o: logic.c atom_proc.h cons.h types.h

object_proc.o: object_proc.c atom_proc.h cons.h object.h symbol_table.h types.h pdecls.h

object.o: object.c types.h

parse_sexp.o: parse_sexp.c atom_proc.h cons.h object.h symbol_table.h types.h

print_sexp.o: print_sexp.c atom_proc.h object.h stream.h types.h

spec.o: spec.c cons.h types.h

read_sexp.o: read_sexp.c

stream.o: stream.c types.h object.h atom_proc.h

symbol_table.o: symbol_table.c symtbl.h types.h

vm_stack.o: vm_stack.c atom_proc.h stream.h types.h

# Executables
liutcl.o: liutcl.c env_types.h eval_sexp.h environment.h init.h parse_sexp.h print_sexp.h read_sexp.h stream.h types.h

liutcl: liutcl.o $(OBJS)
	$(CC) $(CFLAGS) $^ -o $@
liutcl2: liutcl.o $(OBJS) spec.o
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
