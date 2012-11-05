CC= cc
CFLAGS= -Wall -g

OBJS= \
atom_proc.o \
cons.o \
environment.o \
eval_sexp.o \
hash_table.o \
init.o \
object.o \
parse_sexp.o \
primitives.o \
primitive/arit.o \
primitive/io.o \
primitive/logic.o \
print_sexp.o \
read_sexp.o \
stream.o \
symbol_table.o \
vm_stack.o

types.h: decls.h
env_types.h: decls.h

atom_proc.o: atom_proc.c cons.h object.h types.h symbol_table.h
	$(CC) -c $(CFLAGS) $< -o $@
cons.o: cons.c types.h
	$(CC) -c $(CFLAGS) $< -o $@
environment.o: environment.c atom_proc.h env_types.h print_sexp.h symbol_table.h types.h
	$(CC) -c $(CFLAGS) $< -o $@
eval_sexp.o: eval_sexp.c types.h environment.h atom_proc.h primitives.h stream.h
	$(CC) -c $(CFLAGS) $< -o $@
hash_table.o: hash_table.c object.h types.h
	$(CC) -c $(CFLAGS) $< -o $@
init.o: init.c atom_proc.h environment.h types.h primitive/arit.h primitive/io.h stream.h primitive/logic.h
	$(CC) -c $(CFLAGS) $< -o $@
object.o: object.c types.h
	$(CC) -c $(CFLAGS) $< -o $@
parse_sexp.o: parse_sexp.c atom_proc.h cons.h object.h symbol_table.h types.h
	$(CC) -c $(CFLAGS) $< -o $@
print_sexp.o: print_sexp.c atom_proc.h object.h stream.h types.h
	$(CC) -c $(CFLAGS) $< -o $@
primitive/arit.o: primitive/arit.c atom_proc.h cons.h types.h
	$(CC) -c $(CFLAGS) $< -o $@
primitive/io.o: primitive/io.c atom_proc.h cons.h stream.h types.h
	$(CC) -c $(CFLAGS) $< -o $@
primitive/logic.o: primitive/logic.c atom_proc.h cons.h types.h
	$(CC) -c $(CFLAGS) $< -o $@
primitives.o: primitives.c types.h
	$(CC) -c $(CFLAGS) $< -o $@
read_sexp.o: read_sexp.c
	$(CC) -c $(CFLAGS) $< -o $@
stream.o: stream.c types.h object.h atom_proc.h
	$(CC) -c $(CFLAGS) $< -o $@
symbol_table.o: symbol_table.c symtbl.h types.h
	$(CC) -c $(CFLAGS) $< -o $@
vm_stack.o: vm_stack.c atom_proc.h print_sexp.h stream.h types.h
	$(CC) -c $(CFLAGS) $< -o $@

liutcl.o: liutcl.c env_types.h eval_sexp.h environment.h init.h parse_sexp.h print_sexp.h read_sexp.h stream.h types.h vm_stack.h
	$(CC) -c $(CFLAGS) $< -o $@
liutcl: liutcl.o $(OBJS)
	$(CC) $(CFLAGS) $^ -o $@

xcons.o: xcons.c types.h cons.h
	$(CC) -c $(CFLAGS) $< -o $@
xcons: xcons.o $(OBJS)
	$(CC) $(CFLAGS) $^ -o $@

xread-print.o: xread-print.c stream.h
	$(CC) -c $(CFLAGS) $< -o $@
xread-print: xread-print.o atom_proc.o cons.o object.o stream.o symbol_table.o
	$(CC) $(CFLAGS) $^ -o $@

xread_sexp.o: xread_sexp.c read_sexp.h
	$(CC) -c $(CFLAGS) $< -o $@
xread_sexp: xread_sexp.o read_sexp.o
	$(CC) $(CFLAGS) $^ -o $@

.PHONY: clean
clean:
	rm *.o xread-print
